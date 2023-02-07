# data_import.R
#
# Functions for data import.

#' Import flow data exported from WEAP in the ReadFromFile csv format.
#'
#' @param file the full path to the file to read, or the relative path if
#' data.dir is set to a base path
#' @param divisor (optional) divisor for all flow or volume variables (e.g.,
#' if you want to convert data from AF to TAF)
#' @param discardCA whether or not to discard the current accounts year
#' @param endYear an optional year at which to truncate the data
#' @param include.sum whether or not to include a sum of all flow or volume
#' columns if absent
#' @param data.dir optional base path on the filesystem
#' @param date.only whether to exclude "timestep" and "year" columns from the
#' returned data frame
#' @param skip (optional) number of lines to skip before the header row
#' @param narrow (optional) if TRUE, convert data frame to narrow format
#' @export
readExportedWEAP <- function(file, divisor=1, discardCA=TRUE,
                             endYear=NULL, include.sum=FALSE,
                             data.dir=NULL, date.only=FALSE, skip=5,
                             narrow=FALSE) {
  file <- ifelse(is.null(data.dir), file, paste(data.dir, file, sep="/"))
  metadata <- readLines(file, n=3)
  description <- gsub("[[:space:]]+", "_",
                   trimws(gsub("[^[:alnum:]]", " ", metadata[1])))
  # get key-value pairs from the second line of metadata; we will parse them
  # later, when it comes time to add columns to the data frame
  keyvals <- strsplit(metadata[2], ",")[[1]]
  # parse metadata line 3 to get the unit of measurement
  unit <- gsub("[[:space:]]+", "_",
               trimws(tolower(strsplit(metadata[3], "=")[[1]][2])))
  # sometimes we get trailing commas if files have been edited in Excel; remove
  # them
  unit <- gsub(",", "", unit)
  # WEAP_name_pattern <- "([0-9].?)\\.\\.\\..+"
  # WEAP_name_replace <- "\\1"
  WEAP_delete_front <- "^.*\\\\ "
  WEAP_delete_back <- "\\[.*$"
  # we have 5 lines of pre-header information
  # -- get the header information
  header <- utils::read.csv(file, skip=skip, nrows=1, header=FALSE)
  # -- get the data
  df <- utils::read.csv(file, skip=skip+1, header=FALSE)
  df[,-c(1,2)] <- df[,-c(1,2)]/divisor
  # -- clean up the header information
  splitHeader <- strsplit(as.character(unlist(header)), "\\\\")
  h1 <- lapply(splitHeader, "[", 1)
  h2 <- lapply(splitHeader, "[", 2)
  header <- ifelse(grepl("Reach", h2) | grepl("Headflow", h2) | is.na(h2), h1, h2)
  header <- trimws(header)
  header[1:2] <- c("year", "timestep")
  #header <- gsub(WEAP_delete_front, "", unlist(header))
  header <- gsub(WEAP_delete_back, "", header)
  names(df) <- make.names(header)
  #names(df)[1:2] <- c("year", "timestep")
  names(df) <- gsub("\\.+", "_", names(df))
  names(df) <- tolower(sub("X_", "", names(df)))
  # -- if the user requested a sum to be included, add it if absent
  if (include.sum & !is.element("Sum", names(df))) {
    df$Sum <- rowSums(df[,-c(1,2)])
  }
  # add a date column containing the last day in each month
  df$date <- eomDate_ym(year=df$year, month=df$timestep)
  # reorder columns to put the date up front
  df <- df[,c(ncol(df),1:(ncol(df)-1))]
  # add columns for variable, keys (e.g., "scenario"), and unit
  df$description <- description
  df$unit <- unit
  # make a list of identifier columns
  id_cols <- c("date", "year", "timestep", "description", "unit")
  # iterate over any remaining key-value pairs from the metadata and add
  # columns if values are not NA
  for (keyval in keyvals) {
    key <-
      tolower(gsub("[^[:alpha:]]", "",
                   strsplit(strsplit(keyval, ",")[[1]][1], ":")[[1]][1]))
    value <- trimws(strsplit(strsplit(keyval, ",")[[1]][1], ":")[[1]][2])
    if (!is.na(value)) {
      # add the column to the data frame
      df[[key]] <- value
      # add the name to the list of identifier columns
      id_cols <- append(id_cols, key)
    }
  }

  # discard current accounts year if requested
  if (discardCA==TRUE) {
    df <- subset(df, waterYear(date)>min(waterYear(date)))
  }
  if (!is.null(endYear)) {
    if (endYear < max(waterYear(df$date))) {
      df <- subset(df, waterYear(date)<=endYear)
    }
  }
  # discard the timestep and year columns if the user has requested
  # dates only
  if (date.only==TRUE) {
    df$timestep <- NULL
    df$year <- NULL
    # discard the names from id_cols as well
    id_cols <- id_cols[!(id_cols %in% c("timestep", "year"))]
  }
  # melt the data frame if requested by user
  if (narrow==TRUE) {
    df <- reshape2::melt(df, id.vars=id_cols)
  }
  return(df)
}

#' Import DSS data from an Excel (XLSX) file.
#'
#' @param file XLSX file containing one or more worksheets exported from a DSS
#' file
#' @param sheet integer index or name of sheet to read
#' @param dataRow first row that contains data
#' @param nameRow row to use for column names
#' @param debug if TRUE, print debugging messages
#' @export
readExportedDSS <- function(file, sheet=1, dataRow=8, nameRow=2, debug=FALSE) {
  data <- openxlsx::read.xlsx(file, sheet, startRow=dataRow-1, detectDates=T,
                              rowNames=TRUE)
  #data[1] <- NULL # delete the column of integer indices
  header <- openxlsx::read.xlsx(file, sheet, rows=c(nameRow-1,nameRow))
  if (debug) {
    print(header)
    print(names(data))
    print(ncol(data))
  }
  # sometimes we have one more entry in the header than columns in the data; in
  # that case we need to delete the first header entry
  if (length(header)==length(data)+1) { header[1] <- NULL }
  names(data) <- header
  names(data)[1] <- "date"
  if (class(data$date)=="character") {
    data$date <- lubridate::ymd(data$date)
  }
  else if (class(data$date)=="numeric") {
    data$date <- as.Date(data$date, origin="1899-12-30")
  }
  return(data)
}

#' Import DSS data from an Excel (XLSX) file, retaining full metadata.
#'
#' @param file XLSX file containing one or more worksheets exported from a DSS
#' file
#' @param sheet integer index or name of sheet to read
#' @param namePart which of the DSS parts (A-F) to use as variable names
#' @export
readExportedDSSFull <- function(file, sheet=1, namePart="B") {
  # Read in the data, starting in row 8
  data <- openxlsx::read.xlsx(file, sheet=sheet, startRow=8, colNames=FALSE,
                              rowNames=TRUE, detectDates=TRUE)
  # Read metadata from first 7 rows of file (empty columns are skipped by
  # read.xlsx, although this behavior is not documented)
  meta <- openxlsx::read.xlsx(file, sheet=sheet, rows=seq(1,7),
                              colNames=FALSE, rowNames=TRUE)
  # The metadata should have as many columns as we have data series (date not
  # included). This means that if we have the same number of metadata entries as
  # data columns, we need to delete the first (empty) metdata entry.
  if (length(meta)==length(data)) { meta[1] <- NULL }
  # Transpose the metadata
  meta <- data.frame(t(meta), stringsAsFactors=FALSE)
  # Set column names using default of Part B value or alternative specified by
  # user
  names(data) <- c("date", meta[[namePart]])
  # Format dates
  if (class(data$date)=="character") {
    data$date <- lubridate::ymd(data$date)
  }
  else if (class(data$date)=="numeric") {
    data$date <- as.Date(data$date, origin="1899-12-30")
  }
  data_melt <- reshape2::melt(data, id.vars="date")
  data <- merge(data_melt, meta, by.x="variable", by.y=namePart)
  # Make units conform to baydeltautils package conventions
  names(data)[which(names(data)=="Units")] <- "unit"
  data$unit[which(data$unit=="CFS")] <- "cubic_feet_per_second"
  data$unit[which(data$unit=="AF")] <- "acre_feet"
  data$unit[which(data$unit=="TAF")] <- "thousand_acre_feet"
  data$unit[which(data$unit=="MAF")] <- "million_acre_feet"
  # Reorder the columns to put the most pertinent information first
  firstNames <- c("date", "variable", "value", "unit")
  data <- data[,c(firstNames, setdiff(names(data), firstNames))]
  names(data) <- tolower(names(data))
  return(data)
}

#' Read a table of C2VSim results converted to reach-per-column format from the
#' default.
#'
#' @param sheet integer index or name of sheet to read
#' @param file name of XLSX file containing C2VSim results
#' @param short.names if TRUE, shorten names to form "REACH.xy"; if FALSE,
#' retain longer names
#' @export
readExportedC2VSim <- function(file, sheet, short.names=TRUE) {
  df <- openxlsx::read.xlsx(file, sheet=sheet, colNames=TRUE, startRow=4)
  if (short.names) {
    names(df) <- gsub("(REACH.[0-9]+).*", "\\1", names(df))
  }
  else {
    names(df) <- make.names(names(df))
    names(df) <- gsub("\\.+AF.$", "", names(df))
    names(df) <- gsub("\\.+", "_", names(df))
  }
  names(df)[1] <- "date"
  df$date <- lubridate::mdy_hm(df$date)-lubridate::days(1)
  return(df)
}

#' Adapter function for reading multiple XLSX sheets using lapply.
#'
#' TODO: clarify documentation.
#'
#' @param sheet integer index or name of sheet to read
#' @param ... additional arguments passed to read.xlsx
#' @export
read.xlsx_sheet <- function(sheet, ...) {
  openxlsx::read.xlsx(..., sheet=sheet)
}
