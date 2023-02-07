# data_retrieval
#
# Functions for retrieving data from internet sources.

#' Retrieve data from CDEC for a single sensor at a single station.
#'
#' TODO: Reassess how we want to represent date and time in the returned data.
#' At present we are returning a bunch of individual fields representing time,
#' day, month, year, and WY. This is convenient when we want to aggregate over
#' different time scales, but it is redundant and makes merging data frames a
#' pain. It would be better to use a single datetime field and use functions to
#' extract, day, month, year, or WY. Maybe we can already do this and it's just
#' a matter of tracking down dependencies.
#'
#' @param station CDEC three character station code (case insensitive)
#' @param sensor CDEC numeric sensor code [TODO: document codes]
#' @param duration CDEC single character duration code (event=E, hourly=H,
#' daily=D, or monthly=M)
#' @param startDate start date for time series
#' @param endDate end date for time series
#' @param mode "retrieve" to retrieve data, "url" to return URL
#' @export
getCDECData <- function(station, sensor, duration="M",
                        startDate="1900-10-01", endDate=Sys.Date(),
                        mode="retrieve") {
  baseURL <- "http://cdec.water.ca.gov/dynamicapp/req/CSVDataServlet?"
  queryURL <- paste(baseURL, "Stations=", station, "&SensorNums=",
                    as.character(sensor), "&dur_code=", duration,
                    "&Start=", format.Date(startDate, "%Y-%m-%d"),
                    "&End=", format.Date(endDate, "%Y-%m-%d"), sep="")
  if (mode=="retrieve") {
    data <- utils::read.csv(queryURL, skip=1, na.strings="m", quote="'")
    names(data)[1:3] <- c("date", "time", paste(station, as.character(sensor),
                                                sep="_"))
    data$date <- as.Date(as.character(data$date), "%Y%m%d")
    data$time <- lubridate::force_tz(
      data$date + lubridate::hm(sprintf("%.02f", data$time/100)),
      tz="Etc/GMT-8")
    data$year <- as.numeric(format.Date(data$date, "%Y"))
    data$month <- as.numeric(format.Date(data$date, "%m"))
    data$day <- as.numeric(format.Date(data$date, "%d"))
    data$WY <- waterYear(data$date)
    return(data)
  }
  else { return(queryURL) }
}

#' Retrieve data from CDEC for multiple sensors at multiple stations.
#'
#' @param stations n-element vector of station IDs
#' @param sensors n-element vector of sensor IDs
#' @param duration common duration code
#' @param startDate start date for time series
#' @param endDate end date for time series
#' @export
getCDECMulti <- function(stations, sensors, duration="D",
                         startDate="1900-10-01", endDate=Sys.Date()) {
  dataFrames <- list()
  for (i in seq_along(stations)) {
    dataFrames[[i]] <- getCDECData(stations[i], sensors[i], duration,
                                   startDate, endDate)
  }
  df <- Reduce(function(...) merge(...,
                                   by=c("date", "time", "day", "month",
                                        "year", "WY"), all=T), dataFrames)
}

#' Return a data frame containing the daily sum of full natural flows for Eight
#' River Index stations in cubic feet per second.
#'
#' @param startDate start date for time series
#' @param endDate end date for time series
#' @param raw if TRUE, return a data frame with daily values for each station;
#' if FALSE, return a data frame containing only the summed index
#' @export
eightRiverDaily <- function(startDate=as.Date("1999-10-01"),
                            endDate=Sys.Date(), raw=FALSE) {
  dailyStations <- c("BND", "ORO", "YRS", "FOL",
                     "NML", "TLG", "MRC", "MIL")

  df <- data.frame(date=seq.Date(as.Date(startDate), as.Date(endDate), by=1))
  for (station in dailyStations) {
    df <- merge(df, getCDECData(station, 8, "D", startDate=startDate,
                                endDate=endDate)[,c(1,3)], by="date")
  }

  if (raw)
    return(df)
  else
    return(data.frame(date=df$date, ERI=rowSums(df[,2:9])))
}

#' Return a data frame containing the monthly full natural flow volumes for the
#' Eight River Index stations in acre-feet.
#'
#' @param startDate start date for time series
#' @param endDate end date for time series
#' @param raw if TRUE, return a data frame with monthly values for each station;
#' if FALSE, return a data frame containing only the summed index
#' @export
eightRiverMonthly <- function(startDate=as.Date("1905-10-01"),
                              endDate=Sys.Date(), raw=FALSE) {
  monthlyStations <- c("SBB", "FTO", "YRS", "AMF",
                       "SNS", "TLG", "MRC", "SJF")

  df <- data.frame(date=seq.Date(startDate, endDate, by="month"))
  for (station in monthlyStations) {
    tmp.df <- getCDECData(station, 65, "M", startDate=startDate)[,c(1,3)]
    df <- merge(df, getCDECData(station, 65, "M", startDate=startDate,
                                endDate=endDate)[,c(1,3)], by="date")
  }

  if (raw) { return(df) }
  out.df <- data.frame(date=df$date, ERI=rowSums(df[,2:9]))
  return(out.df)
}

#' Return a data frame containing the monthly full natural flow volumes for the
#' Sacramento Valley Index stations in acre-feet.
#'
#' @param startDate start date for time series
#' @param endDate end date for time series
#' @param raw if TRUE, return a data frame with monthly values for each station;
#' if FALSE, return a data frame containing only the summed index
#' @export
SacFourRiverMonthly <- function(startDate=as.Date("1905-10-01"),
                                endDate=Sys.Date(), raw=FALSE) {
  monthlyStations <- c("SBB", "FTO", "YRS", "AMF")
  # CDEC sensor: monthly full natural flow in acre-feet
  sensor <- 65
  df <- data.frame(date=seq.Date(startDate, endDate, by="month"))
  for (station in monthlyStations) {
    tmp.df <- getCDECData(station, 65, "M", startDate=startDate)[,c(1,3)]
    df <- merge(df, getCDECData(station, 65, "M", startDate=startDate,
                                endDate=endDate)[,c(1,3)], by="date")
  }

  if (raw) { return(df) }
  out.df <- data.frame(date=df$date, FRI=rowSums(df[,2:5]))
  return(out.df)
}

#' Downloads and compile the Dayflow data set from
#' http://www.water.ca.gov/dayflow/output/Output.cfm.
#'
#' @param files list of files in form list(merged=list(), single=list())
#'
#' @export
getDayflow <- function(files) {
  # Retrieve merged files and reduce them to a single data frame
  merged.dfs <- lapply(files$merged, utils::read.csv)
  merged.dfs <- lapply(merged.dfs, function(x) stats::setNames(x, tolower(names(x))))
  merged.names <- Reduce(intersect, lapply(merged.dfs, names))
  dayflow <- Reduce(rbind, lapply(merged.dfs, "[", merged.names))
  # Add an empty column for X2, which was not computed prior to 1997
  dayflow$x2 <- NA
  # Change column names for compatibility with newer files
  names(dayflow)[which(names(dayflow)=="export")] <- "exports"
  names(dayflow)[which(names(dayflow)=="dive")] <- "diver"
  names(dayflow)[which(names(dayflow)=="effect")] <- "effec"
  names(dayflow)[which(names(dayflow)=="effd")] <- "effdiv"

  # Retrieve all of the per-year files and combine them with the results
  # from above
  #
  # These files have the following oddities:
  # 1. All lines except the header contain a trailing comma. This causes
  #    problems for read.csv, so we have to work around them.
  # 2. There is a summary table below the actual data with monthly totals
  #    for the flow values. We don't want to import this, so we have to
  #    specify the number of rows of data to read. This varies, due to leap
  #    years.
  num.rows <- lubridate::leap_year(as.numeric(names(files$single)))+365
  single.dfs <-
    mapply(function (x,y) utils::read.csv(file=x, nrows=y, header=F, skip=1,
                                   colClasses = c(rep(NA, 29), "NULL")),
           files$single, as.list(num.rows), SIMPLIFY = F)
  single.names <-
    lapply(files$single, function(x) strsplit(readLines(x, 1), ",")[[1]])
  single.dfs <- mapply(function(x,y) stats::setNames(x, tolower(y)), single.dfs,
                       single.names, SIMPLIFY=FALSE)
  dayflow <- Reduce(rbind, c(list(dayflow), single.dfs))

  # Format the date
  dayflow$date <- lubridate::dmy(dayflow$date)
  # Fix the date to correct for default parsing of years; we do this by
  # subtracting 100 years from any date greater than system time. We'll need
  # to make some changes if we're still using this script in WY 2056.
  dayflow$date <-
    dayflow$date - lubridate::years(100)*(lubridate::year(dayflow$date) >
                                            lubridate::year(Sys.time()))
  return(dayflow)
}
