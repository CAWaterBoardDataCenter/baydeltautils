# conversions.R
#
# Functions for unit conversion.

#' Convert a monthly time series of volumes or flows to thousand acre-feet.
#'
#' The data frame should contain a column named "unit" that specifies the source
#' units for each row of data. This function can accommodate data frames of
#' mixed units, provided that all variables in a given row share the same units.
#'
#' The current implementation recognizes the following units:
#' \itemize{
#'  \item cubic_feet_per_second
#'  \item million_acre_feet
#'  \item thousand_acre_feet
#'  \item acre_feet
#' }
#'
#' @param data data frame containing time series to be converted
#' @param ignore_cols vector of names of numeric columns not to convert. By
#' default, ignores any column named "timestep", "month", "year", or "wy".
#' @export
taf_monthly <- function(data, ignore_cols=NULL) {
  # conversion factors to TAF
  rate_conversion <- list(cubic_feet_per_second=86400/43560/1000)
  volume_conversion <- list(thousand_acre_feet=1, million_acre_feet=1e3,
                            acre_feet=1e-3)
  rval <- convert_monthly(data, ignore_cols=ignore_cols,
                          rate_conversion=rate_conversion,
                          volume_conversion=volume_conversion,
                          destination_type="volume",
                          destination_unit="thousand_acre_feet")
  return(rval)
}

#' Convert a monthly time series of volumes or flows to cubic feet per second.
#'
#' The data frame should contain a column named "unit" that specifies the source
#' units for each row of data. This function can accommodate data frames of
#' mixed units, provided that all variables in a given row share the same units.
#'
#' The current implementation recognizes the following units:
#' \itemize{
#'  \item cubic_feet_per_second
#'  \item cubic_meters_per_second (needs testing)
#'  \item million_acre_feet
#'  \item thousand_acre_feet
#'  \item acre_feet
#'  \item cubic_meters (needs testing)
#'  \item cubic_kilometers (needs testing)
#' }
#'
#' @param data data frame containing time series to be converted
#' @param ignore_cols vector of names of numeric columns not to convert. By
#' default, ignores any column named "timestep", "month", "year", or "wy".
#' @export
cfs_monthly <- function(data, ignore_cols=NULL) {
  # conversion factors to cfs
  rate_conversion <- list(cubic_feet_per_second=1,
                          cubic_meters_per_second=0.3048^-3) # test!
  volume_conversion <- list(million_acre_feet=43560*1e6/86400,
                            thousand_acre_feet=43560*1000/86400,
                            acre_feet=43560/86400,
                            cubic_meters=0.3048^-3/86400, # test!
                            cubic_kilometers=0.3048^-3*1e9/86400) # test!
  rval <- convert_monthly(data, ignore_cols=ignore_cols,
                          rate_conversion=rate_conversion,
                          volume_conversion=volume_conversion,
                          destination_type="rate",
                          destination_unit="cubic_feet_per_second")
  return(rval)
}

#' Convert a monthly time series of volumes or flows between unit systems.
#'
#' The data frame should contain a column named "unit". Although this function
#' can be invoked directly by specifying conversion factors, it is primarily
#' intended to be a utility function to ease implementation of other conversion
#' functions.
#'
#' @param data data frame containing time series to be converted
#' @param ignore_cols vector of names of numeric columns not to convert. By
#' default, ignores any column named "timestep", "month", "year", or "wy".
#' @param rate_conversion a list of conversion factors to convert daily rates to
#' the desired destination units; named for the source units
#' @param volume_conversion a list of conversion factors to convert monthly
#' volumes to the desired destination units; named for the source units
#' @param destination_type either "rate" or "volume" according to the type of
#' destination units
#' @param destination_unit name of destination units
#' @export
convert_monthly <- function(data, ignore_cols=NULL, rate_conversion,
                            volume_conversion, destination_type,
                            destination_unit) {
  # specify known units to ignore
  ignore_units <- c("feet", "meter")
  # specify which columns to ignore
  ignore_cols <- c("timestep", "month", "year", "wy", ignore_cols)
  # get the class of each column
  classes <- lapply(data, class)
  # make vectors of names of columns to convert (cols) and leave alone (other)
  cols <- setdiff(names(data)[which(classes %in% c("numeric", "integer"))],
                  ignore_cols)
  other <- setdiff(names(data), cols)
  # perform the conversions
  # -- get the units
  units <- unique(data$unit)
  convert <- list()
  for (i in seq_along(units)) {
    do.convert <- TRUE
    this.unit <- units[[i]]
    convert[[i]] <- data[which(data$unit==this.unit),]
    if (this.unit %in% names(rate_conversion) && destination_type=="volume") {
      cfactor <- outer(lubridate::day(convert[[i]]$date),
                       rep(rate_conversion[[this.unit]], length(cols)))
    }
    else if (this.unit %in% names(rate_conversion) && destination_type=="rate") {
      cfactor <- outer(rep(1, nrow(convert[[i]])),
                       rep(rate_conversion[[this.unit]], length(cols)))
    }
    else if (this.unit %in% names(volume_conversion) && destination_type=="volume") {
      cfactor <- outer(rep(1, nrow(convert[[i]])),
                       rep(volume_conversion[[this.unit]], length(cols)))
    }
    else if (this.unit %in% names(volume_conversion) && destination_type=="rate") {
      cfactor <- outer(1/lubridate::day(convert[[i]]$date),
                       rep(volume_conversion[[this.unit]], length(cols)))
    }
    else if (this.unit %in% ignore_units) {
      do.convert <- FALSE
    }
    else {
      message(sprintf("unit = %s, destination_type = %s", this.unit, destination_type))
      stop("Unrecognized unit or type of destination unit.")
    }
    # data.frame() needed on RHS to convert from matrix to data frame columns
    if (do.convert) {
      convert[[i]][,cols] <- data.frame(cfactor*convert[[i]][,cols])
      convert[[i]]$unit <- destination_unit
    }
  }
  rval <- Reduce(rbind, convert)
  return(rval)
}

#' Convert a sequence of daily flows in cfs to discharges in TAF.
#'
#' @param flow a sequence of flow rates in cfs
#' @param days (optional) number of days over which to accumulate flows
#' @export
cfs2TAF <- function(flow, days=1) {
  days*flow*86400/43560/1000
}

#' Convert a data frame of monthly flows in cfs to total monthly volume in TAF.
#' The data frame should consist of monthly average flows and contain a column
#' named "date" of class POSIXt or Date. All columns of class "numeric" are
#' taken to be monthly average flows in cfs. All other columns beside "date" are
#' discarded.
#'
#' This function should be rendered obsolete by \code{taf_monthly}, but is
#' retained for backward compatibility with earlier scripts.
#'
#' @param data a data frame formatted as described above
#' @export
cfs2TAF.monthly <- function(data) {
  # get the class of each column
  classes <- lapply(data, class)
  volume <- cfs2TAF(lubridate::day(data$date)*data[,which(classes=="numeric")])
  volNames <- names(data)[which(classes=="numeric")]
  other <- data[,which(classes!="numeric")]
  otherNames <- names(data)[which(classes!="numeric")]
  return(stats::setNames(data.frame(other, volume), c(otherNames, volNames)))
}

#' Convert a sequence of daily discharges in TAF to daily flows in cfs.
#'
#' @param vol a sequence of daily or multi-day discharges in TAF
#' @param days (optional) number of days over which discharges are specified
#' @export
TAF2cfs <- function(vol, days=1) {
  vol*43560*1000/86400/days
}

#' Convert a sequence of daily discharges in AF to daily flows in cfs.
#'
#' @param vol a sequence of daily or multi-day discharges in AF
#' @param days (optional) number of days over which discharges are specified
#' @export
AF2cfs <- function(vol, days=1) {
  vol*43560/86400/days
}

#' Convert a data frame of total monthly discharges in TAF to average monthly
#' flows in cfs.
#'
#' Behaves as cfs2TAF.monthly, but as its inverse.
#'
#' This function should be rendered obsolete by \code{cfs_monthly}, but is
#' retained for backward compatibility with earlier scripts.
#'
#' @param data a data frame formatted as described above
#' @export
TAF2cfs.monthly <- function(data) {
  # get the class of each column
  classes <- lapply(data, class)
  flow <- TAF2cfs(data[,which(classes=="numeric")]/lubridate::day(data$date))
  flowNames <- names(data)[which(classes=="numeric")]
  other <- data[,which(classes!="numeric")]
  otherNames <- names(data)[which(classes!="numeric")]
  return(stats::setNames(data.frame(other, flow), c(otherNames, flowNames)))
}

#' Convert a data frame of total monthly discharges in AF to average monthly
#' flows in cfs.
#'
#' Behaves as TAF2cfs.monthly, but assumes that input are in AF.
#'
#' This function should be rendered obsolete by \code{cfs_monthly}, but is
#' retained for backward compatibility with earlier scripts.
#'
#' @param data a data frame formatted as described above
#' @export
AF2cfs.monthly <- function(data) {
  # get the class of each column
  classes <- lapply(data, class)
  flow <- AF2cfs(data[,which(classes=="numeric")]/lubridate::day(data$date))
  flowNames <- names(data)[which(classes=="numeric")]
  other <- data[,which(classes!="numeric")]
  otherNames <- names(data)[which(classes!="numeric")]
  return(stats::setNames(data.frame(other, flow), c(otherNames, flowNames)))
}
