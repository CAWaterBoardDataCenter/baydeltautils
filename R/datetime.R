# datetime.R
#
# Functions for dealing with dates and times.

#' Return the water year for a date or vector of dates.
#'
#' Dates must be in one of the date formats supported by lubridate.
#'
#' @param date in Date or POSIXt format (scalar or vector).
#' @param wyStart (optional) first month in the water year as integer, if
#' different from California convention of October
#' @param offset (optional) signed integer offset between the calendar year
#' and water year in the first month of the water year; set equal to 0 if
#' the water year begins during the calendar year
#' @export
waterYear <- function(date, wyStart=10, offset=1) {
  waterYear <- lubridate::year(date) + (lubridate::month(date) > (wyStart-1)) +
    offset - 1
  return(waterYear)
}

#' Return the month from one or more dates as a factor in water year order.
#'
#' This is a generic function that dispatches the appropriate S3 method.
#' Operates on scalar or vector arguments.
#'
#' Default order is "Oct", "Nov", ..., "Sep" per the convention in use in
#' California.
#'
#' @param date in Date, POSIXt, character (ymd or name of month), or integer
#' (month) format (scalar or vector).
#' @param wyStart (optional) first month in the water year as integer, if
#' different from California convention of October
#' @export
wyMonth <- function(date, wyStart=10) {
  UseMethod("wyMonth", date)
}

#' S3 method implementing \code{wyMonth} for dates in Date format.
#'
#' @param date date in Date format
#' @param wyStart (optional) first month in the water year as integer, if
#' different from California convention of October
#' @export
wyMonth.Date <- function(date, wyStart=10) {
  mo <- lubridate::month(date)
  return(factor(month.abb[mo], month.abb[c(seq(wyStart,12), seq(1,wyStart-1))]))
}

#' S3 method implementing \code{wyMonth} for dates in POSIXt format.
#'
#' @param date date in Date format
#' @param wyStart (optional) first month in the water year as integer, if
#' different from California convention of October
#' @export
wyMonth.POSIXt <- function(date, wyStart=10) {
  mo <- lubridate::month(date)
  return(factor(month.abb[mo], month.abb[c(seq(wyStart,12), seq(1,wyStart-1))]))
}


#' S3 method implementing \code{wyMonth} for character strings (ymd format or
#' long or short name of month).
#'
#' Vector arguments must be consistent in format. Do not mix ymd strings with
#' month names from month.abb or month.name. Do not mix names from month.abb
#' with month.name. Month names are case insensitive, however.
#'
#' @param date date string in ymd format or month name (element of month.abb or
#' month.name from R base)
#' @param wyStart (optional) first month in the water year as integer, if
#' different from California convention of October
#' @export
wyMonth.character <- function(date, wyStart=10) {
  if (setequal(union(tolower(unique(date)), tolower(month.abb)),
               tolower(month.abb))) {
    mo <- as.numeric(factor(tolower(date), tolower(month.abb)))
  }
  else if (setequal(union(tolower(unique(date)), tolower(month.name)),
                     tolower(month.name))) {
    mo <- as.numeric(factor(tolower(date), tolower(month.name)))
  }
  else {
    mo <- lubridate::month(lubridate::ymd(date))
  }
  return(factor(month.abb[mo], month.abb[c(seq(wyStart,12), seq(1,wyStart-1))]))
}

#' S3 method implementing \code{wyMonth} for months specified as integers.
#'
#' @param date month as integer
#' @param wyStart (optional) first month in the water year as integer, if
#' different from California convention of October
#' @export
wyMonth.integer <- function(date, wyStart=10) {
  return(factor(month.abb[date], month.abb[c(seq(wyStart,12), seq(1,wyStart-1))]))
}

#' S3 method implementing \code{wyMonth} for months specified as numeric values.
#'
#' @param date month as numeric
#' @param wyStart (optional) first month in the water year as integer, if
#' different from California convention of October
#' @export
wyMonth.numeric <- function(date, wyStart=10) {
  mo <- as.integer(date)
  return(factor(month.abb[mo], month.abb[c(seq(wyStart,12), seq(1,wyStart-1))]))
}


#' Return the date of the last day in the month for a given date.
#'
#' This is a generic function that dispatches the appropriate S3 method.
#' Operates on scalar or vector arguments.
#'
#' @param date date in POSIXt, Date, or ymd string
#' @export
eomDate <- function(date) {
  UseMethod("eomDate", date)
}

#' S3 method implementing \code{eomDate} for character strings in ymd format.
#'
#' @param date date string in ymd format
#' @export
eomDate.character <- function(date) {
  date <- lubridate::ymd(date)
  return(lubridate::floor_date(date, "month") + months(1) - lubridate::days(1))
}

#' S3 method implementing \code{eomDate} for dates specified with class Date.
#'
#' @param date date of class Date
#' @export
eomDate.Date <- function(date) {
  return(lubridate::floor_date(date, "month") + months(1) - lubridate::days(1))
}

#' S3 method implementing \code{eomDate} for dates specified with class POSIXt.
#'
#' @param date date or datetime of class POSIXt
#' @export
eomDate.POSIXt <- function(date) {
  date <- as.Date(date, tz=lubridate::tz(date))
  return(lubridate::floor_date(date, "month") + months(1) - lubridate::days(1))
}

#' Return the date of the last day in the month for a given year, month pair.
#'
#' @param year year as integer
#' @param month month as integer or string from \code{month.abb}
#' @export
eomDate_ym <- function(year, month) {
  if ((is.factor(month) || is.character(month))
      && setequal(union(tolower(as.character(month)), tolower(month.abb)),
                  tolower(month.abb))) {
    month <- match(tolower(month), tolower(month.abb))
  }
  date <- lubridate::ymd(sprintf("%d-%02d", year, month), truncated = 1)
  return(lubridate::floor_date(date, "month") + months(1) - lubridate::days(1))
}

#' Return the date of the last day of the month in a given water year, month
#' pair.
#'
#' @param wy water year as integer
#' @param month month as integer or string from \code{month.abb}
#' @param wyStart (optional) first month in the water year as integer, if
#' different from California convention of October
#' @export
eomDate_wym <- function(wy, month, wyStart=10) {
  if ((is.factor(month) || is.character(month))
      && setequal(union(tolower(as.character(month)), tolower(month.abb)),
                  tolower(month.abb))) {
    month <- match(tolower(month), tolower(month.abb))
  }
  year <- wy - ifelse(month>(wyStart-1), 1, 0)
  return(eomDate_ym(year, month))
}

#' Return the date of the last day in the quarter for a given date.
#'
#' This is a generic function that dispatches the appropriate S3 method.
#' Operates on scalar or vector arguments.
#'
#' @param date date in POSIXt, Date, or ymd string
#' @export
eoqDate <- function(date) {
  UseMethod("eoqDate", date)
}

#' S3 method implementing \code{eoqDate} for character strings in ymd format.
#'
#' @param date date string in ymd format
#' @export
eoqDate.character <- function(date) {
  date <- lubridate::ymd(date)
  return(eoqDate.Date(date))
}

#' S3 method implementing \code{eoqDate} for dates specified with class Date.
#'
#' @param date date of class Date
#' @export
eoqDate.Date <- function(date) {
  y <- lubridate::year(date)
  m <- lubridate::quarter(date)*3
  date <- lubridate::ymd(sprintf("%d-%02d-01", y, m))
  return(lubridate::floor_date(date, "month") + months(1) - lubridate::days(1))
}

#' S3 method implementing \code{eoqDate} for dates specified with class POSIXt.
#'
#' @param date date or datetime of class POSIXt
#' @export
eoqDate.POSIXt <- function(date) {
  date <- as.Date(date, tz=lubridate::tz(date))
  return(eoqDate.Date(date))
}
