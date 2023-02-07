# statistics.R
#
# Functions for summarizing data.

#' Return a closure that calculates the empirical exceedance function.
#'
#' Modified from stats::ecdf, which calculates the empirical cumulative
#' distribution function.
#'
#' @param x numeric vector with one or more non-missing values
#' @export
excf <- function(x) {
  x <- sort(x)
  n <- length(x)
  if (n < 1)
    stop("'x' must have 1 or more non-missing values")
  vals <- unique(x)
  rval <- stats::approxfun(vals, 1-cumsum(tabulate(match(x, vals)))/n,
                    method = "constant", yleft = 1, yright = 0, f = 0, ties = "ordered")
  class(rval) <- c("excf", "stepfun", class(rval))
  assign("nobs", n, envir = environment(rval))
  attr(rval, "call") <- sys.call()
  rval
}

#' Return a sequence of values constrained within an inclusive minimum and
#' maximum value.
#'
#' @param x sequence of orderable values
#' @param a minimum value
#' @param b maximum value
#' @export
min_max_threshold <- function(x, a, b) {
  return(ifelse(x<a, a, ifelse(x>b, b, x)))
}

#' Scale a data frame containing a time series in which all observations are
#' numeric, and any time index columns are represented as a type other than
#' numeric (e.g., integer, date, or POSIXt).
#'
#' @param a scaling factor
#' @param x data frame containing values to scale
#' @export
scale_ts <- function(a, x) {
  # get the class of each column
  classes <- lapply(x, class)
  obsNames <- names(x)[which(classes=="numeric")]
  otherNames <- names(x)[which(classes!="numeric")]
  obs <- a*x[,obsNames]
  other <- x[,which(classes!="numeric")]
  return(stats::setNames(data.frame(other, obs), c(otherNames, obsNames)))
}

#' Calculate a cumulative distribution on deciles and mean of a numeric vector.
#'
#' This function is more limited than the general \code{summary} function, and
#' is primarily useful for generating summary tables for multiple variables
#' when used with \code{lapply}, as, \code{lapply(df[,cols], summary.decile)},
#' where \code{cols} describes columns of a data frame containing variables to
#' be compared.
#'
#' @param x numeric vector
#' @param na.rm if TRUE, remove NA values
#' @export
summary_decile <- function(x, na.rm=FALSE) {
  return(c(stats::quantile(x, seq(0,1,0.1), na.rm=na.rm), mean=mean(x, na.rm=na.rm)))
}

#' Calculate a cumulative distribution on specified quantiles and mean of a
#' numeric vector.
#'
#' This function is more limited than the general \code{summary} function, and
#' is primarily useful for generating summary tables for multiple variables
#' when used with \code{lapply}, as, \code{lapply(df[,cols], summary.quantile)},
#' where \code{cols} describes columns of a data frame containing variables to
#' be compared.
#'
#' @param x numeric vector
#' @param q vector of quantiles to calculate
#' @param na.rm if TRUE, remove NA values
#' @export
summary_quantile <- function(x, q, na.rm=FALSE) {
  return(c(stats::quantile(x, q, na.rm=na.rm), mean=mean(x, na.rm=na.rm)))
}
