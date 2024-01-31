# flow_schedule.R
#
# Define a class to model flow schedules on a fixed time step (defaults to
# monthly).

#' An S3 class to represent flow schedules on a fixed time step (defaults
#' to monthly). This class is under development and should be used with caution.
#'
#' @param flow a sequence of flows
#' @param time a sequence of time steps
#' @param return_freq desired return frequency for the flow schedule, if any
#' @param step units of time step
#' @param wy_start first month of water year (integer)
#'
#' @export
flow_schedule <- function(flow, time, return_freq=0.5, step="month",
                          wy_start=10) {
  if (length(flow)==1) { flow <- rep(flow, length(time)) }
  else if (length(flow)!=length(time)) { stop("length mismatch") }
  rval <- list(flow=flow, time=time, step=step, return_freq=return_freq,
               wy_start=wy_start)
  class(rval) <- "flow_schedule"
  return(rval)
}

#' Override the default behavior of \code{length} for \code{flow_schedule}
#' objects, which are implemented as lists, to prevent them returning the
#' number of elements in the object itself.
#'
#' TODO: Evaluate whether this is the correct solution. To this point, we have
#' used lists to contain multiple \code{flow_schedule} objects, but there is no
#' reason that we could not use vectors. This implementation simply returns
#' \code{1}, which would not be the correct behavior for a vector.
#'
#' @param x \code{flow_schedule} object
#'
#' @method length flow_schedule
length.flow_schedule <- function(x) {
  return(1)
}

#' Override the default behavior of \code{unlist} for \code{flow_schedule}
#' objects, which are themselves lists, to prevent them from being
#' unintentionally broken down into their components.
#'
#' TODO: Document the circumstances where this arises and evaluate whether this
#' is really the correct solution.
#'
#' @param x object of class \code{flow_schedule}
#' @param ... [TODO: Document why this is here; maybe just the signature of
#' \code{unlist}?]
#'
#' @method unlist flow_schedule
unlist.flow_schedule <- function(x, ...) {
  return(x)
}

#' Implement an S3 method for \code{mean} for \code{flow_schedule}
#'
#' @param x object of class \code{flow_schedule}
#'
#' @method mean flow_schedule
mean.flow_schedule <- function(x) {
  # This is only implemented for monthly time steps, which is all we're using
  # for now.
  sum(x$flow*lubridate::days_in_month(x$time))/
    sum(lubridate::days_in_month(x$time))
}

#' Implement an S3 method for \code{plot} for \code{flow_schedule}
#'
#' @param x object of class \code{flow_schedule}
#' @param ... additional parameters passed along to \code{plot}
#'
#' @method plot flow_schedule
plot.flow_schedule <- function(x, ...) {
  graphics::plot(stats::stepfun(x$time[2:length(x$time)]-0.5, x$flow), ...)
}
