% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/conversions.R
\name{convert_monthly}
\alias{convert_monthly}
\title{Convert a monthly time series of volumes or flows between unit systems.}
\usage{
convert_monthly(
  data,
  ignore_cols = NULL,
  rate_conversion,
  volume_conversion,
  destination_type,
  destination_unit
)
}
\arguments{
\item{data}{data frame containing time series to be converted}

\item{ignore_cols}{vector of names of numeric columns not to convert. By
default, ignores any column named "timestep", "month", "year", or "wy".}

\item{rate_conversion}{a list of conversion factors to convert daily rates to
the desired destination units; named for the source units}

\item{volume_conversion}{a list of conversion factors to convert monthly
volumes to the desired destination units; named for the source units}

\item{destination_type}{either "rate" or "volume" according to the type of
destination units}

\item{destination_unit}{name of destination units}
}
\description{
The data frame should contain a column named "unit". Although this function
can be invoked directly by specifying conversion factors, it is primarily
intended to be a utility function to ease implementation of other conversion
functions.
}
