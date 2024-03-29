% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/conversions.R
\name{taf_monthly}
\alias{taf_monthly}
\title{Convert a monthly time series of volumes or flows to thousand acre-feet.}
\usage{
taf_monthly(data, ignore_cols = NULL)
}
\arguments{
\item{data}{data frame containing time series to be converted}

\item{ignore_cols}{vector of names of numeric columns not to convert. By
default, ignores any column named "timestep", "month", "year", or "wy".}
}
\description{
The data frame should contain a column named "unit" that specifies the source
units for each row of data. This function can accommodate data frames of
mixed units, provided that all variables in a given row share the same units.
}
\details{
The current implementation recognizes the following units:
\itemize{
 \item cubic_feet_per_second
 \item million_acre_feet
 \item thousand_acre_feet
 \item acre_feet
}
}
