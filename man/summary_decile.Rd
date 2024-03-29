% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/statistics.R
\name{summary_decile}
\alias{summary_decile}
\title{Calculate a cumulative distribution on deciles and mean of a numeric vector.}
\usage{
summary_decile(x, na.rm = FALSE)
}
\arguments{
\item{x}{numeric vector}

\item{na.rm}{if TRUE, remove NA values}
}
\description{
This function is more limited than the general \code{summary} function, and
is primarily useful for generating summary tables for multiple variables
when used with \code{lapply}, as, \code{lapply(df[,cols], summary.decile)},
where \code{cols} describes columns of a data frame containing variables to
be compared.
}
