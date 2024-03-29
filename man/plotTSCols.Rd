% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plotting.R
\name{plotTSCols}
\alias{plotTSCols}
\title{Plot multiple time series from a single data frame on a single set of axes.}
\usage{
plotTSCols(
  df,
  xcol = 1,
  ycol = NULL,
  v.col = NULL,
  main = NULL,
  ylab = NULL,
  xlab = "Date",
  ...
)
}
\arguments{
\item{df}{data frame}

\item{xcol}{column containing x-values}

\item{ycol}{column containing y-values}

\item{v.col}{vector of colors for plotting series; in NULL, a sensible
default of black and blue, or black and a rainbow gradient will be used}

\item{main}{main title of plot}

\item{ylab}{y-axis label}

\item{xlab}{x-axis label}

\item{...}{additional parameters passed to plot}
}
\description{
Similar to plotTsComparison, but rather than using a formula, plot implied
(or specified) y-values as a function of an x-value in a data frame. The
default behavior is to plot columns 2:ncol as a function of column 1. If
"xcol" is specified as an integer in 1:ncol, then the y-values will be taken
to be the remaining values, or a vector of integers specified as "ycol".
}
\details{
Note: there is no obvious reason for this function to be used only for time
series since it is really just an xy scatter plot. Consider renaming to be
more descriptive.
}
