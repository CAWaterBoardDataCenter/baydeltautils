% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data_retrieval.R
\name{getCDECData}
\alias{getCDECData}
\title{Retrieve data from CDEC for a single sensor at a single station.}
\usage{
getCDECData(
  station,
  sensor,
  duration = "M",
  startDate = "1900-10-01",
  endDate = Sys.Date(),
  mode = "retrieve"
)
}
\arguments{
\item{station}{CDEC three character station code (case insensitive)}

\item{sensor}{CDEC numeric sensor code [TODO: document codes]}

\item{duration}{CDEC single character duration code (event=E, hourly=H,
daily=D, or monthly=M; defaults to "M")}

\item{startDate}{start date for time series}

\item{endDate}{end date for time series}

\item{mode}{"retrieve" to retrieve data, "url" to return URL}
}
\description{
TODO: Reassess how we want to represent date and time in the returned data.
At present we are returning a bunch of individual fields representing time,
day, month, year, and WY. This is convenient when we want to aggregate over
different time scales, but it is redundant and makes merging data frames a
pain. It would be better to use a single datetime field and use functions to
extract, day, month, year, or WY. Maybe we can already do this and it's just
a matter of tracking down dependencies.
}
