% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{dayflowFiles}
\alias{dayflowFiles}
\title{The set of files that comprise the Dayfow dataset. Check the Dayflow website
referenced below for updates.}
\format{
A list with two named fields:
\describe{
  \item{merged}{list of URLs for multi-year files, with elements named yyyy_yyyy}
  \item{single}{list of URLs for single-year files, with elements named yyyy}
}
}
\source{
\url{https://water.ca.gov/Programs/Environmental-Services/Compliance-Monitoring-And-Assessment/Dayflow-Data}
}
\usage{
dayflowFiles
}
\description{
Years up until 1996 are contained in pre-merged files; years 1997 and later
are individual files.
}
\details{
Contents last updated July 16, 2018.
}
\keyword{datasets}
