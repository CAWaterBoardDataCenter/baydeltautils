% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data_import.R
\name{readExportedDSSFull}
\alias{readExportedDSSFull}
\title{Import DSS data from an Excel (XLSX) file with full metadata.}
\usage{
readExportedDSSFull(
  file,
  sheet = 1,
  namePart = "B",
  fromPlugin = FALSE,
  startRow = 8,
  metaRows = 1:7,
  rowNames = TRUE,
  missingValue = -901
)
}
\arguments{
\item{file}{XLSX file containing one or more worksheets exported from a DSS
file}

\item{sheet}{integer index or name of sheet to read}

\item{namePart}{which of the DSS parts (A-F) to use as variable names}

\item{fromPlugin}{TRUE if data were exported using Excel plugin (default: FALSE)}

\item{startRow}{first row containing data (default: 8)}

\item{metaRows}{range of rows containing metadata, may be discontinuous (default: 1:7)}

\item{rowNames}{TRUE if row names should be read when importing data (default: TRUE)}

\item{missingValue}{values that should be treated as missing, may be list or vector
(default: -901)}
}
\description{
Import DSS data from an Excel (XLSX) file retaining full metadata.
Defaults should work for data exported from DSSVue to Excel and saved as .XLSX.
If data were imported directly to Excel using the Excel plugin, setting "fromPlugin" to
TRUE will select parameter values that should work. If for some reason neither
of these sets of defaults works, values for "startRow," "metaRows," and "rowNames"
can be set manually.
}
