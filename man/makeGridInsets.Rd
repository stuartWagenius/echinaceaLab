% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/manageXray.R
\name{makeGridInsets}
\alias{makeGridInsets}
\title{Generate x-ray grid insets}
\usage{
makeGridInsets(xrdf, sheetSize = 20, test = FALSE)
}
\arguments{
\item{xrdf}{an x-ray data frame for a given year, generated by makeXrayDf()}

\item{sheetSize}{the number of heads to fit on each x-ray grid sheet}

\item{test}{a boolean for testing inset creation or printing. If TRUE,
then the string "Test" is appended to the end of this function's output.}
}
\value{
an array of cell insets to be used when making x-ray grids
}
\description{
This function takes in an x-ray data frame (see makeXrayDf()) and outputs
insets for x-ray grid cells. These insets will either be the numeric part
of the letno for the head belonging in that cell or the string "Empty".
Export the output of this function as a .csv (with row.names = FALSE)
and use that for the mail merge when making x-ray grids.
}
\keyword{grid}
\keyword{inset}
\keyword{x-ray}
\keyword{xray}
