\name{readMassFile}
\alias{readMassFile}
\title{read a raw mass file and output useful objects}
\usage{
  readMassFile(file, writeCsv = FALSE)
}
\arguments{
  \item{file}{character filename or filename with path of
  raw mass data file}

  \item{writeCsv}{logical indicating whether to print
  output to csv file}
}
\value{
  list of bad lines, strange lines and good masses,
  returned invisibly.  A csv file is returned if writeCsv
  is TRUE.
}
\description{
  This function reads a csv file and outputs df & csv. The
  output is three dataframes--two useful for checking csv.
  dfs are returned invisibly. The writeCsv argument
  defaults to not writing csv file.
}
\seealso{
  \code{\link{combineMassFiles}} and
  \code{\link{listBadFiles}} and
  \code{\link{investigateMassFiles}} which are other useful
  functions that deal with mass files
}
\keyword{file}
\keyword{mass}

