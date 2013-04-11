\name{listBadFiles}
\alias{listBadFiles}
\title{find mass files that don't make proper csvs}
\usage{
  listBadFiles(path = ".")
}
\arguments{
  \item{path}{character designating directory containing
  files of interest}
}
\value{
  character vector of bad file names
}
\description{
  find mass files that don't return a good csv files,
  probably resulting from an extra comma in the first line
}
\seealso{
  \code{\link{combineMassFiles}} and
  \code{\link{readMassFile}} and
  \code{\link{investigateMassFiles}} which are other useful
  functions that deal with mass files
}
\keyword{file}
\keyword{mass}

