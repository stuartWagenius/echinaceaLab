\name{investigateMassFiles}
\alias{investigateMassFiles}
\title{investigate all txt files in a directory}
\usage{
  investigateMassFiles(path = ".")
}
\arguments{
  \item{path}{character designating directory containing
  files of interest}
}
\value{
  data frame that summarizes characteristics of mass files
}
\description{
  investigate all mass files in a directory to find
  potential errors
}
\seealso{
  \code{\link{combineMassFiles}} and
  \code{\link{readMassFile}} and \code{\link{listBadFiles}}
  which are other useful functions that deal with mass
  files
}
\keyword{file}
\keyword{mass}

