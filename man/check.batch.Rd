\name{check.batch}
\alias{check.batch}
\title{Compare letnos or nolets from scan files with harvest list.}
\usage{
  check.batch(scansdf = "scans", batch = "301")
}
\arguments{
  \item{batch}{character batch name}

  \item{scansdf}{dataframe in format of output from
  function loadScans}
}
\value{
  names list of of interest
}
\description{
  This function compares the vector scans$letno with the
  vector hh.2012$letno. Make sure you set the working
  dircory to the directory that contains the datframes
  scans and hh.2012.
}
\seealso{
  \code{\link{loadScans}}
}
\keyword{batch}
\keyword{file}
\keyword{letno}
\keyword{scan}

