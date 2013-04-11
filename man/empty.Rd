\name{empty}
\alias{empty}
\title{count values less than a threshold value}
\usage{
  empty(x, cut.off = 0.002)
}
\arguments{
  \item{x}{numeric vector}

  \item{cut.off}{numeric value threshold default is 0.002}
}
\value{
  integer count of elements in x less than the threshold
  cut.off
}
\description{
  counts empty achenes in a sample of weighed achenes
}
\seealso{
  \code{\link{full}} which counts elements greater than the
  threshold value
}
\keyword{achene}
\keyword{empty}

