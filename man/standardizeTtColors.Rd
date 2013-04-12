\name{standardizeTtColors}
\alias{standardizeTtColors}
\title{Standardize twist-tie colors}
\usage{
  standardizeTtColors(x)
}
\arguments{
  \item{x}{input character vector}
}
\value{
  character vector the same length as input vector with
  each element a legitimate abbreviation of the color
}
\description{
  Standardize twist-tie colors
}
\examples{
x <- c("Black", "blue", "Blue", "Yellow")
standardizeTtColors(x)
}
\seealso{
  \code{\link{all.standardTtColor}} which tests if
  abbreviations are legitimate
}
\keyword{color}
\keyword{twist-tie}

