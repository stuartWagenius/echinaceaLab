\name{all.standardTtColor}
\alias{all.standardTtColor}
\title{Check if twist-ties abbreviation are legitimate.}
\usage{
\method{all}{standardTtColor}(x)
}
\arguments{
  \item{x}{character vector}
}
\value{
logical value returning TRUE if all elements in the string
are legitimate tt color abreviations
}
\description{
Check if twist-ties abbreviation are legitimate.
}
\examples{
all.standardTtColor(c("blue", "Blue", "blu"))
x <- c("bac", "blu", "clr", "yel", "wht")
all.standardTtColor(x)
}
\seealso{
\code{\link{standardizeTtColors}} which abbreviates colors
}
\keyword{twist-tie}

