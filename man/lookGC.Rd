\name{lookGC}
\alias{lookGC}
\title{Visualize conditions in the Conviron Growth Chambers 88 and 89 for a single date.}
\usage{
lookGC(date = "", chamber = "", printPDF = FALSE)
}
\arguments{
  \item{date}{character a date in the format %Y-%m-%d (e.g.
  "2012-01-09")}

  \item{chamber}{character growth chamber88 or 89 (in
  quotes)}

  \item{printPDF}{logical indicating whether to print
  output to pdf file}
}
\value{
multipanel figure. If printPDF = FALSE (the default), the
function will create a new window with multi-panel graph.
If printPDF=TRUE, the function will savea PDF of the graph
to the folder
\url{I:/Departments/Research/GrowthChamber/logPlots}. The
file name will be in the format gc(chamber)_date.pdf (e.g.
gc88_2012-05-06.pdf). The graph size will be 10x8 inches,
with a landscape orientation.
}
\description{
Reads data files located in the folders
\url{I:/Departments/Research/GrowthChamber/chamber88} and
\url{I:/Departments/Research/GrowthChamber/chamber89} and
generates graphs of temperature, humidity, light, and CO2
(CO2 in chamber 88 only). Graphs give measured values of
environmental conditions (black dots) along with the values
specified on the Conviron System (red dots). Graphs also
give range and mean of environmental values throughout 24
hours. The user may visualize graphs in R or save them as a
PDF.
}
\examples{
\dontrun{
lookGC("2013-01-09",chamber="89")
lookGC("2013-01-09",chamber="89", printPDF=TRUE)
lookGC("2012-05-06",chamber= "88")
lookGC("2012-05-06",chamber= "88", printPDF=TRUE)}
}
\keyword{chamber}
\keyword{growth}

