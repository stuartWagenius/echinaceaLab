\name{lookGC}
\alias{lookGC}
\title{Check the Conviron Growth Chambers 88 and 89 for a single date.}
\usage{
  lookGC("date", chamber ="x",printPDF = FALSE) "date" : a
    date in the format %%Y-%m-%d (e.g. "2012-01-09").
    chamber= "x" : specify growth chamber 88 or 89 (in
    quotes).
}
\value{
  Value: If printPDF = FALSE (the default) the function
  will create a new window with multi-panel graph. If
  printPDF=TRUE, the function will save a PDF of the graph
  to the folder
  \url{I:/Departments/Research/GrowthChamber/logPlots}. The
  file name will be in the format gc(chamber)_date.pdf
  (e.g. gc88_2012-05-06.pdf). The graph size will be 10x8
  inches, with a landscape orientation.
}
\description{
  Reads data files downloaded to the folders
  \url{I:/Departments/Research/GrowthChamber/chamber88} and
  \url{I:/Departments/Research/GrowthChamber/chamber89} and
  generates graphs of temperature, humidity, light, and CO2
  (CO2 in chamber 88 only). Graphs give measured values of
  environmental conditions (black dots) along with the
  values specified on the Conviron System (red dots).
  Graphs also give range and mean of environmental values
  throughout 24 hours. The user may visualize graphs in R
  or save them as a PDF.
}
\examples{
lookGC("2013-01-09",chamber="89")
lookGC("2013-01-09",chamber="89",printPDF=TRUE)
lookGC("2012-05-06",chamber= "88")
lookGC("2012-05-06",chamber= "88",printPDF=TRUE)
}
\keyword{chamber}
\keyword{growth}

