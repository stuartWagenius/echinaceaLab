\docType{data}
\name{cgIds}
\alias{cgIds}
\title{Link cgPlaId, cgCd, and expNm for experimental plots 1, 2, 6, 7, 8, and 9}
\format{A 20870 x 3 data frame}
\usage{cgIds}
\description{
  This data frame contains information linking cgPlaId, cgCd, and 
  expNm for experimental plots 1, 2, 6, 7, 8, and 9. There are a total of 20870
  records. This data frame is used in the function "exPt01CoreDataSummary" to link
  cgPlaId from cg1CoreData on the Echinacea Project website and expNm.
  This data frame was generated by the script 
  prepare.cgPlaIDToExpNm.R, which is saved in 
  ..Dropbox/CGData/040_prepareProjectData/cg1 CORE Data/Rcode/prepare.cgPlaIDToExpName.R.
}
\section{Variables}{
  Variables: \itemize{ 
    \item cgPlaId, the common garden ID of each common garden position
    \item cgCd, experimental plot each cgPlaId is in
    \item expNm, name of the experiment each cgPlaId is in 
 }
}
\examples{
  dim(cgIds)
  str(cgIds)
  
  table(cgIds$cgCd)
  table(cgIds[cgIds$cgCd == "cg1", "expNm"])

}

\keyword{datasets}