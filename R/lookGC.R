#'Check the Conviron Growth Chambers 88 and 89 for a single date.
#'@description Reads data files downloaded to the folders
#'  \url{I:/Departments/Research/GrowthChamber/chamber88} and 
#'  \url{I:/Departments/Research/GrowthChamber/chamber89} and generates graphs
#'  of temperature, humidity, light, and CO2  (CO2 in chamber 88 only). Graphs
#'  give measured values of environmental conditions (black dots) along with the
#'  values specified on the Conviron System (red dots). Graphs also give range 
#'  and mean of environmental values throughout 24 hours. The user may visualize
#'  graphs in R or save them as a PDF.
#'@usage lookGC("date", chamber ="x",printPDF = FALSE) "date" : a date in the
#'  format %Y-%m-%d  (e.g. "2012-01-09"). chamber= "x" : specify growth chamber
#'  88 or 89 (in quotes).
#'@return Value: If printPDF = FALSE (the default) the function will create a 
#'  new window with multi-panel graph. If printPDF=TRUE, the function will save
#'  a PDF of the graph to the folder
#'  \url{I:/Departments/Research/GrowthChamber/logPlots}. The file name will be
#'  in the format gc(chamber)_date.pdf (e.g. gc88_2012-05-06.pdf). The graph
#'  size will be 10x8 inches, with a landscape orientation.
#'@keywords growth chamber
#' @examples
#' lookGC("2013-01-09",chamber="89")
#' lookGC("2013-01-09",chamber="89",printPDF=TRUE)
#' lookGC("2012-05-06",chamber= "88")
#' lookGC("2012-05-06",chamber= "88",printPDF=TRUE)


lookGC <- function(date = "", chamber= "", printPDF = FALSE){
  if(chamber== "89"){
    setwd("I:\\Departments\\Research\\GrowthChamber\\chamber89") 
    nn <- read.csv(paste("10.0.0.89 ", date, ".log", sep = ""), header = T)
  }
  if(chamber== "88"){
    setwd("I:\\Departments\\Research\\GrowthChamber\\chamber88")
    nn <- read.csv(paste("10.0.0.88 ", date, ".log", sep = ""), header = T)
  }
  
  nn <- nn[nn$AI_TEMP != "AI_TEMP",] #take out rows that are repeats of column headers
  nn$TempObs <- as.numeric(as.character(nn$AI_TEMP))
  nn$TempExp <- as.numeric(as.character(nn$SP_TEMP))
  nn$RHObs <- as.numeric(as.character(nn$AI_HUM))
  nn$RHExp <- as.numeric(as.character(nn$SP_HUM))
  nn$LightObs <- as.numeric(as.character(nn$AI_LIGHT))
  nn$LightExp <- as.numeric(as.character(nn$SP_LIGHT))
  if(chamber== "88"){
    nn$CO2Obs <- as.numeric(as.character(nn$AI_CO2))
    nn$CO2Exp <- as.numeric(as.character(nn$SP_CO2))
  }
  if(!printPDF){
    windows()
  }
  if(printPDF){
    setwd("I:\\Departments\\Research\\GrowthChamber\\logPlots")
    pdf(file=paste("gc",chamber,"_",date,".pdf",sep=""),paper="USr",width=10,height=8)
  }
  par(mfcol=c(2,2))
  plot(TempObs~TIME,data=nn,col="black",ylab="temp (deg.C)",type="b",main=date)
  points(TempExp~TIME,data=nn,col="red")
  legend("topright",legend=paste(c("min","max","mean"),
                                 c(range(nn$TempObs),round(mean(nn$TempObs),digits=2))
                                 ,sep="="),bty="n")
  
  plot(RHObs~TIME,data=nn,col="black",ylab="%relative humidity",type="b")
  points(RHExp~TIME,data=nn,col="red")
  legend("topright",legend=paste(c("min","max","mean"),
                                 c(range(nn$RHObs),round(mean(nn$RHObs),digits=2))
                                 ,sep="="),bty="n")
  
  plot(LightObs~TIME,data=nn,col="black",ylab="light (uMOL)",type="b")
  points(LightExp~TIME,data=nn,col="red")
  legend("topright",legend=paste(c("min","max","mean"),
                                 c(range(nn$LightObs),round(mean(nn$LightObs),digits=2))
                                 ,sep="="),bty="n")
  legend("topleft",legend=c("specified","observed"),fill=c("red","black"),bty="n")
  
  if(chamber== "88"){
    plot(CO2Obs~TIME,data=nn,col="black",ylab="CO2 (PPM)",type="b")
    points(CO2Exp~TIME,data=nn,col="red")
    legend("topright",legend=paste(c("min","max","mean"),
                                   c(range(nn$CO2Obs),round(mean(nn$CO2Obs),digits=2))
                                   ,sep="="),bty="n")
  }
  par(mfcol=c(1,1))
  if(printPDF){
    dev.off()
  }
}
