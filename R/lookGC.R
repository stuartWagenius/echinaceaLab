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
