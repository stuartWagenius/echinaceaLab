#' Convert an XML file to RData
#' 
#' Converts an XML file into a data frame that is stored in an RData file.
#' Only the information that we want is taken from the XML
#' 
#' @param fn name of the XML file to convert
#' @param readPath file path where the XML file can be found
#' @param writePath file path to write the RData file to
#' @keywords gps
#' 
#' @return The name of the converted file (without the path)
#'
convertXMLtoRdata <- function(fn, 
                              readPath = folderXML,  
                              writePath = folderRD
) {
  fileName <- paste(folderXML, fn, sep="")
  # library(XML)
  
  # get data into a more readable format
  dd.txt <- readLines(fileName)
  
  # replace blank site and textLocId fields ##
  dd.txt <- gsub("<Name>site</Name><Type>", "<Name>site</Name><Value> </Value><Type>", dd.txt)
  dd.txt <- gsub("<Name>textLocId</Name><Type>", "<Name>textLocId</Name><Value> </Value><Type>", dd.txt)
  dd.txt <- gsub("<Name>alias</Name><Type>", "<Name>alias</Name><Value> </Value><Type>", dd.txt)
  dd.txt <- gsub("<Name>survNote</Name><Type>", "<Name>survNote</Name><Value> </Value><Type>", dd.txt)
  
  # convert text to xml ##
  dd <- xmlTreeParse(dd.txt, useInternalNodes=TRUE)
  node <- xpathApply(dd, "//Point")  
  ff <- lapply(node, saveXML)
  # remove unwanted data
  vrsPoint <-  grep("VRS_", ff)
  ff <- ff[-c(vrsPoint)]   ##### ignore VRS point
  demoPoints <- grep("demography", ff)
  ff <- ff[demoPoints]  ## keep only shot point
  
  if (length(ff) < 1) {
    message("No points shot")
    return()
  }
  
  # parse xml using XPath (tutorial at http://www.w3schools.com/xpath/)
  # populate df in order that we want columns
  
  PointNumber <-  unlist(xpathApply(dd, "//Vector//RoverPnt//PointNumber", xmlValue))  
  df <- data.frame(PointNumber = PointNumber)
  
  df$siteName <- unlist(xpathApply(dd, "//Vector//RoverPnt//Feature//Attribute[Name = 'site']/Value", xmlValue))
  
  df$textLocId <- unlist(xpathApply(dd, "//Vector//RoverPnt//Feature//Attribute[Name = 'textLocId']/Value", xmlValue))  
  
  tmp <- xpathApply(dd, "//Vector//RoverPnt//Feature//Attribute[Name = 'alias']/Value", xmlValue)
  if(length(tmp)==0) df$alias <- "" else df$alias <- unlist(tmp)  
  
  tmp <- xpathApply(dd, "//Vector//RoverPnt//Feature//Attribute[Name = 'survNote']/Value", xmlValue)
  if(length(tmp)==0) df$survNote <- "" else df$survNote <- unlist(tmp)  
  
  tmp <- xpathApply(dd, "//Vector//RoverPnt//Feature//Attribute[Name = 'demo_on_visor']/Value", xmlValue)
  
  tmp <- xpathApply(dd, "//Vector//RoverPnt//Feature//Attribute[Name = 'actually_echinacea']/Value", xmlValue)
  
  df$StdDev.Northing <- as.numeric(unlist(xpathApply(dd, "//Point//StdDev//Northing", xmlValue)))
  df$StdDev.Easting <- as.numeric(unlist(xpathApply(dd, "//Point//StdDev//Easting", xmlValue)))
  df$StdDev.Up <- as.numeric(unlist(xpathApply(dd, "//Point//StdDev//Up", xmlValue)))
  
  df$Latitude <- unlist(xpathApply(dd, "//RoverPnt//Latitude", xmlValue))
  df$Northing <- as.numeric(unlist(xpathApply(dd, "//RoverPnt//Northing", xmlValue)))
  df$Longitude <- unlist(xpathApply(dd, "//RoverPnt//Longitude", xmlValue))
  df$Easting <- as.numeric(unlist(xpathApply(dd, "//RoverPnt//Easting", xmlValue)))
  df$EllHeight <- as.numeric(unlist(xpathApply(dd, "//RoverPnt//EllHeight", xmlValue)))
  
  
  ###  spread textLocId to tag, plaStatus
  ## df$textLocId
  tt <- strsplit(df$textLocId, ".", fixed = TRUE)
  df$tt1 <- unlist(lapply(1:length(tt), function(i) tt[[i]][1]))
  df$tag <- suppressWarnings(as.numeric(df$tt1))
  df$plaStatus <- unlist(lapply(1:length(tt), function(i) tt[[i]][2]))
  tt3 <- unlist(lapply(1:length(tt), function(i) tt[[i]][3]))
  if(sum(!is.na(tt3))) warning("Deal with tt3!")
  
  
  tmp <- !(df$tag  == suppressWarnings(as.numeric(df$tt1)))  #check tag
  df <- df[,-c(grep("tt1", names(df)))]                      # delete column
  
  
  # make base map
  plot(df$Easting, df$Northing, asp = 1, pch =".")
  with(df, text(Easting, Northing, df$textLocId, cex = .5, pos = 4))
  
  
  df$echinacea <- TRUE
  
  name <- makeGPSFileName(fn)
  
  assign(name, df)
  fn <- paste(writePath, name, ".RData", sep="")
  if(file.exists(fn)) stop("This RData file already exists\nDelete it if you want save new version.")
  save(list= name, file= fn)
  return(name)
}


#' Make standard GPS job name
#' 
#' Takes the file name and converts it to a standard format
#' 
#' @param fileName the name of the file for which we want a standard name
#' @keywords gps
#' 
#' @return The new file name
#'
makeGPSFileName <- function(fileName) {
  # get job type
  if (grepl("(P|p)(H|h)(E|e)(N|n)", fileName)) jobType <- "phen"
  if (grepl("(S|s)(U|u)(R|r)(V|v)", fileName)) jobType <- "surv"
  if (grepl("(D|d)(E|e)(M|m)(O|o)", fileName)) jobType <- "demo"
  if (grepl("(R|r)(E|e)(C|c)(H|h)(E|e)(C|c)(K|k)", fileName)) jobType <- "recheck"
  if (grepl("(R|r)(E|e)(S|s)(U|u)(R|r)(V|v)", fileName)) jobType <- "resurv"
  if (grepl("(S|s)(L|l)(I|i)(N|n)(G|g)", fileName)) jobType <- "sling"
  
  # get date
  dateRegex1 <- regexpr("[0-9]+(-|_)[0-9]+(-|_)[0-9]+", fileName)
  dateRegex2 <- regexpr("[0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9]", fileName)
  dateStamp1 <- substr(fileName, dateRegex1[1], dateRegex1[1]+attr(dateRegex1, "match.length")-1)
  dateStamp2 <- substr(fileName, dateRegex2[1], dateRegex2[1]+attr(dateRegex2, "match.length")-1)
  if (nchar(dateStamp2) < 1) {
    dateStamp <- gsub("-", "", dateStamp1)
    dateStamp <- gsub("_", "", dateStamp)
  } else {
    dateStamp <- dateStamp2
  }
  
  # get GPS unit
  if (grepl("(S|s)(U|u)(L|l)(U|u)", fileName)) whichGPS <- "sulu"
  if (grepl("(C|c)(H|h)(E|e)(K|k)", fileName)) whichGPS <- "chek"
  
  name <- paste("a", dateStamp, "_", jobType, "_", whichGPS, sep="")
  name
}


#' Check for GPS errors
#' 
#' Check for too high standard deviation in GPS points and print out
#' problem points. Also print out any survNote's.
#' 
#' @param dataFileName the name of the RData file in the standard format.
#'  Adding ".RData" is not necessary. 
#' @param folder the folder where the RData file can be found
#' @keywords gps
#' @return none, but display informative messages about where any errors or notes occur
#'
errorCheckRData <- function(dataFileName, folder = folderRD) {
  load(paste(folder, dataFileName, ".RData", sep=""))
  df <- get(dataFileName)
  
  # check standard deviations for high values
  hist(df$StdDev.Northing)
  problemNorthing <- which(df$StdDev.Northing > 0.015)
  if(length(problemNorthing) > 0) {
    message("Northing error at point(s) ", problemNorthing)
    message(" tag: ", df$tag[problemNorthing])
    message(" site: ", df$site[problemNorthing])
  }
  hist(df$StdDev.Easting)
  problemEasting <- which(df$StdDev.Easting > 0.015)
  if(length(problemEasting) > 0) {
    message("Northing error at point(s) ", problemEasting)
    message(" tag: ", df$tag[problemEasting])
    message(" site: ", df$site[problemEasting])
  }
  
  whereNotes <- which(df$survNote != "" & df$survNote != " ")
  for(i in whereNotes) {
    message("Point ", i, " survNote: ", df$survNote[i])
  }
  
}


#' Add files to surv.csv
#' 
#' Add all points from the RData file into surv.csv. If surv.csv doesn't exist,
#' it will be created. Currently it gets saved in the working directory but
#' it could be changed to a standard dropbox location.
#' 
#' @param dataFileName the name of the RData file in the standard format.
#'  Adding ".RData" is not necessary. 
#' @param folder the folder where the RData file can be found
#' @keywords gps
#'
addToSurv.csv <- function(dataFileName, folder = folderRD) {
  isFirst <- !file.exists("surv.csv")
  line0 <- 0
  if(!isFirst) {
    x <- read.csv("surv.csv")
    line0 <- max(x$line)
  }
  load(paste(folder, dataFileName, ".RData", sep=""))
  df <- get(dataFileName)
  df$line <- line0 + 1:dim(df)[1]
  df$file <- dataFileName
  if (isFirst) {
    message("Creating file surv.csv...")
    write.csv(df, "surv.csv", row.names = FALSE, quote = TRUE)
  } else {
    write.table(df, file="surv.csv", row.names = FALSE, col.names = FALSE, quote=TRUE, append = TRUE, sep = ",", qmethod = "double")
  }
  
  finLine <- max(read.csv("surv.csv")$line)
  message(cat("Added", finLine-line0, "records to surv.csv"))
}