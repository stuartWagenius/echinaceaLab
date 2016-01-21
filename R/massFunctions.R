

#' Edit the raw mass data files into useful objects.
#' 
#' This function reads the raw mass data files created during weighing and edits them into a useful dataframe,
#' with the option of saving them as a csv. The function creates a list of three dataframes, which is returned 
#' invisibly. One gives the cleaned-up, edited mass data (good). The other two give bad or strange data that may be useful
#' for investigating mass files. You have the option of saving good records as a csv with the argument writeCsv. 
#' The writeCsv argument defaults to not writing csv file. 
#' 
#' 
#' 

#' @param file character filename or filename with path of raw mass data file
#' @param writeCsv logical indicating whether to print output to csv file (saves automatically to working directory)
#' @return list of bad lines, strange lines and good masses, returned invisibly.
#'   A csv file is returned if writeCsv is TRUE.
#' @keywords mass file
#' 
#' @examples
#'
#' \dontrun{
#' setwd("I:\\Departments\\Research\\EchinaceaVolunteers\\Balance\\sampleForEchLab\\CG2009_rawFiles")
#' yy <- readMassFile("sm 21 oct 1899 batch 9.txt", writeCsv = FALSE)
#' yy$bad
#' yy$strange
#' str(yy$good)
#' dim(yy$bad)
#' }
#'
#' @seealso \code{\link{combineMassFiles}} and \code{\link{listBadFiles}} and 
#' \code{\link{investigateMassFiles}} which are other useful functions that 
#' deal with mass files

readMassFile <- function(file, writeCsv = FALSE){
  xx <- read.csv(file)
  rawNames <- names(xx)
  mm <- data.frame(lineNo = 1:dim(xx)[1])
  mm$id <- as.character(xx[ ,1])
  mm$timeStamp <- as.character(xx[,2])
  for (i in 2:length(mm$id)) {
    if (mm[i, "id"] == "") 
      mm[i, "id"] <- mm[i - 1, "id"]
  }
  mm$mass <- (xx[, 3])
  if (dim(xx)[2] >=4){
    mm$note <- as.character(xx[, 4])
  } else {
    mm$note <- NA
  }
  if (dim(xx)[2] !=4){
    names(mm)[c(4,5)] <- c("massWrong","noteWrong") 
    warning("wrong number of columns!")
  }
  mm$header <- names(xx)[4]
  badLines <- mm[is.na(mm$mass) & mm$timeStamp == "", ]
  strangeLines <- mm[xor(!is.na(mm$mass), mm$timeStamp != ""), ] 
  mm <- mm[!is.na(mm$mass) & mm$timeStamp != "", ]
  mm$fileName <- file
  goodMasses <- mm
  head(goodMasses)
  newFileName <- paste(file, "-goodMass.csv", sep = "")
  if (writeCsv) 
    write.csv(goodMasses, file = newFileName, row.names = FALSE)
  ans = list(bad = badLines, strange = strangeLines, good = goodMasses)
  invisible(ans)
} #end readMassFile
##################################################

# # examples
# 
# tt <- readMassFile("2 november 2010 batch 9.txt", writeCsv = FALSE)
# yy <- readMassFile("sm 16 sept 2010 batch 7.txt")
# zz <- readMassFile("sm 19 oct 2010, batch 9.txt")
# 
# yy$bad
# yy$strange
# str(yy$good)
# dim(yy$bad)

##################################################
# now write functions to investigate all txt files
# function reports strange, bad, and good lines
# BEWARE: 
# if a call to readMassFile function throws an error, no notice is given


#' investigate all txt files in a directory
#' 
#' Investigate the raw mass files in a directory to find potential errors.
#' 
#' @param path character designating directory containing files of interest
#' @return data frame that summarizes characteristics of mass files
#' @keywords mass file
#' @examples
#'
#'\dontrun{
#'setwd("I:\\Departments\\Research\\EchinaceaVolunteers\\Balance\\sampleForEchLab\\CG2009_rawFiles")
#'investigateMassFiles()
#'}
#'
#' @seealso \code{\link{combineMassFiles}} and \code{\link{readMassFile}} and
#'   \code{\link{listBadFiles}} which are other useful functions that
#'   deal with mass files
investigateMassFiles <- function(path = ".") {
  fn <- list.files(pattern = "\\.txt$")
  count <- length(fn)
  hh <-data.frame(index = 1: count, file= fn, strangeLines= 0, badLines = 0, records = 0)
  try(                    # try enables function to return partial hh  
    for(index in 1:count) { # loop through all txt files
      dd <- readMassFile(fn[index], writeCsv = FALSE)
      hh[index, "strangeLines"] <- dim(dd$strange)[1]
      hh[index, "badLines"] <- dim(dd$bad)[1]
      hh[index, "records"] <- dim(dd$good)[1]
    } # end for loop
  ) # end try
  hh
} # end function investigateMassFiles






#' combine raw mas data txt file into useful output
#' 
#' This function combines all good mass records in a single dataframe and, optionally,
#' writes a csv.
#' 
#' @param path character designating directory containing files of interest
#' @param writeCsv logical indicating whether to print output to csv file
#' @param fileName character for name of output file
#' @return dataframe of mass data from all raw mass files in directory of 
#'   interest, returned invisibly. A csv file is returned if writeCsv is TRUE.
#' @keywords mass file
#' @examples
#'
#'\dontrun{
#'setwd("I:\\Departments\\Research\\EchinaceaVolunteers\\Balance\\sampleForEchLab\\CG2009_goodFiles")
#'tt <- combineMassFiles()
#'}
#' @seealso \code{\link{listBadFiles}} and \code{\link{readMassFile}} and 
#'   \code{\link{investigateMassFiles}} which are other useful functions that 
#'   deal with mass files
combineMassFiles <- function(path = ".", writeCsv = FALSE, fileName = "allMassFiles.csv") {
  # run investigateMassFiles() and  return warning if a record count is zero
  problemFile <- any(investigateMassFiles()$records == 0)
  if(problemFile) stop("file with zero records")
  # make first data frame
  fn <- list.files(pattern = "\\.txt$")
  count <- length(fn)
  ans <- readMassFile(fn[1], writeCsv = FALSE)$good
  # loop through rest of files and rbind
  for(index in 2:count) { # loop through all but first txt file
    ans <- rbind(ans, readMassFile(fn[index], writeCsv = FALSE)$good)
  } # end for loop
  # return one df & optionally write csv
  if(writeCsv) write.csv(ans, file = fileName, row.names = FALSE)
  invisible(ans)
} # end function combineMassFiles

# examples

# combineMassFiles(writeCsv = FALSE)
# 
# xx <- combineMassFiles(writeCsv = FALSE)
# str(xx)




#' Find mass files that don't make proper csvs,
#' 
#' Find mass files that don't return a good csv files. Bad files usually result from an extra
#' comma in the first line.
#' 
#' 
#' @param path character designating directory containing files of interest
#' @return character vector of bad file names
#' @keywords mass file
#' @examples
#'
#'\dontrun{
#'setwd("I:\\Departments\\Research\\EchinaceaVolunteers\\Balance\\sampleForEchLab\\CG2009_rawFiles")
#' listBadFiles( )
#' }
#' @seealso \code{\link{combineMassFiles}} and \code{\link{readMassFile}} and
#'   \code{\link{investigateMassFiles}} which are other useful functions that
#'   deal with mass files
listBadFiles <- function(path = ".") {
  fn <- list.files(pattern = "\\.txt$")
  count <- length(fn)
  #hh <-data.frame(index = 1: count, file= fn, strangeLines= 0, badLines = 0, records = 0)
  jj <- logical(count)
  try(                    # try enables function to return partial hh  
    for(index in 1:count) { # loop through all txt files
      nn <- names(readMassFile(fn[index], writeCsv = FALSE)$good)
      jj[index] <- !all(nn == c("lineNo", "id", "timeStamp", "mass", "note", "header", "fileName"))
    } # end for loop
  ) # end try
  fn[jj]
}# end function listBadFiles






#' Count values greater than a threshold value
#' 
#' Counts full achenes in a sample of weighed achenes. The default threshold is 0.002g.
#' 
#' @param x numeric vector
#' @param cut.off numeric value threshold default is 0.002
#' @return integer count of elements in x greater than the threshold cut.off
#' @keywords full achene
#' @examples
#'
#'\dontrun{
#' dd <- read.csv("I:\\Departments\\Research\\EchinaceaVolunteers\\Balance\\sampleForEchLab\\CG2009_csv\\sample.csv")
#' full(dd$mass)
#' }
#' @seealso \code{\link{empty}} which counts elements less than the threshold
#'   value
full <- function(x, cut.off = 0.002) sum(x > cut.off)

#' count values less than a threshold value
#' 
#' counts empty achenes in a sample of weighed achenes
#' 
#' @param x numeric vector
#' @param cut.off numeric value threshold default is 0.002
#' @return integer count of elements in x less than the threshold cut.off
#' @keywords empty achene
#' @examples
#'
#'\dontrun{
#' dd <- read.csv("I:\\Departments\\Research\\EchinaceaVolunteers\\Balance\\sampleForEchLab\\CG2009_csv\\sample.csv")
#' empty(dd$mass)
#' }
#' @seealso \code{\link{full}} which counts elements greater than the threshold
#'   value
empty <- function(x, cut.off = 0.002) sum(x <= cut.off)


#' Standardize twist-tie colors
#' 
#' @param x input character vector
#' @return  character vector the same length as input vector with each element
#'   a legitimate abbreviation of the color
#' @keywords twist-tie color
#' @seealso \code{\link{all.standardTtColor}} which tests if abbreviations are 
#'   legitimate
#' @export
#' @examples
#' x <- c("Black", "blue", "Blue", "Yellow")
#' standardizeTtColors(x)
standardizeTtColors <- function(x){
  x <- gsub("Yellow", "yel", x)
  x <- gsub("yellow", "yel", x)
  x <- gsub("ylw", "yel", x)
  x <- gsub("Green", "grn", x)
  x <- gsub("green", "grn", x)
  x <- gsub("Black", "bac", x)
  x <- gsub("black", "bac", x)
  x <- gsub("Blk", "bac", x)
  x <- gsub("blk", "bac", x)
  x <- gsub("White", "wht", x)
  x <- gsub("white", "wht", x)
  x <- gsub("Red", "red", x)
  x <- gsub("Clear", "clr", x)
  x <- gsub("clear", "clr", x)
  x <- gsub("Blue", "blu", x)
  x <- gsub("blue", "blu", x)
  x
}

# another function for twist-tie colors# ####
# this needs the entire list of legit colors and color combinations
# we could use a function is.standardTtColor that retuns a logical vector

#' Check if twist-ties abbreviation are legitimate.
#' 
#' @param x character vector
#' @return logical value returning TRUE if all elements in the string are
#'   legitimate tt color abreviations
#' @keywords twist-tie
#' @seealso \code{\link{standardizeTtColors}} which abbreviates colors
#'
#' @examples
#' all.standardTtColor(c("blue", "Blue", "blu"))
#' x <- c("bac", "blu", "clr", "yel", "wht")
#' all.standardTtColor(x)

all.standardTtColor <- function(x) {
  standard <- c("bac", "bacred", "bacwht", "blu", "bluclr", "bluyel", "clr", 
                "grn", "grnbac", "grnred", "grnwht", "org", "red", "redwht",
                "wht", "yel", "yelbac", "yelgrn", "nott")
  all(x %in% standard)
}
