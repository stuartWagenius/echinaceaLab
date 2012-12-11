##################################################
# this function reads a csv file and outputs df & csv
# output is three dataframes--two useful for checking csv
# dfs are invisible
# writeCsv argument defaults to not writing csv file
##################################################

readMassFile <- function(file, writeCsv = FALSE){
  mm <- read.csv(file)
  rawNames <- names(mm)
  mm$lineNo <- 1:dim(mm)[1]
  mm$id <- as.character(mm[ , 1])
  mm$id
  for (i in 2:length(mm$id)) {
    if(mm[i, "id"] == "") mm[i, "id"] <- mm[i-1, "id"]
  }
  mm$timeStamp <- as.character(mm[ , 2])
  mm$mass <- (mm[ , 3])
  mm$note <- as.character(mm[ , 4])
  mm$header <- names(mm)[4]
  badLines <- mm[is.na(mm$mass) & mm$timeStamp == "", ]
  strangeLines <- mm[xor(!is.na(mm$mass), mm$timeStamp != ""), ]
  mm <- mm[!is.na(mm$mass) & mm$timeStamp != "", ]
  mm$fileName <- file
  goodMasses <- mm[ , 5:11]
  newFileName <- paste(file, "-goodMass.csv", sep= "")
  if(writeCsv) write.csv(goodMasses, file = newFileName, row.names = FALSE)
  ans = list(bad = badLines, strange = strangeLines, good = goodMasses)
  invisible(ans)
} # end function readMassFile
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
# if a call to readMassFile function throws and error, no notice is given
##################################################

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
##################################################


# examples

# investigateMassFiles()





#####################################################
# this function puts all good records together in one df and, optionally, writes a csv
##################################################

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
#####################################################

# examples

# combineMassFiles(writeCsv = FALSE)
# 
# xx <- combineMassFiles(writeCsv = FALSE)
# str(xx)

#####################################################
# this function finds mass files that don't make proper csvs
# probably resulting from an extra comma in the first line
##################################################


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
#####################################################




# save("readMassFile", 
#      "investigateMassFiles", 
#      "listBadFiles",
#      "combineMassFiles", 
#      file = "massFileFunctions.v02.RData")

# function for counting full achenes in a sample #####
full <- function(x, cut.off = 0.002) sum(x > cut.off)

# function for counting empty achenes in a sample #####
empty <- function(x, cut.off = 0.002) sum(x <= cut.off)

# function  for twist tie colors #####
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

# another function for twist-tie colors# #####
# this needs the entire list of legit colors and color combinations
# we could use a function is.standardTtColor that retuns a logical vector

all.standardTtColor <- function(x) {
  standard <- c("bac", "bacred", "bacwht", "blu", "bluclr", "bluyel", "clr", 
                "grn", "grnbac", "grnred", "grnwht", "org", "red", "redwht",
                "wht", "yel", "yelbac", "yelgrn", "nott")
  all(x %in% standard)
}
