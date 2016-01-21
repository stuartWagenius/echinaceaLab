#' This function can be used to check count data for errors
#' 
#' @param df is dataframe of count data. This dataframe will be the csv file downloaded from the Ech. Project website
#' @return output, a list containing 5 objects: mismatches, cantCount, checkNotes, missingCounts, and checkZeros
#' @return mismatches, counter noted that the envelope letNo in the scan did not match the computer letNo
#' @return cantCount, counter noted that the scan could not be counted
#' @return checkNotes, records where counter made a note. Check through notes!
#' @return missingCounts, letNos that were not counted 3 times
#' @return checkZeros, 
manageCountData <- function(df){
  
  mismatches <- df[!df$letno_matches_yn %in% "y", 
                   c("seed_count_assignment_id", "image_file_link", "letno", "experiment","batch", "assigned.user")]
  
  cantCount <- df[!df$can_count_yn %in% "y", 
                  c("seed_count_assignment_id", "image_file_link", "letno", "batch", "assigned.user")]
  
  checkNotes <- df[!df$notes %in% c("", NA), c("seed_count_assignment_id", "letno", "experiment", "batch", "assigned.user", 
                                        "count", "notes")]
  
  zz <- as.data.frame(table(as.character(df$letno)))
  colnames(zz) <- c("letno", "counts")
  checkLetnos <- zz[!zz$counts %in% 3, ]
  
  zeros <- unique(df$letno[df$count < 3 | df$count > 900])
  checkZeros <- df[df$letno %in% zeros, c("count", "seed_count_assignment_id", "letno", "experiment", "batch", "assigned.user")]
  checkZeros <- checkZeros[order(checkZeros$letno),]
  
  output <- list(mismatches = mismatches, 
                 cantCount = cantCount, 
                 checkNotes = checkNotes, 
                 missingCounts = checkLetnos, 
                 checkZeros = checkZeros)
  output
}

# manageCountData(exp1996_2012)


#' The batch summary function summarizes information from harvest datasheet (hh.year)
#'
#' @param hh harvest data frame (e.g. hh.2014)
#' @return summary, a summary of each cg batch including letNo range and the number of heads
batchSummary <- function(hh) {
  min <- ddply(hh, .(batch), function(x) min(x[,"No"]))
  max <- ddply(hh, .(batch), function(x) max(x[,"No"]))
  batchSize <- ddply(hh, .(batch), function(x) length(x[,"No"]))
  
  summary <- merge(min, max, by = "batch")
  summary <- merge(summary, batchSize, by = "batch")
  names(summary) <- c("batch", "startNo", "endNo", "batchSize")
  summary <- summary[order(summary$startNo),]
  summary
}


#' create assignment csv to upload
#' 
#' createCSV automates the creation of an upload-ready csv for the Echinacea Project's online counting database.
#' This function works for only achene count assignments, not X-rays.
#' The function is set up to create 3 records for each letno
#' (3 rounds of counting). The function randomizes the order of records,
#' no need for pre-upload randomization
#' 
#' @param scansdf is a data frame of scan filenames and letnos, scans (from function loadScans) by default
#' 
#' @param harvYear is the year heads were harvested, this needs to be entered
#' @param priority is the counting priority of the scans, 50 by default
#' @return returns out, an upload-ready data.frame
#' @return simply type in write.csv(out, "C:/assignmentYear.csv", row.names = FALSE to save)
createCSV = function(scansdf = scans, harvYear, priority = 50) {
  
  filePath = paste("C:/cg", harvYear, "Scans/", sep = "")
  batchName = gsub("[A-z][A-z]-", "", scansdf$letno)
  batchName = gsub("[0-9][0-9][0-9]$", "000", batchName)
  
  out <- data.frame(image_type = "achene",
                    image_file_link = rep(paste(filePath, scansdf$batch, "/", 
                                                scansdf$filename, sep = ""), 3),
                    letno = rep(scansdf$letno, 3), 
                    batch = rep(batchName, 3),
                    experiment = rep(scansdf$batch, 3),
                    harvest_year = harvYear,
                    priority = priority,
                    round = c(rep(1, length(scansdf$letno)), rep(2, length(scansdf$letno)),
                              rep(3, length(scansdf$letno))),
                    user = "")
  out <<- out[sample(nrow(out)),] # randomize rows
  return(paste("out, an upload-ready data frame is in your workspace. Save using write.csv()"))
}

#' Create and write an upload csv
#' 
#' This will create and write a csv with the counting assignments as it
#' should be ready to upload
#' 
#' @param scansFolder the folder in which the scans can be found
#' @param writeTo the file to which the upload csv will be written
#' @param year the harvest year of the scans
#' @param exprio a data frame containing two columns: (1) the experiments
#' to upload and (2) the priorities for the experiments. The first column
#' should have the experiments and the second column should have priorities
#' @param oneCt a data frame whose first column is experiments that should
#' only be counted once and whose second column is the username of the person
#' to count that experiment
#' @return none, write to a csv
writeUploadCSV <- function(scansFolder, writeTo, year, exprio, oneCt = NULL) {
# load in files and remove unecessary ones
  loadScans(path = scansFolder)
  scans <- scans[!(scans$filename %in% c("Thumbs.db", "itfiles.ini")),]
  
  # if there are no missing scans, create assignment csv and write to file
  createCSV(scansdf = scans, harvYear = year, priority = 50)
  # only take ones in the experiments we want to upload
  out <- out[out$experiment %in% exprio[,1],]
  # change priorities to what you want
  for (i in 1:nrow(exprio)) {
    out[out$experiment %in% exprio[i,1], "priority"] <- exprio[i,2]
  }

  if (!is.null(oneCt)) {
    # get only one record per head for these experiments
    out[out$experiment %in% oneCt[,1], "round"] <- 1
    out <- out[!duplicated(out),]
    # change this to decide who counts
    for (i in 1:nrow(oneCt)) {
      out[out$experiment %in% oneCt[i,1], "user"] <- oneCt[i,2]
    }
  }

  # write the file to a csv
  write.csv(out, writeTo, row.names = FALSE)
}