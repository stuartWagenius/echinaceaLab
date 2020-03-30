#' Write plots of characters for common garden experiments 1, 2, and 679
#' 
#' This function will take all data collected from a select common garden experiment
#' in a year and output summary plots - either boxplots or histograms - 
#' of the plant characters in that garden. Use it to diagnose any blatant data issues and
#' to print for folks to see the work they did!
#' 
#' 
#' 
#' @param mf path to the measure form for a given common garden experiment, generally in cgdata repo
#' (e.g. "summer2019/p1MeasureData/2019MeasureExPt1_2019-08-27.txt")
#' @param hf path to the head form for the same common garden experiment, generally in cgdata repo
#' (e.g. "summer2019/p1MeasureData/2019ExPt1HeadSubform_2019-09-27.txt")
#' @param year the year the data in the input file was recorded (e.g. 2019, as above)
#' @param box.plot logical: if TRUE, makeCGGraphs will output boxplots of plant characters.
#' if FALSE, makeCGGraphs will output histograms of plant characters.
#' @param write.pdf logical: if TRUE, function will output a pdf that will save to write.path (good for printing!!!)
#' if FALSE, function will output the plots into the R viewer.
#' @param write.path path to the folder you want to output the write.pdfs to
#' 
#' @details function will not work if input fields are spelled or capitalized incorrectly. They must be:
#' bslLongLfCm, caulLongLfCm, bslLfCt, bslRosetteCt, flRosetteCt, (in mf) and headHeight (in hf). If they
#' are spellect incorrectly, read the .txt into R, rename files, and save it as a .csv.
#' p679 are 3 different plots in real life, but are treated as one by this function. They should also share a
#' Pendragon measuring form.
#' mf and hf are REQUIRED fields. If there are no head records for a plot (likely only in p679),
#' make an empty hf and input that. 
#' 
#' 
#' @examples
#' \dontrun{makeCGGraphs("summer2019/p679MeasureData/2019MeasureHybExPt_2019-08-07.txt",
#' "summer2019/p679MeasureData/2019HybExPtHeadSubform_2019-08-07.txt",
#' 2019, box.plot = F, write.pdf = T, write.path = "summer2018/recheckFunctionTests")}
#' 
#' \dontrun{makeCGGraphs("summer2019/p1MeasureData/2019MeasureExPt1_2019-08-27.txt",
#' "summer2019/p1MeasureData/2019ExPt1HeadSubform_2019-09-27.txt",
#' 2019, box.plot = T, write.pdf = F)}
#' 
#' 
#' 
makeCGGraphs <- function(mf, hf, yr, box.plot = FALSE, write.pdf = FALSE, write.path = NA){
  m <- read.csv(mf, stringsAsFactors = F)
  h <- read.csv(hf, stringsAsFactors = F)
  year <- toString(yr)
  
  path <- write.path
  
  if (grepl("[P|p]t1", mf) & grepl("[P|p]t1", hf)){
    plot <- "exPt1"
    
  } else if (grepl("[P|p]t2", mf) & grepl("[P|p]t2", hf)){
    plot <- "exPt2"
    
  } else if ((grepl("[P|p]t7", mf) | grepl("[P|p]t9", mf) | grepl("[P|p]t6", mf) | grepl("[H|h]yb", mf)) 
             & (grepl("[P|p]t7", hf) | grepl("[P|p]t9", hf) | grepl("[P|p]t6", hf) | grepl("[H|h]yb", hf))){
    plot <- "exPt679"
    
  }
  
  if(box.plot == FALSE){
    if(write.pdf == TRUE){
      pdf(file = paste0(path, "/",year, plot, "Histogram.pdf"), width = 11, height = 8.5)
      par(mfrow = c(2,3), mar = c(6, 5, 5, 1.5))
      basHist <- hist(m$bslLongLfCm, breaks = 25, main = "Distribution of longest basal leaf", xlab = "Basal leaf length (cm)")
      caulHist <- hist(m$caulLongLfCm, breaks = 25, main = "Distribution of longest cauline leaf", xlab = "Cauline leaf length (cm)")
      lfHist <- hist(m$bslLfCt, breaks = 25, main = "Distribution of basal leaf number", xlab = "Basal leaf count")
      bRosHist <- hist(m$bslRosetteCt, breaks = 15, main = "Distribution of basal rosette counts", xlab = "Basal rosette count")
      fRosHist <- hist(m$flRosetteCt, breaks = 13, main = "Distribution of flowering rosette counts", xlab = "Flowering rosette count")
      hdHis <- hist(h$headHeight, breaks = 25, main = "Distribution of head heights", xlab = "Head height (cm)")
      mtext(paste("Distributions of plant characters in", plot, year), side = 3, line = -1.5, outer = TRUE)
      dev.off()
    }
    else if(write.pdf == FALSE){
      par(mfrow = c(2,3))
      basHist <- hist(m$bslLongLfCm, breaks = 25, main = "Distribution of longest basal leaf", xlab = "Basal leaf length (cm)")
      caulHist <- hist(m$caulLongLfCm, breaks = 25, main = "Distribution of longest cauline leaf", xlab = "Cauline leaf length (cm)")
      lfHist <- hist(m$bslLfCt, breaks = 25, main = "Distribution of basal leaf number", xlab = "Basal leaf count")
      bRosHist <- hist(m$bslRosetteCt, breaks = 15, main = "Distribution of basal rosette counts", xlab = "Basal rosette count")
      fRosHist <- hist(m$flRosetteCt, breaks = 13, main = "Distribution of flowering rosette counts", xlab = "Flowering rosette count")
      hdHis <- hist(h$headHeight, breaks = 25, main = "Distribution of head heights", xlab = "Head height (cm)")
      mtext(paste("Distributions of plant characters in", plot, year), side = 3, line = -1.5, outer = TRUE)
    }
  }
  else if(box.plot == TRUE){
    if(write.pdf == TRUE){
      pdf(file = paste0(path, "/",year, plot, "Boxplot.pdf"), width = 11, height = 8.5)
      par(mfrow = c(2,3), mar = c(3, 5, 7, 2))
      basHist <- boxplot(m$bslLongLfCm, main = "Distribution of longest basal leaf", ylab = "Basal leaf length (cm)")
      caulHist <- boxplot(m$caulLongLfCm, main = "Distribution of longest cauline leaf", ylab = "Cauline leaf length (cm)")
      lfHist <- boxplot(m$bslLfCt, main = "Distribution of basal leaf number", ylab = "Basal leaf count")
      bRosHist <- boxplot(m$bslRosetteCt, main = "Distribution of basal rosette counts", ylab = "Basal rosette count")
      fRosHist <- boxplot(m$flRosetteCt, main = "Distribution of flowering rosette counts", ylab = "Flowering rosette count")
      hdHis <- boxplot(h$headHeight, main = "Distribution of head heights", ylab = "Head height (cm)")
      mtext(paste("Distributions of plant characters in", plot, year), side = 3, line = -1.5, outer = TRUE)
      dev.off()
    }
    else if(write.pdf == FALSE){
      par(mfrow = c(2,3))
      basHist <- boxplot(m$bslLongLfCm, main = "Distribution of longest basal leaf", ylab = "Basal leaf length (cm)")
      caulHist <- boxplot(m$caulLongLfCm, main = "Distribution of longest cauline leaf", ylab = "Cauline leaf length (cm)")
      lfHist <- boxplot(m$bslLfCt, main = "Distribution of basal leaf number", ylab = "Basal leaf count")
      bRosHist <- boxplot(m$bslRosetteCt, main = "Distribution of basal rosette counts", ylab = "Basal rosette count")
      fRosHist <- boxplot(m$flRosetteCt, main = "Distribution of flowering rosette counts", ylab = "Flowering rosette count")
      hdHis <- boxplot(h$headHeight, main = "Distribution of head heights", ylab = "Head height (cm)")
      mtext(paste("Distributions of plant characters in", plot, year), side = 3, line = -1.5, outer = TRUE)
    }
  }
  
  if (box.plot == FALSE){
    if (write.pdf == TRUE){
      cat(paste("pdf of histograms of", year, plot, "plant characters available at:", paste0(path, "/",year, plot, "Histogram.pdf")))
    }
    else if (write.pdf == FALSE){
      cat(paste("see histograms of", year, plot, "plant characters in viewer"))
    }
  }
  else if (box.plot == TRUE){
    if (write.pdf == TRUE){
      cat(paste("pdf of boxplots of", year, plot, "plant characters available at:", paste0(path, "/",year, plot, "Boxplot.pdf")))
    }
    else if (write.pdf == FALSE){
      cat(paste("see boxplots of", year, plot, "plant characters in viewer"))
    }
  }
  
}


#' Create field check dataframes for common gardens 1, 2, and 679
#' 
#' This function will take common garden measuring forms from a given year and output
#' a field check data frame. This data frame will contain all "mismatched" action-statuses
#' and will have 5 neighbor statuses on each side of the focal position. This function also takes recheck
#' data frames, and will output all "mismatched" action-statuses from rechecks in the same format as the 
#' measuring form. The best way to search positions is by printing paper datsheets of the data frame
#' this function outputs.
#' 
#' 
#'
#' @param mf path to the measure form for a given common garden experiment, generally in cgdata repo
#' (e.g. "summer2019/p1MeasureData/2019MeasureExPt1_2019-08-27.txt")
#' @param rf path to the recheck form for the same common garden experiment, if rechecks are done.
#' If rechecks are not completed, this field should be FALSE (which is the default)
#' (e.g. "/summer2019/p1MeasureData/2019p1Recheck_2019-10-03.txt")
#' @param yr the year the data in the input file was recorded (e.g. 2019, as above)
#' @param write.it logical: if TRUE, function will output a csv that will save to the folder specified in write.path.
#' The csv will contain row-positions of mismatched action-statuses to field check (and 5 neighbors on each side).
#' if FALSE, function will output the plots into the R viewer.
#' @param write.path path to the folder you want to output the write.it csv to
#' @details use this function after measuring is finished for each common garden (1, 2, or all of 679).
#' The function will pull out all mismatched action-statuses (e.g. action=Staple and status=Basal).
#' Check all positions the function outputs, and then integrate the changes into the dataset before
#' running cgRecheck and cgRemeasure. This will save a TON of time and improve data reliability.
#' Also run this function and go into the plots when you finish stapling (p1 only) and rechecking. 
#' Once again, this will improve data reliability and will make your job easier in the future.
#' 
#' 
#' @examples
#' \dontrun{cgFieldCheck("summer2019/p2MeasureData/2019MeasureExPt2_2019-07-26.txt", yr = 2019,
#' write.it = T, write.path = "summer2019/recheckFunctionTests")}
#' 
#' \dontrun{cgFieldCheck("summer2019/p1MeasureData/2019MeasureExPt1_2019-09-30WithStaplesAndCorrections.csv",
#' rf = "summer2019/p1MeasureData/2019p1Recheck_2019-10-03.txt", 
#' 2019, write.it = T, write.path = "summer2019/recheckFunctionTests")}
#' 
#' 
#'
cgFieldCheck <- function(mf, rf = FALSE, yr, write.it = FALSE, write.path = NA){
  
  # read in data and decide which plot the data is from
  if (rf == FALSE){
    m <- read.csv(mf, stringsAsFactors = F)
    pp <- read.csv(mf, stringsAsFactors = F)
  }
  else if(rf != FALSE){
    m <- read.csv(rf, stringsAsFactors = F)
    pp <- read.csv(mf, stringsAsFactors = F)
  }
  
  if (grepl("[P|p]t1", mf) | grepl("[P|p]1", mf)){
    plot <- "p1"
  } else if (grepl("[P|p]t2", mf) | grepl("[P|p]2", mf)){
    plot <- "p2"
  } else if (grepl("[P|p]t7", mf) | grepl("[P|p]t9", mf) | grepl("[P|p]t6", mf) | grepl("[H|h]yb", mf) |
             grepl("[P|p]7", mf) | grepl("[P|p]9", mf) | grepl("[P|p]6", mf)){
    plot <- "hybrid"
  }
  
  # rechecks (and some measure files) have 'status' as 'statusThisYr... will rewrite for this.
  
  if ('statusThisYr' %in% colnames(m)){
    colnames(m)[colnames(m)=="statusThisYr"] <- "status"
  }
  
  if ('statusThisYr' %in% colnames(pp)){
    colnames(pp)[colnames(pp)=="statusThisYr"] <- "status"
  }
  
  # eliminate original neighborStatus from rechecks
  
  if ('neighborStatus' %in% colnames(m)){
    m <- m[,!grepl("neighborStatus",names(m))]
  }
  
  # correct cgPlaid:
  
  if ('cgPlaId' %in% colnames(m)){
    colnames(m)[colnames(m)=="cgPlaId"] <- "cgPlaid"
  }
  
  if ('cgPlaId' %in% colnames(pp)){
    colnames(pp)[colnames(pp)=="cgPlaId"] <- "cgPlaid"
  }
  
  # create wrongStatus... a dataframe that will report which statuses and actions are mismatched
  wrongStatus <<- data.frame(matrix(ncol = ncol(m), nrow = 0))
  colnames(wrongStatus) <<- colnames(m)
  
  # checks for mismatches in p1 action - statuses
  if(plot == "p1"){
    for (i in 1:nrow(m)){
      if (m[i,'action'] == "" & m[i,'status'] == "Rebar" | 
          m[i,'action'] == "" & m[i,'status'] == "Staple" |
          m[i,'action'] == "" & m[i,'status'] == "Wooden Stake Only" | 
          m[i,'action'] == "" & m[i,'status'] == "")
      { wrongStatus <<- rbind(wrongStatus, m[i,])
      }
      else if(m[i, 'action'] == "rebar or tin can only" & m[i, 'status'] == "Basal" |
              m[i, 'action'] == "rebar or tin can only" & m[i, 'status'] == "Dead Lvs (last year's)"|
              m[i, 'action'] == "rebar or tin can only" & m[i, 'status'] == "Dead Lvs (this year's)" | 
              m[i, 'action'] == "rebar or tin can only" & m[i, 'status'] == "" |
              m[i, 'action'] == "rebar or tin can only" & m[i, 'status'] == "Flowering"){
        wrongStatus <<- rbind(wrongStatus, m[i,])
      }
      else if(m[i, 'action'] == "Staple" & m[i, 'status'] == "Basal" |
              m[i, 'action'] == "Staple" & m[i, 'status'] == "Dead Lvs (last year's)"|
              m[i, 'action'] == "Staple" & m[i, 'status'] == "Dead Lvs (this year's)" |
              m[i, 'action'] == "Staple" & m[i, 'status'] == "" |
              m[i, 'action'] == "Staple" & m[i, 'status'] == "Flowering"){
        wrongStatus <<- rbind(wrongStatus, m[i,])
      }
      else if(m[i, 'action'] == "Pull flag" & m[i, 'status'] != ""){
        wrongStatus <<- rbind(wrongStatus, m[i,])
      }
      else if(m[i, 'action'] == "search" & m[i,'status'] == "Rebar" |
              m[i, 'action'] == "search" & m[i,'status'] == "Staple" | 
              m[i, 'action'] == "search" & m[i,'status'] == "Wooden Stake Only" |
              m[i, 'action'] == "search" & m[i,'status'] == ""){
        wrongStatus <<- rbind(wrongStatus, m[i,])
      }
      else if(m[i, 'action'] == "staple?" & m[i, 'status'] != "Staple"){
        wrongStatus <<- rbind(wrongStatus, m[i,])
      } 
      else if(m[i, 'action'] == "remeasure" & m[i, 'status'] == "Staple" |
              m[i, 'action'] == "Remeasure" & m[i, 'status'] == "Staple" |
              m[i, 'action'] == "remeasure" & m[i, 'status'] == "Rebar" |
              m[i, 'action'] == "Remeasure" & m[i, 'status'] == "Rebar" |
              m[i, 'action'] == "remeasure" & m[i, 'status'] == "Wooden Stake Only" |
              m[i, 'action'] == "Remeasure" & m[i, 'status'] == "Wooden Stake Only" |
              m[i, 'action'] == "Remeasure" & m[i, 'status'] == "" |
              m[i, 'action'] == "remeasure" & m[i, 'status'] == ""){
        wrongStatus <<- rbind(wrongStatus, m[i,])
      } 
      else if(m[i, 'action'] != "" & 
              m[i, 'action'] != "rebar or tin can only" &
              m[i, 'action'] != "Staple" &
              m[i, 'action'] != "search" &
              m[i, 'action'] != "staple?" &
              m[i, 'action'] != "pull flag" &
              m[i, 'action'] != "remeasure" &
              m[i, 'action'] != "Remeasure" &
              m[i, 'action'] != "Pull Flag"){
        wrongStatus <<- rbind(wrongStatus, m[i,])
      }
    }
  }
  
  # check for mismatchs if p2 or hybrid plot. It is much less involved
  else if(plot == "p2" | plot == "hybrid"){
    for (j in 1:nrow(m)){
      if(m[j, 'action'] == "Skip" & m[j, 'status'] != "" ){
        wrongStatus <<- rbind(wrongStatus, m[j,])
      }
      else if(m[j, 'action'] == "Pull flag" & m[j, 'status'] != ""){
        wrongStatus <<- rbind(wrongStatus, m[j,])
      }
      else if(m[j, 'action'] == "Pull flag (3 yrs cf)" & m[j, 'status'] != ""){
        wrongStatus <<- rbind(wrongStatus, m[j,])
      }
      else if(m[j, 'action'] == "" & m[j, 'status'] == ""){
        wrongStatus <<- rbind(wrongStatus, m[j,])
      }
      else if(m[j, 'action'] == "search" & m[j, 'status'] == ""){
        wrongStatus <<- rbind(wrongStatus, m[j,])
      }
      else if(m[j, 'action'] == "Remeasure" & m[j, 'status'] == "" |
              m[j, 'action'] == "remeasure" & m[j, 'status'] == ""){
        wrongStatus <<- rbind(wrongStatus, m[j,])
      }
      else if(m[j, 'action'] != "" & 
              m[j, 'action'] != "search" &
              m[j, 'action'] != "Pull Flag" &
              m[j, 'action'] != "pull flag" &
              m[j, 'action'] != "remeasure" &
              m[j, 'action'] != "Remeasure" &
              m[j, 'action'] != "Skip"){
        wrongStatus <<- rbind(wrongStatus, m[j,])
      }
    }
  }
  
  # basically, we will now write a crazy large nearbyNeighbors function... so yeah
  
  pp$abbStat <- ""
  
  # first, create abbreviated status field in m.  
  # no status is "o" (open)... where a record has nothing in the status field
  for (o in 1:nrow(pp)){
    if(pp[o, 'status'] == "Can't Find"){
      pp[o, 'abbStat'] <- "cf"
    } 
    else if(pp[o, 'status'] == "Staple" | pp[o, 'status'] == "skip"){
      pp[o, 'abbStat'] <- "s"
    }
    else if(pp[o, 'status'] == "Basal"){
      pp[o, 'abbStat'] <- "b"
    }
    else if(pp[o, 'status'] == "Flowering"){
      pp[o, 'abbStat'] <- "fl"
    }
    else if (pp[o, 'status'] == "Dead Lvs (last year's)" | pp[o, 'status'] == "Dead Lvs (this year's)"){
      pp[o, 'abbStat'] <- "dead"
    }
    else if (pp[o, 'status'] == "Rebar"){
      pp[o, 'abbStat'] <- "reb"
    }
    else if (pp[o, 'status'] == "Wooden Stake Only"){
      pp[o, 'abbStat'] <- "wood"
    }
    else if (pp[o, 'status'] == ""){
      pp[o, 'abbStat'] <- "o"
    }
  }
  
  pp <- pp[order(pp$row, pp$position),]
  
  # create 5-deep neighbor statuses! woo!  
  pp$neighborStatus <- ""
  for (x in 1:nrow(pp)){
    if (pp[x, 'cgPlaid'] %in% wrongStatus$cgPlaid){
      ifelse(isTRUE(pp$row[x] == pp$row[x-5]), b5 <- pp[x - 5 ,"abbStat"], b5 <- "NA")
      ifelse(isTRUE(pp$row[x] == pp$row[x-4]), b4 <- pp[x - 4 ,"abbStat"], b4 <- "NA")
      ifelse(isTRUE(pp$row[x] == pp$row[x-3]), b3 <- pp[x - 3 ,"abbStat"], b3 <- "NA")
      ifelse(isTRUE(pp$row[x] == pp$row[x-2]), b2 <- pp[x - 2 ,"abbStat"], b2 <- "NA")
      ifelse(isTRUE(pp$row[x] == pp$row[x-1]), b1 <- pp[x - 1 ,"abbStat"], b1 <- "NA")
      ifelse(isTRUE(pp$row[x] == pp$row[x+1]), a1 <- pp[x + 1 ,"abbStat"], a1 <- "NA")
      ifelse(isTRUE(pp$row[x] == pp$row[x+2]), a2 <- pp[x + 2 ,"abbStat"], a2 <- "NA")
      ifelse(isTRUE(pp$row[x] == pp$row[x+3]), a3 <- pp[x + 3 ,"abbStat"], a3 <- "NA")
      ifelse(isTRUE(pp$row[x] == pp$row[x+4]), a4 <- pp[x + 4 ,"abbStat"], a4 <- "NA")
      ifelse(isTRUE(pp$row[x] == pp$row[x+5]), a5 <- pp[x + 5 ,"abbStat"], a5 <- "NA")
      pp$neighborStatus[x] <- paste(b5, b4, b3, b2, b1, "X", a1, a2, a3, a4, a5, sep = "-")
    }
  }
  
  # need to now make a small dummy dataframe, with just cgPlaid and neighborStatus... to join with wrongStatus
  cn <- data.frame("cgPlaid" = pp$cgPlaid, "neighborStatus" = pp$neighborStatus)
  cn <- cn[cn$neighborStatus != "",]
  
  wrongStatus <<- merge(wrongStatus, cn)
  wrongStatus <<- wrongStatus[order(wrongStatus$row, wrongStatus$position),]
  wrongStatus <<- wrongStatus[!duplicated(wrongStatus$cgPlaid),]
  
  # write file path!
  if (rf == FALSE){
    if (write.it == TRUE & plot == "p1"){
      mFile <- paste0("fieldCheckExPt1", yr, ".csv", sep = "")
      write.csv(wrongStatus, paste(write.path, mFile, sep = "/"), row.names=F)
    }
    else if (write.it == TRUE & plot == "p2"){
      mFile <- paste0("fieldCheckExPt2", yr, ".csv", sep = "")
      write.csv(wrongStatus, paste(write.path, mFile, sep = "/"), row.names=F)
    }
    else if (write.it == TRUE & plot == "hybrid"){
      mFile <- paste0("fieldCheckHybExPt", yr, ".csv", sep = "")
      write.csv(wrongStatus, paste(write.path, mFile, sep = "/"), row.names=F)
    }
  }
  
  if (rf != FALSE){
    if (write.it == TRUE & plot == "p1"){
      mFile <- paste0("fieldCheckRechecksExPt1", yr, ".csv", sep = "")
      write.csv(wrongStatus, paste(write.path, mFile, sep = "/"), row.names=F)
    }
    else if (write.it == TRUE & plot == "p2"){
      mFile <- paste0("fieldCheckRechecksExPt2", yr, ".csv", sep = "")
      write.csv(wrongStatus, paste(write.path, mFile, sep = "/"), row.names=F)
    }
    else if (write.it == TRUE & plot == "hybrid"){
      mFile <- paste0("fieldCheckRechecksHybExPt", yr, ".csv", sep = "")
      write.csv(wrongStatus, paste(write.path, mFile, sep = "/"), row.names=F)
    }
  }
  
  
  if(plot == "p1"){
    longPlot <- "exPt1"
  }
  else if (plot == "p2"){
    longPlot <- "exPt2"
  }
  else if (plot == "hybrid"){
    longPlot <- "exPt679"
  }
  
  if (rf == FALSE){
    if (write.it == FALSE){
      cat(paste("data frame 'wrongStatus' contains field checks for", longPlot, yr))
    }
    else if (write.it == TRUE){
      cat(paste("data frame of", longPlot, yr, "field checks available at", paste("cgdata",write.path, mFile, sep = "/")))
      cat("\n")
      cat("\n")
      cat(paste("also, data frame object 'wrongStatus' contains field checks for", longPlot, yr))
    }
  }
  else if (rf != FALSE){
    if (write.it == FALSE){
      cat(paste("data frame 'wrongStatus' contains field checks for rechecks in", longPlot, yr))
    }
    else if (write.it == TRUE){
      cat(paste("data frame of", longPlot, yr, "recheck field checks available at", paste("cgdata", write.path, mFile, sep = "/")))
      cat("\n")
      cat("\n")
      cat(paste("also, data frame object 'wrongStatus' contains field checks for rechecks in", longPlot, yr))
    }
  }
  cat("\n")
  cat("\n")
  cat(paste("table below contains action-statuses of", longPlot, yr, ifelse(rf != FALSE, "rechecks", "")))
  print(table(m$status, m$action))
}


#' Create recheck, staple, and skip data frames for common gardens
#' 
#' This function will take common garden measuring forms from a given year and output
#' recheck and future skips/staple data frames. 
#' 
#'
#' @param mf path to the measure form for a given common garden experiment in cgdata repo. It should have updates
#' implemented from the cgFieldCheck function
#' (e.g. "summer2019/p1MeasureData/2019MeasureExPt1_2019-09-30WithStaplesAndCorrections.csv")
#' @param yr the year the data in the input file was recorded (e.g. 2019)
#' @param userNum the number of visor users you want to assign to do rechecks. Default is one. 
#' Visor users will be added in alphabetical order from "vv" in 'build.R'. Make sure the file 'build.R' in cgData 
#' has been updated to your year when you run this function. You are encouraged to find and replace users in the
#' final data frame, as the alphabetically first visor users may not be the ones you want to use for rechecks.
#' @param write.it logical: if TRUE, function will output two csvs that will save to the folder specified in write.path.
#' The csvs are: recheckExPt_20__.csv and stapleListExPt120__ or futureSkipsexPt_20__, depending on the plot.
#' The recheck csv has the list of rechecks and recheck actions (skip, search, staple?, etc..).
#' The stapleList csv has the list of new positions to staple this year BEFORE DOING RECHECKS.
#' The futureSkips csv has the list of future skips in p2 or p679 - plants that have been 3 yrs cf. 
#' It's mostly just a referenece to prepare data to be collected next field season. 
#' @param write.path path to the folder you want to output the write.it csvs to
#' @details use this function after measuring and the first round of field checks are finished for each common garden 
#' (1, 2, or all of 679). This function will pull out all "Can't Finds" to be rechecked (or skipped) and output a dataFrame
#' of those "Can't Finds" with actions on how to treat them in rechecks. If plants are 3-yr "Can't Finds," this function will
#' pull those out as well and return lists of "staple" positions or "future skips;" these positions will be "pull flags" in
#' the recheck data frame. The previous years' measuring data must be uploaded to the Echinacea Project Website because this 
#' function pulls from "core" datasets. That said, it may take a bit to run, as web links to csvs aren't always the quickest. 
#' 
#' 
#' 
#' @examples
#' \dontrun{recheckCG("summer2019/p1MeasureData/2019MeasureExPt1_2019-09-30WithStaplesAndCorrections.csv", 2019, userNum = 6,
#' write.it = T, write.path = "summer2018/recheckFunctionTests")}
#' 
#' \dontrun{recheckCG("summer2018/p679measureData/2018MeasureHybridExPts_2018-08-02.txt", 2018, userNum = 7,
#' write.it = F)}
#' 
#' 
#'
recheckCG <- function(mf, yr, userNum = 1, write.it = FALSE, write.path = NA){
  
  # assign name to dataframe 
  m <- read.csv(mf, stringsAsFactors = FALSE)
  
  # function decides which plot is used and how to treat data (ie. p1 needs staples)
  # here, we also obtain core data, so we can compare this years' data with previous.
  # we need this for Staple/Skip actions in the future
  if (grepl("[P|p]t1", mf)){
    # read core
    plot <- "p1"
    core <<- read.csv("http://echinaceaproject.org/data/cg1CoreData.csv")
    
    maxRow <- 56
    minRow <- 0
    maxPos <- 983
    minPos <- 860
  } else if (grepl("[P|p]t2", mf)){
    # read core
    plot <- "p2"
    core <<- read.csv("http://echinaceaproject.org/data/exPt2CoreData.csv")
    
    maxRow <- 80
    minRow <- 1
    maxPos <- 50
    minPos <- 0
  } else if ((grepl("[P|p]t7", mf) | grepl("[P|p]t9", mf) | grepl("[P|p]t6", mf) | grepl("[H|h]yb", mf))){
    plot <- "p7"
    core6 <- read.csv("http://echinaceaproject.org/data/exPt6HybridMeasure.csv")
    core7 <- read.csv("http://echinaceaproject.org/data/exPt7HybridMeasure.csv")
    core9 <- read.csv("http://echinaceaproject.org/data/exPt9HybridMeasure.csv")
    
    gc <- c("cgPlaId", "measureYr", "plaStatusDesc")
    
    core6 <- core6[, colnames(core6) %in% gc]
    core7 <- core7[, colnames(core7) %in% gc]
    core9 <- core9[, colnames(core9) %in% gc]
    
    coreh <- rbind(core6, core7, core9)
    coreh$ld <- ifelse(grepl("basal|flow|this y", coreh$plaStatusDesc), 1, 0)
    coreh <- coreh[,!(colnames(coreh) %in% "plaStatusDesc")]
    
    core <<- reshape(coreh, timevar = "measureYr", idvar = "cgPlaId", direction = "wide")
    colnames(core) <<- gsub("\\.", "", colnames(core))
    
    maxRow  <- 702
    minRow <- 1
    maxPos <- 30
    minPos <- 0.5
  }
  
  # neighbor status. This code chunk creates neighbor status field for rechecks
  m <- m[order(m$row, m$position),]
  
  if (plot == "p1"){
    for (i in 1:nrow(m)){
      if (m$status[i] == "" & m$action[i] == "Staple"){
        m$status[i] <- m$action[i]
      }
    }
  } else if (plot == "p2" | plot == "p7"){
    for (i in 1:nrow(m)){
      if (m$status[i] == "" & m$action[i] == "Skip"){
        m$status[i] <- m$action[i]
      }
    }
  }
  
  
  for (i in 1:nrow(m)){
    if(isTRUE(m$row[i] == m$row[i-1])){
      b4 <- m[i - 1 ,"status"]
      if (b4 == "Can't Find"){
        b4 <- "cf"
      }else if (b4 == "Staple" | b4 == "Skip"){
        b4 <- "s"
      }else if (b4 == "Basal"){
        b4 <- "b"
      }else if (b4 == "Flowering"){
        b4 <- "fl"
      }else if (b4 == "Dead Lvs (last year's)" | b4 == "Dead Lvs (this year's)"){
        b4 <- "dead"
      }
    } else{b4 <- "NA"}
    if(isTRUE(m$row[i] == m$row[i+1])){
      a4 <- m[i + 1 ,"status"]
      if (a4 == "Can't Find"){
        a4 <- "cf"
      }else if (a4 == "Staple" | a4 == "Skip"){
        a4 <- "s"
      }else if (a4 == "Basal"){
        a4 <- "b"
      }else if (a4 == "Flowering"){
        a4 <- "fl"
      }else if (a4 == "Dead Lvs (last year's)" | a4 == "Dead Lvs (this year's)"){
        a4 <- "dead"
      }
    } else {a4 <- "NA"}
    m$neighborStatus[i] <- paste(b4, a4, sep = "-")
  }
  
  m <<- m 
  
  # create dataframe of can't finds
  cantFinds <<- m[m$status %in% "Can't Find",]
  
  # get core dataset prev yrs set
  
  prevYr <- yr - 1
  prevYr2 <- yr - 2
  ldp <- paste("ld", prevYr, sep = "")
  ldp2 <- paste("ld", prevYr2, sep = "")
  
  if ("cgPlaId" %in% colnames(cantFinds)){
    colnames(cantFinds)[colnames(cantFinds) %in% "cgPlaId"] <- "cgPlaid"
  }  
  if ("cgPlaID" %in% colnames(cantFinds)){
    colnames(cantFinds)[colnames(cantFinds) %in% "cgPlaID"] <- "cgPlaid"
  } 
  
  if ("cgPlaId" %in% colnames(core)){
    colnames(core)[colnames(core) %in% "cgPlaId"] <- "cgPlaid"
  }  
  if ("cgPlaID" %in% colnames(core)){
    colnames(core)[colnames(core) %in% "cgPlaID"] <- "cgPlaid"
  }  
  
  # merge core and cantFinds
  
  cantFinds <- merge(cantFinds, core[,colnames(core) %in% c("cgPlaid", ldp, ldp2)], by = "cgPlaid", all.x = T)
  
  # p1: pull out staple list (3 years cf and staple CFs) then search for 1 + 2 year cfs!
  
  if (plot %in% "p1"){
    stapleList <<- cantFinds[cantFinds$action %in% "Staple" | (cantFinds$action %in% "" & 
                                                                 cantFinds[,ldp] %in% 0 & cantFinds[, ldp2] %in% 0),]
    cantFinds$rechAction <- ""
    cantFinds$rechAction <- as.character(cantFinds$rechAction)
    
    for (i in 1:nrow(cantFinds)){
      if (cantFinds[i, "cgPlaid"] %in% stapleList$cgPlaid){
        cantFinds[i, "rechAction"] <- "Staple?"
      } else {
        cantFinds[i, "rechAction"] <- "search"
      }
    }
    
    stapleList <- stapleList[c("cgPlaid", "row", "position", "neighborStatus")]
    stapleList$action <- "new staple"
    stapleList <- stapleList[order(stapleList$cgPlaid),]
    stapleList <<- stapleList
    
  } else if (plot %in% "p2" | plot %in% "p7"){
    cantFinds <- cantFinds[!(cantFinds$action %in% "Skip"),]
    
    futureSkips <<- cantFinds[cantFinds$action %in% "" & cantFinds[,ldp] %in% 0 & cantFinds[, ldp2] %in% 0,]
    
    cantFinds$rechAction <- ""
    cantFinds$rechAction <- as.character(cantFinds$rechAction)
    
    for (i in 1:nrow(cantFinds)){
      if (cantFinds[i, "cgPlaid"] %in% futureSkips$cgPlaid){
        cantFinds[i, "rechAction"] <- "Pull flag (3 yrs cf)"
      } else {
        cantFinds[i, "rechAction"] <- "search"
      }
    }
    
    futureSkips <- futureSkips[c("cgPlaid", "row", "position", "neighborStatus", "plot")]
    futureSkips$action <- "new skip"
    futureSkips <- futureSkips[order(futureSkips$cgPlaid),]
    futureSkips <<- futureSkips
    
  } 
  
  
  cantFinds$action <- cantFinds$rechAction
  cantFinds <- cantFinds[,!(colnames(cantFinds) %in% "rechAction")]
  
  goodCols <- c("cgPlaid", "plot", "expNm","segmentCd", "RecordId", "UnitID",
                "flRosetteCt", "bslRosetteCt", "bslLfCt", "bslLongLfCm", "caulLongLfCm", 
                "UserName", "TimeStamp", "row", "position", "action", "status", "neighborStatus", ldp, ldp2) 
  
  cantFinds <- cantFinds[, colnames(cantFinds) %in% goodCols]
  
  cantFinds <- cantFinds[order(cantFinds$row, cantFinds$position),]
  
  # empty columns   
  cantFinds$status <- ''
  cantFinds$UserName <- ''
  cantFinds$flRosetteCt <- NA
  cantFinds$bslRosetteCt <- NA
  cantFinds$bslLfCt <- NA
  cantFinds$bslLongLfCm <- NA
  cantFinds$caulLongLfCm <- NA
  
  # add username to cols
  source("build.R")
  # vv is the list of visor usernames for the year in build.R
  no <- userNum
  v <- 1
  while (v <= no){
    for (u in 1:nrow(cantFinds)){
      if (u/nrow(cantFinds) <= v/no & u/nrow(cantFinds) > (v-1)/no){
        cantFinds[u, 'UserName'] <- vv[v]
      }
    }
    v <- v + 1
  }
  cantFinds <<- cantFinds
  
  if (write.it == TRUE & plot == "p1"){
    cfFile <- paste0("recheckExPt1", yr, ".csv", sep ="")
    write.csv(cantFinds, paste(write.path, cfFile, sep = "/"), row.names = F)
    sFile <- paste0("stapleListExPt1", yr, ".csv", sep = "")
    write.csv(stapleList, paste(write.path, sFile, sep = "/"), row.names = F)
  } else if (write.it == TRUE & plot == "p2"){
    cfFile <- paste0("recheckExPt2", yr, ".csv", sep ="")
    write.csv(cantFinds, paste(write.path, cfFile, sep = "/"), row.names = F)
    sFile <- paste0("futureSkipsExPt2", yr, ".csv", sep = "")
    write.csv(futureSkips, paste(write.path, sFile, sep = "/"), row.names = F)
    
  } else if (write.it == TRUE & plot == "p7"){
    cfFile <- paste0("recheckExPt679", yr, ".csv", sep = "")
    write.csv(cantFinds, paste(write.path, cfFile, sep = "/"), row.names = F)
    sFile <- paste0("futureSkipsExPt679", yr, ".csv", sep = "")
    write.csv(futureSkips, paste(write.path, sFile, sep = "/"), row.names = F)
    
  }
  
  if(plot == "p1"){
    longPlot <- "exPt1"
  }
  else if (plot == "p2"){
    longPlot <- "exPt2"
  }
  else if (plot == "p7"){
    longPlot <- "exPt679"
  }
  
  
  # get to cating things!
  if (write.it == TRUE){
    cat(paste("the csv", paste(write.path, cfFile, sep = "/"), "contains a data frame of Can't Finds to recheck and recheck actions for", longPlot, yr))
    cat("\n")
    cat("\n")
    if (plot == "p1"){
      cat(paste("the csv", paste(write.path, sFile, sep = "/"), "contains a list of positions to add staples to in fall", yr))
      cat("\n")
      cat("\n")
    } else if (plot == "p2" | plot == "p7"){
      cat(paste("the csv", paste(write.path, sFile, sep = "/"), "contains a list of positions to Skip after", yr, "in", longPlot))
      cat("\n")
      cat("\n")
    }
  }
  
  cat(paste("data frame cantFinds contains all can't finds and recheck actions for", longPlot, "in", yr))
  cat("\n")
  cat("\n")
  if (plot == "p1"){
    cat(paste("data frame stapleList contains list of staples to add in exPt1 in", yr))
    cat("\n")
    cat("\n")
  } else {
    cat(paste("data frame futureSkips contains list of all new 3-year can't finds for", longPlot, "in", yr))
    cat("\n")
    cat("\n")
  }
  
  
  
}


#' Create remeasure and updated measure data frames for common garden experiments
#' 
#' This function will take common garden measuring forms from a given year and output
#' remeasure forms. It will also output an IMPROVED (final) measure csv for that year. (Final before
#' adding in rechecks and remeasures, that is).
#' 
#'
#' @param mf path to the measure form for a given common garden experiment in cgdata repo. It should have updates
#' implemented from the cgFieldCheck function
#' (e.g. "summer2019/p1MeasureData/2019MeasureExPt1_2019-09-30WithStaplesAndCorrections.csv")
#' @param hf path to the head form for a given common garden experiment in the cgdata repo
#' (e.g. "summer2019/p1MeasureData/2019ExPt1HeadSubform_2019-09-27.txt)
#' @param yr the year the data in the input file was recorded (e.g. 2019)
#' @param userNum the number of visor users you want to assign to do remeasures. Default is one. 
#' Visor users will be added in alphabetical order from "vv" in 'build.R'. Make sure the file 'build.R' in cgData 
#' has been updated to your year when you run this function. You are encouraged to find and replace users in the
#' final data frame, as the alphabetically first visor users may not be the ones you want to use for rechecks.
#' @param write.it logical: if TRUE, function will output three csvs that will save to the folder specified in write.path.
#' The csvs are: remeasureExPt_20__.csv, headsToRemeasureExPt_20__, and updated20__MeasureExPt__Final.csv
#' The remeasure has the list of remeasures and notes of why they're remeasures. It includes all data from original
#' measuring form, so the cgDirector can choose what is important to include and what to exclude.
#' The headsToRemeasure csv has a list of heads that have parts that need remeasuring. It's a reference, as all
#' plants in the headsToRemeasure.csv will also be in the remeasure csv.
#' The updated Measure csv is an improved measure form for a given year. It adds neighbor status and also shifts
#' some data to correct fields. Thus, THIS SHOULD BE CONSIDERED THE NEW FINAL MEASURE FORM FOR THE YEAR
#' (until rechecks and remeasures are implemented, or course!). 
#' @param write.path path to the folder you want to output the write.it csvs to
#' @details use this function after the first round of field checks are finished for each common garden experiment
#' (exPt 1, 2, or all of 679). This function will pull out all positions that need remeasures due to some form of
#' discrepancy, whether it's an unrealitic measure or just missing data. The output csv remeasureExPt_20__.csv will 
#' have explanations for everything that was pulled out. The function will also pull out a head form of remeasures,
#' but that form is simply a reference; all positions in that form will also be in the normal remeasure csv. FINALLY,
#' this function will make minor adjustments to the original measure csv (with field checks implemented). It will output
#' a csv (updated20__MeasureExPt__Final.csv) that has neighborStatus and measurements moved to the correct columns
#' in cases that the measures were in incorrect columns. This should be the new official final measure data frame
#' UNTIL rechecks and remeasures are implemented (into that df, actually).
#' 
#' 
#' @examples
#' \dontrun{remeasureCG("summer2019/p1MeasureData/2019MeasureExPt1_2019-09-30WithStaplesAndCorrections.csv",
#' "summer2019/p1MeasureData/2019ExPt1HeadSubform_2019-09-27.txt", 2019, write.it = T, userNum = 4,
#' write.path = "summer2018/recheckFunctionTests")}
#' 
#' 
#' 
remeasureCG <- function(mf, hf, yr, userNum = 1, write.it = FALSE, write.path = NA){
  
  m <- read.csv(mf, stringsAsFactors = FALSE)
  h <- read.csv(hf, stringsAsFactors = FALSE)
  
  # add cols - need a remeasure reasoning and rp + rph
  m$rp <- paste(m$row, m$position)
  m$remeasureNote <- ""
  
  h$rp <- paste(h$row, h$position)
  h$rph <- paste(h$row, h$position, h$head)
  h$remeasureNote <- ""
  
  # get cgPlaid in head
  h <- merge(h, m[, colnames(m) %in% c("rp", "cgPlaid")], by = "rp", all.x = T)
  
  #create mr and hr, remeasure dataframes (empty for now)
  mr <- m[0,]
  hr <- h[0,]
  
  # pick out the correct plot(s) we are inputting ####
  # function decides which plot is used and how to treat data (ie. p1 needs staples)
  # also collecing the core datasets here - perchaps useful later???
  # we need this for staple/skip actions in the future
  if (grepl("[P|p]t1", mf)){
    # read core
    plot <- "p1"
    
    maxRow <- 56
    minRow <- 0
    maxPos <- 983
    minPos <- 860
  } else if (grepl("[P|p]t2", mf)){
    # read core
    plot <- "p2"
    
    maxRow <- 80
    minRow <- 1
    maxPos <- 50
    minPos <- 0
  } else if ((grepl("[P|p]t7", mf) | grepl("[P|p]t9", mf) | grepl("[P|p]t6", mf) | grepl("[H|h]yb", mf))){
    plot <- "p7"
    
    maxRow <- 702
    minRow <- 1
    maxPos <- 30
    minPos <- 0.5
  }
  
  # assign max/min measures based on plot. ####
  # hybrid plots will have slightly higher maxes, as E. pallida is just larger!
  # numbers based on Amy W's original recheck measure function.
  if (plot %in% "p1" | plot %in% "p2"){
    bsl.lf.max = 54
    caul.lf.max = 46
    bsl.ros.max = 10
    bsl.ct.max = 45
    fl.ros.max = 7
    hd.height.max = 85
    hd.height.min = 20
    
  } else if (plot %in% "p7"){
    bsl.lf.max = 58
    caul.lf.max = 50
    bsl.ros.max = 10
    bsl.ct.max = 45
    fl.ros.max = 7
    hd.height.max = 85
    hd.height.min = 20
  }
  
  # i think it's ok to pull a single position more than once - as long as we have a good note for why we pulled it.
  
  # correct any basals that are all messed up so they don't get pulled out by the function/
  # essentially, this is looking for "Basal" records that have the measurements in the wrong place - like shifted over cells
  
  # shifted to start in flRosette:
  shiftR <<- m[m$status %in% "Basal" & is.na(m$bslLongLfCm) & is.na(m$caulLongLfCm) & 
                 !is.na(m$flRosetteCt) & !is.na(m$bslRosetteCt) & !is.na(m$bslLfCt),]
  
  shiftL <<- m[m$status %in% "Basal" & !is.na(m$bslLongLfCm) & !is.na(m$caulLongLfCm) & 
                 is.na(m$flRosetteCt) & is.na(m$bslRosetteCt) & !is.na(m$bslLfCt),]
  
  nshift <<- m[!(m$rp %in% shiftR$rp) & !(m$rp %in% shiftL$rp),]
  
  if(nrow(shiftR) > 0){
    shiftR$bslLongLfCm <- shiftR$bslLfCt
    shiftR$bslLfCt <- shiftR$bslRosetteCt
    shiftR$bslRosetteCt <- shiftR$flRosetteCt
    shiftR$flRosetteCt <- NA
  }
  
  if(nrow(shiftL) > 0){
    shiftL$bslRosetteCt <- shiftL$bslLfCt
    shiftL$bslLfCt <- shiftL$bslLongLfCm
    shiftL$bslLongLfCm <- shiftL$caulLongLfCm
    shiftL$caulLongLfCm <- NA
  }
  
  m <- rbind(nshift, shiftR, shiftL)
  m <- m[order(m$rp),]
  
  # 1. do flRosettes number and number of heads match?? ####
  ch <- unique(c(m[m$status %in% "Flowering",]$rp, h$rp))
  
  for (i in ch){
    sh <- h[h$rp %in% i,]
    sm <- m[m$rp %in% i,]
    if (nrow(sh) %in% 0){
      sm$remeasureNote <- "flRosetteCt and hdCt (headform) don't match"
      mr <- rbind(mr, sm)
    }
    else if(!isTRUE(sm$flRosetteCt == (nrow(sh) - nrow(sh[!(sh$onSameStemAs %in% ""), ])))){
      sm$remeasureNote <- "flRosetteCt and hdCt (headform) don't match"
      sh$remeasureNote <- "flRosetteCt and hdCt (headform) don't match"
      mr <- rbind(mr, sm)
      hr <- rbind(hr, sh)
    }
  }
  
  # this is good and adds stuff to mr and hr df - this bit is done!!
  
  # 2. are plants too big? ####
  # basal rosette measures
  bbl <- m[!is.na(m$bslLongLfCm) & m$bslLongLfCm >= bsl.lf.max,]
  if(nrow(bbl) > 0){bbl$remeasureNote <- "basal lf above length threshold"}
  mr <- rbind(mr, bbl)
  
  bbr <- m[!is.na(m$bslRosetteCt) & m$bslRosetteCt >= bsl.ros.max,]
  if(nrow(bbr) > 0){bbr$remeasureNote <- "basal rosette number above threshold"}
  mr <- rbind(mr, bbr)
  
  bbc <- m[!is.na(m$bslLfCt) & m$bslLfCt >= bsl.ct.max,]
  if(nrow(bbc) > 0){bbc$remeasureNote <- "basal lfCt above threshold"}
  mr <- rbind(mr, bbc)
  
  # fl rosette measures
  bcl <- m[!is.na(m$caulLongLfCm) & m$caulLongLfCm >= caul.lf.max,]
  if(nrow(bcl) > 0){bcl$remeasureNote <- "cauline lf above length threshold"}
  mr <- rbind(mr, bcl)
  
  brf <- m[!is.na(m$flRosetteCt) & m$flRosetteCt >= fl.ros.max,]
  if(nrow(brf) > 0){brf$remeasureNote <- "fl rosette number above threshold"}
  mr <- rbind(mr, brf)
  
  # head heights
  bhh <- h[!is.na(h$headHeight) & h$headHeight >= hd.height.max,]
  if(nrow(bhh) > 0){bhh$remeasureNote <- "head height above threshold"}
  hr <- rbind(hr, bhh)
  
  bashh <- m[m$rp %in% bhh$rp,]
  if(nrow(bashh) > 0){bashh$remeasureNote <- "head height above threshold"}
  mr <- rbind(mr, bashh)
  
  lhh <- h[!is.na(h$headHeight) & h$headHeight <= hd.height.min & (grepl("normal", h$headStatus) | (h$headStatus %in% "")),]
  if(nrow(lhh) > 0){lhh$remeasureNote <- "head normal and height below threshold"}
  hr <- rbind(hr, lhh)
  
  bals <- m[m$rp %in% lhh$rp,]
  if(nrow(bals) > 0){bals$remeasureNote <- "head normal and height below threshold"}
  mr <- rbind(mr, bals)
  
  # 3. plant measure ratios, they good?  (lfCt/rosetteCt, etc..)  ####
  # all I really want to see is if ratio is >=6 or if it's less than 2 (and not 1-1) - otherwise, we good!!!
  
  # ratio too many! 
  hratio <- m[!is.na(m$bslLfCt) & !is.na(m$bslRosetteCt) & (m$bslRosetteCt > 0) & (m$bslLfCt/m$bslRosetteCt > 8),]
  if(nrow(hratio) > 0){hratio$remeasureNote <- "ratio of lfCt to rosetteCt too high"}
  mr <- rbind(mr, hratio)
  
  # low ratio!
  lratio <- m[!is.na(m$bslLfCt) & !is.na(m$bslRosetteCt) & (m$bslRosetteCt > 0) & 
                (m$bslRosetteCt > 2) & (m$bslLfCt/m$bslRosetteCt < 2),]
  if(nrow(lratio) > 0){lratio$remeasureNote <- "ratio of lfCt to rosetteCt too low"}
  mr <- rbind(mr, lratio)
  
  # more rosettes than lvs!
  mros <- m[!is.na(m$bslLfCt) & !is.na(m$bslRosetteCt) & (m$bslRosetteCt > m$bslLfCt),]
  if(nrow(mros) > 0){mros$remeasureNote <- "more basal rosettes than lvs"}
  mr <- rbind(mr, mros)
  
  # 4. make sure all basals have 3 basal measurements ####
  # basals should have bslRos, bslLfCt, bslLongLf!
  
  # BasMes = 0
  bslO <- m[m$status %in% "Basal" & (m$bslRosetteCt %in% 0 | m$bslLongLfCm %in% 0 |
                                       m$bslLfCt %in% 0),]
  if(nrow(bslO) > 0){bslO$remeasureNote <- "bsl but a measurement = 0"}
  mr <- rbind(mr, bslO)
  
  # rosetteCt  
  noBRos <- m[m$status %in% "Basal" & is.na(m$bslRosetteCt),]
  if(nrow(noBRos) > 0){noBRos$remeasureNote <- "bsl but no rosetteCt"}
  mr <- rbind(mr, noBRos)
  
  # lfCt
  noLfCt <- m[m$status %in% "Basal" & is.na(m$bslLfCt),]
  if(nrow(noLfCt) > 0){noLfCt$remeasureNote <- "bsl but no bslLfCt"}
  mr <- rbind(mr, noLfCt)
  
  # longLf
  noBLong <- m[m$status %in% "Basal" & is.na(m$bslLongLfCm),]
  if(nrow(noBLong) > 0){noBLong$remeasureNote <- "bsl but no longLf"}
  mr <- rbind(mr, noBLong)
  
  # 5. make sure flowerings have all measures ####
  # all 5 measures, no longLf if rosette + lfCt = 0!
  # for sure need flRosetteCt and caulLongLf
  
  # flRosette
  noFlr <- m[m$status %in% "Flowering" & is.na(m$flRosetteCt),]
  if(nrow(noFlr) > 0){noFlr$remeasureNote <- "fl but no flRosetteCt"}
  mr <- rbind(mr, noFlr)
  
  # caulLongLf
  noCa <- m[m$status %in% "Flowering" & is.na(m$caulLongLfCm),]
  if(nrow(noCa) > 0){noCa$remeasureNote <- "fl but no caulLongLf"}
  mr <- rbind(mr, noCa)
  
  # bslRosete - should at least be 0
  fbr <- m[m$status %in% "Flowering" & is.na(m$bslRosetteCt),]
  if(nrow(fbr) > 0){fbr$remeasureNote <- "fl but no bslRosetteCt"}
  mr <- rbind(mr, fbr)
  
  # bslLfCt - should at least be 0
  fbl <- m[m$status %in% "Flowering" & is.na(m$bslLfCt),]
  if(nrow(fbl) > 0){fbl$remeasureNote <- "fl but no bslLfCt"}
  mr <- rbind(mr, fbl)
  
  # longLfCm can be NA... if bslRosette is 0!
  okna <- m[m$status %in% "Flowering" & !(m$bslRosetteCt %in% 0) & is.na(m$bslLongLfCm),]
  if(nrow(okna) > 0){okna$remeasureNote <- "fl, bslRosette is not 0, but no bslLongLf"}
  mr <- rbind(mr, okna)
  
  # 6. make sure no other statuses have full measures ####
  # no statuses outside of basal and flowering should have any measures. make sure this is correct!!
  
  badCheese <- m[!(m$status %in% "Flowering") & !(m$status %in% "Basal") &
                   (!is.na(m$flRosetteCt) | !is.na(m$caulLongLfCm) |
                      !is.na(m$bslRosetteCt) | !is.na(m$bslLfCt) | !is.na(m$bslLongLfCm)),]
  if(nrow(badCheese) > 0){badCheese$remeasureNote <- "status not bsl or fl but measurement(s) filled"}
  mr <- rbind(mr, badCheese)
  
  # 7. make sure head records have length + tt ####
  # this should be relatively straightforward
  # pull out head NAs and 0s!
  
  # na head
  nah <- h[is.na(h$headHeight),]
  if(nrow(nah) > 0){nah$remeasureNote <- "headHeight missing"}
  hr <- rbind(hr, nah)
  
  # hdHeight = 0
  zeh <- h[h$headHeight %in% 0,]
  if(nrow(zeh) > 0){zeh$remeasureNote <- "headHeight is 0"}
  hr <- rbind(hr, zeh)
  
  # tt not present
  nott <- h[h$head %in% "" | is.na(h$head),]
  if(nrow(nott) > 0){nott$remeasureNote <- "no tt recorded"}
  hr <- rbind(hr, nott)
  
  # 8. were all positions recorded (completeds) ####
  nc <- m[m$completed %in% "",]
  if(nrow(nc) > 0){nc$remeasureNote <- "record was not completed"}
  mr <- rbind(mr, nc)
  
  # 9. make sure all headMeasures are represented in the main form!!! ####
  rpNN <- hr[!(hr$rp %in% mr$rp),]$rp
  ih <- m[m$rp %in% rpNN,]
  
  for (i in 1:nrow(ih)){
    df <- hr[hr$rp %in% ih[i, "rp"],]
    if (nrow(df) %in% 1){
      ih[i, "remeasureNote"] <- paste("headForm:", df[1, "remeasureNote"])
    }
    else if (nrow(df) > 1){
      for (j in 1:nrow(df)){
        if (j %in% 1){
          ih[i, "remeasureNote"] <- paste("headForm:", df[1, "remeasureNote"])
        }
        else if (j > 1){
          if (!grepl(df[j, "remeasureNote"],ih[i, "remeasureNote"])){
            ih[i, "remeasureNote"] <- paste(ih[i, "remeasureNote"], df[j, "remeasureNote"], sep = ";")
          }
        }
      }
    }
  }
  
  mr <- rbind(mr, ih)
  mr <- mr[order(mr$rp),]
  
  # add neighborStatus ####
  # this will add neighborStatus
  m <- m[order(m$row, m$position),]
  
  if (plot == "p1"){
    for (i in 1:nrow(m)){
      if (m$status[i] == "" & m$action[i] == "Staple"){
        m$status[i] <- m$action[i]
      }
    }
  } else if (plot == "p2" | plot == "p7"){
    for (i in 1:nrow(m)){
      if (m$status[i] == "" & m$action[i] == "Skip"){
        m$status[i] <- m$action[i]
      }
    }
  }
  
  
  for (i in 1:nrow(m)){
    if(isTRUE(m$row[i] == m$row[i-1])){
      b4 <- m[i - 1 ,"status"]
      if (b4 == "Can't Find"){
        b4 <- "cf"
      }else if (b4 == "Staple" | b4 == "skip"){
        b4 <- "s"
      }else if (b4 == "Basal"){
        b4 <- "b"
      }else if (b4 == "Flowering"){
        b4 <- "fl"
      }else if (b4 == "Dead Lvs (last year's)" | b4 == "Dead Lvs (this year's)"){
        b4 <- "dead"
      }
    }
    else{b4 <- "NA"}
    if(isTRUE(m$row[i] == m$row[i+1])){
      a4 <- m[i + 1 ,"status"]
      if (a4 == "Can't Find"){
        a4 <- "cf"
      }else if (a4 == "Staple" | a4 == "skip"){
        a4 <- "s"
      }else if (a4 == "Basal"){
        a4 <- "b"
      }else if (a4 == "Flowering"){
        a4 <- "fl"
      }else if (a4 == "Dead Lvs (last year's)" | a4 == "Dead Lvs (this year's)"){
        a4 <- "dead"
      }
    }
    else {a4 <- "NA"}
    m$neighborStatus[i] <- paste(b4, a4, sep = "-")
  }
  
  m <- m
  
  # get neighborStatus into mr!
  mr <- merge(mr, m[,c("rp", "neighborStatus")], by = "rp", all.x = T)
  hr <- merge(hr, m[,c("rp", "neighborStatus")], by = "rp", all.x = T)
  
  # neighorStatus good to go!
  
  # consolidate duplicated records into once remeasure ####
  # use rp for the measure form and then rph for the head form!
  # measure form
  rr <- unique(mr$rp)
  nmr <- mr[0,]
  for (i in rr){
    df <- mr[mr$rp %in% i,]
    if (nrow(df) > 1){
      for (j in 1:nrow(df)){
        if (j %in% 1){
          df[1, "remeasureNote"] <- df[1, "remeasureNote"]
        }
        else if (j > 1){
          df[1, "remeasureNote"] <- paste(df[1, "remeasureNote"], df[j, "remeasureNote"], sep = ";")
        }
      }
    }
    df <- df[1,]
    nmr <- rbind(nmr, df[1,])
  }
  
  
  # head form!
  pp <- unique(hr$rph)
  nhr <- hr[0,]
  for(i in pp){
    df <- hr[hr$rph %in% i,]
    if (nrow(df) > 1){
      for (j in 1:nrow(df)){
        if (j %in% 1){
          df[1, "remeasureNote"] <- df[1, "remeasureNote"]
        }
        else if (j > 1){
          df[1, "remeasureNote"] <- paste(df[1, "remeasureNote"], df[j, "remeasureNote"], sep = ";")
        }
      }
      df <- df[1,]
      nhr <- rbind(nhr, df[1,])
    }
  }
  
  # edit cols to prep for remeasure!! ####
  # measure form first!
  colnames(nmr)[colnames(nmr) %in% "action"] <- "originalAction"
  nmr$action <- "Remeasure"
  
  colnames(nmr)[colnames(nmr) %in% "status"] <- "originalStatus"
  nmr$status <- ""
  
  remeasures <- nmr
  headRemeasures <- nhr
  
  gCols <- c("cgPlaid", "plot", "expNm", "segmentCd",
             "RecordId", "UnitID",  "UserName", "row", "position",
             "originalAction", "originalStatus", "flRosetteCt", "bslRosetteCt", "bslLfCt",  "bslLongLfCm",
             "caulLongLfCm", "damageNote", "otherNote", "measuredByInitials", "recordedByInitials",
             "remeasureNote", "neighborStatus", "action", "status")
  
  remeasures <- remeasures[, colnames(remeasures) %in% gCols]
  
  remeasures$UserName <- ""
  remeasures$measuredByInitials <- ""
  remeasures$recordedByInitials <- ""
  
  remeasures <- remeasures[order(remeasures$cgPlaid),]
  
  hCols <- c("UserName", "row", "position", "head", "onSameStemAs", "headStatus", "headHeight", "insectsHead",
             "diseaseHeadCheck", "headNote", "measuredBy", "recordedBy", "remeasureNote", "cgPlaid", "neighborStatus")
  
  headRemeasures <- headRemeasures[, colnames(headRemeasures) %in% hCols]
  headRemeasures <- headRemeasures[order(headRemeasures$cgPlaid),]
  
  # add username to cols ####
  source("build.R")
  # vv is the list of visor usernames for the year in build.R
  no <- userNum
  v <- 1
  while (v <= no){
    for (u in 1:nrow(remeasures)){
      if (u/nrow(remeasures) <= v/no & u/nrow(remeasures) > (v-1)/no){
        remeasures[u, 'UserName'] <- vv[v]
      }
    }
    v <- v + 1
  }
  
  updatedMeasure <<- m[,!(colnames(m) %in% "neighborStatus")]
  remeasures <<- remeasures
  headRemeasures <<- headRemeasures
  
  # write files ####
  
  if (write.it == TRUE & plot == "p1"){
    newMes <- paste0("updated", yr, "MeasureExPt1_Final.csv")
    write.csv(updatedMeasure, paste(write.path, newMes, sep = "/"), row.names = F)
    mFile <- paste0("remeasureExPt1", yr, ".csv", sep ="")
    write.csv(remeasures, paste(write.path, mFile, sep = "/"), row.names = F)
    hFile <- paste0("headsToRemeasureExPt1", yr, ".csv", sep = "")
    write.csv(headRemeasures, paste(write.path, hFile, sep = "/"), row.names = F)
  } 
  else if (write.it == TRUE & plot == "p2"){
    newMes <- paste0("updated", yr, "MeasureExPt2_Final.csv")
    write.csv(updatedMeasure, paste(write.path, newMes, sep = "/"), row.names = F)
    mFile <- paste0("remeasureExPt2", yr, ".csv", sep ="")
    write.csv(remeasures, paste(write.path, mFile, sep = "/"), row.names = F)
    hFile <- paste0("headsToRemeasureExPt2", yr, ".csv", sep = "")
    write.csv(headRemeasures, paste(write.path, hFile, sep = "/"), row.names = F)
  } 
  else if (write.it == TRUE & plot == "p7"){
    newMes <- paste0("updated", yr, "MeasureExPt679_Final.csv")
    write.csv(updatedMeasure, paste(write.path, newMes, sep = "/"), row.names = F)
    mFile <- paste0("remeasureExPt679", yr, ".csv", sep ="")
    write.csv(remeasures, paste(write.path, mFile, sep = "/"), row.names = F)
    hFile <- paste0("headsToRemeasureExPt679", yr, ".csv", sep = "")
    write.csv(headRemeasures, paste(write.path, hFile, sep = "/"), row.names = F)
    
  }
  
  
  # write a little bit about the files we have (print) ####
  cat("function returned dataFrames updatedMeasure, remeasures, and headRemeasures")
  cat("\n")
  cat("\n")
  cat("updatedMeasure: updated, cleaned measure form. It is an updated form that should be used as the main final measure form 
      from here on out. Main change is that Basal measures that were shifted over to the left or right are now centered 
      (i.e. measures in flRos, bslRos and bslLfCt moved to bslRos, bslLfCt, and bslLongLfCm). 
      Records corrected in this way were not pulled out by the remeasure function unless that record has other issues.
      If write.it == T, find 'm' at", paste(write.path, newMes, sep = "/"))
  cat("\n")
  cat("\n")
  cat("remeasures: has rows and positions to remeasure and newly assigned visors. Eliminate any remeasures you deem unnecessary
      then use it to remeasure! If write.it == T, find 'remeasures' at", paste(write.path, mFile, sep = "/"))
  cat("\n")
  cat("\n")
  cat("headRemeasures: heads that need to be remeasured. This is a reference form.
      All heads that need to be remeasured will have records in the regular remeasure form.
      if write.it == T find it at", paste(write.path, hFile, sep = "/"))
  
}
