#' Visualize ACE progress in the lab
#' 
#' Visualize ACE progress in the lab for rem, cg, la (Liatris aspera), or lp (Lilium philadelphicum).
#' Update the csv in Dropbox/visualizeAceProgress with the most recent counts of how many heads/batches are done.
#' If you are in a repo, run build.R or pathUpdate.R first so that dropboxPath is in the Global Environment.
#' If dropboxPath exists, a barplot png will be saved in Dropbox/visualizeAceProgress. If not, the png will be saved 
#' to the workspace.
#' 
#' @param experiment can be rem, cg, la, or lp
#' @param year the year the heads/pods were collected 
#' 
#' @return a barplot png showing the total number of heads/batches and the number that are done for each stage of the ACE workflow. 
#' The barplot includes the stages that are listed in the csv in Dropbox/visualizeAceProgress.
#' The dates above each bar are the dates when each stage was last updated.


visualizeAceProgress <- function (experiment = "rem", year = "2020") {
  # create ace stages
  stages <- data.frame(stage = c("inventory","clean","cleanQC","scan","assignScan","count","fixAB","randomize","xray","assignXray","classify","finalInv","freeze"),
                       width = c(1,6,1,4,1,4,3,4,3,1,4,1,2))
  
  # load ace data for rem and cg and define units
  if (experiment == "rem" & exists("dropboxPath") == FALSE) {
    progress <- read.csv("https://www.dropbox.com/s/pnlsth7p2mc9om4/aceProgressEchinacea.csv?dl=1", stringsAsFactors = F)
    units = "heads"
  } else if (experiment == "rem" & exists("dropboxPath") == TRUE) {
    progress <- read.csv(paste0(dropboxPath,"/visualizeAceProgress/aceProgressEchinacea.csv"), stringsAsFactors = F)
    units = "heads"
  } else if (experiment == "cg" & exists("dropboxPath") == FALSE) {
    progress <- read.csv("https://www.dropbox.com/s/pnlsth7p2mc9om4/aceProgressEchinacea.csv?dl=1", stringsAsFactors = F)
    units = "batches"
  } else if (experiment == "cg" & exists("dropboxPath") == TRUE) {
    progress <- read.csv(paste0(dropboxPath,"/visualizeAceProgress/aceProgressEchinacea.csv"), stringsAsFactors = F)
    units = "batches"
  } else if (experiment == "la" & exists("dropboxPath") == FALSE) {
    progress <- read.csv("https://www.dropbox.com/s/hebx93nvglh0hdn/aceProgressOtherSpecies.csv?dl=1", stringsAsFactors = F)
    units = "heads"
  } else if (experiment == "la" & exists("dropboxPath") == TRUE) {
    progress <- read.csv(paste0(dropboxPath,"/visualizeAceProgress/aceProgressOtherSpecies.csv"), stringsAsFactors = F)
    units = "heads"
  } else if (experiment == "lp" & exists("dropboxPath") == FALSE) {
    progress <- read.csv("https://www.dropbox.com/s/hebx93nvglh0hdn/aceProgressOtherSpecies.csv?dl=1", stringsAsFactors = F)
    units = "pods"
  } else if (experiment == "lp" & exists("dropboxPath") == TRUE) {
    progress <- read.csv(paste0(dropboxPath,"/visualizeAceProgress/aceProgressOtherSpecies.csv"), stringsAsFactors = F)
    units = "pods"
  }
  
  # give warning if experiment names do not match
  if (!all(progress$experiment %in% c("rem","cg","lp","la"))) {
    warning("one or more experiment names are misspelled!")
  }
  
  # select just the data in the desired experiment
  progress <- progress[progress$experiment %in% experiment & progress$year %in% year,]
  
  # give warning if stage names do not match
  if (!all(progress$stage %in% c(stages$stage, "total"))) {
    warning("one or more stage names are misspelled!")
  }
  
  # remove unneeded stages
  stages <- stages[stages$stage %in% progress$stage,]
  
  # select the most recent date for each stage
  foo <- aggregate(date ~ stage, progress, FUN = "max") 
  
  # create dataframe with just the most recent count for each stage
  count <- merge(foo, progress, all.x = T)
  rm(foo)
  
  # define maximum - could be # of heads or # of batches
  maxCt <- count[count$stage %in% "total", "done"]
  
  # add column for the # of heads/batches that have not been processed yet (so the barchart bars add to the total # of heads/batches)
  count$remaining <- maxCt-count$done
  
  # order columns by stage, not alphabetical
  count <- count[match(stages$stage, count$stage, nomatch = F),]  
  
  # create dataframe with just the count data
  justCount <- count[,c("done","remaining")]
  
  # switch from long to wide format
  countRows<-as.data.frame(t(justCount))
  rm(justCount)
  
  # add stages as column names and fix column names
  colnames(countRows) <- count$stage
  colnames(countRows)[colnames(countRows) == "assignScan"] <- "assign \nscan"
  colnames(countRows)[colnames(countRows) == "assignXray"] <- "assign \nxray"
  colnames(countRows)[colnames(countRows) == "finalInv"] <- "final \ninv"
  colnames(countRows)[colnames(countRows) == "freeze"] <- "frz"
  
  # turn dataframe into a matrix
  countRowsM <- as.matrix(countRows)
  rm(countRows)
  
  # create png
  dateUpdated <- Sys.Date()
  
  if(exists("dropboxPath") == TRUE){
    png(paste0(dropboxPath,"/visualizeAceProgress/",experiment,year,"AceProgress",dateUpdated,".png"), width = 950, height = 750)
  } else {
    png(paste0(experiment,year,"AceProgress",dateUpdated,".png"), width = 950, height = 750)
  }
  
  # create stacked barplot
  par(cex.main=2, mar= c(5, 5, 2, 2))
  aceBarplot <- barplot(countRowsM, col = c("#1b98e0", "#353436"), width = stages$width, xlab = "ACE stage", ylab = paste("number of", units), cex.axis = 1.35, 
                        cex.names=1.25, cex.lab = 1.5, ylim = c(0, maxCt+(0.2*maxCt)), space = 0.2, mgp = c(3.5,1.5,0))
  
  # add title, legend, and labels for count and date updated
  legend("right", c(paste(units, "done"), paste(units, "to do")), fill = c("#1b98e0", "#353436"), cex = 1.5, bg = "white")
  title(paste("ACE progress:", experiment, year), line = -2)
  text(aceBarplot,countRowsM[1,]-(0.03*maxCt),labels=countRowsM[1,],cex=1.25, col= "white")
  text(aceBarplot,maxCt-(0.03*maxCt),labels=maxCt,cex=1.25,col = "white")
  text(aceBarplot,maxCt+(0.05*maxCt),labels=count$date,cex=1.25, col= "black", srt=30)
  
  # save png
  dev.off()
  if(exists("dropboxPath") == TRUE){
    return(paste0(experiment,year,"AceProgress",dateUpdated,".png", " has been saved to Dropbox in folder visualizeAceProgress"))
  } else {
    return(paste0(experiment,year,"AceProgress",dateUpdated,".png", " has been created in your workspace. Save it in Dropbox/visualizeAceProgress"))
  }
  
}
