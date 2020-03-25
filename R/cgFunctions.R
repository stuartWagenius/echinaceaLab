#' Write plots of common characters for common garden experiments 1, 2, and 679
#' 
#' This function will take all data collected from a select common garden experimet
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
#' @param yr the year the data in the input file was recorded (e.g. 2019, as above)
#' @param box.plot logical: if TRUE, makeCGGraphs will output boxplots of plant characters.
#' if FALSE, makeCGGraphs will output histograms of plant characters.
#' @param write.pdf logical: if TRUE, function will output a pdf that will save to the same folder 
#' that the measuring form came from (good for printing!!!)
#' if FALSE, function will output the plots into the R viewer.
#' @details function will not work if input fields are spelled or capitalized incorrectly. They must be:
#' bslLongLfCm, caulLongLfCm, bslLfCt, bslRosetteCt, flRosetteCt, (in mf) and headHeight (in hf). If they
#' are spellect incorrectly, read the .txt into R, rename files, and save it as a .csv.
#' p679 are 3 differnt plots in real life, but are treated as one by this function. They should also share a
#' Pendragon measuring form.
#' mf and hf are REQUIRED fields. If there are no head records for a plot (likely only in p679),
#' make an empty hf and input that. 
#' 
#' 
#' @examples
#' \dontrun{makeCGGraphs("summer2019/p679MeasureData/2019MeasureHybExPt_2019-08-07.txt",
#' "summer2019/p679MeasureData/2019HybExPtHeadSubform_2019-08-07.txt",
#' 2019, box.plot = F, write.pdf = T)}
#' 
#' \dontrun{makeCGGraphs("summer2019/p1MeasureData/2019MeasureExPt1_2019-08-27.txt",
#' "summer2019/p1MeasureData/2019ExPt1HeadSubform_2019-09-27.txt",
#' 2019, box.plot = T, write.pdf = F)}
#' 
makeCGGraphs <- function(mf, hf, yr, box.plot = FALSE, write.pdf = FALSE){
  m <- read.csv(mf, stringsAsFactors = F)
  h <- read.csv(hf, stringsAsFactors = F)
  year <- toString(yr)
  
  path <- dirname(mf)
  
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

