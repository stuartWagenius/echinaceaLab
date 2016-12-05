##' Split x-ray image
##' 
##' Split x-ray image into individual packets
##' 
##' @param imgFile the x-ray (current jpg) image file to split
##' @param outFolder where to save all the individual images
##' @param hh the hh.year file from which the full letnos will be found.
##' Default is hh.2015
##' @param prefixLet the letter(s) to put in front of the name of the output
##' file e.g. for b1434sd.jpg prefixLet = 'b'
##' @param plotAndAsk logical. If TRUE, it will plot the individual packet
##' images to make sure that you're content with the results. Otherwise, it
##' automatically saves without plotting or asking.
##' @param xScale a vector describing the locations at which the image should
##' be cut as a proportion of the total image width
##' @param yScale a vector describing the locations at which the image should
##' be cut as a proportion of the total image height
##' @param fudge numeric. A fudge factor between 0 and 1 representing the
##' proportion of the height or width to add to each packet image when cutting
##' the original image into chunks
##' @return invisibly returns the list of individuals images as cimg objects
##' @details The imgFile name should be the letno of the first packet on the
##' x-ray image. All letnos on the image should go in order and if letnos are
##' missing, there should be nothing in that location on the sheet. If outfolder
##' doesn't exist, it will be created. There's a slight "fudge factor" so that
##' doesn't split the image equally, but rather, splits it so that each image
##' contains slightly more than "its fair share" of the image. That's not 
##' scientific so I'll also say: every packet image is more than 1/5 the width of
##' the original image and is more than 1/4 the height of the original image
##' @keywords x-ray xray
##' 
splitXrayImage <- function(imgFile, outFolder, hh, prefixLet, plotAndAsk = F,
                           xScale = c(0, 0.21, 0.39, 0.57, 0.75, 1),
                           yScale = c(0, 0.2583333, 0.5, 0.7416667, 1), fudge = 0.03) {
  if (!dir.exists(outFolder))
    dir.create(outFolder)
  
  fnList <- strsplit(imgFile, "/")[[1]]
  fn <- fnList[length(fnList)]
  startNum <- as.numeric(gsub("[^0-9]", "", fn))
  
  img <- imager::load.image(imgFile)
  nr <- nrow(img)
  nc <- ncol(img)
  
  imgCells <- list()
  for (i in 1:(length(yScale)-1)) {
    imgCells[[i]] <- list()
    for (j in 1:(length(xScale)-1)) {
      xLow <- round(xScale[j] * nr) - fudge * nr
      xHigh <- round(xScale[j+1] * nr) + fudge * nr
      yLow <- round(yScale[i] * nc) - fudge * nc
      yHigh <- round(yScale[i+1] * nc) + fudge * nc
      
      if (xLow < 0) xLow <- 0
      if (xHigh > nr) xHigh <- nr
      if (yLow < 0) yLow <- 0
      if (yHigh > nc) yHigh <- nc
      
      imgCells[[i]][[j]] <- img[xLow:xHigh, yLow:yHigh, , ]
    }
  }
  
  if (plotAndAsk) {
    # plot image in cells
    par(mfrow = c(4,5), mar = c(1,1,1,1))
    for (i in 1:length(imgCells))
      for (j in 1:length(imgCells[[i]]))
        plot(imgCells[[i]][[j]], axes = F)
    par(mfrow = c(1,1), mar = c(5,4,4,2)+0.1)
    message("Make sure to wait until all 20 images have loaded in the plot...")
    
    tryAgain <- T
    while (tryAgain) {
      good <- readline("Do these images look okay to you (y/n)? ")
      if (grepl("y", good)) {
        tryAgain <- F
      } else if (grepl("n", good)) {
        cat("Current image cropping:\n", "x limits:", xScale, "\n", "y limits:",
            yScale, "\n", "Image not saved\n")
        invisible(imgCells)
      } else {
        cat("Bad input, try again\n")
      }
    }
  }
  
  for (i in 1:length(imgCells)) {
    for (j in 1:length(imgCells[[i]])) {
      if (nrow(hh[hh$No == startNum & !is.na(hh$No), ]) > 0) {
        lenno <- as.character(hh[hh$No == startNum & !is.na(hh$No), "letno"])
        nolet <- paste(substr(lenno, 4, 7), substr(lenno, 1, 2), sep = "")
      } else {
        nolet <- as.character(startNum)
      }
      fn <- paste(outFolder, "/x", prefixLet, nolet, ".jpg", sep = "")
      toSave = imgCells[[i]][[j]]
      dim(toSave) = c(dim(toSave)[1:2],1,1)
      imager::save.image(toSave, fn)
      startNum <- startNum + 1
    }
  }
  cat("Saved to", outFolder, "\n")
  
  invisible(imgCells)
}


