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
      toSave = imager::cimg(toSave)
      imager::save.image(toSave, fn)
      startNum <- startNum + 1
    }
  }
  cat("Saved to", outFolder, "\n")
  
  invisible(imgCells)
}

##' Make x-ray data frame
##' 
##' This function takes in a harvest data frame (e.g., hh.2016) and assigns
##' a sheet number and subbatch to each head. Subbatches are smaller subunits
##' of large batches (experiments) for ease of processing.
##' 
##' @param batchSize the number of heads to fit in each subbatch. Default is 200.
##' @param sheetSize the number of heads to fit on each x-ray grid sheet
##' @return a data frame with five columns: letno, batch (i.e., experiment), 
##' No (the numeric part of a letno), subbatch, sheetNo
##' @details Subbatches are mainly useful for breaking large batches into smaller,
##' more manageable chunks. If the number of heads is larger than the size specified
##' for subbatches (i.e., the argument 'batchSize'), then the subbatch field will take
##' the format "experimentName_subbatchNumber', where the subbatchNumber is the first
##' No (numeric part of letno) in that batch.
##' @keywords x-ray xray df
##' @examples 
##' xr16 = makeXrayDf(hh.2016, batchSize = 100)
##' head(xr16)
##' tail(xr16)
##' table(xr16$subbatch)
##' 
makeXrayDf = function(hh, batchSize = 200, sheetSize = 20) {
  
  hdf = hh[,c('letno','batch','No')]
  hdf = hdf[order(hh$No),]
  
  # assign sheet number
  hdf$sheetNo = ((hdf$No-1) %/% sheetSize) * sheetSize + 1
  
  # making subbatches for batches with over batchSize heads
  subbatch = as.character(hdf$batch)
  for (bat in which(table(hdf$batch) > batchSize)) {
    batch = names(table(hdf$batch))[bat]
    
    firstIn = min(hdf$sheetNo[hdf$batch == batch], na.rm=TRUE)
    batchNo = ((hdf$No[hdf$batch == batch] - firstIn) %/% batchSize) * batchSize + firstIn
    subbatch[hdf$batch == batch] = paste(batch,batchNo,sep='_')
  }
  hdf$subbatch = subbatch
  
  row.names(hdf) = NULL
  
  return(hdf)
  
}

##' Generate x-ray grid insets
##' 
##' This function takes in an x-ray data frame (see makeXrayDf()) and outputs
##' insets for x-ray grid cells. These insets will either be the numeric part
##' of the letno for the head belonging in that cell or the string "Empty".
##' Export the output of this function as a .csv (with row.names = FALSE)
##' and use that for the mail merge when making x-ray grids.
##' 
##' @param xrdf an x-ray data frame for a given year, generated by makeXrayDf()
##' @param sheetSize the number of heads to fit on each x-ray grid sheet
##' @param test a boolean for testing inset creation or printing. If TRUE,
##' then the string "Test" is appended to the end of this function's output.
##' @return an array of cell insets to be used when making x-ray grids
##' @keywords x-ray xray grid inset
makeGridInsets = function(xrdf, sheetSize = 20, test = FALSE) {
  
  cellz = xrdf[,c('No','No','sheetNo')]
  
  for (x in unique(cellz$sheetNo)[which(table(cellz$sheetNo) < sheetSize)]) {
    empty.cells = (x+(0:(sheetSize-1)))[!((x+(0:(sheetSize-1))) %in% cellz$No[cellz$sheetNo == x])]
    cellz = rbind(cellz, data.frame(No = empty.cells, No.1 = 'Empty', sheetNo = x))
  }
  
  if (test) cellz = rbind(cellz, c(NA,'Test',NA))
  
  return(cellz[order(cellz$No), 2])
  
}

##' Create data sheet rows for x-ray progress in a given year.
##' 
##' This function takes in an x-ray data frame (see makeXrayDf()) and outputs
##' a data frame for the lab manager and volunteers to fill out when assembling
##' x-ray sheets or when using the x-ray machine.
##' 
##' @param xrdf an x-ray data frame for a given year, generated by makeXrayDf()
##' @return a data frame with one row for each x-ray sheet. The data frame has
##' a column for the first and last letno on each sheet. There are also three
##' blank (NA) columns to be filled in during the ACE workflow -- one for grid
##' assembly, one for x-ray scanning, one for quality control by the lab manager.
##' @details When exporting this data frame as a .csv, make sure to specify na = ""
##' (so that the data frame isn't printed with those columns filled with NA) and
##' use row.names = TRUE, as the sheet numbers are the row.names.
##' @keywords x-ray xray datasheet ds
##' @examples
##' xr16 = makeXrayDf(hh.2016)
##' xrds16 = makeXrayDataSheet(xr16)
##' head(xrds16)
##' tail(xrds16)
##' table(xrds16$Experiment)
##' write.csv(xrds16, na = "", file = "xrayDatasheet2016.csv")
makeXrayDataSheet = function(xrdf) {
  
  # data frame row names are the sheet numbers
  outdf = data.frame(row.names = unique(xrdf$sheetNo), stringsAsFactors = FALSE)
  
  for (x in unique(xrdf$sheetNo)) {
    
    xchar = as.character(x)
    
    outdf[xchar,'Experiment'] = as.character(xrdf$batch[xrdf$sheetNo == x][1])
    outdf[xchar,'First_letno'] = xrdf$letno[xrdf$No == min(xrdf$No[xrdf$sheetNo == x])]
    outdf[xchar,'Last_letno'] = xrdf$letno[xrdf$No == max(xrdf$No[xrdf$sheetNo == x])]
    
  }
  
  # blank columns to filled in by grid assembler and x-ray person
  outdf$Assembly_date_init = NA
  outdf$XRay_date_init = NA
  outdf$QC_date = NA
  outdf$Notes = NA
    
  return(outdf)
  
}