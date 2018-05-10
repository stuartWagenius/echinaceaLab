#' Estimate seed set in a head
#' 
#' Estimate seed set in a head that was split into top, middle, and bottom
#' using two different methods: one resampling based, the other a point estimate
#' 
#' @param df a data frame that contains seed set data
#' @param partials if TRUE, partials will be included in 
#' @param idCol the column that contains the ids for each head
#' @param totalCols a vector with names of the columns that contain counts for
#' the total number of achenes in the top, middle, and bottom of the head
#' @param fullCols a vector with names of the columns that contain counts for
#' the number of full achenes in the top, middle, and bottom of the head
#' @param sampCols a vector with names of the columns that contain counts for
#' the number of achenes in the x-rayed sample for the top, middle, and bottom
#' of the head
#' @param partialCols a vector with names of the columns that contain counts for
#' the number of partial achenes in the top, middle, and bottom of the head. If
#' \code{partial} is FALSE, this can be ignored
#' 
#' @return a data frame containing the id and estimates of seed set based on two methods
#' of calculation. Columns "pointEstFull" and "acheneCt" result from the
#' point estimate method and columns "sampleFull" and "sampleCount" result from 
#' the resampling method.
#' 
estimateSeedSet <- function(df, partials = TRUE, idCol = "headID",
                            totalCols = c("topCount", "middleTotalCount", "bottomCount"),
                            fullCols = c("topFull", "middleFull", "bottomFull"),
                            sampCols = c("topSampleCount", "middleSampleCount", "bottomSampleCount"),
                            partialCols = c("topPartial", "middlePartial", "bottomPartial")) {
  newdf <- df[idCol]
  # make columns for fertilized
  if (partials) {
    newdf$topFert <- df[[fullCols[1]]] + df[[partialCols[1]]]
    newdf$midFert <- df[[fullCols[2]]] + df[[partialCols[2]]]
    newdf$botFert <- df[[fullCols[3]]] + df[[partialCols[3]]]
  } else {
    newdf$topFert <- df[[fullCols[1]]]
    newdf$midFert <- df[[fullCols[2]]]
    newdf$botFert <- df[[fullCols[3]]]
  }
  # make columns for empty
  newdf$topEmpty <- df[[sampCols[1]]] - newdf$topFert
  newdf$midEmpty <- df[[sampCols[2]]] - newdf$midFert
  newdf$botEmpty <- df[[sampCols[3]]] - newdf$botFert
  # make columns for achene counts
  newdf$top <- df[[totalCols[1]]]
  newdf$mid <- df[[totalCols[2]]]
  newdf$bot <- df[[totalCols[3]]]
  newdf$acheneCt <- newdf$top + newdf$mid + newdf$bot
  
  # do the calculation method of getting whole head seed set
  mfc <- (newdf$midFert/(newdf$midFert + newdf$midEmpty)) * newdf$mid
  mfc[is.na(mfc)] <- 0
  newdf$pointEstFull <- newdf$topFert + mfc + newdf$botFert
  
  # do the sample based method of getting the whole head seet set
  newdf$sampleFull <- NA
  newdf$sampleCount <- 99
  
  for(i in 1:nrow(newdf)) {
    midSample <- newdf$midFert[i] + newdf$midEmpty[i]
    total <- newdf$top[i] + newdf$bot[i] + midSample
    x <- ifelse(midSample != 0, newdf$mid[i]/midSample, 0)
    fakeHd <- c(rep(1, newdf$topFert[i]), rep(0, newdf$topEmpty[i]),
                rep(1, newdf$botFert[i]), rep(0, newdf$botEmpty[i]),
                rep(1, x * newdf$midFert[i]), rep(0, x * newdf$midEmpty[i]))
    newdf$sampleFull[i] <- ifelse(length(fakeHd) < 30, 
                                  sum(fakeHd),
                                  sum(sample(fakeHd, 30, replace = FALSE)))
    newdf$sampleCount[i] <- ifelse(total < 30, total, 30)
  }
  return(newdf[, c(idCol, "pointEstFull", "acheneCt", "sampleFull", "sampleCount")])
  # out <- newdf[, c(idCol, "pointEstFull", "acheneCt", "sampleFull", "sampleCount")]
  # list(df = newdf, fakeHd = fakeHd, x = x, ms = midSample, z = table(fakeHd), out = out)
}


