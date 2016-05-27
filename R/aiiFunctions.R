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
#' @return a data frame containing the id and seed set data for two methods
#' of calculating it. Columns "pointEstFull" and "acheneCt" correspond to the
#' point estimate method and columns "sampleFull" and "sampleCount" correspond
#' to the resampling method.
#' 
estimateSeedSet <- function(df, partials = TRUE, idCol = "headID",
                            totalCols = c("topCount", "middleCount", "bottomCount"),
                            fullCols = c("topFull", "middleFull", "bottomFull"),
                            sampCols = c("topCount", "informativeCount", "bottomCount"),
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
  newdf$sampleCount <- 30
  
  for(i in 1:nrow(newdf)) {
    top <- newdf$top[i]
    bot <- newdf$bot[i]
    mid <- newdf$mid[i]
    total <- top + mid + bot
    
    topFert <- newdf$topFert[i]
    botFert <- newdf$botFert[i]
    midFert <- newdf$midFert[i]
    
    acheneCt <- newdf$acheneCt[i]
    
    ptop <- top/acheneCt
    pbot <- bot/acheneCt
    pmid <- mid/acheneCt
    
    differ <- data.frame(pos = c(rep("top", top), rep("bot", bot), rep("mid", mid)),
                         fert = c(rep(1, topFert), rep(0, top - topFert), rep(1, botFert), 
                                  rep(0, bot - botFert), rep(1, midFert),
                                  rep(0, mid - midFert)),
                         weight = c(rep(ptop/top, top), rep(pbot/bot, bot),
                                    rep(pmid/mid, mid)))
    
    newdf$sampleFull[i] <- ifelse(total < 30,
                                  sum(differ$fert),
                                  sum(sample(differ$fert, size = 30, replace = FALSE,
                                             prob = differ$weight)))
    newdf$sampleCount[i] <- ifelse(total < 30, total, 30)
  }
  
  return(newdf[, c(idCol, "pointEstFull", "acheneCt", "sampleFull", "sampleCount")])
}
