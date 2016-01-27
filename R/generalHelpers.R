#' Compare the contents of two vectors
#' 
#' This function takes two vectors and displays how many elements are
#' in one, in the other, or in both as well as which ones are only
#' in one of the two vectors.
#' 
#' @param v1 the first vector to compare
#' @param v2 the second vector to compare
#' @param showInBoth whether or not to display the values found in
#' both vectors. Default is FALSE
#' @return a list containing (1) a data frame with various counts (2) a vector
#' with elements only in the first vector (3) a vector with elements only in
#' the second vector (4) if showInBoth == TRUE, a vector with elements found
#' in both vectors
compare2vectors <- function(v1, v2, showInBoth = FALSE) {
  d1 <- sum(duplicated(v1))
  d2 <- sum(duplicated(v2))
  in1st <- sort(setdiff(v1, v2))
  in2nd <- sort(setdiff(v2, v1))
  inBoth <- sort(intersect(v1, v2))
  cts <- data.frame(in.both = length(inBoth), in.v1.only = length(in1st),
                    in.v2.only = length(in2nd), dups.in.1 = d1, dups.in.2 = d2)
  if(showInBoth) {
    return(list(counts = cts, in.first = in1st, in.second = in2nd, 
                      in.both = inBoth))
  } else {
    return(list(counts = cts, in.first = in1st, in.second = in2nd))
  }
}

#' Abbreviate a site name
#' 
#' Create standard abbreviations for remnant populations
#' 
#' @param x a vector of site names
#' @return a vector with all site names abbreviated
abbrevSiteName <- function(x){
  x <- as.character(x)
  x[x %in% c("Aanenson", "aa")] <- "aa"
  x[x %in% c("Around LF", "alf")] <- "alf"
  x[x %in% c("btg", "BTG")] <- "btg"
  x[x %in% c("Common Garden")] <- "cg"
  x[x %in% c("DOG")] <- "dog"
  x[x %in% c("Elk Lake Road East", "eelr")] <- "eelr"
  x[x %in% c("East Riley", "eri")] <- "eri"
  x[x %in% c("East of Town Hall", "eth")] <- "eth"
  x[x %in% c("gc", "Golf Course")] <- "gc"
  x[x %in% c("kj", "KJs")] <- "kj"
  x[x %in% c("Krusemark", "Krusemark.", "ks")] <- "krus"
  x[x %in% c("lc")] <- "lc"
  x[x %in% c("Loeffler Corner East")] <- "lce"
  x[x %in% c("Loeffler Corner West")] <- "lcw"
  x[x %in% c("lf")] <- "lf"
  x[x %in% c("Landfill East")] <- "lfe"
  x[x %in% c("Landfill West")] <- "lfw"
  x[x %in% c("Liatris Hill", "lih")] <- "lih"
  x[x %in% c("mapp", "Martinson")] <- "mapp"
  x[x %in% c("ness", "Nessman")] <- "ness"
  x[x %in% c("North of golf course")] <- "ngc"
  x[x %in% c("nnwlf", "NNWLF")] <- "nnwlf"
  x[x %in% c("NNRRX", "nrrx", "NRRX")] <- "nrrx"
  x[x %in% c("nwlf", "NWLf", "NWLF")] <- "nwlf"
  x[x %in% c("On 27", "on27")] <- "on27"
  x[x %in% c("ri", "Riley")] <- "ri"
  x[x %in% c("Randt", "rndt")] <- "rndt"
  x[x %in% c("rrx", "RRX")] <- "rrx"
  x[x %in% c("rrxdc")] <- "rrxdc"
  x[x %in% c("sgc", "South of Golf Course")] <- "sgc"
  x[x %in% c("sign")] <- "sign"
  x[x %in% c("Staffanson East Unit", "Staffanson West Unit")] <- "spp"
  x[x %in% c("Steven's Approach", "stapp")] <- "sap"
  x[x %in% c("th", "Town Hall")] <- "th"
  x[x %in% c("to", "Tower")] <- "tower"
  x[x %in% c("waa", "West of Aanenson")] <- "waa"
  x[x %in% c("wo", "Woody's")] <- "wood"
  x[x %in% c("Yellow Orchid Hill East", "Yellow Orchid Hill West")] <- "yoh"
  x[x %in% c("Roland Lake Road", "1292")] <- "rlr"
  x[x %in% c("tp", "tplot", "transplant plot")] <- "tplot"
  x[x %in% c("backhill", 
             "near th", 
             "New Site", 
             "other",
             "Other",
             "Riley Hill"
  )] <- "unknown"
  x
}

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