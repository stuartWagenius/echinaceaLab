#' Function is.stakefile: utility function for 
#' 
#' check formatting of stakefiles
#' 
#'@description Function to assess whether data frame conforms to stakefile formatting
#'@param df data frame with potential stakefile
#'@return TRUE or FALSE

is.stakefile <- function(df){
  
  ## check column number
  if(!dim(df)[2] == 4){
    warning("Data frame does not follow stakefile column format. Columns must read c(`label`, `Ax`, `Ly`, `note`).")
    return(FALSE)
  }
  
  ## check column names
  if(!colnames(df)[1] %in% "label") {
    warning("First column of data frame does not read `label`.")
    return(FALSE)
  }
  
  if(!colnames(df)[2] %in% "Ax") {
    warning("Second column of data frame does not read `Ax`.")
    return(FALSE)
  }
  
  if(!colnames(df)[3] %in% "Ly") {
    warning("Third column of data frame does not read `Ly`.")
    return(FALSE)
  }
  
  if(!colnames(df)[4] %in% "note") {
    warning("Fourth column of data frame does not read `label`.")
    return(FALSE)
  }
  
  ## numeric Ax and Ly
  if(is.numeric(df$Ax) == FALSE) {
    warning("Column `Ax` must be numeric.")
    return(FALSE)
  }
  
  if(is.numeric(df$Ly) == FALSE) {
    warning("Column `Ly` must be numeric.")
    return(FALSE)
  }
  
  ## duplicate labels?
  nDups <- length(duplicated(df$label)[duplicated(df$label) %in% TRUE])
  if(nDups > 0) {
    warning(paste0("The `label` column contains ", nDups, " duplicate labels."))
    return(FALSE)
  }
  
  return(TRUE)
  
}


#' Function latlong2stplane: utility function to 
#' 
#' convert MN state plane coordinates to lat/long 
#'@author Jared Beck
#'@description Function to convert latitude/longitude points to MN state plane coordinates
#'@param df data frame with columns for latitude longitude coordinates
#'@return Data frame with original state plane coordinates as well as lat/long coordinates
#'
latlong2stplane <- function(df){
  # require(sf)
  
  ## MN state plane coordinate system
  fromCRS = "+proj=longlat +datum=WGS84"
  
  ## Latitude/longitude
  toCRS = "+proj=lcc +lat_0=45 +lon_0=-94.25 +lat_1=45.6166666666667 +lat_2=47.05 +x_0=800000 +y_0=100000 +ellps=GRS80 +units=m
+no_defs"
  
  ## Standardize naming conventions
  names(df)[names(df) %in% c("lon", "long", "longitude")] = "long"
  names(df)[names(df) %in% c("lat", "latitude")] = "lat"
  
  ## Convert data frame into spatial object
  wgs = st_as_sf(df, coords = c("long", "lat"), crs = fromCRS)
  
  ## Convert lat/long coordinates in data frame to spatial points object with MN state plane spatial reference system
  mnsp = st_transform(wgs, crs = toCRS)
  
  
  out = data.frame(df)
  out$Ax = data.frame(st_coordinates(mnsp))$X
  out$Ly = data.frame(st_coordinates(mnsp))$Y
  
  return(out)
  
} ## end latlong2stplane function


#' Function stplane2latlong: utility function to 
#' 
#' convert MN state plane coordinates to lat/long
#' 
##'@author Jared Beck
##'@description Function to convert MN state plane coordinates in data frame to latitude/longitude
##'@param df data frame with columns for MN state plane coordinates coded as Ax or x and Ly or y
##'@return Data frame with original state plane coordinates as well as lat/long coordinates
stplane2latlong <- function(df) {
  # require(sf)
  
  ## MN state plane coordinate system
  fromCRS = "+proj=lcc +lat_0=45 +lon_0=-94.25 +lat_1=45.6166666666667 +lat_2=47.05 +x_0=800000 +y_0=100000 +ellps=GRS80 +units=m
+no_defs"
  
  ## Latitude/longitude
  toCRS = "+proj=longlat +datum=WGS84"
  
  ## Standardize naming conventions
  names(df)[names(df) %in% c("Ax", "x", "X")] = "Ax"
  names(df)[names(df) %in% c("Ly", "y", "Y")] = "Ly"
  
  ## Convert data frame into spatial object
  mnsp = st_as_sf(df, coords = c("Ax", "Ly"), crs = fromCRS)
  
  ## Convert lat/long coordinates in data frame to spatial points object with WGS84 spatial reference system
  wgs = st_transform(mnsp, crs = toCRS)
  
  
  out = data.frame(df)
  out$lon = data.frame(st_coordinates(wgs))$X
  out$lat = data.frame(st_coordinates(wgs))$Y
  
  return(out)
} ## end stplane2latlong function


#' Function stakefile2avenza: utility function
#'
#' convert stakefile into kml file that can be imported into Avenza
#'
#'
#'@author Jared Beck
#'@description Function to convert stakefile into kml file for Avenza
#'@param df data frame representing stakefile with MN state plane coordinates
#'@param fpath file path for exporting kml file
#'@return writes kml file to specified filepath
stakefile2avenza <- function(df, fpath) {
  # require(sf)
  # require(dplyr)
  
  ## check stakefile formatting
  if(is.stakefile(df) == FALSE){
    stop("Input data frame is not a stakefile")
  }
  
  ## check for NA coordinates
  if(NA %in% c(df$Ax, df$Ly)) {
    stop("Input data frame contains NA spatial coordinates")
  }
  
  ## check that file path specifies kml
  last4 <- substr(fpath, nchar(fpath)-3, nchar(fpath))
  if(!last4 == ".kml") {
    stop("Export file name does not end in `.kml`")
  }
  
  ## MN state plane coordinate system
  fromCRS = "+proj=lcc +lat_0=45 +lon_0=-94.25 +lat_1=45.6166666666667 +lat_2=47.05 +x_0=800000 +y_0=100000 +ellps=GRS80 +units=m
+no_defs"
  
  ## Latitude/longitude
  toCRS = "+proj=longlat +datum=WGS84"
  
  ffsf <- st_as_sf(df, coords = c("Ax","Ly"), crs = fromCRS) %>%
    st_transform(crs = toCRS) %>%
    select(Description = label)
  
  ## write out kml file
  st_write(ffsf, fpath, driver = "kml", delete_dsn = TRUE)
  
} ## end stakefile2avenza function


#' Function assignEchinaceaSite: 
#' 
#' assign standardized site names based on spatial coordinates
#' 
##'@author Jared Beck
##'@description Function to automatically assign standardized Echinacea sites based on spatial coordinates
##'@param df data frame with Ax and Ly columns representing MN state plane coordinates
##'@return vector of standardized site names

assignEchinaceaSite <- function(df) {
  
  if(!"Ax" %in% colnames(df) | !"Ly" %in% colnames(df)) {
    stop("Input data frame is missing Ax and/or Ly columns.")
  }
  
  df$echinaceaSite <- NA
  
  # This is not a table because some boxes overlap and they need to be coded in order.
  
  # If you add or delete any sites, then update siteNames().
  
  ## aa
  df$echinaceaSite[df$Ax > 690290 & df$Ax < 690450 & df$Ly > 190190 & df$Ly < 190340] <- "aa"
  
  ## alf
  df$echinaceaSite[df$Ax > 682800 & df$Ax < 682830 & df$Ly > 194160 & df$Ly < 194460] <- "alf" ## area south
  df$echinaceaSite[df$Ax > 682790 & df$Ax < 682820 & df$Ly > 194460 & df$Ly < 194730] <- "alf" ## area west
  df$echinaceaSite[df$Ax > 682820 & df$Ax < 682920 & df$Ly > 194538 & df$Ly < 194655] <- "alf" ## area east
  
  ## beng
  df$echinaceaSite[df$Ax > 690470 & df$Ax < 690770 & df$Ly > 188610 & df$Ly < 189020] <- "beng"
  
  ## btg
  df$echinaceaSite[df$Ax > 684390 & df$Ax < 684420 & df$Ly > 192410 & df$Ly < 192480] <- "btg"
  
  ## cg
  
  ## dog
  df$echinaceaSite[df$Ax > 687840 & df$Ax < 687880 & df$Ly > 193490 & df$Ly < 193510] <- "dog"
  
  ## eelr
  df$echinaceaSite[df$Ax > 684170 & df$Ax < 684410 & df$Ly > 196720 & df$Ly < 196860] <- "eelr"
  
  ## eth
  df$echinaceaSite[df$Ax > 690440 & df$Ax < 690600 & df$Ly > 192640 & df$Ly < 192720] <- "eth"
  
  ## eri
  df$echinaceaSite[df$Ax > 683600 & df$Ax < 683680 & df$Ly > 188775 & df$Ly < 188800] <- "eri"
  
  ## exPlot
  
  ## fern
  df$echinaceaSite[df$Ax > 683210 & df$Ax < 683480 & df$Ly > 192020 & df$Ly < 192430] <- "fern"
  
  ## gc
  df$echinaceaSite[df$Ax > 684410 & df$Ax < 684440 & df$Ly > 195160 & df$Ly < 195210] <- "gc"
  
  ## hulze
  df$echinaceaSite[df$Ax > 682540 & df$Ax < 682680 & df$Ly > 190570 & df$Ly < 190900] <- "hulze"
  
  ## hulzw
  df$echinaceaSite[df$Ax > 682345 & df$Ax < 682525 & df$Ly > 190470 & df$Ly < 190740] <- "hulzw"
  
  ## hud
  df$echinaceaSite[df$Ax > 682825 & df$Ax < 682840 & df$Ly > 194660 & df$Ly < 195130] <- "hud"
  
  ## hutchw
  df$echinaceaSite[df$Ax > 682820 & df$Ax < 683055 & df$Ly > 194440 & df$Ly < 194538] <- "hutchw"
  
  ## hutche
  df$echinaceaSite[df$Ax > 683055 & df$Ax < 683310 & df$Ly > 194440 & df$Ly < 194538] <- "hutche"
  
  ## kj
  df$echinaceaSite[df$Ax > 683980 & df$Ax < 684020 & df$Ly > 196815 & df$Ly < 196850] <- "kj"
  
  ## krus
  df$echinaceaSite[df$Ax > 685000 & df$Ax < 685210 & df$Ly > 194510 & df$Ly < 194950] <- "krus"
  
  ## koons
  df$echinaceaSite[df$Ax > 690770 & df$Ax < 690870 & df$Ly > 189640 & df$Ly < 189800] <- "koons"
  
  ## lfe
  df$echinaceaSite[df$Ax > 683230 & df$Ax < 683280 & df$Ly > 194380 & df$Ly < 194440] <- "lfe"
  
  ## lfw
  df$echinaceaSite[df$Ax > 683010 & df$Ax < 683230 & df$Ly > 194370 & df$Ly < 194442] <- "lfw"
  
  ## lih
  df$echinaceaSite[df$Ax > 684420 & df$Ax < 684640 & df$Ly > 195560 & df$Ly < 195800] <- "lih"
  
  ## loco
  df$echinaceaSite[df$Ax > 690680 & df$Ax < 690755 & df$Ly > 188120 & df$Ly < 188310] <- "loco"
  
  ## lce
  df$echinaceaSite[df$Ax > 682755 & df$Ax < 682810 & df$Ly > 190740 & df$Ly < 190840] <- "lce"
  
  ## lcw
  df$echinaceaSite[df$Ax > 682660 & df$Ax < 682755 & df$Ly > 190790 & df$Ly < 190910] <- "lcw"
  
  ## mapp
  df$echinaceaSite[df$Ax > 688840 & df$Ax < 688880 & df$Ly > 193490 & df$Ly < 193500] <- "mapp"
  
  ## nrrx
  df$echinaceaSite[df$Ax > 682745 & df$Ax < 682775 & df$Ly > 191200 & df$Ly < 191390] <- "nrrx"
  
  ## nrpal
  
  ## nth
  df$echinaceaSite[df$Ax > 690200 & df$Ax < 690230 & df$Ly > 192640 & df$Ly < 192660] <- "nth"
  
  ## ness
  df$echinaceaSite[df$Ax > 686010 & df$Ax < 686030 & df$Ly > 192520 & df$Ly < 192620] <- "ness"
  
  ## nice
  df$echinaceaSite[df$Ax > 685560 & df$Ax < 685750 & df$Ly > 193290 & df$Ly < 193400] <- "nice"
  
  ## nnwlf
  df$echinaceaSite[df$Ax > 682830 & df$Ax < 682860 & df$Ly > 195540 & df$Ly < 195590] <- "nnwlf"
  
  ## ngc
  df$echinaceaSite[df$Ax > 684400 & df$Ax < 684440 & df$Ly > 195330 & df$Ly < 195530] <- "ngc"
  
  ## nwlf
  df$echinaceaSite[df$Ax > 682820 & df$Ax < 682850 & df$Ly > 195130 & df$Ly < 195290] <- "nwlf"
  
  ## onts
  df$echinaceaSite[df$Ax > 684580 & df$Ax < 684650 & df$Ly > 193500 & df$Ly < 193580] <- "onts"
  
  ## pal
  df$echinaceaSite[df$Ax > 689825 & df$Ax < 689905 & df$Ly > 188450 & df$Ly < 188630] <- "pal"
  
  ## rrxx
  df$echinaceaSite[df$Ax > 682740 & df$Ax < 682790 & df$Ly > 190970 & df$Ly < 191160] <- "rrxx"
  
  ## rndt
  df$echinaceaSite[df$Ax > 686790 & df$Ax < 686850 & df$Ly > 188700 & df$Ly < 188720] <- "rndt"
  
  ## rel
  df$echinaceaSite[df$Ax > 689580 & df$Ax < 689600 & df$Ly > 193010 & df$Ly < 193030] <- "rel"
  
  ## rhc
  
  ## rhx
  df$echinaceaSite[df$Ax > 689930 & df$Ax < 689950 & df$Ly > 188460 & df$Ly < 188480] <- "rhx"
  
  ## rhe
  df$echinaceaSite[df$Ax > 690620 & df$Ax < 690650 & df$Ly > 187930 & df$Ly < 187960] <- "rhe"
  
  ## rhp
  df$echinaceaSite[df$Ax > 689780 & df$Ax < 689800 & df$Ly > 188605 & df$Ly < 188615] <- "rhp"
  
  ## rhs
  df$echinaceaSite[df$Ax > 690500 & df$Ax < 690520 & df$Ly > 187845 & df$Ly < 187865] <- "rhs"
  
  ## rhw
  
  ## rke
  df$echinaceaSite[df$Ax > 686680 & df$Ax < 686710 & df$Ly > 187130 & df$Ly < 187150] <- "rke"
  
  ## rkw
  df$echinaceaSite[df$Ax > 686500 & df$Ax < 686520 & df$Ly > 187220 & df$Ly < 187240] <- "rkw"
  
  ## rih
  df$echinaceaSite[df$Ax > 683380 & df$Ax < 683550 & df$Ly > 188410 & df$Ly < 188690] <- "rih"
  
  ## riley
  df$echinaceaSite[df$Ax > 683490 & df$Ax < 683630 & df$Ly > 188760 & df$Ly < 188815] <- "riley"
  
  ## rlr
  
  ## rrxdc
  df$echinaceaSite[df$Ax > 682790 & df$Ax < 682840 & df$Ly > 190840 & df$Ly < 190870] <- "rrxdc"
  
  ## rrxgc
  
  ## sign
  df$echinaceaSite[df$Ax > 683780 & df$Ax < 683810 & df$Ly > 196840 & df$Ly < 196850] <- "sign"
  
  ## sgc
  df$echinaceaSite[df$Ax > 684420 & df$Ax < 684460 & df$Ly > 193820 & df$Ly < 193860] <- "sgc"
  
  ## sppe
  df$echinaceaSite[df$Ax > 683540 & df$Ax < 683990 & df$Ly > 191530 & df$Ly < 192020] <- "sppe"
  
  ## sppw
  df$echinaceaSite[df$Ax > 683090 & df$Ax < 683540 & df$Ly > 191530 & df$Ly < 192020] <- "sppw"
  
  ## sap
  df$echinaceaSite[df$Ax > 689610 & df$Ax < 689650 & df$Ly > 192740 & df$Ly < 192850] <- "sap"
  
  ## torgen
  df$echinaceaSite[df$Ax > 684080  & df$Ax < 684175  & df$Ly > 185645  & df$Ly < 185725 ] <- "torgen"
  
  ## torges
  df$echinaceaSite[df$Ax > 683950  & df$Ax < 683985  & df$Ly > 185700  & df$Ly < 185750 ] <- "torges"
  
  ## torgew
  df$echinaceaSite[df$Ax > 683770 & df$Ax < 683935 & df$Ly > 185580 & df$Ly < 185860] <- "torgew"
  
  ## tower
  df$echinaceaSite[df$Ax > 685630 & df$Ax < 685710 & df$Ly > 193460 & df$Ly < 193530] <- "tower"
  
  ## th
  df$echinaceaSite[df$Ax > 689740 & df$Ax < 689830 & df$Ly > 192650 & df$Ly < 192730] <- "th"
  
  ## tplot
  df$echinaceaSite[df$Ax > 685570 & df$Ax < 685590 & df$Ly > 193587 & df$Ly < 193600] <- "tplot"
  
  ## waa
  df$echinaceaSite[df$Ax > 689410 & df$Ax < 689610 & df$Ly > 190245 & df$Ly < 190265] <- "waa"
  
  ## wood
  df$echinaceaSite[df$Ax > 684260 & df$Ax < 684330 & df$Ly > 188700 & df$Ly < 188800] <- "wood"
  
  ## yohe
  df$echinaceaSite[df$Ax > 682450 & df$Ax < 682500 & df$Ly > 190420 & df$Ly < 190440] <- "yohe"
  
  ## yohw
  df$echinaceaSite[df$Ax > 682340 & df$Ax < 682390 & df$Ly > 190410 & df$Ly < 190470] <- "yohw"
  
  
  
  
  ## experimental plots
  
  assigned <- length(df$echinaceaSite[!df$echinaceaSite %in% NA])
  notAssigned <- length(df$echinaceaSite[df$echinaceaSite %in% NA])
  
  if(notAssigned > 0) {
    message(paste0("Assigned Echinacea site to ", assigned, 
                   " records based on spatial coordinates.\n\n",
                   "Failed to assign Echinacea site to ", notAssigned, 
                   " records based on spatial coordinates."))
  } else {
    message(paste0("Assigned Echinacea site to ", assigned, 
                   " records based on spatial coordinates."))
  }
  
  df$echinaceaSite[df$echinaceaSite %in% NA] <- "other"
  df$site <- df$echinaceaSite
  df$echinaceaSite <- NULL
  
  return(df$site)
} ## end of assignEchinaceaSite function

#' list standard site names ####
#' 
#' return vector of standard Echinacea site names
#'
#'@author Stuart
#'@return vector of standard site names

siteNames <- function() {
  # 2025-06-09
  c("aa",
    "alf",
    "beng",
    "btg",
    "dog",
    "eelr",
    "eri",
    "eth",
    "fern",
    "gc",
    "hud",
    "hulze",
    "hulzw",
    "hutche",
    "hutchw",
    "kj",
    "koons",
    "krus",
    "lce",
    "lcw",
    "lfe",
    "lfw",
    "lih",
    "loco",
    "mapp",
    "ness",
    "ngc",
    "nice",
    "nnwlf",
    "nrrx",
    "nth",
    "nwlf",
    "onts",
    "pal",
    "rel",
    "rhe",
    "rhp",
    "rhs",
    "rhx",
    "rih",
    "riley",
    "rke",
    "rkw",
    "rndt",
    "rrxdc",
    "rrxx",
    "sap",
    "sgc",
    "sign",
    "sppe",
    "sppw",
    "th",
    "torgen",
    "torges",
    "torgew",
    "tower",
    "tplot",
    "waa",
    "wood",
    "yohe",
    "yohw"
  )
}
