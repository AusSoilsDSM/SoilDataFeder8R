library(magrittr)
library(dplyr)
library(rgdal)
library(raster)


#' Merge ObservedProperties 
#'
#' Takes the standard SoilDataFederator dataframe returned by getSoilData function and transposes the multiple observed propeties into individual columns.
#' @param soilDF Dataframe to transpose
#' @keywords Transform
#' @export
#' @examples
#' mergeObservedProperties(obsPropDF=DF)

mergeObservedProperties <- function(soilDF){
  
  if(!isValidSoilDataframe(soilDF)){return(NULL)}
  
 props <- unique(obsPropDF$ObservedProperty)
    
    lodfs <- vector("list", length(props)-1)
    outdf <-  soilDF[soilDF$ObservedProperty == props[1], ]
    names(outdf)[names(outdf) == "Value"] <- props[1]
    names(outdf)[names(outdf) == "Units"] <- paste0('Units_', props[1])
    outdf <- outdf[, -'ObservedProperty']
    outdf <- as.data.frame(outdf, rownames(NULL))[ ,  c("DataStore", "Dataset", "Provider", "Observation_ID", "SampleID", "SampleDate", "Longitude", "Latitude", "UpperDepth", "LowerDepth", props[1])]
    
    for(i in 2:length(props)){
     # print(i)
      fdf <- as.data.frame(soilDF[soilDF$ObservedProperty == props[i], ])
      # outdf <- merge(outdf, lodfs[[i]], 
      #                by = c('DataStore','Dataset','Provider', 'Observation_ID', 'SampleID', 'SampleDate', 'Longitude', 'Latitude', 
      #                       'UpperDepth', 'LowerDepth', 'PropertyType',  'QualCollection', 'QualSpatialAgg'
      #                       , 'QualManagement', 'ExtractTime'), all = T, allow.cartesian=TRUE)
      

      jdf <- fdf[, c("DataStore", "Dataset", "Provider", "Observation_ID", "SampleID", "SampleDate", "Longitude", "Latitude", "UpperDepth", "LowerDepth", "Value")]
      mdf <- dplyr::full_join(outdf, jdf)
      names(mdf)[names(mdf) == "Value"] <- props[i]
      outdf <- mdf
    }
}

showDuplicateRecords <- function(soilDF){
  
  if(!isValidSoilDataframe(soilDF)){return(NULL)}
  
  idx <- duplicated(soilDF) | duplicated(soilDF, fromLast = TRUE)
  ot <- soilDF[idx, ]
  sot <- ot[order(ot$DataStore, ot$Dataset, ot$Provider, ot$Observation_ID, ot$UpperDepth, ot$LowerDepth, ot$ObservedProperty, ot$Value, decreasing = FALSE),]
  sot
  return(sot)
}

removeDuplicateRecords <- function(soilDF){
  noDups <- soilDF[!duplicated(soilDF),]
  return(noDups)
}


#' Return locations of samples
#'
#' Takes the standard SoilDataFederator dataframe returned by getSoilData function and generates a spatialPointsDataFrame .
#' @param soilDF Dataframe to generate site locations for
#' @keywords spatialPointsDataFrame
#' @export
#' @examples
#' makeLocations(soilDF=DF)


makeLocations <- function(soilDF, drawit=F){
  
  if(!isValidSoilDataframe(soilDF)){return(NULL)}
  
  idxs <- which(is.na(soilDF$Longitude) | is.na(soilDF$Latitude))
  locPts <- soilDF[-idxs, ]
  
  locPts2 <- inAustralia(locPts)
  
  pts <- locPts2 %>% group_by(DataStore, Dataset, Provider, Observation_ID,  SampleDate, Longitude, Latitude) %>% summarise(n())
  coordinates(pts) <- ~Longitude+Latitude
  crs(pts) <- CRS("+proj=longlat +datum=WGS84")
  
  if(drawit){
    plot(AustBdy)
    points(pts, col='red')
  }
  
  return(pts)
}


#' Return locations only within the bounding box od Australia
#'
#' Takes the standard SoilDataFederator dataframe returned by getSoilData function and filters the locations to the extent of Australia .
#' @param soilDF Dataframe to filter
#' @keywords dataframe
#' @export
#' @examples
#' inAustralia(soilDF=DF)

inAustralia <- function(soilDF){
  
  if(!isValidSoilDataframe(soilDF)){return(NULL)}
  bboxExt <- extent(110,153,-43,-9)
  idxs <- which(soilDF$Longitude >= bboxExt@xmin & soilDF$Longitude <= bboxExt@xmax & soilDF$Latitude >= bboxExt@ymin & soilDF$Latitude <= bboxExt@ymax)
  outdf <- soilDF[idxs, ]
}



tidyUpSoilData <- function(soilDF, minVal=0, maxVal=NULL, minDepth=0, maxDepth=NULL, inAust, action='show'){
  
  if(is.null(maxValue)){ stop('You need to specify a maximum value')}
  if(is.null(maxDepth)){ stop('You need to specify a maximum depth')}
  
  if(!isValidSoilDataframe(soilDF)){return(NULL)}
  
  outdf <- data.frame()
  md <- soilDF[which(soilDF$Value < minVal), ]
  md$Issue <- 'Value less than allowable minimum value'
  outdf <- rbind(outdf, md)
  mx <- soilDF[which(soilDF$Value > maxVal), ]
  mx$Issue <- 'Value greater than allowable maximum value'
  outdf <- rbind(outdf, mx)
  mdepth <- soilDF[which(soilDF$LowerDepth < minDepth), ]
  mdepth$Issue <- 'Depth less than allowable minimum depth'
  outdf <- rbind(outdf, mdepth)
  mxdepth <- soilDF[which(soilDF$UpperDepth > maxDepth), ]
  mxdepth$Issue <- 'Depth greater than allowable maximum depth'
  outdf <- rbind(outdf, mxdepth)
  
  tail(outdf)
}









