

#' Merge ObservedProperties 
#'
#' Takes the standard SoilDataFederator dataframe returned by getSoilData function and transposes the multiple observed propeties into individual columns.
#' @param DF Dataframe to transpose
#' @keywords cats
#' @export
#' @examples
#' mergeObservedProperties(obsPropDF=DF)

mergeObservedProperties <- function(obsPropDF){
  
  # check if sdfDF
  
 props <- unique(obsPropDF$ObservedProperty)
    
    lodfs <- vector("list", length(props)-1)
    outdf <-  obsPropDF[obsPropDF$ObservedProperty == props[1], ]
    names(outdf)[names(outdf) == "Value"] <- props[1]
    names(outdf)[names(outdf) == "Units"] <- paste0('Units_', props[1])
    outdf <- outdf[, -'ObservedProperty']
    outdf <- as.data.frame(outdf, rownames(NULL))[ ,  c("DataStore", "Dataset", "Provider", "Observation_ID", "SampleID", "SampleDate", "Longitude", "Latitude", "UpperDepth", "LowerDepth", props[1])]
    
    for(i in 2:length(props)){
     # print(i)
      fdf <- as.data.frame(obsPropDF[obsPropDF$ObservedProperty == props[i], ])
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

showDuplicateRecords <- function(df){
  idx <- duplicated(df) | duplicated(df, fromLast = TRUE)
  ot <- df[idx, ]
  sot <- ot[order(ot$DataStore, ot$Dataset, ot$Provider, ot$Observation_ID, ot$UpperDepth, ot$LowerDepth, ot$ObservedProperty, ot$Value, decreasing = FALSE),]
  sot
  return(sot)
}

removeDuplicateRecords <- function(df){
  noDups <- df[!duplicated(df),]
  return(noDups)
}

# df[!duplicated(df),]
# 
# t1 <- outdf[!duplicated(outdf), c("DataStore", "Dataset", "Provider", "Observation_ID", "SampleID", "SampleDate","Longitude", "Latitude", "UpperDepth", "LowerDepth", "2Z2_Clay")]
# t2 <- jdf[!duplicated(jdf), c("DataStore", "Dataset", "Provider", "Observation_ID", "SampleID", "SampleDate","Longitude", "Latitude", "UpperDepth", "LowerDepth", "Value")]
# ot <- dplyr::full_join(t1, t2)
# sot <- ot[order(ot$DataStore, ot$Dataset, ot$Provider, ot$Observation_ID, ot$UpperDepth, ot$LowerDepth, decreasing = FALSE),]
# sot
# 
# 
# which(duplicated(outdf) )
# df[23083:23084,]
