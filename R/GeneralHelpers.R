



blankResponseDF <- function(){
  
  outDF <- data.frame(DataStore=character(), Dataset=character(), Provider=character(), Observation_ID=character(), SampleID=character(), SampleDate=character() ,
                      Longitude=numeric() , Latitude= numeric(),
                      UpperDepth=numeric() , LowerDepth=numeric() , PropertyType=character(), ObservedProperty=character(), Value=numeric(),
                      Units= character(),   QualCollection=integer(), QualSpatialAgg=integer(), QualManagement=integer(), stringsAsFactors = F)
}

blankLocationsResponseDF <- function(){
  
  outDF <- data.frame(DataStore=character(), Dataset=character(), Provider=character(), Observation_ID=character(),
                      Longitude=numeric() , Latitude= numeric(), SampleDate=character())
                      }


isValidSoilDataframe <- function(soilDF){
  
  templateDF <- data.frame(blankResponseDF(), ExtractTime=character())
  colCnt <- length(colnames(templateDF))
  
  if(length(colnames(soilDF)) != length(colnames(templateDF))){
    warning(paste0('Supplied dataframe column count does not match the required number (', colCnt,') of a standard SoilData dataframe'))
    return(FALSE)
  }

  if(colnames(soilDF) != colnames(templateDF)){
    warning(paste0('Supplied dataframe columns do not match the requirements of a standard SoilData dataframe'))
    return(FALSE)
  }
  return(TRUE)
}



isValidLocationDataframe <- function(LocsDF){
  
  templateDF <- data.frame(blankLocationsResponseDF(), ExtractTime=character())
  colCnt <- length(colnames(templateDF))
  
  if(length(colnames(LocsDF)) != length(colnames(templateDF))){
    warning(paste0('Supplied dataframe column count does not match the required number (', colCnt,') of a standard SoilLocations dataframe'))
    return(FALSE)
  }
  
  if(colnames(LocsDF) != colnames(templateDF)){
    warning(paste0('Supplied dataframe columns do not match the requirements of a standard SoilLocations dataframe'))
    return(FALSE)
  }
  return(TRUE)
}
