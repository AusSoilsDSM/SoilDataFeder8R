library(ggplot2)




#' Summarises values in a SoilData dataframe
#'
#' Returns a list of various summary statistics and charts.
#' @param soilDF Dataframe to summarise
#' @keywords 
#' @export
#' @examples
#' summariseContinuousSoilData(soilDF=DF)

summariseContinuousSoilData <- function(soilDF, outputs='All'){
  
  if(!isValidSoilDataframe(soilDF)){return(NULL)}
  
  bob <- list()
  
  soilDF$Value <- as.numeric(as.character(soilDF$Value))
  
  s <- summary(as.numeric(soilDF$Value))
  dfs <- data.frame(Statistic=character(6), Value=character(6), stringsAsFactors = F)
  dfs[1,] <- c('Min Value ',s[1])
  dfs[2,] <- c('1st Quartile ',s[2])
  dfs[3,] <- c('Median ' ,s[3])
  dfs[4,] <- c('Mean ' ,s[4])
  dfs[5,] <- c('3rd Quartile ' ,s[5])
  dfs[6,] <- c('Max value ',s[6])
  validCnt <- length(which(!is.na(dfs$Value)))
  naCnt <- length(which(is.na(dfs$Value)))
  dfs[7,] <- c('valid Value Count',validCnt)
  dfs[8,] <- c('NA Count',naCnt)
  dfs[10,] <- c('Locations Count', length(unique(soilDF$Observation_ID)))
  dfs[11,] <- c('Record Count', nrow(soilDF))
  dfs[12,] <- c('Raw Minimum Depth', min(soilDF$UpperDepth, na.rm = T))
  dfs[13,] <- c('Raw Maximum Depth', max(soilDF$LowerDepth, na.rm = T))
  
  #soilDF$ObservedProperty[1]
  
  cat('Summary Statistics for', soilDF$ObservedProperty[1],'\n')
  cat('==================================','\n')
  print(dfs)
  
  bob$summaryDF <- dfs
 
  
    
    dp <- ggplot(soilDF) + geom_density(aes(x = Value, fill = Dataset), alpha = 0.2) + ggtitle(paste0("Data Distribution for ", soilDF$ObservedProperty[1], ' Grouped by ', grouping )) +
      xlab(paste0('Value (', soilDF$Units[1],')')) + ylab("Density")
    bob$DensityPlot <-dp
    
    
    bp <- ggplot(soilDF, aes(x=Dataset, y=Value, fill=Dataset)) + geom_boxplot() + theme(axis.text.x  = element_text(angle=90, vjust=0.5))
    bob$BoxPlot <- bp
    
    
  
  return(bob)
  
}

