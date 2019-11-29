library(jsonlite)
library(httr)

apiRoot <- 'http://esoil.io/TERNLandscapes/SoilDataFederatoR/R/SoilDataAPI'


#' Get DataSets
#'
#' Returns information about the DataSets available in the SoilDataFederator.
#' @param usr user name
#' @param key API key
#' @export
#' @examples
#' apiDataSets()
#' 
apiDataSets <- function(usr='Demo', key='Demo'){
  url <- paste0(apiRoot, '/DataSets?usr=', usr, '&key=', key)
  print(url)
  r <-  httr::GET(url)
  stop_for_status(r)
  df <- as.data.frame(fromJSON(content(r, "text")))
  return(df)
}


#' apiIsAlive
#'
#' Checks to see if the SoilDataFederator is alive and well.
#' @export
#' @examples
#' apiIsAlive()
#' 
apiIsAlive <- function(){
  url <- paste0(apiRoot, '/DataSets')
  #print(url)
  r <-  httr::GET(url)
  if( r$status_code==503)
  {return(FALSE)}else{
    return(TRUE)
  }
}