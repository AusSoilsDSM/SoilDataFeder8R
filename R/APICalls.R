library(jsonlite)
library(httr)

apiRoot <- 'http://esoil.io/TERNLandscapes/SoilDataFederatoR/R/SoilDataAPI'


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

#' Get Properties
#'
#' Returns a listing of the available properties
#' @param verbose (Optional) return just the property codes or the full descriptions. Default = True
#' @param PropertyGroup (Optional) return just the properties for a given PropertyGroup. Default = All
#' @export
#' @examples
#' apiProperties(PropertyGroup=NULL, verbose=TRUE)
#' 
apiProperties <- function(PropertyGroup=NULL, verbose=TRUE){
  if (is.null(PropertyGroup)){
    paramPropertyGroup=''
  }else{
    paramPropertyGroup=paste0('PropertyGroup=', PropertyGroup)
  }
  url <- paste0(apiRoot, '/Properties?', paramPropertyGroup, '&verbose=', verbose)
  print(url)
  r <-  httr::GET(url)
  stop_for_status(r)
  df <- as.data.frame(fromJSON(content(r, "text")))
  return(df)
}




#' Get PropertyGroups
#'
#' Returns a listing of the available Property Groups
#' @export
#' @examples
#' apiPropertyGroups()
#' 
apiPropertyGroups <- function(PropertyGroup=NULL, verbose=TRUE){
  if(is.null(PropertyGroup)){paramPropertyGroup=''}else{ paramPropertyGroup=paste0('PropertyGroup=', PropertyGroup)}
  url <- paste0(apiRoot, '/PropertyGroups?', paramPropertyGroup, '&verbose=', verbose)
  print(url)
  r <-  httr::GET(url)
  stop_for_status(r)
  df <- as.data.frame(fromJSON(content(r, "text")))
  return(df)
}



#' Get SoilData

#'
#' Returns soil observation data
#' @param observedProperty (Optional) Specify the soil data property/s to return. It should be a single observedProperty code or a semi-colon delimited text string of observedProperty codes.
#' @param PropertyGroup (Optional)  Extract data for a defined group of soil properties.
#' @param DataSet (Optional) Filter the data returned to a specific set of DataSets. It should be a single DataSet code or a semi-colon delimited text string of DataSet codes. Default = All DataSets
#' @param bbox (Optional) The rectangular bounding box of the area in the form minx;maxx;miny;maxy - semicolon delimited 
#' @param numToReturn (Optional) The number of records to be returned. Default = All
#' @param usr (Required) User name for accessing the API. To register for an API key go to - https://shiny.esoil.io/SoilDataFederator/Register/ You can use usr=Demo & key=Demo but only the first 5 records will be returned
#' @param key (Required) API key for accessing the API.

#' @export
#' @examples
#' apiGetSoilData()
#' 
apiGetSoilData <- function(observedProperty=NULL, PropertyGroup=NULL,DataSet=NULL, bbox=NULL, numToReturn=NULL, usr='Demo', key='Demo'){
  
  if(is.null(observedProperty)){paramobservedProperty=''}else{ paramobservedProperty=paste0('observedProperty=', observedProperty, '&')}
  if(is.null(PropertyGroup)){paramPropertyGroup=''}else{ paramPropertyGroup=paste0('PropertyGroup=', PropertyGroup, '&')}
  if(is.null(DataSet)){paramDataSet=''}else{ paramDataSet=paste0('DataSet=', DataSet, '&')}
  if(is.null(bbox)){parambbox=''}else{ parambbox=paste0('bbox=', bbox, '&')}
  if(is.null(numToReturn)){paramnumToReturn=''}else{ paramnumToReturn=paste0('numToReturn=', numToReturn, '&')}

  
  url <- paste0(apiRoot, '/SoilData?',paramobservedProperty, paramPropertyGroup, '&verbose=', verbose)
  print(url)
  r <-  httr::GET(url)
  stop_for_status(r)
  df <- as.data.frame(fromJSON(content(r, "text")))
  return(df)
}

