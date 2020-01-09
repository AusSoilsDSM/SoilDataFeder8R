library(mapview)
library(leaflet)
library(leafgl)
library(sf)
library(colourvalues)

#' Draw a interactive leaflet map of soil site locations
#'
#' Takes the standard SoilDataFederator dataframe returned by getSoilData function and shows the locations in a leaflet map.
#' @param soilDF Dataframe to map
#' @keywords Map
#' @export
#' @examples
#' showLeafletMap(soilDF=DF)


showLeafletMap <- function(soilDF){

    pts = st_as_sf(soilDF, coords = c("Longitude", "Latitude"), crs = 4326)
    cols = colour_values_rgb(as.numeric(pts$Value), include_alpha = FALSE) / 255
    
    #options(viewer = TRUE) # view in browser
    #pal <- colorNumeric(c("red", "green", "blue"), domain = as.numeric(pts$Value))
    
    soilDF$PopUp <- paste0("Site ID : ", pts$Observation_ID, "<br> Attribute : ", pts$ObservedProperty , "<br> Value : ", pts$Value )
    
    system.time({
      lmap = leaflet() %>%
        addTiles() %>%
        addGlPoints(data = pts, color = cols, group = "SoilData", popup = 11) %>%
        leafem::addMouseCoordinates() %>%
        setView(lng = 134, lat = -24, zoom = 4) %>% 
        addLayersControl(overlayGroups = "SoilData") #%>%
      
      #   addLegend("topright", pal = pal, values = as.numeric(pts$Value),   
      #           title = "Soil Data",
      #           opacity = 1
      # )
    })
    
    lmap
}