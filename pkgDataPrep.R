library(rgdal)

setwd('C:/Users/sea084/Dropbox/RossRCode/Git/SoilDataFeder8R')

AustBdy <- readOGR('C:/Projects/GIS/National', 'Australia')
crs(AustBdy) <- CRS("+proj=longlat +datum=WGS84")
plot(AustBdy)


save(AustBdy, file="data/mydata.RData")

usethis::use_data(AustBdy, internal = TRUE)

SoilDataFeder8R::AustBdy
  