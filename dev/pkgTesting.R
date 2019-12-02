

sql <- 'select * from Datasets where Active=1'
dSets <- doQueryFromFed(sql)

ds <- dSets$DataSet


lodfs <- vector("list", length(ds))
for (i in 1:length(ds)){
  print(ds[i])
  df <- getSoilData(DataSets=ds[i], observedProperty='4A1', usr='ross.searle@csiro.au', key='a')
  print(head(df))
  lodfs[[i]] <- df
}

soilDF = as.data.frame(data.table::rbindlist(lodfs))

write.csv(outDF, 'c:/temp/soilDF.csv', row.names = F)
soilDF <- read.csv('c:/temp/soilDF.csv')
soilDF <- soilDF[soilDF$Dataset != 'EastCentral_Australia', ]
 





p <- makeLocations(df, T)
plot(p)

apiIsAlive()

df <- apiDataSets(usr='ross.searle@csiro.au', key='a')
df <- apiDataSets(usr='Brendan.Malone@csiro.au', key='djwjgrpt74ld7wm')
getDataSets(usr='Brendan.Malone@csiro.au', key='djwjgrpt74ld7wm')
nrow(df)


getProperties()
getProperties(PropertyGroup = 'PSA', verbose = F)
getProperties(PropertyGroup = 'PSA', verbose = T)


isValidSoilDataframe(dSets)
isValidSoilDataframe(soilDF)

odfxyz <- inAustralia(locsDF)
odfxyz <- inAustralia(soilDF)



locsF <- getSiteLocations(DataSets='TERNSurveillance', usr='ross.searle@csiro.au', key='a')
str(locsDF)
isValidLocationDataframe(locsDF)
isValidLocationDataframe(soilDF)


grouping='DataSet'
summaryDF <- summariseContinuousSoilData(soilDF = soilDF)
head(soilDF)


maxVal = 14
maxDepth = 5
