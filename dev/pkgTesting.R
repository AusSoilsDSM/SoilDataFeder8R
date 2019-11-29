

p <- makeLocations(df, T)
plot(p)


df <- apiDataSets(usr='ross.searle@csiro.au', key='a')
df <- apiDataSets(usr='Brendan.Malone@csiro.au', key='djwjgrpt74ld7wm')
getDataSets(usr='Brendan.Malone@csiro.au', key='djwjgrpt74ld7wm')
nrow(df)


apiIsAlive()
