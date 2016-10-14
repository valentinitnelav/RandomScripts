# extract long-lat coordinates from kmz
# worht checking this as well:
# https://gist.github.com/holstius/6631918

library(maptools )

# list the kmz files in the give folder
KMZs <- list.files(path="Data/kmz-files", pattern="*.kmz", full.names=FALSE)

# for each kmz file, unzip and then read coordinates with getKMLcoordinates()
# The indexing "[[1]]" directly takes the matrix of the list returned by getKMLcoordinates()
# this is for simplifying the output, together with sapply
# therefore, the final results is a matrix
LonLat <- sapply(KMZs, 
                 function(x) 
                   getKMLcoordinates(unzip(zipfile=paste0("Data/kmz-files/", x),
                                           exdir="Data/kmz-files/KML"), 
                                     ignoreAltitude=TRUE)[[1]])

# delete the .kmz part from the column names
colnames(LonLat) <- gsub(pattern=".kmz", replacement="", x=colnames(LonLat))

# give names to the rows (the coordinates)
rownames(LonLat) <- c("Longitude", "Latitude")

# transpose the matrix for readability reasons
LonLat <- t(LonLat)

# write coordinates to csv file
write.csv(LonLat, "Data/kmz-files/LonLat_kmz.csv", col.names = TRUE)
