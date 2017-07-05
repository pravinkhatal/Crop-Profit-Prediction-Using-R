latitude <- c(16.70,18.40,16.87,16.95,17.68)
longitude <- c(74.22,76.58,74.57,74.40,75.92)
Coordinates <- c(cbind(latitude,longitude))
Coordinates <- matrix(Coordinates,byrow = TRUE,nrow = 5)
rownames(Coordinates) <- c("Kolhapur","Latur","Sangli","Satara","Solapur")
colnames(Coordinates) <- c("lat","lon")
city <- "Sangli"
Coordinates <- as.data.frame(Coordinates)
Coordinates[city,1]
