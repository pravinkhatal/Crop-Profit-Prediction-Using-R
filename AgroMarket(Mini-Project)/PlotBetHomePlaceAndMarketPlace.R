
dest <- read.csv("GeoCoordinatesRecords.csv")
dest

city <- c("sangli","sangli","sangli","sangli","sangli")
Latitude <- c(16.86,16.86,16.86,16.86,16.86)
Longitude <- c(74.57,74.57,74.57,74.57,74.57)
orig <- data.frame(city = c("sangli","sangli","sangli","sangli","sangli"),
                   Latitude = c(16.86,16.86,16.86,16.86,16.86),
                   Longitude = c(74.57,74.57,74.57,74.57,74.57)
)

orig$sequence <- c(sequence = seq(1, length.out = nrow(orig), by=2))
dest$sequence <- c(sequence = seq(2, length.out = nrow(orig), by=2))

library("sqldf")
q <- "
SELECT * FROM orig
UNION ALL
SELECT * FROM dest
ORDER BY sequence
"
poly_df <- sqldf(q)

library("leaflet")
leaflet() %>%
  addTiles() %>%
  
  addPolylines(
    data = poly_df,
    lng = ~Longitude, 
    lat = ~Latitude,
    weight = 3,
    opacity = 3
  ) 




#Use of multiple addmarkers

rownames(Coordinates) <- c("Kolhapur","Latur","Sangli","Satara","Solapur")
Coordinates[rownames(Coordinates)[1],1]
leaflet(data = Coordinates) %>% addTiles() %>% addMarkers(~longitude,~latitude)
