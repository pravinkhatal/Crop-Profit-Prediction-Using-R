library(shinydashboard)
library(leaflet)
library(dplyr)

# generate base leaflet map
output$map <- renderLeaflet({
  map <- leaflet() %>% addTiles()
  map
})

TransportRate <- 5


AgroMarketForWheat <- function(HomePlace, Quantity, MinPrice, MaxPrice){
  maxProfit <- 0
  place <- ""
  result <- list()
  for(iterator in 1:5){
    price <- DataMatrixforWheat[iterator, 1] + DataMatrixforWheat[iterator, 2] * Quantity + DataMatrixforWheat[iterator, 3] * MinPrice + DataMatrixforWheat[iterator, 4] * MaxPrice 
    distance <- distance2Points(HomePlace, rownames(DataMatrixforWheat)[iterator])
    amount <- price * Quantity - distance * TransportRate
    if(maxProfit < amount){
      maxProfit <- amount
      place <- rownames(DataMatrixforWheat)[iterator]
    }
  }
  result[['Place']] <- place
  result[['MaxProfit']] <- maxProfit
  return(result)
}


function(input, output){
  output$cropSelect <- renderUI({
    plant <- input$cropProducts
    if(grepl(plant,"Wheat")){
      
      # leafletProxy('outputmap') %>%
      leaflet() %>%
        addTiles() %>%
        addMarkers(lng=Coordinates[AgroMarketForWheat,2],lat=Coordinates[AgroMarketForWheat,1])
    }
  })
}


latitude <- c(16.70,18.40,16.87,16.95,17.68)
longitude <- c(74.22,76.58,74.57,74.40,75.92)
Coordinates <- as.data.frame(cbind(latitude,longitude))
rownames(Coordinates) <- c("Kolhapur","Latur","Sangli","Satara","Solapur")
Coordinates
Coordinates[city,2]

#ploting points in graphs
# loading the required packages
library(ggplot2)
library(ggmap)

# creating a sample data.frame with your lat/lon points
lon <- c(74.22,76.58,74.57,74.00,75.89)
lat <- c(16.70,18.41,16.86,17.70,17.67)
df <- as.data.frame(cbind(lon,lat))
df
city <- AgroMarketForWheat("Latur", 10, 1900, 2950)$Place
city
lat <- Coordinates[city,1]
lon <- Coordinates[city,2]

lat
lon
# getting the map
mapgilbert <- get_map(location = c(lon, lat), zoom = 8,
                      maptype = "terrain", scale = 2)
ggmap(mapgilbert)
ggmap(mapgilbert) +
  geom_point(data = Coordinates, aes(x = lon, y = lat, fill = "red", alpha = 0.8), size = 5, shape = 21) +
  guides(fill=FALSE, alpha=FALSE, size=FALSE)




AgroMarket("Latur", 10, 1900, 2950)
x <- list()
y <- list()

fun <- function(){
  for(i in 1:5){
    x <- c(x,i)
    y <- c(y,i)
  }
  z <- as.data.frame(cbind(x,y))
  return(z)
}

z <- fun()
z
