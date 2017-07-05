library(shinydashboard)
library(leaflet)
library(dplyr)


latitude <- c(16.70,18.40,16.87,16.95,17.68)
longitude <- c(74.22,76.58,74.57,74.40,75.92)
Coordinates <- as.data.frame(cbind(latitude,longitude))
rownames(Coordinates) <- c("Kolhapur","Latur","Sangli","Satara","Solapur")
Coordinates



# generate base leaflet map
output$map <- renderLeaflet({
  map <- leaflet() %>% addTiles()
  map
})

TransportRate <- 5
min <- 1900
max <- 3000


function(input, output){
  
  output$selectedCity <- renderUI({
    plant <- input$cropProducts
    if(grepl(plant,"Wheat")){
      leafletProxy('outputmap') %>% addMarkers(lon=Coordinates[input$cities,2],lat=Coordinates[input$cities,1])
      
      AgroMarketForWheat <- function(HomePlace, Quantity, MinPrice, MaxPrice){
        maxProfit <- 0
        place <- ""
        result <- list()
        for(iterator in 1:5){
          price <- DataMatrixforWheat[iterator, 1] + DataMatrixforWheat[iterator, 2] * Quantity + DataMatrixforWheat[iterator, 3] * MinPrice + DataMatrixforWheat[iterator, 4] * MaxPrice 
          distance <- distance2Points(HomePlace, rownames(DataMatrixforWheat)[iterator])
          amount <- price * Quantity - distance * TransportRate
          leafletProxy('outputmap') %>% addMarkers(lon=Coordinates[rownames(DataMatrixforWheat)[iterator],2],lat=Coordinates[rownames(DataMatrixforWheat)[iterator],1], popup = amount)
          if(maxProfit < amount){
            maxProfit <- amount
            place <- rownames(DataMatrixforWheat)[iterator]
          }
        }
        result[['Place']] <- place
        result[['MaxProfit']] <- maxProfit
      }
      
    }
  })
}


leafletProxy('outputmap') %>% addMarkers(lon=Coordinates[AgroMarketForWheat(input$cities,input$quantity,min,max),2],lat=Coordinates[AgroMarketForWheat(input$cities,input$quantity,min,max),1])
