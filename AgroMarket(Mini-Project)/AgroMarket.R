library(shiny)
library(shinydashboard)
library(leaflet)
library(ggmap)
library(XML)
library(RCurl)
distance2Points <- function(origin,destination){
  origin=gsub(" ", "+", origin)
  destination=gsub(" ", "+", destination)
  results <- list();
  xml.url <- paste0('http://maps.googleapis.com/maps/api/distancematrix/xml?origins=',origin,'&destinations=',destination,'&mode=driving&sensor=false')
  xmlfile <- xmlParse(getURL(xml.url))
  dist <- xmlValue(xmlChildren(xpathApply(xmlfile,"//distance")[[1]])$value)
  time <- xmlValue(xmlChildren(xpathApply(xmlfile,"//duration")[[1]])$value)
  distance <- as.numeric(sub(" km","",dist))
  time <- as.numeric(time)/60
  distance <- distance/1000
  #results[['time']] <- time
  results <- distance
  return(results)
}

district <- c("Sangli","Satara","Solapur","Kolhapur")

DataListforWheat <- c(SangliMarketPriceforWheat, SolapurMarketPriceforWheat, SataraMarketPriceforWheat, KolhapurMarketPriceforWheat, LaturMarketPriceforWheat)
DataMatrixforWheat <- matrix(DataListforWheat, byrow = TRUE, nrow = 5)
DataMatrixforWheat

District <- c("Sangli", "Solapur", "Satara", "Kolhapur", "Latur")
Coefficients <- c("intersect", "Coefficient1", "coefficient2", "coefficient3")


rownames(DataMatrixforWheat) <- District
colnames(DataMatrixforWheat) <- Coefficients

DataMatrixforWheat
MarketList <- list()

TransportRate <- 5

AgroMarketForWheat <- function(HomePlace, Quantity, MinPrice, MaxPrice){
  maxProfit <- 0
  place <- ""
  MarketList <- data.frame()
  cityList <- list()
  cityMarket <- list()
  for(iterator in 1:5){
    price <- DataMatrixforWheat[iterator, 1] + DataMatrixforWheat[iterator, 2] * Quantity + DataMatrixforWheat[iterator, 3] * MinPrice + DataMatrixforWheat[iterator, 4] * MaxPrice 
    distance <- distance2Points(HomePlace, rownames(DataMatrixforWheat)[iterator])
    amount <- price * Quantity - distance * TransportRate
    cityList <- c(cityList,rownames(DataMatrixforWheat)[iterator])
    cityMarket <- c(cityMarket,amount)
    
  }
  MarketList <- as.data.frame(cbind(cityList,cityMarket))
  return(MarketList)
}


NetProfitForWheat <- function(HomePlace, Quantity, MinPrice, MaxPrice){
    maxProfit <- 0
    place <- ""
    result <- list()
    for(iterator in 1:5){
      price <- DataMatrixforWheat[iterator, 1] + DataMatrixforWheat[iterator, 2] * Quantity + DataMatrixforWheat[iterator, 3] * MinPrice + DataMatrixforWheat[iterator, 4] * MaxPrice 
      distance <- distance2Points(HomePlace, rownames(DataMatrixforWheat)[iterator])
      amount <- price * Quantity - distance * TransportRate
      print(amount)
      print(distance)
      print(rownames(DataMatrixforWheat)[iterator])
      if(amount > maxProfit){
        maxProfit <- amount
        place <- rownames(DataMatrixforWheat)[iterator]
      }
      
    }
    result[['Place']] <- place
    result[['maxProfit']] <- maxProfit
    return(result)
  }


NetProfitForWheat("Latur",10,1000,3000)

header <- dashboardHeader(
  title = '--AgroMarket--'
)
body <- dashboardBody(
  fluidRow(
    column(width=12,
           leafletOutput('outputmap',height = 550)
    )
  )
)
sidebar <- dashboardSidebar(
  sidebarSearchForm(textId = "origin.val",
                    buttonId = "button_click_count",
                    label = "Home City"),
  
  radioButtons("crops", "Show", 
                          list(
                            "Wheat",
                            "Onion",
                            "Tur"
                          ),
                          selected = NULL,
                          inline = FALSE, 
                          width = NULL
             ),
  textInput("quantity", "Quantity of Product", value = "", width = NULL, placeholder = "Quantity"),
  actionButton("Click", "Click", width = NULL)
             
)


ui <- dashboardPage(
  header,
  sidebar,
  body
)

server <- function(input,output){
  
  # Run Geocode only when button is clicked
  geocode_origin <- eventReactive(input$button_click_count,{
    geocode(input$origin.val)
  })
  
  # generate base leaflet map
  output$outputmap <- renderLeaflet({
    map <- leaflet() %>% addTiles()
    map
  })
  
  
  observeEvent(input$button_click_count,{
    plant <- as.character( substitute(renderText(input$crops)))
    if(grepl("Wheat", as.character(renderText(input$crops)),ignore.case = TRUE)){
  #  priceSheet <- AgroMarketForWheat(format(renderText(input$origin.val)),as.numeric(input$quantity),1900,2900)
     leafletProxy('outputmap')  %>% addMarkers(lng=Coordinates$longitude,lat=Coordinates$latitude)
    }
  })
  
  
}

shinyApp(ui, server)

