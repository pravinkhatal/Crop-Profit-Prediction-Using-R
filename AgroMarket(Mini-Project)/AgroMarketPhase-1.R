
## app.R ##
library(shiny)
library(shinydashboard)
library(leaflet)
library(ggmap)
library(XML)
library(RCurl)
library(shiny)
library(shinydashboard)



icon.glyphicon <- makeAwesomeIcon(icon= 'flag', markerColor = 'blue', iconColor = 'black')
icon.fa <- makeAwesomeIcon(icon = 'flag', markerColor = 'red', iconColor = 'black')
icon.ion <- makeAwesomeIcon(icon = 'home', markerColor = 'green')

TransportRate <- 9

NetProfitForOnion <- function(HomePlace, Quantity, MinPrice, MaxPrice){
  maxProfit <- 0
  place <- ""
  result <- list()
  for(iterator in 1:4){
    price <- DataMatrixforOnion[iterator, 1] + DataMatrixforOnion[iterator, 2] * Quantity + DataMatrixforOnion[iterator, 3] * MinPrice + DataMatrixforOnion[iterator, 4] * MaxPrice 
    distance <- distance2Points(HomePlace, rownames(DataMatrixforOnion)[iterator])
    amount <- price * Quantity - distance * TransportRate
    print(amount)
    print(distance)
    print(rownames(DataMatrixforOnion)[iterator])
    if(amount > maxProfit){
      maxProfit <- amount
      place <- rownames(DataMatrixforOnion)[iterator]
    }
    
  }
  result[['Place']] <- place
  result[['maxProfit']] <- maxProfit
  return(result)
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





India_lat <- 20.5937
India_lon <- 78.9629

latitude <- c(16.705,18.407,16.852,17.680,17.659)
longitude <- c(74.243,76.576,74.581,74.018,75.906)
Coordinates <- as.data.frame(cbind(latitude,longitude))
rownames(Coordinates) <- c("Kolhapur", "Latur", "Sangli", "Satara", "Solapur")


Olatitude <- c(17.659,18.5204,19.9975,20.1491)
Olongitude <- c(75.906,73.8567,73.7898,74.2326)
OCoordinates <- as.data.frame(cbind(Olatitude,Olongitude))
rownames(OCoordinates) <- c("Solapur","Pune", "Nasik", "Lasalgaon")



zoomlevel <- 5


ui <- dashboardPage(
  dashboardHeader(
    title = "--AgroMarket--"
  ),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Wheat", tabName = "Wheat", icon = icon("tree")),
      menuItem("Onion", tabName = "Onion", icon = icon("tree")),
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")))
  ),
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "Wheat",
              fluidRow(
                column(width = 3, textInput(inputId = "wcity", label = "Home Place", value = "", width = NULL, placeholder = "Home Place")),
                column(width = 3, numericInput(inputId = "wquantity", label = "Quantity", value = 0, min = 0, max = NA, step = NA,
                                               width = NULL)), br(),
                column(width = 3, actionButton(inputId = "wbutton", label = " Submit ",width = NULL)),
                column(width = 3, actionButton(inputId = "wsbutton", label = " Show ",width = NULL)),br(),
                br(),
                column(width = 12, leafletOutput('outputmap',height = 500)))
      ),
      
      # Second tab content
      tabItem(tabName = "Onion",
              fluidRow(
                column(width = 3, textInput(inputId = "ocity", label = "Home Place", value = "", width = NULL, placeholder = "Home Place")),
                column(width = 3, numericInput(inputId = "oquantity", label = "Quantity", value = 0, min = 0, max = NA, step = NA,
                                               width = NULL)), br(),
                column(width = 3, actionButton(inputId = "obutton", label = " Submit ",width = NULL)),
                column(width = 3, actionButton(inputId = "osbutton", label = " Show ",width = NULL)),br(),br(),
                column(width =12, leafletOutput('outputmap2',height = 500)))
              
              
      ),
      #Third TabContent
      tabItem(tabName = "dashboard",
              fluidRow(tabsetPanel(
                tabPanel("WheatPlot", plotOutput("Wheatplot")), 
                tabPanel("OnionPlot", plotOutput("Onionplot"))
              )
              )
      )
      
      
    )
    
  )
)


server <- function(input, output) { 
  # Frist TabContent
  geocode_origin <- eventReactive(input$wbutton,{
    geocode(input$wcity)
  })
  
  
  output$outputmap <- renderLeaflet({
    map <- leaflet() %>% addTiles()
    map
  })
  
  observeEvent(input$wbutton, {
    leafletProxy('outputmap') %>% setView('outputmap',lng=India_lon,lat=India_lat, zoom = zoomlevel)  %>% addAwesomeMarkers(lng=Coordinates$longitude,lat=Coordinates$latitude, icon = icon.glyphicon)
  })
  
  observeEvent(input$wbutton,{
    v <- geocode_origin()
    leafletProxy('outputmap') %>% setView('outputmap',lng=India_lon,lat=India_lat, zoom = zoomlevel) %>% addAwesomeMarkers(lng=v$lon,lat=v$lat, icon = icon.ion)
  })
  
  
  observeEvent(input$wsbutton, {
    MinPrice <- 1250
    MaxPrice <- 1625
    Marketplace <- NetProfitForWheat(input$wcity,input$wquantity,MinPrice,MaxPrice)$Place
    leafletProxy('outputmap') %>% setView('outputmap',lng=India_lon,lat=India_lat, zoom = zoomlevel) %>% addAwesomeMarkers(lng=Coordinates[Marketplace,2],lat=Coordinates[Marketplace,1], icon = icon.fa,label = as.character(NetProfitForWheat(input$wcity,input$wquantity,MinPrice,MaxPrice)$maxProfit))
    v <- geocode_origin()
    lonCoordinates <- c(lng=v$lon, Coordinates[Marketplace,2])
    latCoordinates <- c(lat=v$lat,Coordinates[Marketplace,1])
    actualCoordinates <- as.data.frame(cbind(lonCoordinates,latCoordinates))
    
    leafletProxy('outputmap') %>% addPolylines(
      data = actualCoordinates,
      lng = ~lonCoordinates, 
      lat = ~latCoordinates,
      weight = 2,
      opacity = 2
    )
  })
  
  
  #Second TabContent
  output$outputmap2 <- renderLeaflet({
    map <- leaflet() %>% addTiles()
    map
  })
  
  
  geocode_origin2 <- eventReactive(input$obutton,{
    geocode(input$ocity)
  })
  
  observeEvent(input$obutton, {
    leafletProxy('outputmap2') %>% setView('outputmap2',lng=India_lon,lat=India_lat, zoom = zoomlevel)  %>% addAwesomeMarkers(lng=OCoordinates$Olongitude,lat=OCoordinates$Olatitude, icon = icon.glyphicon)
  })
  
  observeEvent(input$obutton,{
    v <- geocode_origin2()
    leafletProxy('outputmap2') %>% setView('outputmap2',lng=India_lon,lat=India_lat, zoom = zoomlevel) %>% addAwesomeMarkers(lng=v$lon,lat=v$lat, icon = icon.ion)
  })
  
  
  observeEvent(input$osbutton, {
    MinPrice <- 350
    MaxPrice <- 450
    
    Marketplace <- NetProfitForOnion(input$ocity,input$oquantity,MinPrice,MaxPrice)$Place
    
    leafletProxy('outputmap2') %>% setView('outputmap2',lng=India_lon,lat=India_lat, zoom = zoomlevel) %>% addAwesomeMarkers(lng=OCoordinates[Marketplace,2],lat=OCoordinates[Marketplace,1], icon = icon.fa, label = as.character(NetProfitForOnion(input$ocity,input$oquantity,MinPrice,MaxPrice)$maxProfit))
    v <- geocode_origin2()
    lonCoordinates <- c(lng=v$lon, OCoordinates[Marketplace,2])
    latCoordinates <- c(lat=v$lat, OCoordinates[Marketplace,1])
    actualCoordinates <- as.data.frame(cbind(lonCoordinates,latCoordinates))
    
    leafletProxy('outputmap2') %>% addPolylines(
      data = actualCoordinates,
      lng = ~lonCoordinates, 
      lat = ~latCoordinates,
      weight = 2,
      opacity = 2
    )
  })
  
  output$Wheatplot <- renderPlot({
    
    WheatPlot <- read.csv("WheatPlot.csv")
    WheatPlot
    family <- as.factor(WheatPlot$City)
    col.rainbow <- rainbow(12)
    palette(col.rainbow)
    plot(WheatPlot$Quantity, WheatPlot$ModePrice, pch = 19, col = family)
    
  })
  
  output$Onionplot <- renderPlot({
    
    OnionPlot <- read.csv("OnionPlot.csv")
    WheatPlot
    family <- as.factor(OnionPlot$City)
    col.rainbow <- rainbow(12)
    palette(col.rainbow)
    plot(OnionPlot$Quantity, OnionPlot$ModePrice, pch = 19, col = family, popup = OnionPlot$City)
    
  })
  
}

shinyApp(ui, server)


