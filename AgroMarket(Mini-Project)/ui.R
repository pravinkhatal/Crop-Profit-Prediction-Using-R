library(shinydashboard)
library(leaflet)

header <- dashboardHeader(
  title = " --AgroMarket-- "
)

body <- dashboardBody(
  fluidRow(column(width = 9,
                  box(width = NULL, solidHeader = TRUE,
                      leafletOutput("map", height = 500)
                  )
  ),
  column(width = 3,
         box(width = NULL, status = "warning",
             checkboxGroupInput("cropProducts", "Show",
                                choices = c(
                                  Arhar (Tur) = 3,
                                  Wheat = 1,
                                  Onion = 2
                                ),
                                selected = c(1, 2, 3)
             ),
             p(
               class = "text-muted",
               paste("Note: A farmer has different Markets for thier crop",
                     "with a different pricesheet. Only the maxmimum pricesheet Market will",
                     "be displayed on the map."
               )
             )
         ),
         box(width = NULL, status = "warning",
             selectInput("cities", "Select City",
                         choices = c(
                           "Sangli" = 1,
                           "Solapur" = 2,
                           "Satara" = 3,
                           "Latur" = 4,
                           "Kolhapur" = 5
                         ),
                         selected = "1"
             ),
             textInput("quantity","Total Quantity"),
             uiOutput("selectedCity"),
              actionButton("marketprice","Market Price")

         )
    )
  )
)
