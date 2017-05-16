library(shinydashboard)
library(leaflet)
library(DT)

header <- dashboardHeader(title = "New York City Real Estate Indicators")

body <- dashboardBody(
    fluidRow(
        column(width = 9,
               box(width = NULL, solidHeader = TRUE,
               leafletOutput("nycMap", height = 400)
               )
              ),
        column(width = 3,
           box(width = NULL,
               uiOutput("indicSelector"),
               uiOutput("spanSelector"),
               uiOutput("statSelector")
               )
           )
    )
)