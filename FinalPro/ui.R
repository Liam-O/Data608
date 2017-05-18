library(shinydashboard)
library(leaflet)
library(plotly)

#Get indicator options
indicators <- read.csv("https://raw.githubusercontent.com/Liam-O/Data608/master/FinalPro/nyc_indicators.csv",
                       stringsAsFactors = FALSE)
boroughs <- c("New York", "Kings", "Richmond", "Bronx", "Queens")

header <- dashboardHeader(title = "NYC Real Estate")

body <- dashboardBody(
    fluidRow(
        br(),
        column(width = 9,
               box(width = NULL, solidHeader = TRUE,
                   leafletOutput("nycMap", height = 750)
                   )
               ),
        column(width = 3,
               box(width = NULL,
                   selectInput("boro", "NYC Borough",
                               choices = c("All", sort(boroughs)),
                               selected = "All"),
                   selectInput("indic", "Real Estate Indicators",
                               choices = sort(indicators$Indicator.Code),
                               selected = "All Homes"),
                   selectInput("stat", "Statistic",
                               choices = (c("min", "max", "mean", "median")),
                               selected = "max"),
                   selectInput("span", "Years Spanned",
                               choices = c(1, 2, 4, 8),
                               selected = 8)
               ),
               box(width = NULL,
                   plotlyOutput("plot")
               )
        )

    )
)

dashboardPage(
    header,
    dashboardSidebar(disable = TRUE),
    body
)