library(shinydashboard)
library(leaflet)
library(DT)

#Get indicator options
indicators <- read.csv("https://raw.githubusercontent.com/Liam-O/Data608/master/FinalPro/nyc_indicators.csv",
                       stringsAsFactors = FALSE)
boroughs <- c("New York", "Kings", "Richmond", "Bronx", "Queens")

bootstrapPage(
    tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
    leafletOutput("nycMap", width = "100%", height = "100%"),
    absolutePanel(top = 10, right = 10,
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
                              selected = 1)
    )
)

# header <- dashboardHeader(title = "New York City Real Estate Indicators")
#
# body <- dashboardBody(
#     fluidRow(
#         column(width = 9,
#                box(width = NULL, solidHeader = TRUE,
#                leafletOutput("nycMap", height = 400)
#                )
#               ),
#         column(width = 3,
#            box(width = NULL,
#                uiOutput("indicSelector"),
#                uiOutput("spanSelector"),
#                uiOutput("statSelector")
#                )
#            )
#     )
# )
#
# dashboardPage(
#     header,
#     dashboardSidebar(disable = TRUE),
#     body
# )