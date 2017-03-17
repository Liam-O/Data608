#Liam Byrne
#Data 608
#Week 6 - Problem 1


###---UI---###
fluidPage(
    headerPanel('2010 Mortality Data Explorer'),
    sidebarPanel(
        selectInput('cause', 'Cause of Death', sort(unique(mort_2010$ICD.Chapter)),
                    selected = sort(unique(mort_2010$ICD.Chapter))[1])
        ),
    mainPanel(
        tabsetPanel(
            tabPanel(h4("Map"), htmlOutput("map1")),
            tabPanel(h4("Table"), DT::dataTableOutput("table1")))
        )
)
