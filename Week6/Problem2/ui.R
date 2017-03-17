#Liam Byrne
#Data 608
#Week 6 - Problem 2


###---UI---###
ui <- fluidPage(
    headerPanel('Mortality Data Explorer'),
    sidebarLayout(
        sidebarPanel(
            selectInput('cause', 'Cause of Death', sort(unique(mortal$ICD.Chapter)),
                        selected = sort(unique(mortal$ICD.Chapter))[1]),
            selectInput('focus', "Focal State", c("Nation", sort(unique(mortal$State[mortal$State != "Nation"]))),
                        selected = sort(unique(mortal$State))[1]),
            selectInput('compare', "Compare to", c("Nation", sort(unique(mortal$State[mortal$State != "Nation"]))),
                        selected = "Nation")
        ),
    #checkboxInput("relative", "Relative(On)/Comparitive(Off)"), value = TRUE),
    mainPanel(
        plotlyOutput("plot1")
        )
    )
)
