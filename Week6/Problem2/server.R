#Liam Byrne
#Data 608
#Week 6 - Problem 2

###!!!__Run to -SERVER- before launch__!!!###
library(shiny)
library(dplyr)
library(plotly)
library(ggplot2)

mortal_url <- "https://raw.githubusercontent.com/charleyferrari/CUNY_DATA608/master/lecture3/
data/cleaned-cdc-mortality-1999-2010-2.csv"

# Recalculate Crude.Rate to increase the floating point for better comparisons
mortal <- read.csv(mortal_url, stringsAsFactors = FALSE) %>%
    select(-Crude.Rate) %>%
    mutate("Crude.Rate" = Deaths/Population*1e5)

# DF to store rates on a national level
us_avg <- mortal %>%
    group_by(ICD.Chapter, Year) %>%
    summarise(State = "Nation", Deaths = sum(Deaths), Population = sum(Population),
              Crude.Rate = Deaths/Population*1e5) %>%
    select(ICD.Chapter, State, Year, Deaths, Population, Crude.Rate)

mortal <- rbind(mortal, as.data.frame(us_avg))

###---SERVER---###
shinyServer(function(input, output, session) {
    selectedData <- reactive({
        mort_slice <- mortal %>%
            filter(ICD.Chapter == input$cause)
    })

    output$plot1 <- renderPlotly({
        #Focus State DF
        foc <- selectedData() %>%
            filter(ICD.Chapter == input$cause &
                       (State == input$focus))

        #Comparitive State DF
        comp <- selectedData() %>%
            filter(ICD.Chapter == input$cause &
                       (State == input$compare)) %>%
            select(Year, Crude.Rate)

        foc <- foc %>%
            left_join(comp, by = "Year") %>%
            filter(!is.na(Crude.Rate.y)) %>%
            mutate(cr_diff = (Crude.Rate.x-Crude.Rate.y)/Crude.Rate.x*100,
                   color = ifelse(cr_diff >0, "rgba(255,0,0,.8", "rgba0,255,0,.8"),
                   name = ifelse(cr_diff>= 0,
                                 paste(input$focus, "has a higher crude rate than",
                                       ifelse(input$compare == "Nation", "the", ""),
                                       input$compare),
                                 paste(input$focus, "has a lower crude rate than",
                                       ifelse(input$compare == "Nation", "the", ""),
                                       input$compare)))

        plot_ly(x = foc$Year,
                y = foc$cr_diff,
                type = "bar",
                name = foc$name,
                color = foc$name) %>%
            layout(title = paste("Relative Difference in Crude Rate for",
                                 input$focus, "and", input$compare),
                   xaxis = list(title = "Year"),
                   yaxis = list(title = "Rel Diff Crude Rate (%)"),
                   showlegend = TRUE)
        })
})