#Liam Byrne
#Data 608
#Week 6 - Problem 1

##
## OUTPUT:
## -Interactive Map giving rank (Rank 1 means highest mortality rate)
## -Searchable, sortable dataTable
##

#!!!_Run to -SERVER- before launch!!!
library(shiny)
library(googleVis)
library(dplyr)
library(datasets)

mortal_url <- "https://raw.githubusercontent.com/charleyferrari/CUNY_DATA608/master/lecture3/
data/cleaned-cdc-mortality-1999-2010-2.csv"

# Recalculate Crude.Rate to increase the floating point for better comparisons
mort_2010 <- read.csv(mortal_url, stringsAsFactors = FALSE) %>%
    filter(Year == 2010) %>%
    select(-Crude.Rate) %>%
    mutate("Crude.Rate" = Deaths/Population*1e5)

# State info for map plot
#!!! DC is not mapped !!!
states <- data.frame(name = state.name, abbrev = state.abb,
                     center = state.center, stringsAsFactors = FALSE)


###---SERVER---###
shinyServer(function(input, output, session) {

    selectedData <- reactive({
        mort_slice <- mort_2010 %>%
            filter(ICD.Chapter == input$cause)
        })

    output$map1 <- renderGvis(
        gvisGeoChart(
            selectedData() %>%
                mutate(Rank = dense_rank(-Crude.Rate)) %>%
                select(-matches("DC")) %>%
                mutate(State.Name = sapply(State, function(x) states[states$abbrev == x,"name"])
                       ),
            "State.Name", "Rank",
            options=list(region="US",
                         displayMode="regions",
                         resolution="provinces",
                         width=1200, height=800)
            )
        )

    output$table1 <- DT::renderDataTable(
        DT::datatable(selectedData() %>%
                      mutate(
                          State.Name = sapply(State, function(x) states[states$abbrev == x,"name"]),
                          Rank = dense_rank(-Crude.Rate)) %>%
                          select(-State.Name) %>%
                          arrange(desc(-Rank))
        )
    )
})