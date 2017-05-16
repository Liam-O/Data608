library(dplyr)
library(tidyr)
library(leaflet)
library(rgdal)

#Shape files needed to be stored locally. To download, go to
#https://github.com/Liam-O/Data608/tree/master/FinalPro/ZillowNeighborhoods-NY

nyc_geo <- readOGR(dsn = "ZillowNeighborhoods-NY", layer = "ZillowNeighborhoods-NY",
                   GDAL1_integer64_policy = TRUE)

#Subset and rearrange object
boroughs <- c("New York", "Kings", "Richmond", "Bronx", "Queens")
nyc_geo <- subset(nyc_geo, City == "New York" & County %in% boroughs)
colnames(nyc_geo@data) <- c("State", "Borough", "City", "Neighborhood", "RegionID")
nyc_geo@data <- as.data.frame(apply(nyc_geo@data, MARGIN = 2, as.character), stringsAsFactors = FALSE)
nyc_geo@data$RegionID <- as.numeric(nyc_geo@data$RegionID)
#Remove duplicates from odd areas
rem_shp <- c(110, 133, 153,177)
nyc_geo<- nyc_geo[!(rownames(nyc_geo@data) %in% rem_shp),]

#Bring in nyc data
nyc_data <- read.csv("https://github.com/Liam-O/Data608/blob/master/FinalPro/nyc_all.csv?raw=true",
                     stringsAsFactors = FALSE)
nyc_data <- left_join(nyc_data, nyc_geo@data, by = c("Borough", "Neighborhood"))
nyc_data <- na.omit(nyc_data)
nyc_data$Date <- as.Date(nyc_data$Date)

#Get indicator options
indicators <- read.csv("https://raw.githubusercontent.com/Liam-O/Data608/master/FinalPro/nyc_indicators.csv",
                       stringsAsFactors = FALSE)
function(input, output, session) {
    # Render map on run
    output$nycMap <- renderLeaflet({
        leaflet() %>%
            addProviderTiles("CartoDB.Positron")
    })

    #Subset data based on input
    getSubData <- reactive({

        nyc_dynam <- nyc_data %>%
            filter((indicator == input$indic) &
                       (as.numeric(format(Date, "%Y")) >= (2017 - (input$span - 1)))) %>%
            group_by(RegionID)

        if(input$stat == "max") {
            nyc_dynam <- nyc_dynam %>%
                summarise(val = max(value))
            }
        else if (input$stat == "min") {
            nyc_dynam <- nyc_dynam %>%
                summarise(val = min(value))
            }
        else if (input$stat == "mean") {
            nyc_dynam <- nyc_dynam %>%
                summarise(val = mean(value))
            }
        else {
            nyc_dynam <- nyc_dynam %>%
                summarise(val = median(value))
        }

        #Join subsetted data with map object by common key, ReigonID

        nyc_map <- nyc_geo
        nyc_map@data <- suppressWarnings(left_join(
            nyc_map@data, nyc_dynam, by = "RegionID"))

        nyc_map
        })



    observe({

        data_render <- getSubData()

        #create color pallete for map
        pal <- colorNumeric("Rdbu", data_render$val, na.color = "#bdbdbd")

        #Pop-up
        nyc_pop <- paste0("<strong>", input$indic, "</strong><br>",
                          "<strong> Neighborhood: </strong>",
                          data_render$Neighborhood, ", ", data_render$Borough, "<br>",
                          "<strong>", input$span, "Year(s)", input$stat, ":</strong>",
                          data_render$val)

        #Render on change
        leafletProxy("nycMap", data = data_render) %>%
            clearShapes() %>%
            addPolygons(data = data_render,
                        fillColor = pal(data_render$val),
                        color = "#bdbdbd",
                        fillOpacity = 0.6,
                        weight = 0.3,
                        smoothFactor = 0.2,
                        highlightOptions = highlightOptions(
                            color = "white", weight = 2,
                            bringToFront = TRUE),
                        popup = nyc_pop) %>%
            addLegend(pal = pal, values = data_render$val,
                      position = "bottomright",
                      labFormat = labelFormat(prefix = "$"))
    })

    # Indicator Selector
    output$indicSelector <- renderUI ({

        indic_lst <- sort(indicators$Indicator.Code)
        selectInput("indic", "Real Estate Indicator",
                    choices = indic_lst, selected = "All Homes")
    })

    # Date Span Selector
    output$spanSelector <- renderUI ({

        span_lst <- c(1, 2, 4, 8)
        selectInput("span", "Year Span",
                    choices = span_lst, selected = 1)
    })

    # Stat Selector
    output$statSelector <- renderUI ({

        stat_lst <- c("min", "max", "mean", "median")
        selectInput("stat", "Statistic",
                    choices = stat_lst, selected = "max")
    })
}