library(hexbin)
library(dplyr)
library(tidyr)
library(leaflet)
library(rgdal)
library(plotly)

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

indicators <- read.csv("https://raw.githubusercontent.com/Liam-O/Data608/master/FinalPro/nyc_indicators.csv",
                       stringsAsFactors = FALSE)

#Remove indicators that do not appear in dataset
indicators <- indicators %>%
    filter(indicators$Indicator.Code %in% unique(nyc_data$indicator))

#Set view for map
limits <- bbox(nyc_geo)

function(input, output, session) {

    #Check to see if subset data has data and display message if not
    check_data <- function(x) {
        if (all(is.na(x))) {
            showModal(modalDialog(
                title = "Invalid Entry",
                "No data available for current parameters.")
                )
            FALSE
        }
        else NULL
    }

    #Get plot data from click event
    getPlotData <- reactive({

        hood <- click_data$clickedShape

        if(is.null(hood)) {
            hood <- 272816
            }

        nyc_plot <- nyc_data %>%
            filter((indicator == input$indic) &
                       (RegionID == hood)
            )
        click_data$clickedShape <- hood

        nyc_plot
    })

    #Subset data based on input
    getSubData <- reactive({
        #group data by respective paramters
        nyc_dynam <- nyc_data %>%
            filter((indicator == input$indic) &
                       (as.numeric(format(Date, "%Y")) >= (2017 - (as.numeric(input$span) - 1)))
            )

        if (input$boro != "All") {
            nyc_dynam <- nyc_dynam %>%
                filter(Borough == input$boro)
        }

        #group by neighborhood
        nyc_dynam <- group_by(nyc_dynam, RegionID)

        #Use respective statistic on grouped data
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

        #Round value for display purposes
        nyc_dynam$val <- round(nyc_dynam$val, 2)

        #Join subsetted data with map object by common key, ReigonID
        nyc_map <- nyc_geo
        nyc_map@data <- left_join(nyc_map@data, nyc_dynam, by = "RegionID")

        #return subset data
        nyc_map
    })

    # Render map on run and set origin view
    output$nycMap <- renderLeaflet({
        leaflet() %>%
            addProviderTiles("CartoDB.Positron") %>%
            setView(lng = mean(limits[1,]), lat = mean(limits[2,]), zoom = 10)
        })

    #Setup methods to catch what shape is clicked on
    click_data <- reactiveValues(clickedShape = NULL)
    observeEvent(input$nycMap_shape_click, {
        click_data$clickedShape <- input$nycMap_shape_click$id
    })

    #Display plot of values on click event
    output$plot <- renderPlotly({

        plot_ly(getPlotData(), x = ~Date, y = ~value,
                type = "scatter", mode = "lines") %>%
            layout(title = paste0(input$indic, "<br>for ",
                                nyc_geo$Neighborhood[nyc_geo$RegionID == click_data$clickedShape][1],
                ", ", nyc_geo$Borough[nyc_geo$RegionID == click_data$clickedShape][1])
                )
        })

    observe({
        data_render <- getSubData()

        #create color pallete for map
        # Check that there is data for parameters
        validate(
            check_data(data_render@data$val)
        )

        #Create color pallete, binned on quantiles
        pal <- colorBin("RdYlBu",
                            domain = data_render@data$val,
                            bins = quantile(data_render@data$val, probs = seq(0, 1, .1), na.rm = TRUE),
                            reverse = TRUE,
                            na.color = "#bdbdbd")

        #Pop-up
        indic_symbol <- indicators$Indicator.Symbol[input$indic == indicators$Indicator.Code]
        nyc_pop <- paste0("<strong>", input$indic, "</strong><br>",
                          "<strong> Neighborhood: </strong>",
                          data_render$Neighborhood, ", ", data_render$Borough, "<br>",
                          "<strong>", input$span, " Year(s) ", input$stat, ":</strong> ",
                          ifelse(indic_symbol == "$", indic_symbol,""),
                          data_render$val, ifelse(indic_symbol == "%", indic_symbol, "")
                          )

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
                        layerId = data_render$RegionID,
                        popup = nyc_pop) %>%
            clearControls() %>%
            addLegend(pal = pal, values = data_render@data$val,
                      position = "bottomleft",
                      title = paste0(input$indic, ifelse(indic_symbol %in% c("$", "%"),
                                                         paste0(" (In ", indic_symbol, ")"), "")
                                     )
                      )
    })
}