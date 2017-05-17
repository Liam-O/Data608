library('rgdal')
library('leaflet')
library("dplyr")
library("sparklyr")
library("tmap")
library("tmaptools")

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

nyc_dynam <- nyc_data %>%
    filter((indicator == "1 Bedroom") &
               (as.numeric(format(Date, "%Y")) >= (2017 - (1 - 1)))) %>%
    group_by(RegionID) %>%
    summarise(val = max(value))

nyc_map <- nyc_geo
nyc_map@data <- left_join(nyc_map@data, nyc_dynam, by = "RegionID")

pal <- colorNumeric("YlGnBu", nyc_map$val, na.color = "#f7f7f7")
nyc_map@polygons

leaflet(nyc_map) %>%
    addProviderTiles("CartoDB.Positron") %>%
    addPolygons(fillColor = ~pal(val),
                color = "#b2aeae",
                fillOpacity = 0.6,
                weight = 0.3,
                smoothFactor = 0.2,
                highlightOptions = highlightOptions(color = "white", weight = 2,
                                                    bringToFront = TRUE)) %>%
    addLegend(pal = pal,
              values = nyc_map@data$val,
              position = "bottomright",
              labFormat = labelFormat(prefix = "$"))