# Liam Byrne
# Data 608
# Final Project
# Script for data colleciton using API calls to Quandl

library("dplyr")
library("jsonlite")

#Get Neighbourhood codes from Quadl and clean
hood_codes <- read.csv(file = "http://static.quandl.com/zillow/hood_codes.csv",
                       stringsAsFactors = FALSE)
hood_codes["n_code"] <- sub(".*\\|", "", hood_codes$City.Code, perl = TRUE)
hood_codes$City.Code <- sub("\\|.*", "", hood_codes$City.Code, perl = TRUE)

#Subset codes for only NYC and restructure
b_list <- c("New York", "Queens", "Bronx", "Richmond", "Kings")
hood_codes <- subset(hood_codes, (hood_codes$State == "New York") & (hood_codes$City.Code %in% b_list))
hood_codes <- subset(hood_codes, select = c(Region, City.Code, n_code))
colnames(hood_codes) <- c("Hood", "Borough", "n_code")

#Get indicator options
indicators <- read.csv("https://raw.githubusercontent.com/Liam-O/Data608/master/FinalPro/nyc_indicators.csv",
                       stringsAsFactors = FALSE)

#Neighborhood dataframe to store data
hoods <- data.frame(Neighborhood = character(),
                    Borough = character(),
                    indicator = character(),
                    Date = as.Date(character()),
                    value = as.numeric(character()),
                    stringsAsFactors=FALSE)

#Base url string for calls
base <- "http://www.quandl.com/api/v3/datasets/ZILL/N"

# Loop through API and get all values for all indicators
# Try/catch will continue on neighborhoods who have no data for indicators
# rbind will append data to master data frame
# ~8,000 API calls used ... must sleep to not go over API limit --> run time > 40 minutes
for (i in 1:nrow(hood_codes)) {
    for (j in 1:nrow(indicators)) {
        getJSON <- paste0(base, hood_codes$n_code[i], "_", indicators$Indicator.Name[j],
                          ".json", "?api_key=j3g7GyHyr_zkJNAAv8wN")
        if (!is.null(tryCatch(data_tmp <- fromJSON(getJSON), error = function(e) NULL))) {
            tmp <- data_tmp$dataset$data
            if (!is.null(nrow(tmp))) {
                hoods_tmp <- data.frame(Neighborhood = rep(hood_codes$Hood[i], nrow(tmp)),
                                        Borough = rep(hood_codes$Borough[i], nrow(tmp)),
                                        indicator = rep(indicators$Indicator.Code[j], nrow(tmp)),
                                        Date = as.Date(tmp[,1]),
                                        value = as.numeric(tmp[,2]),
                                        stringsAsFactors=FALSE)
                hoods <- rbind(hoods, hoods_tmp)
            }
        }
        Sys.sleep(0.3)
    }
}

hoods <- subset(hoods, Date > as.Date.character("2009-6-1"))
write.table(hoods, file = "nyc_all.csv",
            sep = ",", row.names = FALSE)