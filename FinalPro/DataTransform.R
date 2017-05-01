#Liam Byrne
#Data Wrangling prior to js.D3 assignment

library(dplyr)
library(tidyr)

d_url <- "https://raw.githubusercontent.com/Liam-O/Data608/master/FinalPro/Neighborhood_MedianValuePerSqft_AllHomes.csv"

d_df <- read.csv(d_url)

d_df <- d_df %>%
    gather("Date", "Price", 8:ncol(d_df))

d_df$Date <- gsub("X","", d_df$Date, perl = TRUE)
d_df$Date <- gsub("\\.","-", d_df$Date, perl = TRUE)
d_df$Date <- paste0(d_df$Date , "-01")

d_df$Date <- as.Date(d_df$Date, "%Y-%m-%d")

d_df <- na.omit(d_df)

boroughs_lst <- c("New York", "Kings", "Queens", "Bronx")

d_df <- d_df %>%
    filter(State == "NY" &
               City == "New York" &
               CountyName %in% boroughs_lst &
               Date > as.Date("01/01/2008", "%m/%d/%Y")
    ) %>%
    select(CountyName, RegionName, Date, Price)

write.table(d_df, file = "ppsf_nyc.csv", sep = ",", row.names = FALSE)