# Liam Byrne
# Data 608 - Week 3
# Assignment 2

library("ggplot2")
library("bigvis")
library("dplyr")
library("ggthemes")
library("readr")

rep_url <- "https://github.com/Liam-O/Data608/blob/master/Week3/NYC_pluto_all.csv?raw=true"
p_data <- read_csv(rep_url)


# ***---Problem 1---***
# 1. After a few building collapses, the City of New York is going to begin investigating older
# buildings for safety. However, the city has a limited number of inspectors, and wants to find
# a 'cut-off' date before most city buildings were constructed. Build a graph to help the city
# determine when most buildings were constructed. Is there anything in the results that causes
# you to question the accuracy of the data? (note: only look at buildings built since 1850)?

p_data <- p_data %>%
    filter(YearBuilt >= 1850, NumFloors != 0) %>%
    select(Borough, YearBuilt, LotArea, NumFloors, AssessTot, AssessLand)

p_data <- subset(p_data, complete.cases(p_data))

yr_built <- with(p_data, condense(bin(YearBuilt,
                              find_width(YearBuilt, round((2017-1850)/5)))))

png('Byrne_Liam_HW2_prob1.png')

ggplot(yr_built, aes(x = YearBuilt, y = .count)) + geom_line() +
    geom_vline(xintercept = yr_built[which.min(abs(cumsum(yr_built[,2]) - sum(yr_built[,2])/2)),1][[1]],
               linetype = 2, color = "red") +
    geom_text(aes(x = yr_built[which.min(abs(cumsum(yr_built[,2]) - sum(yr_built[,2])/2)),1][[1]]-5,
                  y = 125000, color = "red", angle = 90),
                  label = yr_built[which.min(abs(cumsum(yr_built[,2]) - sum(yr_built[,2])/2)),1][[1]],
              show.legend = FALSE)
dev.off()

    # Half of the existing buildings in NYC were built before 1932, as shown by the vertlical
    # "cut-off" line. The rate of building appears to go through a continious boom - bust cycle
    # during every binned year (5 years), which is unusual. Boom - bust cycles in construction are
    # not as predictable andthere may be some issue with the data because of this.


# ***---Problem 2---***
# 2. The city is particularly worried about buildings that were unusually tall when they were
# built, since best-practices for safety hadn't yet been determined. Create a graph that shows
# how many buildings of a certain number of floors were built in each year (note: you may want
# to use a log scale for the number of buildings). It should be clear when 20-story buildings,
# 30-story buildings, and 40-story buildings were first built in large numbers.

floors <- with(p_data, condense(bin(YearBuilt,find_width(YearBuilt, 2017-1850)),
                                bin(NumFloors,find_width(NumFloors, max(floors[,2])))))

png('Byrne_Liam_HW2_prob2.png')

ggplot(floors, aes(x = YearBuilt, y = NumFloors, fill = .count)) +
    geom_raster() + scale_fill_gradient(low = "white", high = "red", trans = 'log10') +
    scale_x_continuous(breaks = seq(1850,2017,10), limits = c(1850, 2017)) +
    scale_y_continuous(breaks = seq(0,130,10), limits = c(0,80)) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

dev.off()

    # The count color gradient is log10 transformed. Essentially, where there is pink to red
    # there is a significant frequency of construciton of x-floored structures. Using this method
    # to distinguish when 20, 30 and 40-story building became common. 20-story buildings became
    # common after 1900; 30-story buildings became common after 1915; 40-story buildings became
    # common after 1925.


# ***---Problem 3---***
# 3. Your boss suspects that buildings constructed during the US's involvement in World War
# II (1941-1945) are more poorly constructed than those before and after the way due to the
# high cost of materials during those years. She thinks that, if you calculate assessed value
# per floor, you will see lower values for buildings at that time vs before or after. Construct a
# chart/graph to see if she's right

p_data <- p_data %>%
    mutate(AssessFloor = round((p_data$AssessTot - p_data$AssessLand)/p_data$NumFloors)) %>%
    filter(AssessFloor > 0)

assess <- with(p_data, condense(bin(YearBuilt, find_width(YearBuilt, 2017-1850)),
                                bin(AssessFloor, find_width(AssessFloor, 1e4))))
png('Byrne_Liam_HW2_prob3.png')

ggplot(peel(assess), aes(x = YearBuilt, y = AssessFloor, fill = .count)) +
    geom_raster() + scale_fill_gradient(low = "white", high = "red", trans = 'log10') +
    scale_x_continuous(breaks = seq(1850,2017,10), limits = c(1850, 2017)) +
    geom_vline(xintercept = c(1941,1945), linetype = 4, color = "blue", size = 1, alpha = .2) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

dev.off()

    # My boss would appear to be right in regards to the assessed value per floor being affected
    # by WWII. The area bounded by the dashed blue lines is the time-frame of WWII, 1941 - 1945.
    # There is a noticable void of red-hued counts for the data in this boundary relative to that
    # outside of it. There appears to be other periods of time where the assessed value per floor
    # is low, namely the period during the Early 1980's recession.