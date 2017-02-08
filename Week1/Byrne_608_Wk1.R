# Liam Byrne
# Data 608
# Week 1

library(ggplot2)
library(dplyr)

data_URL <- "https://raw.githubusercontent.com/charleyferrari/CUNY_DATA608/master/lecture1/Data/inc5000_data.csv"

inc5000 <- read.csv(data_URL, stringsAsFactors = FALSE)

#--------------+++++++--------------
# 1. Create a graph that shows the distribution of companies in the dataset by State
# (ie how many are in each state). There are a lot of States, so consider which axis you
# should use assuming I am using a 'portrait' oriented screen (ie taller than wide).

png("Figure1.png")
ggplot(inc5000, aes(factor(x = State))) +
    geom_bar() + xlab("State") + coord_flip() +
    ggtitle("States with the Fastest Growing Companies")
dev.off()

#--------------+++++++--------------
# 2. Let's dig in on the State with the 3 rd most companies in the data set. Imagine you
# work for the state and are interested in how many people are employed by
# companies in different industries employ. Create a plot of average employment by
# industry for companies in this state (only use cases with full data (user R's
# complete.cases() function). Your graph should show how variable the ranges are,
# and exclude outliers.

# Use dplyr to count states and rank their appearance in the data. Ranking allows for ties.
state_rank <- inc5000 %>%
    count(State) %>%
    mutate(n_rank = dense_rank(-n)) %>%
    arrange(n_rank)

# Isolate 3rd ranked state(s). If there is a tie, all 3rd ranked states will be observed
state3 <- subset(state_rank, n_rank == 3, select = State)
inc_state3 <- subset(inc5000, complete.cases(inc5000) & State %in% state3)

# The Employee axis will be limited by a 95% quantile to remove most of the outliers.
png("Figure2.png")
ggplot(inc_state3, aes(factor(x = reorder(Industry, Employees, FUN = median)), y = Employees)) +
    geom_boxplot(outlier.shape = NA) +
    ylim(c(0,quantile(inc_state3$Employees, probs = 0.95))) +
    coord_flip() +
    xlab("Industry") +
    ggtitle("Employee Distribution per Industry")
dev.off()

#--------------+++++++--------------
# 3. Now imagine you work for an investor and want to see which industries generate the
# most revenue per employee. Create a chart makes this information clear.

inc_complete <- subset(inc5000, complete.cases(inc5000)) %>%
    mutate(rev_per_empl = Revenue/Employees)

png("Figure3.png")
ggplot(inc_complete, aes(factor(x = reorder(Industry, rev_per_empl, FUN = median)))) +
    stat_summary_bin(aes(y = rev_per_empl), fun.y = "median", geom = "bar") +
    coord_flip() +
    xlab("Industry") + ylab("Median Revenue/Employee") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
dev.off()