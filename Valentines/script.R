#Analysing Valentines data from TidyTuesday
library(tidyverse)
library(tidytuesdayR)
library(treemap)

#Load the data
tuesdata <- tidytuesdayR::tt_load('2024-02-13')

historical_spending <- tuesdata$historical_spending
gifts_age <- tuesdata$gifts_age
gifts_gender <- tuesdata$gifts_gender
str(historical_spending)
str(gifts_gender)
str(gifts_age)

#Investigating the proportion of those celebrating as years go by
ggplot(historical_spending, aes(x=Year, y=PercentCelebrating, fill = PerPerson)) +
  geom_line()

