library(tidyverse)
library(tidycensus)
library(tidytuesdayR)
library(dplyr)


#Load the data
tuesdata <- tidytuesdayR::tt_load(2025, week = 4)
water_insecurity_2022 <- tuesdata$water_insecurity_2022
water_insecurity_2023 <- tuesdata$water_insecurity_2023

water_insecurity_2022
