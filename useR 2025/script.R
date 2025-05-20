library(tidytuesdayR)
library(tidyverse)
library(tidyr)
library(packcircles)

tuesdata <- tidytuesdayR::tt_load(2025, week = 17)
user2025 <- tuesdata$user2025
write.csv(user2025,"useR2025.csv")
summary(user2025)
# Days and date?

n_distinct(user2025$date) # 4 days of the event

# Q: How many will have video recordings?
user2025 |>
  group_by(video_recording) |> 
  summarise(count = n())
# A: 112 recorded, 11 nein.


# Q: How many will be online/in-person?
user2025$location = case_when(
  user2025$room == "Online" ~ "Online",
  user2025$room == "TBD" ~ "TBD",
  TRUE ~ "In-Person")

user2025 |>
  group_by(location) |> 
  summarise(count = n())
# A: 89 In-person, 31 online, 8 are TBD. 


# Most occurring keywords?
word_frequencies <- user2025 |>
  mutate(words = strsplit(as.character(keywords), ",\\s*")) |>
  unnest(words) |>
  mutate(words = trimws(words)) |> 
  count(words, sort = TRUE)

# Most common
top10_keywords <- head(word_frequencies, 10) |> 
  pull(words)

filtered <- user2025 %>%
  filter(sapply(strsplit(as.character(keywords), ",\\s*"), function(x) {
    any(trimws(x) %in% top10_keywords)
  }))
user2025 |>
  group_by(session) |>
  summarise(count = n()) |> 
  arrange(desc(count))



grouping <- user2025 |> 
  mutate(day = day(date)) |>
  mutate(words = strsplit(as.character(keywords), ",\\s*")) |> 
  unnest(words) |> 
  mutate(words = trimws(words)) |> 
  group_by(day, words) |> 
  summarise(n = n(), .groups = 'drop') |>
  arrange(day, desc(n)) 

events_per_day <- user2025 |>
  mutate(day = day(date)) |> 
  group_by(day) |>
  summarise(events = n(), .groups = "drop")





