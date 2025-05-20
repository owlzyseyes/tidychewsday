library(tidyverse)
library(tidytuesdayR)
library(gganimate)
library(ggridges)
library(ggthemes)
library(gifski)

# Load the data
tt <- tt_load("2025-01-21")
exped_tidy <- tt$exped_tidy
peaks_tidy <- tt$peaks_tidy


#How have success rates changed over time?
# Define success: If any route (SUCCESS1 to SUCCESS4) is TRUE, count as a successful expedition
exped_tidy <- exped_tidy%>%
  mutate(success_binary = ifelse(rowSums(select(., starts_with("SUCCESS"))) > 0, 1, 0))

#I am interested in the number of expeditions by Principal Nationality (NATION)
#Curious to see which Nations have the most expeditions!
expeditions_by_nation <- exped_tidy %>%
  group_by(NATION) %>%
  summarise(
    total_expeditions = n()
  ) %>%
  arrange(desc(total_expeditions)) %>%
  print(expeditions_by_nation)

#Interested in knowing the success rates for the top 5 countries with the most
#expeditions over the years

top_nations <- expeditions_by_nation %>%
  top_n(5, total_expeditions) %>%
  pull(NATION)

#Their success rates over the years
success_rates_by_nation <- exped_tidy %>%
  filter(NATION %in% top_nations) %>%
  group_by(NATION, YEAR) %>%
  summarise(
    total_expeditions = n(),
    successful_expeditions = sum(success_binary),
    success_rate = successful_expeditions / total_expeditions
  )

success_rates_by_nation <- success_rates_by_nation %>%
  mutate(YEAR = as.numeric(YEAR))


#Plotting an animated bar plot with success rates over the years
animated_plot <- ggplot(success_rates_by_nation, aes(x = NATION, y = success_rate, fill = NATION)) +
  geom_bar(stat = "identity") +
  labs(
    title = "Year: {closest_state}",
    subtitle = "Success Rates of the Top 5 Nations from 2020(Y1) to 2024(Y5)",
    caption = "Data Source: Himalayas Expeditions",
    x = "Nation",
    y = "Success Rate",
    fill = "Nation"
  ) +
  theme_bw() +
  transition_states(YEAR, transition_length = 1, state_length = 1, wrap = FALSE) +
  ease_aes('cubic-in-out') +
  theme(
    plot.title = element_text(size = 20, face = "bold"),  # Adjusting title font size
    plot.subtitle = element_text(size = 14),  # Adjusting subtitle font size
    plot.caption = element_text(size = 12, margin = margin(t = 10, b = 20))  # Adjusting caption font size and margin
  )

anim <- animate(animated_plot, renderer = gifski_renderer())
anim_save("success_rates.gif", animation = anim)
