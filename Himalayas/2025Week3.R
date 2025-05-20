library(ggplot2)
library(tidytuesdayR)

#set theme minimal
theme_set(theme_minimal())

#Load the data
tuesdata <- tidytuesdayR::tt_load(2025, week = 3)

exped <- tuesdata$exped_tidy
peaks <- tuesdata$peaks_tidy

peaks$PSTATUS <- as.factor(peaks$PSTATUS)

#
ggplot(peaks, aes(x = HIMAL_FACTOR, fill = PSTATUS)) +
  geom_bar(position = "fill") +
  labs(x = "Mountain Range",
       y = "Proportion of Climbing Status",
       title = "Proportion of Climbing Status across Different Mountain Ranges")+coord_flip() 


#Which mountain range has the highest average peak height?
peaks %>%
  group_by(HIMAL_FACTOR) %>%
  summarize(mean_height = mean(HEIGHTM)) %>% 
  arrange(desc(mean_height))

ggplot(peaks, aes(x = HIMAL_FACTOR, y = HEIGHTM)) +
  geom_boxplot() +
  labs(x = "Mountain Range",
       y = "Height (m)",
       title = "Height Distribution of Peaks across Different Mountain Ranges")+coord_flip()

ggplot(peaks, aes(x = HEIGHTM, color = OPEN)) +
  geom_density() + ggtitle("Density of Peak Heights by Open Status")
  
# Which climbing routes (ROUTE1, ROUTE2, ROUTE3, ROUTE4) have the
# highest success rates (SUCCESS1, SUCCESS2, SUCCESS3, SUCCESS4) 
# across all expeditions?
success_rates <- exped %>%
  summarise(
    Route1_Success_Rate = mean(SUCCESS1),
    Route2_Success_Rate = mean(SUCCESS2),
    Route3_Success_Rate = mean(SUCCESS3),
    Route4_Success_Rate = mean(SUCCESS4)
  )

highest_success_route <- success_rates %>%
  gather(key = "Route", value = "Success_Rate") %>% 
  filter(Success_Rate == max(Success_Rate))
  

#How does the use of oxygen affect summit success rates?
exped %>%
  group_by(O2USED) %>%
  summarize(success_rate = mean(SUCCESS1 | SUCCESS2 | SUCCESS3 | SUCCESS4)) %>%
  ggplot(aes(x = O2USED, y = success_rate)) +
  geom_col()

#How often does TERMREASON 4 play a role in termination as compared
#To TERMNREASON 10?
exped %>% 
  group_by(TERMREASON) %>% 
  summarize(count = n()) %>%
  filter(TERMREASON %in% c(4, 10)) %>%
  ggplot(aes(x = TERMREASON, y = count)) + geom_col() + scale_x_discrete(
    labels = c("Bad Weather" = "4", "Technical Difficulty" = "10")
  )
