library(tidyverse)
library(lubridate)
library(tibbletime)
library(ggplot2)
library(Hmisc)
############################### Question 4: Visualize ####################################
## Question 4.1
# Load data ---------------------------
explore_data <- readr::read_csv('~/Desktop/explore_data.csv')

# Create the following two scatter plots ---------------------------
# • total_trips on the x-axis and total_earnings on the y-axis.
plot_1 <- ggplot(data = explore_data) + 
  geom_point(mapping = aes(x = total_trips, y = total_earnings), colour = "steelblue") +
  labs(title = "scatter plot of total_trips and total_earnings",
       x = "total_trips(times)",
       y = "total_earnings(dollors)") 

# • total_distance on the x-axis and total_earnings on the y-axis.
plot_2 <- ggplot(data = explore_data) + 
  geom_point(mapping = aes(x = total_distance, y = total_earnings), colour = "steelblue") +
  labs(title = "scatter plot of total_distance and total_earnings",
       x = "total_distance(miles)",
       y = "total_earnings(dollors)") 

## Question 4.2
# Save these plots in the figures/ directory as trips.png and distance.png respectively.
png(file="~/Desktop/trips.png")
plot_1
dev.off()

png(file="~/Desktop/distance.png")
plot_2
dev.off()

## Question 4.3
# Load data ---------------------------
explore_more_data <- readr::read_csv('~/Desktop/explore_more_data.csv')
# separate the hour into 4 different slots
explore_more_data <- explore_more_data %>% 
  mutate(time_slot = cut2(hour, c(0,5,11,17,23)))
#as.numeric can change the slot into threshold values

plot_3 <- ggplot(data = explore_more_data) + 
  geom_point(mapping = aes(x = total_time_passenger, y = total_earnings_hour), colour = "steelblue") +
  facet_wrap(~ time_slot, nrow = 2)
  labs(title = "scatter plot of total_time_passenger and total_earnings_hour for different time slots",
       x = "total_time_passenger(times)",
       y = "total_earnings_hour(dollors)") 

png(file="~/Desktop/my_plot.png")
plot_3
dev.off()












