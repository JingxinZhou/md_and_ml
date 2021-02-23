library(tidyverse)
library(lubridate)
############################### Question 2: Explore ####################################
## Question 2.2
# Load data ---------------------------
taxi_data <- readr::read_csv('~/Desktop/joined_taxi_data.csv')


## Question 2.3
# For each taxicab, compute the following for trips started on August 15, 2013 ---------------------------
#• total_trips: the total number of trips.
taxi_data <- taxi_data %>% 
  group_by(medallion) %>% 
  mutate(total_trips = n())
  
#• total_passengers: the total number of passengers.
taxi_data <-taxi_data %>% 
  group_by(medallion) %>% 
  mutate(total_passengers = sum(passenger_count)) 

#• total_time_with_passengers: the total time spent carrying passengers.
taxi_data <-taxi_data %>% 
  group_by(medallion) %>% 
  mutate(total_time_with_passengers = sum(trip_time_in_secs)) 

#• total_distance: the total distance traveled.
taxi_data <-taxi_data %>% 
  group_by(medallion) %>% 
  mutate(total_distance = sum(trip_distance)) 

#• total_earnings: the total amount of money earned.
taxi_data <-taxi_data %>% 
  group_by(medallion) %>% 
  mutate(total_earnings = sum(total_amount)) 


join_tibble <- distinct(select(taxi_data, medallion, total_trips, total_passengers, total_time_with_passengers,
                             total_distance, total_earnings))

# Write taxi_data to your data/ folder ---------------------------
write.csv(join_tibble,"~/Desktop/explore_data.csv", row.names = FALSE)

