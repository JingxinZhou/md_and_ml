library(tidyverse)
library(lubridate)
library(tibbletime)
############################### Question 3: Explore More ####################################
## Question 3.1
# Load data ---------------------------
taxi_data <- readr::read_csv('~/Desktop/joined_taxi_data.csv')


## Question 3.2
# For each driver, identified by their anonymized hack_license,
# and for each of the 24 hours of the day on August 15, 2013, compute: ---------------------------
taxi_data <- taxi_data %>% mutate(hour_pick=hour(pickup_datetime))

#• total_passengers_picked_up: the number of passengers picked up during that hour.
tibble_1 <- taxi_data %>%
  group_by(hack_license, hour_pick) %>%
  summarise(total_hour = sum(passenger_count)) 

#• trips_started: the total number of trips started during the hour.
tibble_2<- taxi_data %>%
  group_by(hack_license, hour_pick) %>%
  summarise(trips_started=n())

#• total_time_with_passengers: total amount of time with passengers in the car during that hour.  
taxi_hour <- crossing(taxi_data, hour = c(0:23)) 
taxi_hour <- taxi_hour %>% 
  #regard those drop off on 8/16 as "2013-08-16 00:00:00" because we only consider the time period on 8/15
  mutate(dropoff_datetime=case_when(day(dropoff_datetime)==15~dropoff_datetime,
                                    day(dropoff_datetime)==16~as.POSIXct("2013-08-16 00:00:00")))

tibble_3 <- taxi_hour %>% filter(hour == hour(pickup_datetime)) %>% 
         # calculate time(in seconds) in each trip with passengers in the car during that hour
  mutate(time_in_corresponding_hour = pmin(dropoff_datetime - pickup_datetime, 
                                           floor_date(pickup_datetime + 3600, unit = "hour") - pickup_datetime),
         # miles_hour: number of miles traveled with passengers in the hour in one trip.
         mile_hour = as.numeric(trip_distance/trip_time_in_secs*time_in_corresponding_hour),
         # earnings: the amount of money the driver earned (fare plus tip) in that hour in one trip.
         earnings_hour = as.numeric(total_amount/trip_time_in_secs*time_in_corresponding_hour)) %>%
  group_by(hack_license, hour) %>%
          # calculate the total time with passengers for each car
  summarise(total_time_passenger = sum(time_in_corresponding_hour),
            #• calculate the total time with passengers for each car
            total_mile_hour= sum(mile_hour),
            #• miles_with_passengers: total number of miles traveled with passengers in the hour.
            total_earnings_hour = sum(earnings_hour)) %>% 
            #• earnings: the total amount of money the driver earned (fare + tip) in that hour.
   select(hack_license, hour, total_time_passenger, total_mile_hour, total_earnings_hour)

joined_tibble <- left_join(tibble_3,tibble_1, by = c("hack_license"="hack_license","hour"="hour_pick"))
joined_tibble <- left_join(joined_tibble,tibble_2, by = c("hack_license"="hack_license","hour"="hour_pick"))

# Write taxi_data to your data/ folder ---------------------------
write.csv(joined_tibble,"~/Desktop/explore_more_data.csv", row.names = FALSE)
























