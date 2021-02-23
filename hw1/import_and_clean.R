library(tidyverse)
library(lubridate)
###############################Question 1: Import and Clean####################################
## Question 1.3
# Load data ---------------------------
trips <- readr::read_csv('~/Desktop/trip_data_8.csv')
fares <- readr::read_csv('~/Desktop/trip_fare_8.csv')


## Question 1.4
# filter trips and fares ---------------------------
trips <- filter(trips, pickup_datetime >= as_datetime("2013-8-15 00:00:00") & pickup_datetime<= as_datetime("2013-08-15 23:59:59"))
fares <- filter(fares, pickup_datetime >= as_datetime("2013-8-15 00:00:00") & pickup_datetime<= as_datetime("2013-08-15 23:59:59"))
# or use trips[grep('2013-08-15',trips$pickup_datetime),]

## Question 1.5

# 1. remove the trips with impossible coordinates ---------------------------
trips <- filter(trips, pickup_latitude != 0 & pickup_longitude != 0 
                & dropoff_latitude != 0 & dropoff_longitude != 0)

# 2. modify trips to only include realistic distances ---------------------------
trips_cleaned <- trips %>% 
  mutate(trip_time_in_mins = trip_time_in_secs/60,
         trip_time_in_hours = trip_time_in_mins/60,
         avg_speed = trip_distance/trip_time_in_hours) %>% 
  filter(trip_distance < 50 & trip_distance > 0) %>% 
  filter(avg_speed > 0 & avg_speed < 100) %>% 
  filter(trip_time_in_secs >60 & trip_time_in_hours < 8)

# remain reasonable fares
fares_cleaned <- fares %>% 
  filter(fare_amount < 300 & fare_amount > 0) %>% 
  filter(surcharge >= 0 & surcharge <10) %>% 
  filter(mta_tax >= 0) %>% 
  filter(tip_amount >= 0) %>% 
  filter(tolls_amount >= 0 & tolls_amount <= 30) %>% 
  distinct( medallion, hack_license, pickup_datetime, .keep_all= TRUE) # remove the duplicate trips

# Check whether there are duplicate trips
fares_cleaned %>% count(medallion, hack_license, pickup_datetime) %>% 
  filter(n>1)

fares_cleaned <- fares_cleaned %>% 
  pivot_wider(
     names_from = payment_type,
     names_glue = "{payment_type}_{.value}",
     values_from = c(fare_amount, surcharge,mta_tax,tip_amount,tolls_amount,total_amount)
) 


## Question 1.6
# Join cleaned trips and fares tibbles together ---------------------------
taxi_data <- left_join(trips_cleaned, fares_cleaned,
                       by = c("medallion" = "medallion",
                              "hack_license" = "hack_license",
                              "pickup_datetime" = "pickup_datetime"))


## Question 1.7
# Write taxi_data to your data/ folder ---------------------------
write.csv(taxi_data,"~/Desktop/taxi_data.csv", row.names = FALSE)



