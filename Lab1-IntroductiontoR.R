# Lab 1 - Introduction to R
 
# 1. Create a new project associated with this class. Go to File-->New Project-->New Directory-->New Project, then choose a subdirectory
# and call your project md_and_ml

# 2. Put this script in the newly created md_and_ml folder

# 3. Find the NYC Open Data Portal, and the 'DOHMH New York City Restaurant Inspection Results' dataset. 
# a) how many rows and columns does this dataset have?
# b) download this dataset and put it in the md_and_ml folder
# c) load the dataset using the following command (command+enter on a mac):
restaurant.data <- read.csv('DOHMH_New_York_City_Restaurant_Inspection_Results.csv')
# d) take a look at the first 6 rows of restaurant.data using the 'head' command
# e) how many Brazilian restaurants are there?
#    [hint: type ?table. Also, use restaurant.data$CUISINE.DESCRIPTION]
# f) how many different types of restaurants are there?
#    [hint: ?length and ?unique]
# g) what kind of variable is RECORD.DATE? What about SCORE?
#    [hint: ?class]

# 3. Install and load the tidyverse package. *Don't* include installation commands in your scripts!
# install.packages('tidyverse')
library(tidyverse)
# a) save restaurant.data as a tibble (could have loaded it as a tibble using read_csv instead)
#    [hint: use <- to save, and the as_tibble command]
restaurant.data <- as_tibble(restaurant.data)
# b) inspect your tibble by typing restaurant.data into the console
# c) to see 20 rows and all columns, execute the following:
print(restaurant.data, n=20, width=Inf)
#    or the following
View(restaurant.data)
# d) save just the BORO column as a vector (use single brackets to save as tibble)
boro <- restaurant.data[[3]]
boro <- restaurant.data[['BORO']]
# e) extract the inspection date of the 104th row
restaurant.data[104, 9]
# f) extract all (complete) rows for Brazilian restaurants (check that the number of rows agrees with your answer for 2e)
brazilian.ind <- restaurant.data$CUISINE.DESCRIPTION == 'Brazilian'
brazilian.restaurants <- restaurant.data[brazilian.ind, ]
# g) do all the brazilian restaurants have a grade? What do the fields other than A/B/C mean?
# h) save brazilian.restaurants to file
write_csv(brazilian.restaurants, path='brazilian_restaurants.csv')

