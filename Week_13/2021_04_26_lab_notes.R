### Iterative coding ######################################
# Created on 2021-04-26
# Created by Jamie Kerlin
###########################################################

### Load libraries ########################################
library(tidyverse)
library(here)

### For loops #############################################
# Two major parts- one indexing statement, one repeated code
# (index in sequence){command to repeat}
# Example- ask R to print statement

print(paste("The year is", 2000))

# Put it in a for loop
years <- c(2015:2021)

for(i in years){ #set up the for loop where i is the index
  print(paste("The year is", i)) #loop over i
}

# Simple for loop
# Create new vector with all years- pre-allocate space and tell R where to save
# Create empty matrix
year_data <- data.frame(matrix(ncol = 2, nrow = length(years)))

# Add column names
colnames(year_data) <- c("year", "year_name")

for (i in 1:length(years)){ #set up loop for where i is the index
  year_data$year_name[i] <- paste("The year is", years[i]) #loop over i
  year_data$year[i] <- years[i] #loop over year
}

### Load data ##############################################

testdata <- read_csv(here("Week_13", "Data", "cond_data", "011521_CT316_1pcal.csv"))

# Point to location on computer
CondPath <- here("Week_13", "Data", "cond_data")

# List all files in path with specific pattern
# Look for everything with .csv

files <- dir(path = CondPath, pattern = ".csv")

### Create for loop with cond data #########################
cond_data <- data.frame(matrix(nrow = length(files), ncol = 3))

# Give dataframe column names
colnames(cond_data) <- c("filename", "mean_temp", "mean_sal")

# Write basic code to calculate mean and build out
raw_data <- read_csv(paste0(CondPath, "/", files[1])) #test by reading in first file

mean_temp <- mean(raw_data$Temperature, na.rm = TRUE)

# Turn it into a for loop
for(i in 1:length(files)){ # loop over 1:3 the number of files
  raw_data <- read_csv(paste0(CondPath, "/", files[i]))
  cond_data$filename[i] <- files[i]
  cond_data$mean_temp[i] <- mean(raw_data$Temperature, na.rm = TRUE)
  cond_data$mean_sal[i] <- mean(raw_data$Salinity, na.rm = TRUE)
}

cond_data

### Map functions ##########################################
1:10 %>% # a vector from 1 to 10 (we are going to do this 10 times) %>% # the vector to iterate over
  map(rnorm, n = 15) # calculate 15 random numbers based on a normal distribution in a list

1:10 %>% # a vector from 1 to 10 (we are going to do this 10 times) %>% # the vector to iterate over
  map(rnorm, n = 15)  %>% # calculate 15 random numbers based on a normal distribution in a list 
  map_dbl(mean) # calculate the mean. It is now a vector which is type "double"

# Make own function
1:10 %>% # list 1:10
  map(function(x) rnorm(15, x)) %>% # make your own function
  map_dbl(mean)

# Use a formula when you want to change arguments within the function
1:10 %>%
  map(~ rnorm(15, .x)) %>% #changes the arguments inside the function
  map_dbl(mean)

# Read in files
CondPath<-here("Week_13", "Data", "cond_data")
files <- dir(path = CondPath,pattern = ".csv")

data <- files %>%
  set_names() %>% #sets id of each list to the file name
  map_df(read_csv, .id = "filename") %>% #map everything to a dataframe and put id in column named filename
  group_by(filename) %>%
  summarize(mean_temp = mean(Temperature, na.rm = TRUE),
            mean_sal = mean(Salinity, na.rm = TRUE))

### Cool package ##########################################
gganatogram() - teaching anatomy 
