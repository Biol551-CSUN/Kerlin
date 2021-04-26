### Lab Assignment 2021-04-26 #########################
# Created by Jamie, Claudia, Jasmin, and Roland
# Created on 2021-04-26
######################################################

### Load libraries ###################################
library(tidyverse)
library(here)

### Bring in files ###################################
DataPath <- here("Week_13", "Data", "homework")

# List all files in path with specific pattern
# Look for everything with .csv
files <- dir(path = DataPath, pattern = ".csv")

### Create for loop #################################
hw_data <- data.frame(matrix(nrow = length(files), ncol = 5))

# Give dataframe column names
colnames(hw_data) <- c("filename", "mean_temp", "mean_light", "sd_temp", "sd_light")

# Turn it into a for loop
for(i in 1:length(files)){ # loop over the number of files
  raw_data <- read_csv(paste0(DataPath, "/", files[i])) # set path to our files
  hw_data$filename[i] <- files[i] # loop over files
  hw_data$mean_temp[i] <- mean(raw_data$Temp.C, na.rm = TRUE) # calculate mean for temperature
  hw_data$mean_light[i] <- mean(raw_data$Intensity.lux, na.rm = TRUE) # calculate mean for light
  hw_data$sd_temp[i] <- sd(raw_data$Temp.C, na.rm = TRUE) # calculate sd for temp
  hw_data$sd_light[i] <- sd(raw_data$Intensity.lux, na.rm = TRUE) # calcuate sd for light
}

hw_data

### Map functions using purr ######################

# Bring in files 
DataPath<-here("Week_13", "Data", "homework")
files <- dir(path = DataPath,pattern = ".csv", full.names = TRUE)

data <- files %>%
  set_names() %>% #sets id of each list to the file name
  map_df(read_csv, .id = "filename") %>% #map everything to a dataframe and put id in column named filename
  group_by(filename) %>% #group by only the files
  summarize(mean_temp = mean(Temp.C, na.rm = TRUE), # calculate mean temp
            mean_light = mean(Intensity.lux, na.rm = TRUE), # calculate mean for light
            sd_temp = sd(Temp.C, na.rm = TRUE), # calculate sd for temp
            sd_light = sd(Intensity.lux, na.rm = TRUE)) # calculate sd for light

data
