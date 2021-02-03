#Week 2 Wednesday Notes

#Created by: Jamie Kerlin
#Created on: 2021-02-03
#Last updated: 2021-02-03
#################################################

#Load libraries ##########
library(here)
library(tidyverse)

#Read in data ##########
#here::here() function makes unbreakable paths
#for example, unbreakable path to a data file:
my_data <- read_csv(here("Week_2", "Data", "weightdata.csv"))

#Difference between read_csv and read.csv
#read_csv used in tidyverse and has some differences
#that make it a little cleaner

