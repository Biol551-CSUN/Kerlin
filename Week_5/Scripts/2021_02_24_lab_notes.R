#### BIOL 551 Lab Notes 2021-02-24 #################################
#Lubridate date & time
#Created by Jamie Kerlin
#Created on 2021-02-24
####################################################################

### Load Libraries #################################################
library(tidyverse)
library(here)
library(lubridate)

### Load data ######################################################
cond <- read_csv(here("Week_5", "Data", "CondData.csv"))
depth <- read_csv(here("Week_5", "Data", "DepthData.csv"))

### What time is it now? ###########################################
#good for wanting to time stamp something for whenever something
#is ran
now() #what time is it now?
now(tzone = "EST") #what time is it on the east coast?
now(tzone = "GMT") #what time is it in GMT?
today() #what is the date?
today(tzone = "GMT") #what is the date in GMT time zone?
am(now()) #is it AM now?
lear_year(now()) #is this a leap year?

### Date specifications ############################################
#Your dates must be a character
#If numeric, will get error
ymd() #2021-02-24
mdy() #02/24/2021
mdy() #February 24 2021
dmy() #24/02/2021

#These all convert to ISO date
ymd("2021-02-24")
mdy("02/24/2021")
mdy("February 24 2021")
dmy("24/02/2021")

#Dates and times
ymd_hms() #2021-02-24 10::22:20 PM
mdy_hms() #02/24/2021 22:22:20
mdy_hm() #February 24 2021 10:22 PM

ymd_hms("2021-02-24 10:22:20 PM")
mdy_hms("02/24/2021 22:22:20")
mdy_hm("February 24 2021 10:22 PM")

#Can change time zone 
mdy_hm("February 24 2021 10:22 PM", tzone = "PST")

### Extract elements from datetimes ################################
#First make vector of data
datetimes <- c("02/24/2021 22:22:20",
               "02/25/2021 11:21:10",
               "02/26/2021 8:01:52")
#convert to datetimes
datetimes <- mdy_hms(datetimes)

#extract 
month(datetimes) #get months, gives vector of months
month(datetimes, label = TRUE) #get months in abbrev name format
month(datetimes, label = TRUE, abbr = FALSE) #spell out months
day(datetimes) #extract day
wday(datetimes, label = TRUE) #extract day of week in abbrev name format
hour(datetimes)
minute(datetimes)
second(datetimes)

### Adding dates and times #########################################
#Say you forgot to change to a timezone, add 4 hours
datetimes + hours(4) #this adds 4 hours
#Add days
datetimes + days(2) #adds 2 days

### Rounding dates #################################################
round_date(datetimes, "minute") #round to nearest minute
round_date(datetimes, "5 mins") #round to nearest five minutes

### Think pair share ###############################################
#Read in Cond and Depth data
#convert date column to datetime in Cond data

cond <- cond %>%
  mutate(datetime = ymd_hms(date)) #mutate as a new column

