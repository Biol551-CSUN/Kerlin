#### BIOL 551 Lab Notes 2021-02-22 #################################
#Joining data
#Created by Jamie Kerlin
#Created on 2021-02-22
####################################################################

### Load Libraries #################################################
library(tidyverse)
library(here)

### Load data ######################################################
envirodata <- read_csv(here("Week_5", "Data", "site.characteristics.data.csv"))
TPCdata <- read.csv(here("Week_5", "Data", "Topt_data.csv"))

### Pivot wider ####################################################
envirodata_wide <- envirodata %>%
  pivot_wider(names_from = parameter.measured,
              values_from = values)

### Clean the data #################################################
envirodata_wide <- envirodata_wide %>%
  arrange(site.letter)
view(envirodata_wide)

### Left join data #################################################
#Joins by unique identifying key based on the left data set in code
fulldata_left <- left_join(TPCdata, envirodata_wide)
#joins by site.letter 
view(fulldata_left)

### Relocate columns ###############################################
fulldata_left <- fulldata_left %>%
  relocate(where(is.numeric), 
           .after = where(is.character)) 
#relocate all numeric data after character data

### Think, pair, share #############################################
#take data set and calc mean and variance of all TPC and environmental
#data by site summarise_at() or pivot_longer
fulldata_longer <- fulldata_left %>%
  pivot_longer(E:substrate.cover,
               names_to = "Variables",
               values_to = "Values") %>%
  group_by(site.letter, Variables) %>%
  summarise(param_means = mean(Values, na.rm = TRUE),
            param_var = var(Values, na.rm = TRUE))

fulldata_summarize <- fulldata_left %>%
  group_by(site.letter) %>%
  summarise_at(vars(E:substrate.cover, 
                    list(mean = mean, var = var), na.rm = TRUE))

### Create a tibble ###############################################
T1 <- tibble(Site.ID = c("A", "B", "C", "D"),
       Temperature = c(14.1, 16.7, 15.3, 12.8))

T2 <- tibble(Site.ID = c("A", "B", "D", "E"),
             pH = c(7.3, 7.8, 8.1, 7.9))

### Left join vs. right join #####################################
#Just differs in which dataframe is used as the base 
left_join(T1, T2)
right_join(T1, T2)

### Inner join vs. full join #####################################
#Inner join only keeps data complete in both data sets 
inner_join(T1, T2)
full_join(T1, T2)

### Semi join vs. anti join ######################################
#Semi join keeps all rows from first with matching in second
#Anti join keeps only missing data 
#Usually used when trying to find where missing data is 
semi_join(T1, T2)
anti_join(T1, T2)

### Fun package of the day #######################################
library(cowsay)

say("", by = "clippy")

### For lab- practice joining data on own ########################
