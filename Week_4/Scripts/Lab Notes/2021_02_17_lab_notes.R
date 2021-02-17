#### BIOL 551 Lab Notes 2021-02-17 #################################
#Tidyr data wrangling Nyssa's Hawaii data
#Created by Jamie Kerlin
#Created on 2021-02-17
####################################################################

### Load Libraries #################################################
library(tidyverse)
library(here)

### Load data ######################################################
chemdata <- read_csv(here("Week_4", 
                           "Data", 
                           "chemicaldata_maunalua.csv"))
view(chemdata)

### Clean data  ####################################################
chemdata_clean <- chemdata %>%
  filter(complete.cases(.)) %>% #filters out all non-complete rows
  separate(col = Tide_time, #choose the tide time column
           into = c("Tide", "Time"), #separate into two columns
           sep = "_") %>% #separate by underscore
#if you want to keep original col, , "remove = FALSE")
  unite(col = "Site_Zone", #the name of new column
        c(Site, Zone), #the columns to unite
        sep = ".", #lets put a . in the middle
        remove = FALSE) #keep the original
  
chemdata_long <- chemdata_clean %>%
  pivot_longer(cols = Temp_in:percent_sgd, #columns you want to pivot
               names_to = "Variables", #name of new column with name of variable
               values_to = "Values")# %>% #name of new column with values of variable

chemdata_params <- chemdata_long %>%
  group_by(Variables, Site, Zone, Tide) %>% #group by everything we want
  summarise(Param_means = mean(Values, na.rm = TRUE), #get mean
            Param_vars = var(Values, na.rm = TRUE), #get variance
            Param_sd = sd(Values, na.rm = TRUE)) #get sd

view(chemdata_clean)
### Think, pair, share 
#Calculate mean, variance, and std dev for all variables by 
#site, zone, and tide

#Using facet_wrap with long data

chemdata_long %>%
  ggplot(aes(x = Site, y = Values)) +
  geom_boxplot() +
  facet_wrap(~Variables, scales = "free") #facet wrap by variables

chemdata_wide <- chemdata_long %>%
  pivot_wider(names_from = Variables,
            values_from = Values)

### Same thing but in complete pipeline #############################

chemdata_all <- chemdata %>%
  filter(complete.cases(.)) %>% #filter out everything not complete row
  separate(col= Tide_time, #choose tide time col
           into = c ("Tide", "Time"), #separate into two columns
           sep = "_", #separate by underscore
           remove = FALSE) %>%
  filter(Zone == "Transition" | Zone == "Ambient")



  pivot_longer(cols = Temp_in:percent_sgd, #columns you want to pivot
               names_to = "Variables", #name of new column w variable names
               values_to = "Values") %>% #name of new column with values
  group_by(Variables, Site, Time) %>% #group by variables, site, and time
  summarise(mean_vals = mean(Values, na.rm = TRUE)) %>% #summarise to get mean
  pivot_wider(names_from = Variables,
              values_from = mean_vals) %>% #pivot wide with mean values
  write_csv(here("Week_4", "Output", "summary.csv")) #export as csv


