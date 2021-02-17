### Lab Homework Assignment###
### By: Roland, Claudia, Jamie, and Jasmin ###
### 2021-02-17 ###

### Libraries ###
library(tidyverse)
library(here)
library(ghibli)


### Data Set ###
ChemData<-read_csv(here("Week_4", "Data", "chemicaldata_maunalua.csv"))

### Data ###
ChemData_clean<-ChemData %>% #renaming ChemData to ChemData_clean
  filter(complete.cases(.)) %>% #filters out everything that is not a complete row
  separate(col = Tide_time, #selecting column
           into = c("Tide", "Time"), #separate it into two columns Tide and Time
           sep = "_", #taking out _ 
           remove = FALSE) %>% #keeping the original column
  pivot_longer(cols = Temp_in:percent_sgd, #the cols you want to pivot
               names_to = "Variables", # the names of the new cols
               values_to = "Values") %>% #names of the new column
  group_by(Variables, Site, Zone, Time) %>% #using statistics for variables by zone and time
  summarise(Param_means = mean(Values, na.rm=TRUE), #get mean
            Param_vars = var(Values, na.rm=TRUE), #get variance
            Param_sd = sd(Values, na.rm=TRUE)) %>% #get standard deviation
            write_csv(here("week_4", "Output", "2021-02-17_lab_assignment.csv")) #export as a csv to the right folder


View(ChemData_clean) #view data

ChemData_long <- ChemData %>%
  filter(complete.cases(.)) %>% 
  separate(col = Tide_time, #selecting column
           into = c("Tide", "Time"), #separate it into two columns Tide and Time
           sep = "_", #taking out _ 
           remove = FALSE) %>%
  pivot_longer(cols = Temp_in:percent_sgd, #columns you want to pivot
               names_to = "Variables", #name of new column with name of variable
               values_to = "Values") %>% 
  #name of new column with values of variable
  mutate(Variables = recode(Variables, "NN" = 'Nitrate + Nitrite', "Temp_in" = 'Temperature', "TA" = 'Total Alkalinity', "percent_sgd" = '% SGD')) #renaming

### Graph ###
ChemData_long %>% # selecting data
  ggplot(aes(x = Zone, y = Values, color = Season))+ # plotting using zone and values
  geom_jitter(width = 0.2, alpha = 0.5)+ #jitter style plot
  facet_wrap(~Variables, scales = "free") + #y plot has their own scales
  labs(title = "Values Of Abiotic Variables", # title for graph
       subtitle = "By: NN, Percent SGD, pH, Phosphate, Salinity, Silicate, TA, Temp In", #subtitle for graph
       x = "Zone", # x axis
       y = "Values", # y axis
       caption = "Source: Dr. Sibliger Data") + #caption for graph
  theme_classic() + #change theme
  theme(axis.title.x = element_text(color = "#67B9E9FF", face = 2), #change axis text color
        axis.title.y = element_text(color = "#67B9E9FF", face = 2)) + #change axis text color
  scale_fill_manual(values = ghibli_palette(1)[c(3, 5, 7)]) + #Ghibli palette fill
  ggsave(here("Week_4", "Output", "2021-02-17_lab_assignment_graph.png")) #save plot
  

