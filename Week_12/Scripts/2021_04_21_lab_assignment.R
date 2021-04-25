### Week 12 Lab Assignment
### Created by Roland, Jamie, Jasmin, Claudia
### Created on 2021-04-21

### Load Libraries
library(tidyverse)
library(here)
library(forcats)
library(magick)
library(PNWColors)


### Load Data
intertidaldata <- read_csv(here("Week_12","Data","intertidaldata.csv"))
intertidaldata <- read_csv(here("Week_12","Data","intertidaldata_latitude.csv"))

### Wrangle data
tidaldatafactor <- intertidaldata %>% 
  filter(complete.cases(.)) %>%  #filter out rows with any NAs
  mutate(Quadrat = recode(Quadrat, 'Low  .' = "Low", #fix column names
                          'Mid  1' = "Mid")) %>%
  select(1:10) %>% #select columns 1 through 10
  mutate(Quadrat = fct_lump(Quadrat)) %>% #lump all quadrats from transects together
  pivot_longer(cols = 4:10, #choose columns to pivot longer
               names_to = "type", #set pivot longer column names
               values_to = "cover") %>% 
  group_by(Site, Quadrat, type) %>% #group by site, quadrat, and type 
  summarise(average_cover = mean(cover, na.rm=TRUE)) %>% #calculate mean percent cover and remove NAs
  inner_join(intertidaldata_lat) %>% #inner join the latitude dataset
  mutate(Quadrat = factor(Quadrat,
                          levels = c("Low",
                                     "Mid",
                                     "High"))) #set the quadrat levels

tidaldatafactor %>% 
  ggplot(aes(x = Latitude, y = average_cover, #create ggplot with latitude and average cover on x and y axes
             fill = fct_reorder2(type,Latitude,average_cover)))+ #factor reorder the type of organism
  stat_smooth(
    geom = 'area', method = 'loess', #smooth area plot
    alpha = .3) +#set transparency
  facet_wrap(~Quadrat)+ #facet wrap by tidal level (low, mid, or high)
  labs(fill = "Type", #set labels
       y = "Average Percent Cover")+
  ylim(0, 100) + #set y limits
  theme_classic() + #Set theme
  scale_fill_hue() + #set scale color palette
  ggsave(here("Week_12", "Output", "2021_04_21_2021_lab_assignment.png"))


