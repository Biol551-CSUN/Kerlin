#### BIOL 551 Lab Assignment 2021-04-05 ################################
#Code bugs
#Created by Jamie, Claudia, Jasmin, and Roland
#Created on 2021-04-05
########################################################################

### Load libraries ###
library(tidyverse)
library(palmerpenguins)

### Code ###
penguins %>%
  filter(complete.cases(.)) %>% #take out NA rows
  ggplot(mapping(x = sex, y = body_mass_g, fill = species)) + #set aesthetics
  geom_violin(position="dodge", alpha=0.5, outlier.colour="transparent") + #violin plot aesthetics
  geom_jitter(alpha = 0.5, color = "purple", width = 0.4) %>% #add jitter points with color and aesthetics
  facet_wrap(~species) + #facet wrap by species
  theme_classic() + #set theme to classic
  guides(fill = FALSE, color = FALSE) + #take out legend guide
  labs(x = "Sex", y = "Body Mass (g)", #label x and y axes
       title = "Body Mass (g) by Sex and Species", #set title and caption 
       caption = "Palmer Station LTER/palmerpenguins package") 
  