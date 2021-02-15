#### BIOL 551 Lab Assignment 2021-02-10 ######
#Group assignment during lab
#Plotting penguin data from palmerpenguin package
#Created by Jamie, Roland, and Jasmin
#Created on 2021-02-10
#Updated on 2021-02-14
#######################################################################
### Load libraries #######
library(palmerpenguins)
library(tidyverse)
library(here)
library(praise)
library(PNWColors)
library(beyonce)
library(ghibli)
library(ggthemes)

### Load data #########################################################
#The data is part of the package and is called penguins
#Look at data using glimpse
glimpse(penguins)

#### Create boxplot ##################################################
penguins <- na.omit(penguins) #remove NAs in sex and species

penguins <- penguins %>%
  mutate(sex = recode(sex, 'male' = "Male", 'female' = "Female")) #change to proper capitalization

ggplot(data = penguins, 
       mapping = aes(x = species, #compare species for bill depth (mm)
                     y = bill_depth_mm,
                     fill = species), #fill color species
       stat = "identity") +  
  geom_boxplot() +
  facet_grid(~sex, switch = "x") + #facet for sex within species
  scale_fill_manual(values = pnw_palette(1)[c(3, 5, 7)]) + #PNW palette fill
  labs(title = "Comparison of Bill Depth (mm) Across Species Within Sex", #add title
       x = "Species", #change x axis label
       y = "Bill depth (mm)", #change y axis label
       fill = "Species", #change legend label
       caption = "Source: Palmer Station LTER / palmerpenguins packages") + #add caption
  theme_classic() + #change theme
  theme(axis.title.x = element_text(color = "deepskyblue4", face = 2), #change axis text color
        axis.title.y = element_text(color = "deepskyblue4", face = 2)) +
  ggsave(here("Week_3", "Output", "penguin_lab_assignment.png"), #save plot
         width = 7, height = 5)
