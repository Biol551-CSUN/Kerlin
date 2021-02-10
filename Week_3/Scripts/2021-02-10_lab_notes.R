#### BIOL 551 Lab Notes 2021-02-10 ######
#Plotting penguin data from palmerpenguin package
#Created by Jamie Kerlin
#Created on 2021-02-10
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

### Plot bill depth & bill length ######################################
ggplot(data = penguins,
       mapping = aes(x = bill_depth_mm,
                     y = bill_length_mm)) +
  geom_point() + 
  geom_smooth(method = "lm") + #puts data into regression line
  labs(x = "Bill depth (mm)",
       y = "Bill length (mm)")

### this excludes that there is multiple species, how should
### we view the data instead? Separate species!
ggplot(data = penguins,
       mapping = aes(x = bill_depth_mm,
                     y = bill_length_mm,
                     color = species)) +
  geom_point() + 
  scale_color_viridis_d() + 
  geom_smooth(method = "lm") +
  labs(x = "Bill depth (mm)",
       y = "Bill length (mm)",
       color = 'Species')

#Yay! Plot done :)
praise()

### Scales #############################################################
ggplot(data = penguins,
       mapping = aes(x = bill_depth_mm,
                     y = bill_length_mm,
                     color = species)) +
  geom_point() + 
  #scale_color_viridis_d() + 
  scale_color_manual(values = c("orange", 
                                "purple",
                                "green")) + #manually set colors 
  geom_smooth(method = "lm") +
  labs(x = "Bill depth (mm)",
       y = "Bill length (mm)",
       color = 'Species') +
  scale_x_continuous(limits = c(12,22), #limits sets x axis from 0-20
                     breaks = c(14, 17, 21), #breaks says which #s on axis
                     labels = c("low", "medium", "high")) #match label to breaks
  #if it was discrete variable, put discrete in place of continuous
  #for y axis, scale_y_continuous

### Trying out new palettes
ggplot(data = penguins,
       mapping = aes(x = bill_depth_mm,
                     y = bill_length_mm,
                     color = species)) +
  geom_point() + 
  geom_smooth(method = "lm") +
  labs(x = "Bill depth (mm)",
       y = "Bill length (mm)",
       color = 'Species') +
  scale_color_manual(values = pnw_palette(1)[c(3, 5, 7)])

### Coordinates #########################################################
ggplot(data = penguins,
       mapping = aes(x = bill_depth_mm,
                     y = bill_length_mm,
                     color = species)) +
  geom_point() + 
  geom_smooth(method = "lm") +
  labs(x = "Bill depth (mm)",
       y = "Bill length (mm)",
       color = 'Species') +
  scale_color_manual(values = pnw_palette(1)[c(3, 5, 7)]) +
  coord_flip() + #flip x and y axes 
  coord_fixed() + #fix axes
  coord_polar("x") #make polar coordinates on x

#Log transforming both axes- log(10) 
ggplot(diamonds, aes(carat, price)) +
  geom_point() +
  coord_trans(x = "log10", y = "log10")

### Themes ############################################################
ggplot(data = penguins,
       mapping = aes(x = bill_depth_mm,
                     y = bill_length_mm,
                     color = species)) +
  geom_point() + 
  geom_smooth(method = "lm") +
  labs(x = "Bill depth (mm)",
       y = "Bill length (mm)",
       color = 'Species') +
  scale_color_manual(values = pnw_palette(1)[c(3, 5, 7)]) +
  theme_classic() + #no gridlines
  #theme_bw() #only major gridlines
  #theme_solarized(light = FALSE) + #custom theme- solarized
  #scale_colour_solarized("blue") + 
  theme(axis.title.x = element_text(size = 20), #change size of x axis title
        axis.title.y = element_text(face = 10, #change font of y axis title
                                    color = "red"), #change color of y axis title
        panel.background = element_rect(fill = "linen"), #change background fill
        legend.position = "top") +
  ggsave(here("Week_3", "Output", "penguin.png"),
         width = 7, height = 5) #save plot aspect ratio in inches
        
        
        
#type ?theme() to see all different elements that you can change
#survivorship plots in website she showed in class today- in lecture slides
#make plot with group- cannot be same plot we just made & can't be scatterplot

