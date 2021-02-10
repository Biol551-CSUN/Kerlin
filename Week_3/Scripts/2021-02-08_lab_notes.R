#BIOL551 Lab CSUN Spring 2021
#Created by Jamie Kerlin
#Created on 2021-02-08

#Reviewing making ggplot
#Making plot of bill depth and length using Palmer penguins package data

### Load libraries #######
library(ggplot2)
library(tidyverse)
library(palmerpenguins)

### Template for ggplot ######
#ggplot(data = [dataset],
       #mapping = aes(x = [x-variable],
                     #y = [y-variable])) +
  #geom_xxx() +
  #other options

### Look at data #######
glimpse(penguins)

### Plotting penguin data #######
ggplot(data = penguins, #start with data frame
       mapping = aes(x = bill_depth_mm, #map bill depth to x axis
                     y = bill_length_mm, #map bill length to y axis
                     color = species, #map species to color of each point
                     size = body_mass_g)) + #map body mass to size
  geom_point() + #Represent each observation with a point
  labs(title = "Bill depth and length", #Add title and subtitle to plot
      subtitle = "Dimensions for Adelie, Chinstrap, and Gentoo Penguins",
      x = "Bill depth (mm)", y = "Bill length (mm)", #Change x and y axis labels
      color = "Species", size = "Body mass (g)", #Change legends to proper capitalization
      caption = "Source: Palmer Station LTER / palmerpenguins package") + #Add caption
  scale_color_viridis_d() #Changes colors to those that are clear for color-blindness


### Faceting data ######
ggplot(penguins,
       aes(x = bill_depth_mm,
           y = bill_length_mm)) + 
  geom_point() +
  facet_grid(species~sex) #species on y is a function of sex on x axis

#Facet_wrap
ggplot(penguins,
      aes(x = bill_depth_mm,
          y = bill_length_mm,
          color = species)) + 
  geom_point() +
  scale_color_viridis_d() +
  facet_wrap(~species, ncol = 1) + #only 1 variable, can specify columns
  guides(color = FALSE) #take out legend for color bc redundant
