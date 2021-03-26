### Lecture/Lab 2021_03_24 #############################################
# Creating functions
# Created by Jamie Kerlin
# Created on 2021_03_24
########################################################################

### Load libraries ####################################################
library(tidyverse)
library(palmerpenguins)
library(PNWColors)

### Create tibble #####################################################
df <- tibble::tibble(
  a = rnorm(10),
  b = rnorm(10),
  c = rnorm(10),
  d = rnorm(10)
)
head(df)

### Create a function ################################################
#If we want to rescale every column individually
#Copy paste method is hard
df <- df %>%
    mutate(a = (a-min(a, na.rm = TRUE))/(max(a, na.rm = TRUE)-min(a, na.rm = TRUE)),
         b = (b-min(b, na.rm = TRUE))/(max(a, na.rm = TRUE)-min(b, na.rm = TRUE)),
         c = (c-min(c, na.rm = TRUE))/(max(c, na.rm = TRUE)-min(c, na.rm = TRUE)),
         d = (d-min(d, na.rm = TRUE))/(max(d, na.rm = TRUE)-min(d, na.rm = TRUE)))

#Can make a function instead
rescale01 <- function(x) {
  value <- (x-min(x, na.rm = TRUE))/(max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
  return(value)
}

df %>%
  mutate(a = rescale01(a),
         b = rescale01(b),
         c = rescale01(c),
         d = rescale01(d))

### Create function convert F to C ###########################################
#Here is the equation
temp_c <- (temp_F - 32) * 5 / 9

#Create function
fahrenheit_to_celsius <- function(temp_F) {
  temp_C <- (temp_F - 32) * 5 / 9
  return(temp_C)
}

#Test it
fahrenheit_to_celsius(32)

#Create function to convert C -> Kelvin
#Equation
temp_K <- (temp_C + 273.15)

#Function
celsius_to_kelvin <- function(temp_C) {
  temp_K <- (temp_C + 273.15)
  return(temp_K)
}

#Test
celsius_to_kelvin(0)

### Making plots into a function #####################################
myplot <- function(data, x, y){
pal <- pnw_palette("Lake", 3, type = "discrete") #color palette

ggplot(data, aes(x = {{x}}, #says it is assigned variable name from df
                     y = {{y}},
                     color = island)) +
  geom_point() + 
  geom_smooth(method = "lm") + #add linear model
  scale_color_manual("Island", values = pal) + 
  theme_bw()
}

myplot(data = penguins, x = body_mass_g, y = bill_length_mm)
myplot(data = penguins, x = body_mass_g, y = flipper_length_mm)

### Defaults #########################################################
#To default to penguin dataframe
myplot2 <- function(data, x, y){
pal <- pnw_palette("Lake", 3, type = "discrete") #color palette
  
ggplot(penguins, aes(x = {{x}}, #says it is assigned variable name from df
                   y = {{y}},
                   color = island)) +
geom_point() + 
geom_smooth(method = "lm") + #add linear model
scale_color_manual("Island", values = pal) + 
theme_bw()
}

#Can add layers onto called function
myplot2(x = body_mass_g, y = flipper_length_mm) +
  labs(x = "Body mass (g)",
       y = "Flipper length (mm)")

### Add if-else statement for more flexibility #########################
a <- 4
b <- 5

if(a > b){ #my question
  f <- 20 #if it is true give me answer 1
} else {
  f <- 10
}

f

### Add if else statements to plots ###################################
myplot <- function(data = penguins, x, y, lines = TRUE) {
  pal <- pnw_palette("Lake", 3, type = "discrete") #color palette
  
  if(lines==TRUE){
  ggplot(penguins, aes(x = {{x}}, #says it is assigned variable name from df
                       y = {{y}},
                       color = island)) +
    geom_point() + 
    geom_smooth(method = "lm") + #add linear model
    scale_color_manual("Island", values = pal) + 
    theme_bw()
}
else{
  ggplot(penguins, aes(x = {{x}}, #says it is assigned variable name from df
                       y = {{y}},
                       color = island)) +
    geom_point() + 
    scale_color_manual("Island", values = pal) + 
    theme_bw()
}
}

myplot(x = body_mass_g, y = flipper_length_mm, lines = FALSE)
