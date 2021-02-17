#### BIOL 551 Lab Notes 2021-02-15 #################################
#Wrangling penguin data from palmerpenguin package
#Created by Jamie Kerlin
#Created on 2021-02-15
####################################################################

### Load libraries #################################################
library(palmerpenguins)
library(tidyverse)
library(here)

### Filter data ####################################################
#Filter for females
filter(.data = penguins, sex == "female")

#Filter for 2008 and then for body mass over 5000 g
filter(.data = penguins, year == "2008")
filter(.data = penguins, body_mass_g > 5000)

#Now filter for both together
filter_penguins <- penguins %>% 
  filter(year == "2008" & body_mass_g > 5000)

#Can also just put commas between instead
filter(.data = penguins, sex == "female", body_mass_g > 4000)

#Another filter practice
filter(.data = penguins, year == 2008 | year == 2009)

filter(.data = penguins, island != "Dream")  

filter(.data = penguins, species == "Adelie" | species == "Gentoo")

filter(.data = penguins, species %in% c("Adelie", "Gentoo"))

### Mutate #########################################################
data2 <- mutate(.data = penguins,
                body_mass_kg = body_mass_g/1000)

#Change multiple columns at once
data2 <- mutate(.data = penguins,
                body_mass_kg = body_mass_g/1000,
                bill_length_depth = bill_length_mm/bill_depth_mm)

#Conditional test with mutate
data2 <- mutate(.data = penguins,
                after_2008 = ifelse(year > 2008),
                "After 2008", "Before 2008")

#Mutate practice
data_1 <- mutate(.data = penguins,
                 flipper_body_mass = flipper_length_mm + body_mass_g)

data_2 <- mutate(.data = penguins,
                 sex_capitalized = ifelse(sex == "male",
                "Male", "Female"))

### Using pipeline ################################################
penguins %>%
  filter(sex == "female") %>%
  mutate(log_mass = log(body_mass_g))

### Select ########################################################
penguins %>%
  filter(sex == "female") %>%
  mutate(log_mass = log(body_mass_g)) %>%
  select(Species = species, island, sex, log_mass) 

### Summarize #####################################################
penguins %>%
  summarise(mean_flipper = mean(flipper_length_mm, na.rm = TRUE),
            min_flipper = min(flipper_length_mm, na.rm = TRUE))
#this skips over missing data when you calculate means

### Group by ######################################################
penguins %>% 
  group_by(island, sex) %>%
  summarise(mean_bill_length = mean(bill_length_mm, na.rm = TRUE),
            max_bill_length = max(bill_length_mm, na.rm = TRUE))

### Remove NAs ####################################################
penguins %>%
  drop_na(sex) %>% #drops all rows that are missing data on sex
  group_by(island, sex) %>%
  summarise(mean_bill_length = mean(bill_length_mm, na.rm = TRUE))

#Pipe directly into ggplot
penguins %>%
  drop_na(sex) %>%
  ggplot(aes(x = sex, y = flipper_length_mm)) + 
  geom_boxplot() 

#Ctrl + shift + m to add pipe


#Assignment tasks
#1. calculate the mean and variance of body mass by 
#species, island, and sex without any NAs
#2. filters out (excludes) male penguins, then calcs the log
#body mass, then selects only the columns for species, island,
#sex, and body mass, then use these data to make any plot. 
#Make sure the plot has clean and clear labels and follows best
#practices. Save the plot in the correct output folder. 
#Include both part 1 and part 2 in your script and push to Github
#in appropriate folders 