### Lab Assignment- Bad plots & good plots ###################################
### Created by Jamie Kerlin
### Created on 2021-03-14
##############################################################################

### Load libraries ###########################################################
library(tidyverse)
library(here)
library(png)
library(palmerpenguins)
library(ggpubr)
library(ghibli)
library(scatterpie)

### Load data ################################################################
penguins <- penguins
penguin_image <- readPNG(file.path("Week_7",
                                  "Data",
                                  "penguin_image.png"))

### Make bad plot ############################################################
penguins2 <- penguins %>%
  pivot_wider(names_from = sex,
              values_from = body_mass_g) 


ggplot(data = penguins,
       mapping = aes(x = bill_length_mm,
                     y = bill_depth_mm)) +
  background_image(penguin_image) +
  geom_scatterpie(aes(x = bill_length_mm,
                      y = bill_depth_mm),
                      data = penguins2,
                  cols= c("Male", "Female")) + 
  coord_equal() 
  
  geom_point(aes(shape = island), color = "purple", size = 6) +
  geom_point(aes(color = species, shape = island), size = 4) +
  guides(color = FALSE, shape = FALSE) +
  labs(title = "penguin bill data") +
  scale_color_manual(values = c("#00FF2A", "#FFFB00", "#5B561D")) +
  theme(axis.title.x = element_text(color = "brown", face = 8, size = 30), #change axis text color
        axis.title.y = element_text(color = "magenta", face = 12, size = 15))  +
  xlim(c(30, 100)) +
  ylim(c(10, 20)) 



  ggsave(here("Week_7", 
              "Output",
              "2021_03_22_bad_plot_assignment.png")) 

### Make good plot ##########################################################
penguins <- na.omit(penguins) %>%
  mutate(sex = recode(sex, "female" = "Female", "male" = "Male"))
  

ggplot(data = penguins,
       mapping = aes(x = bill_length_mm, 
                     y = bill_depth_mm)) +
  geom_violin(aes(fill = species), alpha = 0.4) + 
  geom_jitter(aes(color = species)) +
  guides(color = FALSE) +
  facet_wrap(~sex) +
    theme_pubclean() +
    labs(title = "Bill depth (mm) and bill length (mm) of penguins by species and sex",
         x = "Bill length (mm)",
         y = "Bill depth (mm)",
         fill = "Species", 
         caption = "Source: Palmer Station LTER / palmerpenguins packages") +
  scale_fill_manual(values = ghibli_palette("YesterdayMedium")[c(4, 5, 6)]) +
  scale_color_manual(values = ghibli_palette("YesterdayMedium")[c(4, 5, 6)]) +
  ggsave(here("Week_7", 
              "Output",
              "2021_03_22_good_plot_assignment.png")) 
