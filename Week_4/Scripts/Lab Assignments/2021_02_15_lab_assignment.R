### week 4 HW ##
### Created by: Claudia Rea
### Created on: 2021-02-15

### Libraries ###
library(palmerpenguins)
library(tidyverse)
library(here)
library(PNWColors)

### Load data ######
# The data is part of the package and is called penguins
glimpse(penguins)
head(penguins)
tail(penguins)

## Part 1 ##
Part_1 <- penguins %>% #renaming penguins to Part_1
  drop_na(sex) %>% #dropped NA values within sex
  group_by(species,island,sex) %>% # grouped based on variable
  summarize(mean_body_mass = mean(body_mass_g, na.rm = TRUE), #calculated mean and variance of body mass
            var_body_mass = var(body_mass_g, na.rm = TRUE))
view(Part_1)

## Part 2 ##
Part_2 <- penguins %>% #renaming penguins to Part_2
  filter(sex == "female")%>% #filtered out males
  mutate(log_mass = log(body_mass_g))%>% #log calc for mass
  select(species,island,sex,log_mass)%>% # filter out select columns 
  ggplot(aes(x = island, y = log_mass, fill = species))+ #boxplot of data
  geom_boxplot(width = 0.3, alpha = 0.5, outlier.shape = NA) +
  facet_grid(~species)+ #group islands into one graph by species
  guides(color = FALSE)+
  guides(fill = FALSE)+
  scale_fill_manual(values = pnw_palette(6)[c(2,4,6)])+ #changed fill color of species 
  scale_color_manual(values = pnw_palette(6)[c(2,4,6)])+ #changed color for jitter pts.
  geom_jitter(aes(x = island, y = log_mass, color = species), #jitter style plot 
              alpha = 0.5, position = position_jitterdodge(dodge = 0.5, jitter.width = 0.9))+
  theme_classic()+ #added theme
  labs(title = "Penguin log Body Mass by Species", subtitle = "For Islands Biscoe, Dream, Torgersen", x = "Island", y = "Log (Body Mass)", caption = "Source; Palmer Station LTER") + #labeled, titled and captioned graph
ggsave(here("Week_4", "Output", "2021-02-15_lab_assignment.png"))

