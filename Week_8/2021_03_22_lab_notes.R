### BIOL551 Lecture Notes ##########################################
# Today we are talking about advanced plotting 
# Created by Jamie Kerlin
# Created on 2021-03-22
####################################################################

### Libraries ######################################################
library(tidyverse)
library(here)
library(palmerpenguins)
library(patchwork) #bringing plots together
library(ggrepel) #repel your labels on your plots
library(gganimate) #add some animations to your plots
library(magick) #photo processing

### Patchwork ######################################################
#Create plot 1

p1 <- penguins %>%
  ggplot(aes(x = body_mass_g,
             y = bill_length_mm,
             color = species)) +
  geom_point()

p1

#Create plot 2

p2 <- penguins %>%
  ggplot(aes(x = sex,
             y = body_mass_g,
             color = species)) +
  geom_jitter(width = 0.2)

p2

#Add plots together in subplot
p1 + p2 +
  plot_layout(guides = 'collect') +
  plot_annotation(tag_levels = 'A')

#put one over the other
p1/p2 +
  plot_layout(guides = 'collect') +
  plot_annotation(tag_levels = 'A')

#more info on patchwork vignette

### ggrepel ############################################################
view(mtcars)
#can use for any x y coordinates
ggplot(mtcars, aes(x = wt,
                   y = mpg,
                   label = rownames(mtcars))) +
  geom_text_repel() + #created text labels
  geom_point(color = 'red')

#with label background
ggplot(mtcars, aes(x = wt,
                   y = mpg,
                   label = rownames(mtcars))) +
  geom_label_repel() + #created text labels
  geom_point(color = 'red')

### gganimate ###########################################################
penguins %>%
  ggplot(aes(x = body_mass_g,
             y = bill_length_mm,
             color = species)) +
  geom_point() +
  transition_states(
    year, #what we are animating by
    transition_length = 2, #the relative length of the transition
    state_length = 1) + #length of the pause between transitions
  ggtitle('Year: {closest_state') +
    anim_save(here("Week_8", "Output", "mypenguingif.gif"))

### magick ##############################################################
#Advanced image processing

penguin <- image_read("https://pngimg.com/uploads/penguin/pinguin_PNG9.png")

penguins %>%
  ggplot(aes(x = body_mass_g, 
             y = bill_depth_mm, 
             color = species)) +
  geom_point() +
  ggsave(here("Week_8","output","penguinplot.png"))

penplot<-image_read(here("Week_8","output","penguinplot.png"))
out <- image_composite(penplot, penguin, offset = "+70+30")
out

pengif<-image_read("https://media3.giphy.com/media/H4uE6w9G1uK4M/giphy.gif")
outgif <- image_composite(penplot, pengif, gravity = "center")
animation <- image_animate(outgif, fps = 10, optimize = TRUE)
animation
