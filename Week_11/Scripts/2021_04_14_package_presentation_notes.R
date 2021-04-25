### Package presentations ###################################
# Jamie
# 2021-04-14
#############################################################

### rinat package ###########################################
library(rinat)

get_inat_obs()
get_inat_obs_project()
get_inat_obs_user()

get_inat_obs(query = "Parrotfishes",
             quality = "research")
get_inat_obs(taxon_name = "Isocoma menziesii",
             quality = "research")
library(tidyverse)
mule_deer <- get_inat_obs(query = "Mule Deer",
                          bounds = c(38.44047, -125, 40.86652, -121/837),
                          quality = "research",
                          year = 2019)
plot(mule_deer$longitude, mule_deer$latitude)


sceloporus <- get_inat_obs(taxon_name = "Sceloporus occidentalis",
                           quality = "research",
                           place_id = 962,
                           maxresults = 500)

squirrel<- get_inat_obs(taxon_name = "Sciurus niger",
                           quality = "research",
                           place_id = 962,
                           maxresults = 500)

bee<- get_inat_obs(taxon_name = "Apis millifera",
                        quality = "research",
                        place_id = 962,
                        maxresults = 500,
                   year = 2019)

animals <- rbind(bee, squirrel, sceloporus)
animals

ggplot(data = animals, 
       aes(x = latitude, 
           y = latitude,
           color = scientific_name)) +
geom_polygon(data = map_data("usa"),
             aes(x = long, y = lat, group = group),
             fill = "grey95",
             color = "grey40",
             size = 0.1) +
  geom_point(size = 1, alpha = 0.5) +
  theme_bw() + 
  guides(color = FALSE)

### See by Easystats #######################################
library(see)
library(correlation)
library(ggraph)
library(gridExtra)
library(performance)
library(tidyverse)
library(here)
library(tidytuesdayR)

# Load data
tuition_cost <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-10/tuition_cost.csv')

#view(tuition_cost) # check data structure, only used during early code composition
kirill_density <- tuition_cost # pass data to a new frame to preserve the original
x <- kirill_density %>% 
  pull(in_state_total) # create vector "x" with the data to be analyzed

density_kernel <- bayestestR::estimate_density(x) # perform the analysis, storing results in a new dataframe "density_kernel"

hist(x, prob = TRUE, main = NULL, width = 10, height = 7, noRStudioGD = TRUE, xlab = "Total in-state spendings") # create a histogram for demonstration of the results of the function, remove default title, set size and use a parameter for correct rendering, set x axis label
lines(density_kernel$x, density_kernel$y, col = "black", lwd = 2) # set line color and width
title("estimate_density function over traditional histogram", lwd = 1)
legend("topright",
       legend = ("estimate_density output"),
       col = ("black"), lwd = 2, lty = c(1, 2)  # add legend, set width and line type to match rendered line
)

kirill_outliers <- tuition_cost %>% # pass data to a new frame to preserve the original
  slice(1:30) # select the first 30 rows of data for visualization
model <- lm(room_and_board ~ in_state_tuition + out_of_state_tuition, data = kirill_outliers) # create a set of coordinates containing the linear model of the data based on relationship between room_and_board and combined effect of in_state_tuition and out_of_state_tuition
plot(performance::check_outliers(model, method = "zscore"), width = 10, height = 7) + # create plot and specify our function applied to the linear model as the input. Set method to Zscore
  theme(legend.position = "none") + # remove legend that would otherwise be a useless "Zscore"
  labs(title = "Outliers of the linear model and their Zscores",
       x = "Number of the institution in the list",
       y = "Deviation from the model") # set title and axes labels

tuesdata <- tidytuesdayR::tt_load('2020-03-10')

salary <- tuesdata$salary_potential


salary_Corr <- correlation(salary) #creates matrix of correlation statistics between each variable
salary_Corr

#Create summary table of correlations between variables

summary_corr <- summary(salary_Corr)
summary_corr

#Visualize correlations between all variables
#Plot function is from from see package helps efficiently visualize correlations between multiple variables 

plot_corr <- plot(summary_corr, 
                  show_values = TRUE, #r values 
                  show_p = TRUE, #show significance 
                  size_point = 3, #size of the points
                  size_text = 5, #size of the text
                  type = "tile") #Tile or circle
plot_corr


tuition_clean <- tuition_cost %>% 
  filter(complete.cases(.))

p1 <- ggplot(tuition_clean, aes(x=room_and_board, y=in_state_tuition, color=type))+
  geom_point()+
  geom_smooth(method="lm")+
  theme_lucid()#Try one of See's added themes. More will be showcased later

p2 <- ggplot(tuition_clean, aes(x=in_state_tuition, y=out_of_state_tuition, color=type))+
  geom_point(size=1.2)+
  #geom_smooth(method="lm")+
  theme_lucid()+
  scale_color_pizza()

plots(p1, p2, tags = FALSE) #Package 'gridExtra' required for this function to work.

plots(p1, p2, tags = TRUE)

plots(p1, p2, n_rows = 2,
      tags = paste("Fig.", 1:3))
plots(p1, p2, n_columns = 2,
      tags = paste("Fig.", 1:3))
#Reasons to use this over facet-wrapping
tuition_profit <- tuition_clean %>% #let's isolate a smaller set of data
  filter(type=="For Profit") %>% 
  filter(degree_length == "4 Year")

view(tuition_profit)

ggplot(tuition_profit, aes(x = state, y = in_state_total))+
  geom_poolpoint(label=rownames(tuition_profit))+
  theme_lucid()+ #same theme as before from package
  theme(
    axis.text.x = element_text(size=8))

tuition_cost %>% 
  filter(state %in% c("Colorado", "Alaska", "Kansas", "New York")) %>% # only keep a few states for aesthetics' sake
  ggplot(aes(x = state, y = in_state_tuition, fill = state)) + # set up a ggplot graph as you normally would
  labs(x = "State", # clean up labels
       y = "In-State Tuition") +
  geom_violinhalf() +
  theme_modern(legend.position = "none") + # use a See theme
  # All of the See scale options:
  # scale_fill_flat()
  # scale_fill_material()
  # scale_fill_metro()
  # scale_fill_pizza()
  # scale_fill_see()
  # scale_fill_social()
  scale_fill_bluebrown() # use a See scale


tuition_cost %>% 
  filter(state %in% c("Colorado", "Alaska", "Kansas", "New York")) %>% # only keep a few states for aesthetics' sake
  ggplot(aes(x = state, y = in_state_tuition, fill = state)) + # set up a ggplot graph as you normally would
  labs(x = "State", # clean up labels
       y = "In-State Tuition") +
  geom_violindot(dots_size = 20000) + # set up the half-violin half-dotplot; sizes can get weird with this geom
  theme_blackboard(legend.position = "none") + # use a See theme
  scale_fill_material() # use a See scale

tuition_cost %>% 
  filter(state %in% c("Colorado", "Alaska", "Kansas", "New York")) %>% # only keep a few states for aesthetics' sake
  ggplot(aes(x = state, y = in_state_tuition, fill = state)) + 
  labs(x = "State", # clean up labels
       y = "In-State Tuition") +
  geom_jitter_borderless(size = 4) + # use geom_jitter_borderless for jittered "borderless" points
  theme_modern(legend.position = "none") + # use another See theme
  scale_fill_material() # use a See scale

ggplot(tuition_cost, aes(x = in_state_tuition, y = room_and_board)) +
  labs(x = "In-State Tuition", # clean up labels
       y = "Room and Board") +
  geom_point2(size = 4, alpha = 0.3) + # use geom_point2 for plain, borderless points
  theme_modern() # use a See theme

tuition_cost %>%
  filter(!type == "Other") %>%
  ggplot(aes(x = in_state_tuition, y = room_and_board, fill = type)) +
  labs(x = "In-State Tuition", # clean up labels
       y = "Room and Board",
       fill = "School Type") +
  geom_point_borderless(size = 4, alpha = 0.5) + # use geom_point2 for "borderless" points using multiple colors
  scale_fill_metro() + # use another See scale
  theme_modern() # use a See theme

### Janitor package ###################################################
library(janitor)
library(readxl)
library(kableExtra)
library(tidyverse)
library(here)

# Load data
coralgrowth<-read_csv(here("project", "Data", "CoralGrowth.csv"))
corals_messy <- read_csv(here("project", "Data", "coraldata.csv"))


#clean_names()
corals_messy%>%
  kbl() %>% # make a table in RMarkdown
  kable_classic()%>% #  Theme of the table
  kable_styling() %>% 
  scroll_box(width = "700px", height = "300px")# Table dimensions

corals<-clean_names(corals_messy)%>% ## function we are looking at ##
  write_csv(here("project", "output", "clean_names_corals.csv"))

corals %>%
  kbl() %>% # make a table in RMarkdown
  kable_classic()%>% # the theme of the table
  kable_styling() %>%
  scroll_box(width = "700px", height = "300px") # table dimensions 

#tabyl()
tabyl(corals, change_mg_cm2)%>% #we can put the object into a frequency table
  kbl() %>% # make a table in RMarkdown
  kable_classic()%>% # theme of the table
  kable_styling() %>% 
  scroll_box(width = "300px", height = "300px")# table dimensions

corals %>%
  tabyl(change_mg_cm2)%>%  ### The pipe version ###
  kbl() %>% # make a table in RMarkdown
  kable_classic()%>% # theme of table
  kable_styling() %>%
  scroll_box(width = "300px", height = "300px")# table dimensions

#remove_empty()
#first make a simple data frame 
a <- data.frame(v1 = c(7, 6, 4, 5),
                v2 = c(NA, NA, NA, NA),
                v3 = c("a", "b", "c", "d"), 
                v4 = c(6, 5, 8, 10))
a %>% #data frame name
  kbl()%>% #make a table in RMarkdown
  kable_classic()%>% #classic theme
  kable_styling(full_width = FALSE, position = "left")

a_clean<-a %>% #rename data frame
  remove_empty(c("rows", "cols")) #checks for empty columns and rows, removes them!
a_clean%>%  #data frame name 
  kbl()%>% #make a table in RMarkdown
  kable_classic()%>% #classic theme
  kable_styling(full_width = FALSE, position = "left")

#get_dupes()
corals %>%
  get_dupes("change_mg_cm2")%>%  ## the function we are using ##
  arrange(dupe_count)%>%  # arranged it by dupe count to make it organized 
  kbl() %>% # make a table in RMarkdown
  kable_classic()%>% # table theme
  kable_styling() %>%
  scroll_box(width = "700px", height = "300px")# table dimensions 


### Plotly package ###################################################
library(tidytuesdayR)
library(tidyverse)
library(here)
library(plotly)

#Load data
# clear environment
# load Tidy Tuesday data
tuesdata <- tidytuesdayR::tt_load('2020-02-18')

# assign data to dataframe
food.data<-tuesdata$food_consumption
glimpse(food.data)


#Plot the data
fig <- plot_ly(data = food.data, 
               x = ~co2_emmission, 
               y = ~consumption)
fig

fig <- plot_ly(data = food.data, 
               x = ~co2_emmission, 
               y = ~consumption,
               color = ~food_category)
fig

fig <- plot_ly(data = food.data, 
               x = ~co2_emmission, 
               y = ~consumption,
               color = ~food_category,
               colors = "viridis")
fig

fig <- plot_ly(data = food.data, 
               x = ~co2_emmission, 
               y = ~consumption,
               color = ~co2_emmission,
               colors = "viridis")
fig

fig <- plot_ly(data = food.data, 
               x = ~co2_emmission, 
               y = ~consumption,
               color = ~food_category,
               colors = "viridis",
               marker = list(size = 10), #change the marker size
               text = ~paste("CO2 emission:", co2_emmission, #change the hover data labels
                             "<br>Consumption:", consumption, #<br> moves the label to a new line
                             "<br>Food category:", food_category)) %>% #must PIPE to the layout 
  layout(title = 'CO2 Emissions and Food Consumption by Food Type', #add a title
         xaxis = list(title = "CO2 Emission"), #change the axes titles
         yaxis = list(title = "Consumption"),
         legend = list(title = list(text = 'Food Category'))) #add a legend title
fig

#turn scatterplot into line graph
fig <- plot_ly(data = food.data, 
               x = ~co2_emmission, 
               y = ~consumption,
               color = ~food_category,
               colors = "viridis",
               mode = 'lines', #change to lines
               text = ~paste("CO2 emission:", co2_emmission, 
                             "<br>Consumption:", consumption, 
                             "<br>Food category:", food_category)) %>% 
  layout(title = 'CO2 Emissions and Food Consumption by Food Type', 
         xaxis = list(title = "CO2 Emission"), 
         yaxis = list(title = "Consumption"),
         legend = list(title = list(text = 'Food Category'))) 

fig

#bar chart
bar <- food.data %>%
  filter(country == "USA" | country == "United Kingdom" | country == "China", #select countries
         food_category == "Beef") #select 1 food category

bar <- food.data %>%
  filter(country == "USA" | country == "United Kingdom" | country == "China",
         food_category == "Beef") %>%
  plot_ly(x = ~country, #create bar chart showing beef consumption by country
          y = ~consumption,
          type = "bar")

bar

#Created stacked bar plot
bar_stacked <- food.data %>%
  filter(country == "USA" | country == "United Kingdom" | country == "China") #filter out 3 countries of interest

bar_stacked <- food.data %>%
  filter(country == "USA" | country == "United Kingdom" | country == "China") %>% 
  group_by(country) %>% 
  mutate(total_consumption = (sum(consumption)),
         percent_consumption = ((consumption/total_consumption)*100)) #calculate amount of each food consumed as percent of total consumption

bar_stacked <- food.data %>%
  filter(country == "USA" | country == "United Kingdom" | country == "China") %>% 
  group_by(country) %>% 
  mutate(total_consumption = (sum(consumption)),
         percent_consumption = ((consumption/total_consumption)*100)) %>%
  plot_ly(x = ~country, #countries on x-axis
          y = ~percent_consumption, #percent consumption on y-axis
          color = ~food_category, #color by type of food
          type = "bar") #make it a bar chart

bar_stacked

bar_stacked <- food.data %>%
  filter(country == "USA" | country == "United Kingdom" | country == "China") %>% 
  group_by(country) %>% 
  mutate(total_consumption = (sum(consumption)),
         percent_consumption = ((consumption/total_consumption)*100)) %>%
  plot_ly(x = ~country,
          y = ~percent_consumption,
          color = ~food_category,
          text = ~paste("Total consumption:", total_consumption, "kg/person/year"), #customize hover label text
          type = "bar")

bar_stacked