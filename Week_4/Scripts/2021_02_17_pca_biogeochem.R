#### BIOL 551 PCA Biogeochemical Data #################################
#Data wrangling Nyssa's Hawaii data
#Created by Jamie Kerlin
#Created on 2021-02-17
####################################################################

### Load Libraries #################################################
library(tidyverse)
library(here)
library(ggbiplot)
library(ggfortify)

### Load data ######################################################
chemdata <- read_csv(here("Week_4", 
                          "Data", 
                          "chemicaldata_maunalua.csv"))
view(chemdata)

### Clean data ####################################################
chemdata <- chemdata %>%
  filter(complete.cases(.)) #remove all NAs

chemdata1 <- chemdata %>%
  select(!Waypoint:Tide_time) #deselect columns with names

### Create and run PCA ###########################################
chemdata.scale <- scale(chemdata1, scale=TRUE, center=TRUE) #create PCA

PCAmodel <- princomp(chemdata.scale, cor=FALSE) #Run PCA
#in princomp, the default is to use the covariance matrix. If you want to use the correlation
#matrix, then must change this to cor=TRUE. Here we converted to z-scores first, so all 
#variables are on the same scale and we can use the covariance matrix

### Plot ########################################################
ggbiplot(PCAmodel, obs.scale= .1, var.scale = .1, #add observation and variance scale
         groups=chemdata$Season, #add groups to compare
         ellipse=TRUE, varname.size=3.5, #add ellipses between groups and change variable name size
         varname.adjust = 1, repel = TRUE, circle=FALSE, scale = 1) + #trying to fix name overlap
  scale_color_discrete(name='') + #color
  geom_point(aes(colour=factor(chemdata$Season)), size = .2, alpha = 0.5) + #Color codes by Site
  theme(legend.direction = 'vertical', legend.position='right', legend.text=element_text(size=7)) + #change legend
  theme_classic() + #change general theme
  labs(title = "Principal Components Analysis of Hawaii Biogeochemical Data by Season",
       subtitle = "Salinity, Total Alkalinity (TA), Nitrate + Nitrite (NN), Phosphate, Percent Groundwater (percent_sgd), Silicate, 
       pH, Temperature in situ (Temp_in)") + #add title and subtitle 
  ggsave(here("Week_4", "Output", "PCA_biogeochem.png"))

