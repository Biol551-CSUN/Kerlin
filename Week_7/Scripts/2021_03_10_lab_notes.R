### GGMaps ######################################
### Working with ggmaps package
### Created by Jamie Kerlin
### Created on 2021-03-10
#################################################


### Load libraries ##############################
library(ggmap)
library(tidyverse)
library(here)
library(ggsn)

### Register google key API #####################
#Do not save this to github
register_google(key = "YOUR KEY HERE")

### Load data ##################################
chemdata <- read_csv(here("Week_7", "Data", "chemicaldata_maunalua.csv"))
oahu <- get_map("Oahu")

### Plot base layer ############################
ggmap(oahu)

### Create Wailupe map #########################
wp <- data.frame(lon = -157.7621, lat = 21.27427)
map1 <- get_map(wp)
ggmap(map1)

### Can zoom in ################################
map2 <- get_map(wp, zoom = 17)
ggmap(map2)

### Change map type ############################
map3 <- get_map(wp, zoom = 17, maptype = "satellite")
ggmap(map3)

map4 <- get_map(wp, zoom = 17, maptype = "watercolor")
ggmap(map4)

### Add to ggplot ##############################
map5 <- get_map(wp, zoom = 17, maptype = "satellite")
ggmap(map5) +
  geom_point(data = chemdata,
             aes(x = Long, y = Lat, color = Salinity),
             size = 2) +
  scale_color_viridis_c() +
  scalebar( x.min = -157.766, x.max = -157.758,
            y.min = 21.2715, y.max = 21.2785,
            dist = 250, dist_unit = "m", model = "WGS84",
            transform = TRUE, st.color = "white",
            box.fill = c("yellow", "white"))

### Can use geocode to get exact locations #######
geocode("the white house")
geocode("California State University, Northridge")
geocode("Empire Polo Club")



