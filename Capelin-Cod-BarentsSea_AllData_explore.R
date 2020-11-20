# Capelin and Cod Abundance in Barents Sea

# Data are from acoustic surveys by IMR
######################################################################

##########
# Setup
##########
# Working directory
setwd("/Users/janellemorano/Git/Reference-R-scripts/Spatial-Stats/Cod-Capelin-Project")

# Libraries

library(sf)
library (rnaturalearth)
library (rnaturalearthdata)
library(viridis)

 
#All of the data from 2007-2013
barents = read.table("Norwegian part of ecosystem survey cod-capelin 2004-2015.txt", sep = " ",header = T)
head(barents)
dim(barents)
# 2455   42

world <- ne_countries(scale = "medium", returnclass = "sf")  

# Plot locations of surveys
ggplot(data = world) +
  geom_sf() +
  coord_sf (xlim = c(-20, 75), ylim = c (65,85), expand = FALSE ) +
  geom_point (aes (x = lon, y = lat), 
              alpha = 0.7, 
              data = barents) + 
  theme_bw()
# theme (axis.text = element_blank())



