# Using OceanAdapt data to explore species distribution along Northeast US coast

# setup
# setwd("/Users/janellemorano/Git/MAB_ecosystem")
ocean <- read.csv("/Users/janellemorano/Git/MAB_ecosystem/data/oceanadapt_20200923.csv", header = TRUE)
dim(ocean)
head(ocean)
# unique number of values for each column
sapply(ocean, function(x) length(unique(x)))

library(sp)
library(tidyverse)

#make each year a unique number for reference
iyear = unique(ocean$Year)
# 1972-2017




#change 0 lat/long to NA
ocean[which(ocean$Latitude==0)] = NA

#assign the lat and lon
fortify.ocean <-fortify(as.data.frame(ocean))
names(fortify.ocean)
dim(fortify.ocean)
flon<-fortify.ocean$Longitude
flat<-fortify.ocean$Latitude
ggplot(data=fortify.ocean, aes(flon, flat)) +
  geom_point() +
  theme_classic() +
  ggtitle("NES Survey from OceanAdapt") +
  xlab("longitude") + 
  ylab("latitude")
#plotting the 0 lat/long, but haven't fixed it yet

#plot location of species
ggplot(data=fortify.ocean, aes(flon, flat))+
  geom_point(aes(color = Species)) +
  scale_color_continuous(type = "viridis") 


# one option: ggplot with rnaturalearth----
#https://www.r-spatial.org/r/2018/10/25/ggplot2-sf.html
library (sf)
library (rnaturalearth)
library (rnaturalearthdata)

world <- ne_countries(scale = "medium", returnclass = "sf")  

# Plot location of all species in NES
ggplot(data = world) +
  geom_sf() +
  coord_sf (xlim = c(-77,-65), ylim = c (34,45), expand = FALSE ) +
  geom_point (aes (x = Longitude, y = Latitude), 
              alpha = 0.7, 
              data = ocean) +
  theme_bw() +
  theme (axis.text = element_blank())

# Now add color
ggplot(data = world) +
  geom_sf() +
  coord_sf (xlim = c(-77,-65), ylim = c (34,45), expand = FALSE ) +
  geom_point (aes (x = Longitude, y = Latitude, color = Species), 
              alpha = 0.7, 
              data = ocean) +
  theme_bw() +
  theme (axis.text = element_blank())

# Plot just Brevoortia tyrannus (menhaden), Shad Alosa sapidissima, Alosa pseudoharengus (alewife), Alosa aestivalis, Centropristis striata (black sea bass), Paralichthys dentatus (summer flounder), Stenotomus chrysops(scup)
ggplot(data = world) +
  geom_sf() +
  coord_sf (xlim = c(-77,-65), ylim = c (34,45), expand = FALSE ) +
  geom_point (aes (x = Longitude, y = Latitude, color = Species), 
              alpha = 0.7,
              data = subset(ocean, Species %in% c("Brevoortia tyrannus", "Alosa sapidissima", "Alosa pseudoharengus", "Alosa aestivalis", "Centropristis striata", "Paralichthys dentatus", "Stenotomus chrysops"))) +
  scale_fill_viridis() +
  theme_bw() +
  theme (axis.text = element_blank())



# another option: ggmap, fancier options for basemaps----
library (ggmap)

isl_basemap <- get_map (location = c(-77, 34, -65, 45), source = "stamen", maptype = "terrain", crop = FALSE)

ggmap(isl_basemap) +
  geom_point (aes (x = Longitude, y = Latitude),
              data = subset(ocean, Species %in% c("Brevoortia tyrannus", "Alosa sapidissima", "Alosa pseudoharengus", "Alosa aestivalis", "Centropristis striata", "Paralichthys dentatus", "Stenotomus chrysops")))

