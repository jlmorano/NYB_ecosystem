# NY Peconic Bay Trawl Survey (PBTS)

# setup
# setwd("/Users/janellemorano/Git/NYB_ecosystem")
PBTS <- read.csv(file = "/Users/janellemorano/DATA/NYPeconic Trawl Survey/PBTS stations.csv", head = TRUE)
dim(PBTS)
# 77  3
head(PBTS)
# unique number of values for each column
sapply(PBTS, function(x) length(unique(x)))
# Station Lat.dd.ddddd Lon.dd.ddddd 
# 77           29           38 
# Rename headers
PBTS$LAT <- PBTS$Lat.dd.ddddd
PBTS$LON <- PBTS$Lon.dd.ddddd

library(tidyverse)
library(sp)
library(rnaturalearth)
library(rnaturalearthdata)
library(viridis)


# Create basemap
world <- ne_countries(scale = "medium", returnclass = "sf")  

# Plot survey locations
ggplot(data = world) +
  geom_sf() +
  coord_sf (xlim = c(-74,-71), ylim = c (40.5,41.5), expand = FALSE ) +
  geom_point(data = PBTS,
             aes (x = LON, y = LAT)) + 
  theme_bw() +
  theme (axis.text = element_blank()) +
  xlab("longitude") + 
  ylab("latitude")

# Other basemap options
library (ggmap)

peconic_basemap <- get_map (location = c(-72.7, 40.75, -72.1, 41.15), source = "stamen", maptype = "terrain", crop = FALSE)
ggmap(peconic_basemap) +
  geom_point (aes (x = LON, y = LAT), data = PBTS) 
