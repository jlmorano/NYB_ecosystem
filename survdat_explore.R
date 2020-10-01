#Using survdat data (from Kevin Friedland) to explore species distribution along Northeast US coast

# setup
# setwd("/Users/janellemorano/Git/MAB_ecosystem")
load("/Users/janellemorano/Git/MAB_ecosystem/data/Survdat_3_2020.RData")
dim(survdat)
#2892498      21
head(survdat)
# unique number of values for each column
sapply(survdat, function(x) length(unique(x)))

#read list of species
spp <- read.csv("/Users/janellemorano/Git/MAB_ecosystem/data/FHDBS_Data/FHSPECIES.csv")
class(survdat$SVSPP)
# 33=alewife
# 34=blueback herring
# 35=shad
# 36=menhaden
# 103= summer flounder
# 141= black sea bass
# 143= scup
# 502= shortfin squid
# 503 = longfin squid

library(sp)
library(tidyverse)

#make each year a unique number for reference
iyear = unique(survdat$YEAR)
# 1963-2019

#assign the lat and lon
fortify.survdat <-fortify(as.data.frame(survdat))
names(fortify.survdat)
dim(fortify.survdat)
flon<-fortify.survdat$LON
flat<-fortify.survdat$LAT


# Plot by multiple species (SVSPP)
# 33=alewife
# 34=blueback herring
# 35=shad
# 36=menhaden
# 103= summer flounder
# 141= black sea bass
# 143= scup
# 502= shortfin squid
# 503 = longfin squid
ggplot(data = world) +
  geom_sf() +
  coord_sf (xlim = c(-77,-65), ylim = c (34,45), expand = FALSE ) +
  geom_point (aes (x = LON, y = LAT, color = SVSPP), 
              alpha = 0.7,
              data = subset(survdat, SVSPP %in% c(33, 34, 35, 36, 103, 141, 143, 502, 503))) +
  scale_fill_viridis() +
  theme_bw() +
  theme (axis.text = element_blank())


# Plot by 1 species (SVSPP), 34=blueback herring
ggplot(data = world) +
  geom_sf() +
  coord_sf (xlim = c(-77,-65), ylim = c (34,45), expand = FALSE ) +
  geom_point (aes (x = LON, y = LAT, color = YEAR), 
              alpha = 0.7,
              data = subset(survdat, SVSPP %in% c(34))) +
  scale_fill_viridis() +
  theme_bw() +
  ggtitle("Blueback Herring") +
  xlab("longitude") + 
  ylab("latitude")
