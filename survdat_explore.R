#Using survdat data (from Kevin Friedland) to explore species distribution along Northeast US coast

# setup
# setwd("/Users/janellemorano/Git/NYB_ecosystem")

library (tidyverse)
library(sp)
library (rnaturalearth)
library (rnaturalearthdata)
library(viridis)
library(gganimate)

load("/Users/janellemorano/DATA/Survdat_3_2020.RData")
dim(survdat)
#2892498      21
head(survdat)
# unique number of values for each column
sapply(survdat, function(x) length(unique(x)))

min(survdat$YEAR)
max(survdat$YEAR)

#read list of species
spp <- read.csv("/Users/janellemorano/DATA/FHDBS_Data/FHSPECIES.csv")
class(survdat$SVSPP)


# create a subset df for each species

# 33=alewife
survdat.alewife <- subset(survdat, SVSPP %in% c(33))
survdat.alewife$species <- c("alewife")
#as_tibble(survdat.alewife)

# 34=blueback herring
survdat.bbherring <- subset(survdat, SVSPP %in% c(34))
survdat.bbherring$species <- c("blueback_herring")

# 35=shad
survdat.shad <- subset(survdat, SVSPP %in% c(35))
survdat.shad$species <- c("shad")

# 36=menhaden
survdat.menhaden <- subset(survdat, SVSPP %in% c(36))
survdat.menhaden$species <- c("menhaden")

# 103= summer flounder
survdat.sumfld <- subset(survdat, SVSPP %in% c(103))
survdat.sumfld$species <- c("summer_flounder")

# 141= black sea bass
survdat.black <- subset(survdat, SVSPP %in% c(141))
survdat.black$species <- c("black_sea_bass")

# 143= scup
survdat.scup <- subset(survdat, SVSPP %in% c(143))
survdat.scup$species <- c("scup")

# 502= shortfin squid
survdat.shortsq <- subset(survdat, SVSPP %in% c(502))
survdat.shortsq$species <- c("shortfin_squid")

# 503 = longfin squid
survdat.longsq <- subset(survdat, SVSPP %in% c(503))
survdat.longsq$species <- c("longfin_squid")


# Create basemap
world <- ne_countries(scale = "medium", returnclass = "sf")  

######## ALL SPECIES
# Plot locations of collections of multiple species (SVSPP)
# This is not pretty, need to assign different colors to each species (SVSPP)
ggplot(data = world) +
  geom_sf() +
  coord_sf (xlim = c(-77,-65), ylim = c (34,45), expand = FALSE ) +
  geom_point(data = subset(survdat, SVSPP %in% c(33, 34, 35, 36, 103, 141, 143, 502, 503)),
             aes (x = LON, y = LAT, color = YEAR)) + #remove , size=BIOMASS for survey loc
  scale_color_continuous(type = "viridis") +
  scale_fill_viridis() +
  theme_bw() +
  theme (axis.text = element_blank()) +
  facet_wrap(~SVSPP) +
  xlab("longitude") + 
  ylab("latitude") #+
  # gganimate code
  # ggtitle("{YEAR}") +
  # transition_time(SVSPP$YEAR) +
  # ease_aes("linear") +
  # enter_fade() +
  # exit_fade()

# Trying animation, but not working

# animate(plot, fps = 10)
# anim_save("test.gif")

## Plot presence of each species over the years (SVSPP)
# NOT WORKING loop function
key = list(survdat.alewife, survdat.bbherring, survdat.black, survdat.longsq, survdat.menhaden, survdat.scup, survdat.shad, survdat.shad, survdat.shortsq, survdat.sumfld)
for (i in key) {
  print(ggplot(data = world) +
    geom_sf() +
    coord_sf (xlim = c(-77,-65), ylim = c (34,45), expand = FALSE ) +
    geom_point(data = i,
               aes (x = LON, y = LAT,
                    color = YEAR)) + 
    scale_color_continuous(type = "viridis") +
    scale_fill_viridis() +
    theme_bw() +
    theme (axis.text = element_blank()) +
    xlab("longitude") + 
    ylab("latitude") +
    ggtitle("Alewife"))
}

# Presence for each species (not in a loop)
#NY Bight region
ggplot(data = world) +
  geom_sf() +
  coord_sf (xlim = c(-75,-70.5), ylim = c (39,42), expand = FALSE ) +
  geom_point(data = survdat.longsq,
             aes (x = LON, y = LAT,
                  color = YEAR)) + 
  scale_color_continuous(type = "viridis") +
  scale_fill_viridis() +
  theme_bw() +
  theme (axis.text = element_blank()) +
  xlab("longitude") + 
  ylab("latitude") +
  ggtitle("Longfin Squid") +
  ggsave('Presence_1968-2019_LongfinSquid_NYBight.png') 


####### ALEWIFE
#Presence in NY Bight region, 2014:2019
ggplot(data = world) +
  geom_sf() +
  coord_sf (xlim = c(-75,-70.5), ylim = c (39,42), expand = FALSE ) +
  geom_point(data = subset(survdat.alewife, YEAR %in% c(2014:2019)),
             aes (x = LON, y = LAT,
                  color = BIOMASS)) + 
  scale_color_continuous(type = "viridis") +
  scale_fill_viridis() +
  theme_bw() +
  theme (axis.text = element_blank()) +
  xlab("longitude") + 
  ylab("latitude") +
  ggtitle("Alewife") +
  ggsave('BIOMASS_2014-2019_Alewife_NYBight.png') +
  facet_wrap(~YEAR)


# Average biomass per year
annual.bio <- survdat.alewife %>%
  group_by(YEAR) %>%
  summarise(avg = mean(BIOMASS))

ggplot(data = annual.bio, aes(YEAR, avg)) +
  geom_line() +
  theme_bw() +
  ggtitle("Alewife") +
  xlab("") + 
  ylab("Average Biomass (?)")

#Biomass at survey sites across entire survey area, each year for ALEWIFE
ggplot(data = world) +
  geom_sf() +
  coord_sf (xlim = c(-78,-64), ylim = c (34,45), expand = FALSE ) +
  geom_point(data = filter(survdat.alewife, YEAR %in% 2011:2019),
             aes (x = LON, y = LAT,
                  color = BIOMASS,
                  shape = ifelse(BIOMASS == 0, '21'))) +
  scale_color_continuous(type = "viridis") +
  ggtitle("Alewife") +
  xlab("longitude") + 
  ylab("latitude")+
  theme_bw() + #or theme_classic()
  facet_wrap (~YEAR, ncol = 3) # nrow = 3

#Biomass at survey sites across entire survey area, since 2000 for ALEWIFE
ggplot(data = world) +
  geom_sf() +
  coord_sf (xlim = c(-78,-64), ylim = c (34,45), expand = FALSE ) +
  geom_point(data = survdat.alewife,
             aes (x = LON, y = LAT,size = BIOMASS, color = BIOMASS, stroke = 1)) +
  scale_shape_manual(values = c(21), name = 'BIOMASS') +
  scale_color_continuous(type = "viridis") +
  ggtitle("Alewife") +
  xlab("longitude") + 
  ylab("latitude")+
  theme_bw() + #or theme_classic()
  facet_wrap (~YEAR)

# Let's try animation for each year
# First, start with 2 years to look as I want
library(gganimate)
library(magick) #not sure if needed
library(gifski) #not sure if needed

ggplot(data = world) +
  geom_sf() +
  coord_sf (xlim = c(-78,-64), ylim = c (34,45), expand = FALSE ) +
  geom_point(data = subset(survdat.alewife, YEAR %in% c(2000:2019)),
             aes (x = LON, y = LAT,
                  color = BIOMASS,
                  group = seq_along(YEAR))) +
  scale_color_continuous(type = "viridis") +
  ggtitle("Alewife") +
  xlab("longitude") + 
  ylab("latitude")+
  theme_bw() +
  transition_states(YEAR,
                    transition_length = 2,
                    state_length = 1) +
  enter_fade() +
  exit_shrink() + 
  anim_save("alewife_animation_2000-2019.gif")


######## BLUEBACK HERRING
# Average biomass per year
annual.bio <- survdat.bbherring %>%
  group_by(YEAR) %>%
  summarise(avg = mean(BIOMASS))

ggplot(data = annual.bio, aes(YEAR, avg)) +
  geom_line() +
  theme_bw() +
  ggtitle("Blueback Herring") +
  xlab("") + 
  ylab("Average Biomass (?)")

#Presence in NY Bight region, 2014:2019
ggplot(data = world) +
  geom_sf() +
  coord_sf (xlim = c(-75,-70.5), ylim = c (39,42), expand = FALSE ) +
  geom_point(data = subset(survdat.bbherring, YEAR %in% c(2014:2019)),
             aes (x = LON, y = LAT,
                  color = BIOMASS)) + 
  scale_color_continuous(type = "viridis") +
  scale_fill_viridis() +
  theme_bw() +
  theme (axis.text = element_blank()) +
  xlab("longitude") + 
  ylab("latitude") +
  ggtitle("Blueback Herring") +
  ggsave('BIOMASS_2014-2019_BluebackHerring_NYBight.png') +
  facet_wrap(~YEAR)


# View over the entire survey area, for one year at a time with subsetting
ggplot(data = world) +
  geom_sf() +
  coord_sf (xlim = c(-78,-64), ylim = c (34,45), expand = FALSE ) +
  geom_point (data = subset(survdat.bbherring, YEAR %in% c(2000)),
              aes (x = LON, y = LAT, color = BIOMASS), 
              alpha = 0.7) +
  scale_fill_viridis() +
  theme_bw() +
  ggtitle("Blueback Herring") +
  xlab("longitude") + 
  ylab("latitude")

#Biomass at survey sites across entire survey area, each year for blueback herring
ggplot(data = world) +
  geom_sf() +
  coord_sf (xlim = c(-78,-64), ylim = c (34,45), expand = FALSE ) +
  geom_point(data = survdat.bbherring,
             aes (x = LON, y = LAT,size = BIOMASS, color = BIOMASS)) +
  scale_color_continuous(type = "viridis") +
  ggtitle("Blueback Herring") +
  xlab("longitude") + 
  ylab("latitude")+
  theme_classic() + #or theme_bw()
  facet_wrap (~YEAR)


# Look at each average annual biomass by year side-by-side
ggplot(data = world) +
  geom_sf() +
  coord_sf (xlim = c(-75,-69), ylim = c (39,42), expand = FALSE ) +
  stat_summary_2d(data = survdat.bbherring, 
                  aes (x = LON, y = LAT, z = BIOMASS), 
                  fun = mean, binwidth = c (.25, .25)) +
  scale_fill_viridis_c(name = "Mean log biomass") +
  theme_bw() +
  facet_wrap (~YEAR)

# Plot and animate by biomass of 1 species (SVSPP) at location each year, 34=blueback herring
library(gganimate)

ggplot(data = world) +
  geom_sf() +
  coord_sf (xlim = c(-75,-69), ylim = c (39,42), expand = FALSE ) +
  geom_point (data = subset(survdat, SVSPP %in% c(34)), aes (x = LON, y = LAT, size = BIOMASS, color = BIOMASS)) +
  scale_fill_viridis() +
  theme_bw() +
  #theme(legend.position = "bottom") +
  ggtitle("Blueback Herring {YEAR}") +
  xlab("longitude") + 
  ylab("latitude") +
  gganimate::transition_states(YEAR,
                    transition_length = 10,
                    state_length = 1)


###### SHAD
# Average biomass per year
annual.bio <- survdat.shad %>%
  group_by(YEAR) %>%
  summarise(avg = mean(BIOMASS))

ggplot(data = annual.bio, aes(YEAR, avg)) +
  geom_line() +
  theme_bw() +
  ggtitle("Shad") +
  xlab("") + 
  ylab("Average Biomass (?)")

#Biomass in NY Bight region, 2014:2019
ggplot(data = world) +
  geom_sf() +
  coord_sf (xlim = c(-75,-70.5), ylim = c (39,42), expand = FALSE ) +
  geom_point(data = subset(survdat.shad, YEAR %in% c(2014:2019)),
             aes (x = LON, y = LAT,
                  color = BIOMASS)) + 
  scale_color_continuous(type = "viridis") +
  scale_fill_viridis() +
  theme_bw() +
  theme (axis.text = element_blank()) +
  xlab("longitude") + 
  ylab("latitude") +
  ggtitle("Shad") +
  ggsave('BIOMASS_2014-2019_Shad_NYBight.png') +
  facet_wrap(~YEAR)


###### MENHADEN
# Average biomass per year
annual.bio <- survdat.menhaden %>%
  group_by(YEAR) %>%
  summarise(avg = mean(BIOMASS))

ggplot(data = annual.bio, aes(YEAR, avg)) +
  geom_line() +
  theme_bw() +
  ggtitle("Menhaden") +
  xlab("") + 
  ylab("Average Biomass (?)")

#Biomass in NY Bight region, 2014:2019
ggplot(data = world) +
  geom_sf() +
  coord_sf (xlim = c(-75,-70.5), ylim = c (39,42), expand = FALSE ) +
  geom_point(data = subset(survdat.menhaden, YEAR %in% c(2010:2013)),
             aes (x = LON, y = LAT,
                  color = BIOMASS)) + 
  scale_color_continuous(type = "viridis") +
  scale_fill_viridis() +
  theme_bw() +
  theme (axis.text = element_blank()) +
  xlab("longitude") + 
  ylab("latitude") +
  ggtitle("Menhaden") +
  ggsave('BIOMASS_2010-2013_Menhaden_NYBight.png') +
  facet_wrap(~YEAR)

###### SUMMER FLOUNDER
# Average biomass per year
annual.bio <- survdat.sumfld %>%
  group_by(YEAR) %>%
  summarise(avg = mean(BIOMASS))

ggplot(data = annual.bio, aes(YEAR, avg)) +
  geom_line() +
  theme_bw() +
  ggtitle("Summer Flounder") +
  xlab("") + 
  ylab("Average Biomass (?)")

#NY Bight region
ggplot(data = world) +
  geom_sf() +
  coord_sf (xlim = c(-75,-70.5), ylim = c (39,42), expand = FALSE ) +
  geom_point(data = survdat.sumfld,
             aes (x = LON, y = LAT,
                  color = YEAR)) + 
  scale_color_continuous(type = "viridis") +
  scale_fill_viridis() +
  theme_bw() +
  theme (axis.text = element_blank()) +
  xlab("longitude") + 
  ylab("latitude") +
  ggtitle("Summer Flounder") +
  ggsave('Presence_1968-2019_SummerFlounder_NYBight.png') 

ggplot(data = world) +
  geom_sf() +
  coord_sf (xlim = c(-75,-70.5), ylim = c (39,42), expand = FALSE ) +
  geom_point(data = subset(survdat.sumfld, YEAR %in% c(2014:2019)),
             aes (x = LON, y = LAT,
                  color = BIOMASS)) + 
  scale_color_continuous(type = "viridis") +
  scale_fill_viridis() +
  theme_bw() +
  theme (axis.text = element_blank()) +
  xlab("longitude") + 
  ylab("latitude") +
  ggtitle("Summer Flounder") +
  ggsave('BIOMASS_2014-2019_SummerFlounder_NYBight.png') +
  facet_wrap(~YEAR)

###### BLACK SEA BASS
# Average biomass per year
annual.bio <- survdat.black %>%
  group_by(YEAR) %>%
  summarise(avg = mean(BIOMASS))

ggplot(data = annual.bio, aes(YEAR, avg)) +
  geom_line() +
  theme_bw() +
  ggtitle("Black Sea Bass") +
  xlab("") + 
  ylab("Average Biomass (?)")

# Presence NYBight
ggplot(data = world) +
  geom_sf() +
  coord_sf (xlim = c(-75,-70.5), ylim = c (39,42), expand = FALSE ) +
  geom_point(data = survdat.alewife,
             aes (x = LON, y = LAT,
                  color = YEAR)) + 
  scale_color_continuous(type = "viridis") +
  scale_fill_viridis() +
  theme_bw() +
  theme (axis.text = element_blank()) +
  xlab("longitude") + 
  ylab("latitude") +
  ggtitle("Alewife") +
  ggsave('Presence_1968-2019_Alewife_NYBight.png') 

# Biomass 2014-2019
ggplot(data = world) +
  geom_sf() +
  coord_sf (xlim = c(-75,-70.5), ylim = c (39,42), expand = FALSE ) +
  geom_point(data = subset(survdat.black, YEAR %in% c(2014:2019)),
             aes (x = LON, y = LAT,
                  color = BIOMASS)) + 
  scale_color_continuous(type = "viridis") +
  scale_fill_viridis() +
  theme_bw() +
  theme (axis.text = element_blank()) +
  xlab("longitude") + 
  ylab("latitude") +
  ggtitle("Black Seabass") +
  ggsave('BIOMASS_2014-2019_BlackSeabass_NYBight.png') +
  facet_wrap(~YEAR)

###### SCUP
# Average biomass per year
annual.bio <- survdat.scup %>%
  group_by(YEAR) %>%
  summarise(avg = mean(BIOMASS))

ggplot(data = annual.bio, aes(YEAR, avg)) +
  geom_line() +
  theme_bw() +
  ggtitle("Scup") +
  xlab("") + 
  ylab("Average Biomass (?)")

#Biomass in NY Bight region, 2014:2019
ggplot(data = world) +
  geom_sf() +
  coord_sf (xlim = c(-75,-70.5), ylim = c (39,42), expand = FALSE ) +
  geom_point(data = subset(survdat.scup, YEAR %in% c(2014:2019)),
             aes (x = LON, y = LAT,
                  color = BIOMASS)) + 
  scale_color_continuous(type = "viridis") +
  scale_fill_viridis() +
  theme_bw() +
  theme (axis.text = element_blank()) +
  xlab("longitude") + 
  ylab("latitude") +
  ggtitle("Scup") +
  ggsave('BIOMASS_2014-2019_Scup_NYBight.png') +
  facet_wrap(~YEAR)

####### SHORTFIN SQUID
# Average biomass per year
annual.bio <- survdat.shortsq %>%
  group_by(YEAR) %>%
  summarise(avg = mean(BIOMASS))

ggplot(data = annual.bio, aes(YEAR, avg)) +
  geom_line() +
  theme_bw() +
  ggtitle("Shortfin Squid") +
  xlab("") + 
  ylab("Average Biomass (?)")

#Biomass in NY Bight region, 2014:2019
ggplot(data = world) +
  geom_sf() +
  coord_sf (xlim = c(-75,-70.5), ylim = c (39,42), expand = FALSE ) +
  geom_point(data = subset(survdat.shortsq, YEAR %in% c(2014:2019)),
             aes (x = LON, y = LAT,
                  color = BIOMASS)) + 
  scale_color_continuous(type = "viridis") +
  scale_fill_viridis() +
  theme_bw() +
  theme (axis.text = element_blank()) +
  xlab("longitude") + 
  ylab("latitude") +
  ggtitle("Shortfin Squid") +
  ggsave('BIOMASS_2014-2019_ShortfinSquid_NYBight.png') +
  facet_wrap(~YEAR)

###### LONGFIN SQUID
# Average biomass per year
annual.bio <- survdat.longsq %>%
  group_by(YEAR) %>%
  summarise(avg = mean(BIOMASS))

ggplot(data = annual.bio, aes(YEAR, avg)) +
  geom_line() +
  theme_bw() +
  ggtitle("Longfin Squid") +
  xlab("") + 
  ylab("Average Biomass (?)")

#Biomass in NY Bight region, 2014:2019
ggplot(data = world) +
  geom_sf() +
  coord_sf (xlim = c(-75,-70.5), ylim = c (39,42), expand = FALSE ) +
  geom_point(data = subset(survdat.longsq, YEAR %in% c(2014:2019)),
             aes (x = LON, y = LAT,
                  color = BIOMASS)) + 
  scale_color_continuous(type = "viridis") +
  scale_fill_viridis() +
  theme_bw() +
  theme (axis.text = element_blank()) +
  xlab("longitude") + 
  ylab("latitude") +
  ggtitle("Longfin Squid") +
  ggsave('BIOMASS_2014-2019_LongfinSquid_NYBight.png') +
  facet_wrap(~YEAR)

####### Sea Surface TEMPERATURE
sst <- survdat %>%
  group_by(YEAR) %>%
  summarise(avg = mean(SURFTEMP))

ggplot(data = sst, aes(YEAR, avg)) +
  geom_line() +
  theme_bw() +
  ggtitle("SST") +
  xlab("") + 
  ylab("Average Temp")


####### Bottom TEMPERATURE
# Annual bottom temp
bottemp <- survdat %>%
  group_by(YEAR) %>%
  summarise(avg = mean(BOTTEMP))

ggplot(data = bottemp, aes(YEAR, avg)) +
  geom_line() +
  theme_bw() +
  ggtitle("Bottom Temperature") +
  xlab("") + 
  ylab("Average Temp")

# Monthly bottom temp
bottemp.mon <- survdat %>%
  group_by(YEAR) %>%
  summarise(avg = mean(BOTTEMP))

ggplot(data = bottemp, aes(YEAR, avg)) +
  geom_line() +
  theme_bw() +
  ggtitle("Bottom Temperature") +
  xlab("") + 
  ylab("Average Temp")


# Surface and Bottom Temp combined
# Data from survdat are incomplete, so built this with just the Fall survey data in NEFSC_trawl_explore script
ggplot() +
  geom_line(data = sst, aes(YEAR, avg), color = "red") +
  geom_line(data = bottemp, aes(YEAR, avg), color = "blue") +
  theme_bw() +
  theme(legend.position="bottom") +
  ggtitle("Surface and Bottom Temperature") +
  xlab("") + 
  ylab("Average Temp")

######### Survey strata
library(sf)
strata <- readOGR("/Users/janellemorano/DATA/strata/finstr_nad83.shp")

####### PLANKTON
# Add plankton
plankton <- read.csv("/Users/janellemorano/DATA/plankton/1.1/data/0-data/EcoMon_Plankton_Data_v3_5.csv")


