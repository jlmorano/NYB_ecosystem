# Using NEFSC trawl data to explore species distribution along Northeast US coast

# setup
# setwd("/Users/janellemorano/Git/MAB_ecosystem")
library (dplyr)

fall_twl_svbio <- read_csv ("/Users/janellemorano/DATA/NEFSC_trawl/Fall_trawl/22560_FSCSTables/22560_UNION_FSCS_SVBIO.csv")
colnames(fall_twl_svbio)
# [1] "CRUISE6"     "STRATUM"     "TOW"         "STATION"     "ID"          "SVSPP"       "CATCHSEX"   
# [8] "INDID"       "LENGTH"      "INDWT"       "SEX"         "MATURITY"    "AGE"         "STOM_VOLUME"
# [15] "STOM_WGT"   


fall_twl_svcat <- read_csv ("/Users/janellemorano/DATA/NEFSC_trawl/Fall_trawl/22560_FSCSTables/22560_UNION_FSCS_SVCAT.csv")
colnames(fall_twl_svcat)
# [1] "CRUISE6"             "STRATUM"             "TOW"                 "STATION"            
# [5] "ID"                  "LOGGED_SPECIES_NAME" "SVSPP"               "CATCHSEX"           
# [9] "EXPCATCHNUM"         "EXPCATCHWT"        


fall_twl_svlen <- read_csv ("/Users/janellemorano/DATA/NEFSC_trawl/Fall_trawl/22560_FSCSTables/22560_UNION_FSCS_SVLEN.csv")
colnames(fall_twl_svlen)
# [1] "CRUISE6"             "STRATUM"             "TOW"                 "STATION"            
# [5] "ID"                  "LOGGED_SPECIES_NAME" "SVSPP"               "CATCHSEX"           
# [9] "LENGTH"              "EXPNUMLEN"      

fall_twl_svsta <- read_csv ("/Users/janellemorano/DATA/NEFSC_trawl/Fall_trawl/22560_FSCSTables/22560_UNION_FSCS_SVSTA.csv")
colnames(fall_twl_svsta)
# [1] "CRUISE6"           "STRATUM"           "TOW"               "STATION"           "ID"               
# [6] "AREA"              "SVVESSEL"          "CRUNUM"            "SVGEAR"            "BEGIN_EST_TOWDATE"
# [11] "END_EST_TOWDATE"   "BEGIN_GMT_TOWDATE" "END_GMT_TOWDATE"   "EST_YEAR"          "EST_MONTH"        
# [16] "EST_DAY"           "EST_JULIAN_DAY"    "EST_TIME"          "GMT_YEAR"          "GMT_MONTH"        
# [21] "GMT_DAY"           "GMT_JULIAN_DAY"    "GMT_TIME"          "TOWDUR"            "SETDEPTH"         
# [26] "ENDDEPTH"          "MINDEPTH"          "MAXDEPTH"          "AVGDEPTH"          "BEGLAT"           
# [31] "BEGLON"            "ENDLAT"            "ENDLON"            "DECDEG_BEGLAT"     "DECDEG_BEGLON"    
# [36] "DECDEG_ENDLAT"     "DECDEG_ENDLON"     "CABLE"             "PITCH"             "HEADING"          
# [41] "COURSE"            "RPM"               "DOPDISTB"          "DOPDISTW"          "DESSPEED"         
# [46] "AIRTEMP"           "CLOUD"             "BAROPRESS"         "WINDDIR"           "WINDSP"           
# [51] "WEATHER"           "WAVEHGT"           "SWELLDIR"          "SWELLHGT"          "BKTTEMP"          
# [56] "XBT"               "SURFTEMP"          "SURFSALIN"         "BOTTEMP"           "BOTSALIN"   

# SST
svsta_surftemp <- fall_twl_svsta %>%
  group_by(EST_YEAR) %>%
  filter(!is.na(SURFTEMP)) %>%
  summarise(avg = mean(SURFTEMP), min = min(SURFTEMP), max = max(SURFTEMP))

ggplot(data = svsta_surftemp, aes(EST_YEAR, avg)) +
  geom_line() +
  theme_bw() +
  ggtitle("Surface Temperature (from Fall: 22560_UNION_FSCS_SVSTA.csv)") +
  xlab("") + 
  ylab("Average Temp")


# Bottom Temp
svsta_bottemp <- fall_twl_svsta %>%
  group_by(EST_YEAR) %>%
  filter(!is.na(BOTTEMP)) %>%
  summarise(avg = mean(BOTTEMP), min = min(BOTTEMP), max = max(BOTTEMP))

ggplot(data = svsta_bottemp, aes(EST_YEAR, avg)) +
  geom_line() +
  theme_bw() +
  ggtitle("Bottom Temperature (from Fall: 22560_UNION_FSCS_SVSTA.csv)") +
  xlab("") + 
  ylab("Average Temp")

# Surface and Bottom Temp combined
ggplot() +
  geom_line(data = svsta_surftemp, aes(EST_YEAR, avg), color = "red") +
  geom_line(data = svsta_bottemp, aes(EST_YEAR, avg), color = "blue") +
  theme_bw() +
  theme(legend.position="bottom") +
  ggtitle("Surface and Bottom Temperature (from Fall: 22560_UNION_FSCS_SVSTA.csv)") +
  xlab("") + 
  ylab("Average Temp")


