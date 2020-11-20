# NOAA's OISST
# satellite sea surface temperature data

# Helpful advice from https://theoceancode.netlify.app/post/dl_env_data_r/
# Packages not available via CRAN
remotes::install_github("skgrange/threadr")
remotes::install_github("markpayneatwork/RCMEMS")

# The packages we will use
library(tidyverse) # A staple for most modern data management in R
library(RCurl) # For helping R to make sense of URLs for web hosted data
library(XML) # For reading the HTML tables created by RCurl
library(tidync) # For easily dealing with NetCDF data
library(doParallel) # For parallel processing
library(threadr) # For downloading from FTP sites that require user credentials: NOT AVAILABLE FOR R 3.6.3
library(RCMEMS) # For subsetting CMEMS data before download
# Make sure the data hasn't changed, like it had April 2020
# https://www.ncdc.noaa.gov/oisst
OISST_url_month <- "https://www.ncei.noaa.gov/data/sea-surface-temperature-optimum-interpolation/v2.1/access/avhrr/"

# Then we pull that into a happy format
# There is a lot here so it takes ~1 minute
OISST_url_month_get <- getURL(OISST_url_month)

# DO NOT DOWNLOAD ALL OF THE AVAILABLE DATA
# INSTEAD, set date and lat/lon limits
start_date <- as.Date("2019-01-01")

# Now we strip away all of the unneeded stuff to get just the months of data that are available
OISST_months <- data.frame(months = readHTMLTable(OISST_url_month_get, skip.rows = 1:2)[[1]]$Name) %>%
  mutate(months = lubridate::as_date(str_replace(as.character(months), "/", "01"))) %>% 
  filter(months >= max(lubridate::floor_date(start_date, unit = "month"))) %>% # Filtering out months before Jan 2019
  mutate(months = gsub("-", "", substr(months, 1, 7))) %>% 
  na.omit()



###################################

# Helpful code below modified from https://www.r-bloggers.com/2016/08/a-netcdf-4-in-r-cheatsheet/
# and https://pjbartlein.github.io/REarthSysSci/netCDF.html#get-coordinate-including-time-variables
# Setup
library(ncdf4)
library(reshape2)
library(dplyr) # A staple for modern data management in R
library(lubridate) # Useful functions for dealing with dates
library(ggplot2) # The preferred library for data visualisation
library(tidync) # For easily dealing with NetCDF data
library(rerddap) # For easily downloading subsets of data
library(doParallel) # For parallel processing

# This url is from Julia Mason's python code. Why is this a different location?
nc_data <- nc_open('https://psl.noaa.gov/thredds/dodsC/Aggregations/OISSThires/sst.mean.nc')
# get info about the file and save to a txt file
sink("sst.mean.nc.info.txt") # create empty txt file
print(nc_data) # get the info
sink() # close the connection to the file
# the sst.mean.nc data is a collection of files, so get the list of them
nc_data_list <- nc_data$dim

# Get a statement of the NetCDF's R attributes:
attributes(nc_data)$names
print(paste("The file has",nc_data$nvars,"variables,",nc_data$ndims,"dimensions and",nc_data$natts,"NetCDF attributes"))

# read nc-attributes
nc_data_attributes <- ncatt_get(nc_data,varid=0)
str(nc_data_attributes)
sink("sst.mean.nc.attributes.txt") # create empty txt file
print(nc_data_attributes) # get the info
sink() # close the connection to the file

# Get a list of the nc variable names.
attributes(nc_data$var)$names
#sst
# Take a look at the sst variable's attributes (units etc).
ncatt_get(nc_data, attributes(nc_data$var)$names[1])

# This function downloads and prepares data based on user provided start and end dates
OISST_sub_dl <- function(time_df){
  OISST_dat <- griddap(x = "ncdcOisst21Agg_LonPM180", 
                       url = "https://coastwatch.pfeg.noaa.gov/erddap/", 
                       time = c(time_df$start, time_df$end), 
                       zlev = c(0, 0),
                       latitude = c(-40, -35),
                       longitude = c(15, 21),
                       fields = "sst")$data %>% 
    mutate(time = as.Date(stringr::str_remove(time, "T00:00:00Z"))) %>% 
    dplyr::rename(t = time, temp = sst) %>% 
    select(lon, lat, t, temp) %>% 
    na.omit()
}






# Maybe not useful from here on out?
# get longitude and latitude
lon <- ncvar_get(nc_data,"lon")
nlon <- dim(lon)
head(lon)

lat <- ncvar_get(nc_data,"lat")
nlat <- dim(lat)
head(lat)

print(c(nlon,nlat))

# get time
time <- ncvar_get(nc_data,"time")
time

tunits <- ncatt_get(nc_data,"time","units")
nt <- dim(time)
nt
tunits

# get SST
tmp_array <- ncvar_get(nc_data,"sst")
dlname <- ncatt_get(nc_data,sst,"long_name")
dunits <- ncatt_get(nc_data,sst,"units")
fillvalue <- ncatt_get(nc_data,sst,"_FillValue")
dim(tmp_array)



## Tidyverse way but maybe not good enough
library(tidync)
# Daily 1/4 degree OISST Mean
src <- tidync('https://psl.noaa.gov/thredds/dodsC/Aggregations/OISSThires/sst.mean.nc')
print(src)

## lazy subsetting by value or index
src_slc <- src %>% 
  hyper_filter(
    time = time > 1982 & time < 2011, 
    lon = lon > -77 & lon < -65, 
    lat = lat > 34 & lat < 45) %>%
  hyper_tibble()
## Hmmm...


