## Download CMEMS SSH data -- this is for downloading sea surface height netCDFs from Copernicus (https://marine.copernicus.eu/), where you register and get a password for the FTP 

# Julia Mason
# 11 5 2020

library (RCurl)
library(R.utils)
library (tidyverse)
library (lubridate)

# Code from Heather Welch

# Waitfor function (from R_Turtlewatch.R) -- I think this is to keep servers from crashing or thinking you're a bot

waitfor <- function (x) {
  p1 <- proc.time()
  Sys.sleep(x)
  print (proc.time() - p1)
}

cmems.pwd <- "jmason:JuliaCMEMS2017"

url <- "ftp://ftp.sltac.cls.fr/Core/SEALEVEL_GLO_PHY_L4_REP_OBSERVATIONS_008_047/dataset-duacs-rep-global-merged-allsat-phy-l4-v3/"

name = "MSLA_all" # prefix for how I'm going to name the data files on my local machine 

acquire_CMEMS <- function (date) { # date is in the form YYYYMMDD
  
  # calculate year as first four digits of date
  year <- substr (date, 1, 4)
  
  filenames <- getURL(paste (url, year, "/", sep = ""),  
                      userpwd = cmems.pwd,
                      ftp.use.epsv = FALSE,
                      ssl.verifypeer = FALSE,
                      dirlistonly = TRUE
  ) ## this is clunky by necessity. The CMEMS files are named by the date they were uploaded to the ftp site, therefore there is no way to predict the actual name of the file for the date we are interested in. So we go a roundabout way:
  
  waitfor(2)
  
  list_filenames <- unlist(strsplit(filenames,"\r\n")) ## get a list of all the files in the CMEMS directory
  
  string <- grep (date, list_filenames, value=TRUE) # find the file that corresponds to my date of interest
  
  if(length (string) > 0){
    
    # grab data behind url
    data <- getBinaryURL( 
      paste (url, year, "/", string, sep = ""),
      userpwd = cmems.pwd,
      ftp.use.epsv = FALSE,
      ssl.verifypeer = FALSE,
      noprogress = FALSE
    ) 
    
    waitfor(2)
    
    # write data to a file
    con <- file (
      paste(getwd(), "/Data_tables/CMEMS_SSH/", name, date, ".nc.gz", sep=""),
      open = "wb"
    )  
    writeBin(data,con)
    
    waitfor(2)
    
    close(con)
    
    # unzip the file
    gunzip(paste(getwd(), "/Data_tables/CMEMS_SSH/", name, date,".nc.gz",sep=""),
           ext="gz", 
           FUN=gzfile,
           overwrite = TRUE) 
  }
}

# make a vector of the dates in my survey data, in the YYYYMMDD format
sz.PA <- read_csv ("Data_tables/juv_sz_P_A_models.csv")
sz.dates <- paste (year(sz.PA$Date), 
                   ifelse (month (sz.PA$Date) < 10, paste ("0", month (sz.PA$Date), sep = ""),
                           month (sz.PA$Date)),
                   ifelse (day(sz.PA$Date) < 10, paste ("0", day(sz.PA$Date), sep = ""),
                           day(sz.PA$Date)),
                   sep = "")


## Download

lapply (sz.dates, acquire_CMEMS)


### Open and compile my downloaded data, and match to my survey data of hammerhead shark presences ----

library (ncdf4)
library (tidyverse)
library (lubridate)


# open sample file to see what it looks like, what the variables are
sla_tmp <- nc_open("Data_tables/CMEMS_SSH/MSLA_all20001117.nc") # opens the netCDF file
print (sla_tmp) # view metadata

# best practice to close
nc_close (sla_tmp)

sz.PA <- read_csv ("Data_tables/juv_sz_P_A_models.csv") # my presence/absence hammerhead points


# make dates in YYYYMMDD format, and as numeric to calculate difference
sz.PA.SSH <- sz.PA %>%
  filter (Lon < 0 & Lat < 0 & Date > 0) %>% # remove NA
  select (Lance.code, Date, Lon, Lat) %>% # pare down the columns I want
  mutate (date.code =  as.numeric (
    paste (year(Date), 
           ifelse (month (Date) < 10, 
                   paste ("0", month (Date), sep = ""),
                   month (Date)),
           ifelse (day(Date) < 10, 
                   paste ("0", day(Date), sep = ""),
                   day(Date)),
           sep = "")
  ),
  adt = NA, # blank columns to hold the CMEMS SSH data
  sla = NA
  )
# 4119 

# Generate a list of all the files I've downloaded in my CMEMS folder
all_filenames <- list.files (path = "Data_tables/CMEMS_SSH")

for (i in 1:nrow (sz.PA.SSH)) {
  
  # grab the file with the relevant date
  day_file <- grep (sz.PA.SSH$date.code[i], all_filenames, value = TRUE)
  
  # open the netcdf
  ssh_open <- nc_open (paste ("Data_tables/CMEMS_SSH/", day_file, sep = ""))
  
  # grab the relevant variables: lat, lon, adt (don't remember what this stands for), sla (sea level anomaly)
  lat <- ncvar_get (ssh_open, "latitude")
  # lon is 0-360
  lon <- ncvar_get (ssh_open, "longitude") - 180
  
  adt <- ncvar_get (ssh_open, "adt")
  sla <- ncvar_get (ssh_open, "sla")
  
  # reshape into data frame
  # http://geog.uoregon.edu/bartlein/courses/geog490/week04-netCDF.html
  
  lonlat <- as.matrix (expand.grid (lon, lat))
  adt_vec <- as.vector (adt)
  sla_vec <- as.vector (sla)
  
  df  <- data.frame (cbind (lonlat, adt_vec, sla_vec))
  names (df) <- c ("lon", "lat", "adt", "sla")
  
  # find closest lat and lon to my survey points
  # https://stackoverflow.com/questions/20916013/how-to-match-data-in-two-columns-lat-long-to-closest-values-in-two-other-colum
  
  lat_mdpts <- head (lat, -1) + diff (lat) / 2
  lon_mdpts <- head (lon, -1) + diff (lon) / 2
  
  lat_intv <- findInterval(sz.PA.SSH$Lat[i], lat_mdpts)
  lon_intv <- findInterval (sz.PA.SSH$Lon[i], lon_mdpts)
  
  sz.PA.SSH$adt[i] <- df[which (df$lat == lat[lat_intv + 1] & df$lon == lon[lon_intv + 1]), "adt"]
  sz.PA.SSH$sla[i] <- df[which (df$lat == lat[lat_intv + 1] & df$lon == lon[lon_intv + 1]), "sla"]
  
  # best practice to close each netCDF file once you've gotten the data you want
  nc_close (ssh_open)
  
}

#image (lon, lat, sla, col = rev (brewer.pal (10, "RdBu")))

write.csv (select (sz.PA.SSH, Lance.code, adt, sla), file = "Data_tables/Sz_CMEMS_ssh.csv", row.names = FALSE)