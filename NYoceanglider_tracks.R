# Read Matlab glider casts files into R

library(R.matlab)

# read in mat file
sbu01_02_llt <- readMat("/Users/janellemorano/DATA/NY ocean trawl survey/sbu01_02_llt.mat")

# check out data structure
str(sbu01_02_llt)

sbu01_02_llt$sbu01.02.date.time
sbu01_02_llt$sbu01.02.lat
sbu01_02_llt$sbu01.02.lon

## UGH. Not working

data = lapply(sbu01_02_llt, unlist, use.names=FALSE)
df <- as.data.frame(data) # now has correct number of obs and vars
head(df)

matlabFile <- readMat("/Users/janellemorano/DATA/NY ocean trawl survey/sbu01_02_llt.mat")
varNames    <- names(matlabFile$data[,,1])
datList     <- matlabFile$data
datList     <- lapply(datList, unlist, use.names=FALSE)
data        <- as.data.frame(datList)
names(data) <- varNames
head(data)
