# Download NOAA OISST satellite data and calculate a monthly mean for Iceland's waters. Using a 30 year climatology, 1982-2011.

from matplotlib import pyplot as plt
import numpy as np
import pandas as pd
import xarray as xr

xr.set_options(display_style='html')
%matplotlib inline
%config InlineBackend.figure_format = 'retina'

# link from Don Murray, NOAA, from Pangeo_ESGF_search_and_load nb
oisst_full = xr.open_dataset('https://psl.noaa.gov/thredds/dodsC/Aggregations/OISSThires/sst.mean.nc', chunks = {'time':100}) # this seems to be a good size
%oisst_full # 1981-09-01 to 2020-07-06
%oisst_month_mn = oisst_full.sst.sel(time = slice ('1982', '2011'), lon=slice(-45, 15), lat=slice(50, 85)).groupby('time.month').mean('time') oisst_month_mn.nbytes/1e6 # 49 MB for global file

# subset appropriate time and take monthly average
# From Vince: Actually, there is only partial data for 1981.  Go with 1982 to 2011.  That will give you an even 30-year climatology.
oisst_month_mn = oisst_full.sst.sel(time = slice ('1982', '2011'), lon=slice(-45, 15), lat=slice(50, 85)).groupby('time.month').mean('time')
oisst_month_mn.nbytes/1e6 # 49 MB for global file

# crop to GINS region, but would have to change coords. lon is -45 to 15, which would be 
oisst_180 = oisst_month_mn.assign_coords(lon=(((oisst_month_mn.lon + 180) % 360) - 180)).sortby('lon') 
oisst_180_crop = oisst_180.sel(lon = slice (-45, 15), lat = slice (50, 85))
oisst_180_crop.nbytes/1e6

%time oisst_180_crop.to_netcdf('oisst_month_climatology_1982_2011.nc')

oisst_180_crop = xr.open_dataset('oisst_month_climatology_1982_2011.nc')
oisst_180_crop

oisst_180_crop.sst.isel(month = 1).plot()