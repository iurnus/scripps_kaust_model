#!/home/rus043/anaconda2/bin/python
from mpl_toolkits.basemap import Basemap, cm
import cmocean
import sys, os, os.path
import matplotlib.pyplot as plt
from netCDF4 import Dataset
import numpy as np
from scipy import interpolate
import MITgcmutils

# modify wind file
wind_file_org = 'ERA5-20120525-20120605-wind.nc'
print 'This script will convert ' + wind_file_org
print 'into wind.nc file that can be used in WW3.'
print 'A zero_current.nc file will also be generated.'

wind_file_new = 'ww3_wind_2012.nc'
os.system('mv ' + wind_file_org + ' ' + wind_file_new)
wind_data = Dataset(wind_file_new,'r+',format='NETCDF4');

wind_lat = wind_data.variables['latitude']
wind_u10 = wind_data.variables['u10']
wind_v10 = wind_data.variables['v10']

[ntt,njj,nii] = np.shape(wind_u10)
print ntt, njj, nii

reverse_lat = np.zeros((njj))
reverse_u10 = np.zeros((ntt,njj,nii))
reverse_v10 = np.zeros((ntt,njj,nii))

for j in range(njj):
  reverse_lat[j] = wind_lat[njj-j-1]
  reverse_u10[:,j,:] = wind_u10[:,njj-j-1,:]
  reverse_v10[:,j,:] = wind_v10[:,njj-j-1,:]

wind_data.variables['latitude'][:] = reverse_lat
wind_data.variables['u10'][:] = reverse_u10
wind_data.variables['v10'][:] = reverse_v10

wind_data.close()

# modify current file
zero_current_file = 'ww3_current_zero.nc'
os.system('cp ' + wind_file_new + ' ' + zero_current_file)

current_data = Dataset(zero_current_file,'r+',format='NETCDF4');
current_uwnd = current_data.variables['u10'][:];
current_vwnd = current_data.variables['v10'][:];

current_data.renameVariable('u10','uoce')
current_data.variables['uoce'].scale_factor = 1.0
current_data.variables['uoce'].add_offset = 0.0
current_data.variables['uoce'][:] = current_uwnd*0.0

current_data.renameVariable('v10','voce')
current_data.variables['voce'].scale_factor = 1.0
current_data.variables['voce'].add_offset = 0.0
current_data.variables['voce'][:] = current_vwnd*0.0;

current_data.close()

