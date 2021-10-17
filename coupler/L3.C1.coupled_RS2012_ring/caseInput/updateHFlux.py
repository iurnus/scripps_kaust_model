#!/home/rus043/anaconda2/bin/python
import sys, os, os.path
import matplotlib.pyplot as plt
from netCDF4 import Dataset
import numpy as np
from scipy import interpolate

nIniStep = 0
set_results = Dataset('wrfout_d01_2012-06-01_00:00:00','r',format='NETCDF4');
set_swupb = set_results.variables['SWUPB'][nIniStep+1,:,:]
set_swdnb = set_results.variables['SWDNB'][nIniStep+1,:,:]
set_lwupb = set_results.variables['LWUPB'][nIniStep+1,:,:]
set_lwdnb = set_results.variables['LWDNB'][nIniStep+1,:,:]
set_lh = set_results.variables['LH'][nIniStep+1,:,:]
set_sh = set_results.variables['HFX'][nIniStep+1,:,:]

ini_results = Dataset('wrfinput_d01','r+',format='NETCDF4');
ini_swupb = ini_results.variables['SWUPB'][nIniStep,:,:]
ini_swdnb = ini_results.variables['SWDNB'][nIniStep,:,:]
ini_lwupb = ini_results.variables['LWUPB'][nIniStep,:,:]
ini_lwdnb = ini_results.variables['LWDNB'][nIniStep,:,:]
ini_lh = ini_results.variables['LH'][nIniStep,:,:]
ini_sh = ini_results.variables['HFX'][nIniStep,:,:]

ini_results.variables['SWUPB'][nIniStep,:,:] = set_swupb
ini_results.variables['SWDNB'][nIniStep,:,:] = set_swdnb
ini_results.variables['LWUPB'][nIniStep,:,:] = set_lwupb
ini_results.variables['LWDNB'][nIniStep,:,:] = set_lwdnb
ini_results.variables['LH'][nIniStep,:,:] = set_lh
ini_results.variables['HFX'][nIniStep,:,:] = set_sh

ini_results.close()

print("new swupb: ", np.mean(set_swupb))
print("new swdnb: ", np.mean(set_swdnb))
print("new lwupb: ", np.mean(set_lwupb))
print("new lwdnb: ", np.mean(set_lwdnb))
print("new lh: ", np.mean(set_lh))
print("new sh: ", np.mean(set_sh))
