#!/home/x_sunr/anaconda2/bin/python
import sys, os, os.path
from netCDF4 import Dataset
import numpy as np

old_results = Dataset('wrfout.nc','r',format='NETCDF4');
old_sst  = old_results.variables['SST'][:,:,:]
old_uoce = old_results.variables['UOCE'][:,:,:]
old_voce = old_results.variables['VOCE'][:,:,:]
old_alb  = old_results.variables['ALBBCK'][:,:,:]

new_results = Dataset('cplFlux','r+',format='NETCDF4');
new_sst  = new_results.variables['SST'][:,:,:]
new_uoce = new_results.variables['UOCE'][:,:,:]
new_voce = new_results.variables['VOCE'][:,:,:]
new_alb  = new_results.variables['ALBBCK'][:,:,:]

nt = np.shape(old_sst)[0]

for k in range(nt-1):
  new_results.variables['SST'][k,:,:]  = old_sst[k+1,:,:]
  new_results.variables['UOCE'][k,:,:] = old_uoce[k+1,:,:]
  new_results.variables['VOCE'][k,:,:] = old_voce[k+1,:,:]
  new_results.variables['ALBBCK'][k,:,:] = old_alb[k+1,:,:]

new_results.close()

