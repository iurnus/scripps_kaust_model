#!/home/rus043/anaconda2/bin/python
import matplotlib
matplotlib.use('Agg')
from mpl_toolkits.basemap import Basemap, cm
import cmocean
import sys, os, os.path
import matplotlib.pyplot as plt
from netCDF4 import Dataset
import numpy as np
from scipy import interpolate
from copy import copy
from matplotlib.ticker import AutoMinorLocator

folder = '../run/'

ocnSSTFile = folder + 'sstOCN.nc'
atmSSTFile = folder + 'sstATM000001.nc'
ocnHfluxFile = folder + 'rsnsOCN000001.nc'
atmHfluxFile = folder + 'rsnsATM.nc'

esmf_ocnsst_nc = Dataset(ocnSSTFile,'r',format='NETCDF4');
esmf_atmsst_nc = Dataset(atmSSTFile,'r',format='NETCDF4');
esmf_ocnhf_nc = Dataset(ocnHfluxFile,'r',format='NETCDF4');
esmf_atmhf_nc = Dataset(atmHfluxFile,'r',format='NETCDF4');
esmf_ocnsst = esmf_ocnsst_nc.variables['sst'][:]
esmf_atmsst = esmf_atmsst_nc.variables['sst'][:]
esmf_ocnhf = esmf_ocnhf_nc.variables['rsns'][:]
esmf_atmhf = esmf_atmhf_nc.variables['rsns'][:]

esmf_ocnx_nc = Dataset(folder + 'ocn_xa.nc','r',format='NETCDF4');
esmf_ocny_nc = Dataset(folder + 'ocn_ya.nc','r',format='NETCDF4');
esmf_ocnx = esmf_ocnx_nc.variables['Array009'][:]
esmf_ocny = esmf_ocny_nc.variables['Array010'][:]
esmf_atmx_nc = Dataset(folder + 'atm_xa.nc','r',format='NETCDF4');
esmf_atmy_nc = Dataset(folder + 'atm_ya.nc','r',format='NETCDF4');
esmf_atmx = esmf_atmx_nc.variables['Array009'][:]
esmf_atmy = esmf_atmy_nc.variables['Array010'][:]

# sst_level = np.linspace(0,75,25)
# hf_level = np.linspace(0,20,20)
sst_level = np.linspace(13,17,20)
hf_level = np.linspace(13,17,20)

f, axarr = plt.subplots(2,2)
ax_i = plt.subplot(221)
ax_i.title.set_text('SST, ATM (2km, destination)')
plt.contourf(esmf_atmx, esmf_atmy, esmf_atmsst, levels=sst_level)

ax_i = plt.subplot(222)
ax_i.title.set_text('SST, OCN (1km, source)')
plt.contourf(esmf_ocnx, esmf_ocny, esmf_ocnsst, levels=sst_level)

ax_i = plt.subplot(223)
ax_i.title.set_text('HFLUX, ATM (2km, source)')
plt.contourf(esmf_atmx, esmf_atmy, esmf_atmhf, levels=hf_level)

ax_i = plt.subplot(224)
ax_i.title.set_text('HFLUX, OCN (1km, destination)')
plt.contourf(esmf_ocnx, esmf_ocny, esmf_ocnhf, levels=hf_level)

plt.gcf().set_size_inches(12.0, 12.0)
plt.savefig('interpl-test.png');
