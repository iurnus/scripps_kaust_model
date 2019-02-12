#!/home/rus043/anaconda2/bin/python
from mpl_toolkits.basemap import Basemap, cm
import cmocean
import sys, os, os.path
import matplotlib.pyplot as plt
from netCDF4 import Dataset
import numpy as np
from scipy import interpolate
from copy import copy

p = [];
wrf_uncpl_result = '../../runWRFtest/wrfout_d01_2012-06-01_00:00:00';
wrf_cpl_result = '../../runCase/wrfout_d01_2012-06-01_00:00:00';

wrf_uncpl_nc = Dataset(wrf_uncpl_result,'r',format='NETCDF4');
wrf_uncpl_t2 = wrf_uncpl_nc.variables['T2'][:]-273.15;
wrf_lat = wrf_uncpl_nc.variables['XLAT'][:];
wrf_lon = wrf_uncpl_nc.variables['XLONG'][:];
wrf_cpl_nc = Dataset(wrf_cpl_result,'r',format='NETCDF4');
wrf_cpl_t2 = wrf_cpl_nc.variables['T2'][:]-273.15;

fig = plt.figure(num=None, figsize=(8,6), dpi=80, facecolor='w', edgecolor='k')
f, axarr = plt.subplots(2,3)
nWRFStep1 = 0;
nWRFStep2 = 60;
clevs_show = np.arange(8.,52.,2.);
# clevs_diff = np.arange(-1.01,1.02,0.02);
# clevs_diff = np.arange(-0.0525,0.0526,0.005);
clevs_diff = np.arange(-0.21,0.22,0.02);

# PLOT UNCPL T2, STEP 1
m = Basemap(projection='cyl', llcrnrlat=10,urcrnrlat=30,\
            llcrnrlon=30,urcrnrlon=50,resolution='c',ax=axarr[0,0])
m.drawcoastlines()
m.drawcountries()
# draw parallels.
parallels = np.arange(10.,30.1,5.)
m.drawparallels(parallels,labels=[1,0,0,0],fontsize=10)
# draw meridians
meridians = np.arange(30.,50.1,5.)
m.drawmeridians(meridians,labels=[0,0,0,0],fontsize=10)
cs_contourf = m.contourf(wrf_lon[0,:,:],wrf_lat[0,:,:],wrf_uncpl_t2[nWRFStep1,:,:],\
                         clevs_show,cmap=cmocean.cm.thermal)

# PLOT CPL T2, STEP 1
m = Basemap(projection='cyl', llcrnrlat=10,urcrnrlat=30,\
            llcrnrlon=30,urcrnrlon=50,resolution='c',ax=axarr[0,1])
m.drawcoastlines()
m.drawcountries()
# draw parallels.
parallels = np.arange(10.,30.1,5.)
m.drawparallels(parallels,labels=[0,0,0,0],fontsize=10)
# draw meridians
meridians = np.arange(30.,50.1,5.)
m.drawmeridians(meridians,labels=[0,0,0,0],fontsize=10)
m.contourf(wrf_lon[0,:,:],wrf_lat[0,:,:],wrf_cpl_t2[nWRFStep1,:,:],\
           clevs_show,cmap=cmocean.cm.thermal)

# PLOT CPL-UNCPL, STEP 1
m = Basemap(projection='cyl', llcrnrlat=10,urcrnrlat=30,\
            llcrnrlon=30,urcrnrlon=50,resolution='c',ax=axarr[0,2])
m.drawcoastlines()
m.drawcountries()
# draw parallels.
parallels = np.arange(10.,30.1,5.)
m.drawparallels(parallels,labels=[0,0,0,0],fontsize=10)
# draw meridians
meridians = np.arange(30.,50.1,5.)
m.drawmeridians(meridians,labels=[0,0,0,0],fontsize=10)
cs_diff1 = m.contourf(wrf_lon[0,:,:],wrf_lat[0,:,:],\
                      wrf_cpl_t2[nWRFStep1,:,:]-wrf_uncpl_t2[nWRFStep1,:,:],\
                      clevs_diff,extend='both',cmap=cmocean.cm.balance)

# PLOT UNCPL T2, STEP 2
m = Basemap(projection='cyl', llcrnrlat=10,urcrnrlat=30,\
            llcrnrlon=30,urcrnrlon=50,resolution='c',ax=axarr[1,0])
m.drawcoastlines()
m.drawcountries()
# draw parallels.
parallels = np.arange(10.,30.1,5.)
m.drawparallels(parallels,labels=[1,0,0,0],fontsize=10)
# draw meridians
meridians = np.arange(30.,49.9,5.)
m.drawmeridians(meridians,labels=[0,0,0,1],fontsize=10)
m.contourf(wrf_lon[0,:,:],wrf_lat[0,:,:],wrf_uncpl_t2[nWRFStep2,:,:],\
           clevs_show,cmap=cmocean.cm.thermal)

# PLOT CPL T2, STEP 2
m = Basemap(projection='cyl', llcrnrlat=10,urcrnrlat=30,\
            llcrnrlon=30,urcrnrlon=50,resolution='c',ax=axarr[1,1])
m.drawcoastlines()
m.drawcountries()
# draw parallels.
parallels = np.arange(10.,30.1,5.)
m.drawparallels(parallels,labels=[0,0,0,0],fontsize=10)
# draw meridians
meridians = np.arange(30.,49.9,5.)
m.drawmeridians(meridians,labels=[0,0,0,1],fontsize=10)
m.contourf(wrf_lon[0,:,:],wrf_lat[0,:,:],wrf_cpl_t2[nWRFStep2,:,:],\
           clevs_show,cmap=cmocean.cm.thermal)

# PLOT CPL-UNCPL T2, STEP 2
m = Basemap(projection='cyl', llcrnrlat=10,urcrnrlat=30,\
            llcrnrlon=30,urcrnrlon=50,resolution='c',ax=axarr[1,2])
m.drawcoastlines()
m.drawcountries()
# draw parallels.
parallels = np.arange(10.,30.1,5.)
m.drawparallels(parallels,labels=[0,0,0,0],fontsize=10)
# draw meridians
meridians = np.arange(30.,49.9,5.)
m.drawmeridians(meridians,labels=[0,0,0,1],fontsize=10)
cs_diff1 = m.contourf(wrf_lon[0,:,:],wrf_lat[0,:,:],\
                      wrf_cpl_t2[nWRFStep2,:,:]-wrf_uncpl_t2[nWRFStep2,:,:],\
                      clevs_diff,extend='both',cmap=cmocean.cm.balance)

cbar_ax = f.add_axes([0.10, 0.05, 0.25, 0.03])
fig.colorbar(cs_contourf, cax=cbar_ax, ticks=[10,20,30,40,50], orientation='horizontal')

cbar_ax = f.add_axes([0.65, 0.05, 0.25, 0.03])
fig.colorbar(cs_diff1, cax=cbar_ax, ticks=[-0.2,-0.1,0,0.1,0.2], orientation='horizontal')

f.subplots_adjust(hspace=0.05)
f.subplots_adjust(wspace=0.10)
f.subplots_adjust(left=0.10)
f.subplots_adjust(right=0.90)
f.subplots_adjust(top=0.90)
f.subplots_adjust(bottom=0.10)

axarr[0,0].text(0.01, 1.11, 'ring', transform=axarr[0,0].transAxes, fontsize=13)
axarr[0,0].text(0.01, 1.01, '1 CPU, WRF.DYN', transform=axarr[0,0].transAxes, fontsize=13)
axarr[0,1].text(0.01, 1.11, 'ring', transform=axarr[0,1].transAxes, fontsize=13)
axarr[0,1].text(0.01, 1.01, '1 CPU, CPL', transform=axarr[0,1].transAxes, fontsize=13)
axarr[0,2].text(0.01, 1.01, 'diff(CPL$-$WRF.DYN)', transform=axarr[0,2].transAxes, fontsize=13)
axarr[0,0].text(-0.20, 1.22, 'Initial condition',\
                transform=axarr[0,0].transAxes, fontsize=13)
axarr[1,0].text(-0.20, 1.07, 'After 1 hour', \
                transform=axarr[1,0].transAxes, fontsize=13)
axarr[1,0].text(1.02, -0.28, '$^\circ$C', transform=axarr[1,0].transAxes, fontsize=15)
axarr[1,2].text(1.02, -0.28, '$^\circ$C', transform=axarr[1,2].transAxes, fontsize=15)

plt.gcf().set_size_inches(8.0, 6.0)
plt.savefig('verify_T2.png');
