#!/home/rus043/anaconda2/bin/python
from mpl_toolkits.basemap import Basemap, cm
import cmocean
import sys, os, os.path
import matplotlib.pyplot as plt
from netCDF4 import Dataset
import numpy as np
from scipy import interpolate
import MITgcmutils

p = [];
mitgcm_cpl_result = '../..//runCase/';
mitgcm_uncpl_result = '../../runMITtest/';

fig = plt.figure(num=None, figsize=(8,5), dpi=80, facecolor='w', edgecolor='k')
f, axarr = plt.subplots(2,3)
mitgcm_uncpl_data1 = MITgcmutils.rdmds(mitgcm_uncpl_result + 'pickup.0000000003')
mitgcm_cpl_data1   = MITgcmutils.rdmds(mitgcm_cpl_result   + 'pickup.0000000003')
mitgcm_uncpl_data2 = MITgcmutils.rdmds(mitgcm_uncpl_result + 'pickup.0000000009')
mitgcm_cpl_data2   = MITgcmutils.rdmds(mitgcm_cpl_result   + 'pickup.0000000009')
mitgcm_meshX   = MITgcmutils.rdmds(mitgcm_uncpl_result + 'XC')
mitgcm_meshY   = MITgcmutils.rdmds(mitgcm_uncpl_result + 'YC')
mitgcm_mask   = MITgcmutils.rdmds(mitgcm_cpl_result   + 'hFacC')
nZ = 40;
nPickup = 2*nZ;

parallels = np.arange(36.,54.1,6.)
meridians = np.arange(212.,236.1,8.)
latMin = 35
latMax = 55
lonMin = 210
lonMax = 240

mitgcm_ocean = mitgcm_mask[0,:,:]
for i in range(256):
  for j in range(432):
    if mitgcm_mask[0,i,j] == 1:
       mitgcm_ocean[i,j] = 1
    else:
       mitgcm_ocean[i,j] = np.nan

clevs_show = np.arange(0.,18.1,1.);
clevs_diff = np.arange(-0.00042,0.000421,0.00004);

# PLOT UNCPL SST, STEP 1
m = Basemap(projection='cyl', llcrnrlat=latMin,urcrnrlat=latMax,\
            llcrnrlon=lonMin,urcrnrlon=lonMax,resolution='c',ax=axarr[0,0])
m.drawcoastlines()
m.drawcountries()
# draw parallels.
m.drawparallels(parallels,labels=[1,0,0,0],fontsize=10)
# draw meridians
m.drawmeridians(meridians,labels=[0,0,0,0],fontsize=10)
m.contourf(mitgcm_meshX,mitgcm_meshY,mitgcm_ocean*mitgcm_uncpl_data1[nPickup,:,:],\
           clevs_show,cmap=cmocean.cm.thermal)

# PLOT CPL SST, STEP 1
m = Basemap(projection='cyl', llcrnrlat=latMin,urcrnrlat=latMax,\
            llcrnrlon=lonMin,urcrnrlon=lonMax,resolution='c',ax=axarr[0,1])
m.drawcoastlines()
m.drawcountries()
# draw parallels.
m.drawparallels(parallels,labels=[0,0,0,0],fontsize=10)
# draw meridians
m.drawmeridians(meridians,labels=[0,0,0,0],fontsize=10)
m.contourf(mitgcm_meshX,mitgcm_meshY,mitgcm_ocean*mitgcm_cpl_data1[nPickup,:,:],\
           clevs_show,cmap=cmocean.cm.thermal)

# PLOT CPL-UNCPL SST, STEP 1
m = Basemap(projection='cyl', llcrnrlat=latMin,urcrnrlat=latMax,\
            llcrnrlon=lonMin,urcrnrlon=lonMax,resolution='c',ax=axarr[0,2])
m.drawcoastlines()
m.drawcountries()
# draw parallels.
m.drawparallels(parallels,labels=[0,0,0,0],fontsize=10)
# draw meridians
m.drawmeridians(meridians,labels=[0,0,0,0],fontsize=10)
m.contourf(mitgcm_meshX,mitgcm_meshY,\
           mitgcm_ocean*(mitgcm_cpl_data1[nPickup,:,:]-mitgcm_uncpl_data1[nPickup,:,:]),\
           # mitgcm_ocean*(mitgcm_cpl_data2[nPickup,:,:]-mitgcm_cpl_data1[nPickup,:,:]),\
           clevs_diff,cmap=cmocean.cm.balance)

# PLOT UNCPL SST, STEP 2
m = Basemap(projection='cyl', llcrnrlat=latMin,urcrnrlat=latMax,\
            llcrnrlon=lonMin,urcrnrlon=lonMax,resolution='c',ax=axarr[1,0])
m.drawcoastlines()
m.drawcountries()
# draw parallels.
m.drawparallels(parallels,labels=[1,0,0,0],fontsize=10)
# draw meridians
m.drawmeridians(meridians,labels=[0,0,0,1],fontsize=10)
m.contourf(mitgcm_meshX,mitgcm_meshY,mitgcm_ocean*mitgcm_uncpl_data2[nPickup,:,:],\
           clevs_show,cmap=cmocean.cm.thermal)

# PLOT CPL SST, STEP 2
m = Basemap(projection='cyl', llcrnrlat=latMin,urcrnrlat=latMax,\
            llcrnrlon=lonMin,urcrnrlon=lonMax,resolution='c',ax=axarr[1,1])
m.drawcoastlines()
m.drawcountries()
# draw parallels.
m.drawparallels(parallels,labels=[0,0,0,0],fontsize=10)
# draw meridians
m.drawmeridians(meridians,labels=[0,0,0,1],fontsize=10)
cs_contourf = m.contourf(mitgcm_meshX,mitgcm_meshY,mitgcm_ocean*mitgcm_cpl_data2[nPickup,:,:],\
                         clevs_show,cmap=cmocean.cm.thermal)

# PLOT CPL-UNCPL SST, STEP 2
m = Basemap(projection='cyl', llcrnrlat=latMin,urcrnrlat=latMax,\
            llcrnrlon=lonMin,urcrnrlon=lonMax,resolution='c',ax=axarr[1,2])
m.drawcoastlines()
m.drawcountries()
# draw parallels.
m.drawparallels(parallels,labels=[0,0,0,0],fontsize=10)
# draw meridians
m.drawmeridians(meridians,labels=[0,0,0,1],fontsize=10)
cs_diff1 = m.contourf(mitgcm_meshX,mitgcm_meshY,\
                      mitgcm_ocean*(mitgcm_cpl_data2[nPickup,:,:]-mitgcm_uncpl_data2[nPickup,:,:]),\
                      clevs_diff,extend='both',cmap=cmocean.cm.balance)

cbar_ax = f.add_axes([0.10, 0.05, 0.25, 0.03])
fig.colorbar(cs_contourf, cax=cbar_ax, ticks=[0,4,8,12,16], orientation='horizontal')

cbar_ax = f.add_axes([0.65, 0.05, 0.25, 0.03])
fig.colorbar(cs_diff1, cax=cbar_ax, ticks=[-0.0004,0,0.0004], orientation='horizontal')

f.subplots_adjust(hspace=0.05)
f.subplots_adjust(wspace=0.10)
f.subplots_adjust(left=0.10)
f.subplots_adjust(right=0.90)
f.subplots_adjust(top=0.90)
f.subplots_adjust(bottom=0.10)

axarr[0,0].text(0.01, 1.01, 'OCN.DYN.TEST', transform=axarr[0,0].transAxes, fontsize=13)
axarr[0,1].text(0.01, 1.01, 'CPL', transform=axarr[0,1].transAxes, fontsize=13)
axarr[0,2].text(0.01, 1.01, 'diff(CPL$-$OCN.DYN.TEST)', transform=axarr[0,2].transAxes, fontsize=13)
axarr[0,0].text(-0.20, 1.12, '0100 UTC, Jan 27, 2018',\
                transform=axarr[0,0].transAxes, fontsize=13)
axarr[1,0].text(-0.20, 1.07, '1200 UTC, Jan 27, 2018', \
                transform=axarr[1,0].transAxes, fontsize=13)
axarr[1,0].text(1.02, -0.38, '$^\circ$C', transform=axarr[1,0].transAxes, fontsize=15)
axarr[1,2].text(1.02, -0.38, '$^\circ$C', transform=axarr[1,2].transAxes, fontsize=15)

plt.gcf().set_size_inches(8.0, 5.0)
plt.savefig('verify_SST.png');
