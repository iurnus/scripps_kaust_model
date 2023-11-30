#!/home/rus043/anaconda2/bin/python
from mpl_toolkits.basemap import Basemap, cm
import cmocean
import sys, os, os.path
import matplotlib.pyplot as plt
from netCDF4 import Dataset
import numpy as np
import MITgcmutils

iStep = 1

p = [];
ww3_current_file = '../../runCase/ww3.201206_cur.nc'
ww3_wind_file = '../../runCase/ww3.201206_wnd.nc'
ww3_hs_file = '../../runCase/ww3.201206_hs.nc'
ww3_lm_file = '../../runCase/ww3.201206_lm.nc'

ww3_current_results = Dataset(ww3_current_file,'r',format='NETCDF4');
ww3_wind_results = Dataset(ww3_wind_file,'r',format='NETCDF4');
ww3_hs_results = Dataset(ww3_hs_file,'r',format='NETCDF4');
ww3_lm_results = Dataset(ww3_lm_file,'r',format='NETCDF4');

ww3_lon = ww3_current_results.variables['longitude'][:];
ww3_lat = ww3_current_results.variables['latitude'][:];
ww3_wind_x = ww3_wind_results.variables['uwnd'][:];
ww3_wind_y = ww3_wind_results.variables['vwnd'][:];
ww3_wind_mag = (ww3_wind_x**2 + ww3_wind_y**2)**0.5
ww3_current_x = ww3_current_results.variables['ucur'][:];
ww3_current_y = ww3_current_results.variables['vcur'][:];
ww3_current_mag = (ww3_current_x**2 + ww3_current_y**2)**0.5
ww3_hs = ww3_hs_results.variables['hs'][:];
ww3_lm = ww3_lm_results.variables['lm'][:];
ww3_meshX, ww3_meshY = np.meshgrid(ww3_lon,ww3_lat)

parallels = np.arange(12.,28.1,4.)
meridians = np.arange(30.,50.1,4.)

fieldString = ['current','wind','waveheight','wavelength']
fieldName = [ww3_current_mag,ww3_wind_mag,ww3_hs,\
             ww3_lm]
clevsList = [np.arange(0,2.01,0.1),np.arange(0,20.01,1),np.arange(0,2.005,0.02),\
             np.arange(0,40.005,0.1)]
cmapList = [cmocean.cm.speed,cmocean.cm.speed,cmocean.cm.deep,\
            cmocean.cm.deep]
tickList = [np.arange(0,2.01,0.4),np.arange(0,20.01,4),np.arange(0,2.005,0.4),\
            np.arange(0,40.005,10)]
nFigures = 4
  
print "plot WW3..."
  
for i in range(nFigures):
  print "  plot " + fieldString[i] + ' field'
  fig = plt.figure()
  f, axarr = plt.subplots(1,1)
  
  # PLOT CPL T2 after 12H
  m = Basemap(projection='cyl', llcrnrlat=10,urcrnrlat=30,\
              llcrnrlon=30,urcrnrlon=50,resolution='c')
  m.drawcoastlines()
  m.drawcountries()
  m.drawparallels(parallels,labels=[1,0,0,0],fontsize=14,linewidth=0)
  m.drawmeridians(meridians,labels=[0,0,0,1],fontsize=14,linewidth=0)
  clevs = clevsList[i]
  cs = m.contourf(ww3_meshX,ww3_meshY,fieldName[i][iStep,:,:],\
                  clevs,extend='both',cmap=cmapList[i])

  if (fieldString[i] == 'wind'):
    nQ = 8
    cs_quiver = m.quiver(ww3_meshX[::nQ,::nQ],ww3_meshY[::nQ,::nQ],\
                         ww3_wind_x[iStep,::nQ,::nQ],ww3_wind_y[iStep,::nQ,::nQ])
  elif (fieldString[i] == 'current'):
    nQ = 2
    cs_quiver = m.quiver(ww3_meshX[::nQ,::nQ],ww3_meshY[::nQ,::nQ],\
                         ww3_current_x[iStep,::nQ,::nQ],ww3_current_y[iStep,::nQ,::nQ],\
                         scale=2,scale_units='inches')
  
  captionStr = 'Time Step: ' + str(iStep).zfill(4) + ', ' + fieldString[i]
  axarr.text(0.0, 1.01, captionStr, transform=axarr.transAxes, fontsize=15)
  cbar_ax = f.add_axes([0.85, 0.15, 0.03, 0.70])
  fig.colorbar(cs, cax=cbar_ax, ticks=tickList[i], orientation='vertical')
  
  f.subplots_adjust(hspace=0.05)
  f.subplots_adjust(wspace=0.10)
  f.subplots_adjust(left=0.15)
  f.subplots_adjust(right=0.85)
  f.subplots_adjust(top=0.95)
  f.subplots_adjust(bottom=0.10)
  
  plt.gcf().set_size_inches(6.4, 4.0)
  fileName = 'ww3_' + fieldString[i] + '.png'
  plt.savefig(fileName);
  plt.close('all')
