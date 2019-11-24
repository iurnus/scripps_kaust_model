#!/home/rus043/anaconda2/bin/python
from mpl_toolkits.basemap import Basemap, cm
import cmocean
import sys, os, os.path
import matplotlib.pyplot as plt
from netCDF4 import Dataset
import numpy as np
import MITgcmutils

# iStep = 0

p = [];
wrf_output = '../../runCase/wrfout_d01_2018-01-27_00:00:00'

wrf_output_results = Dataset(wrf_output,'r',format='NETCDF4');
wrf_output_lh = wrf_output_results.variables['LH'][:];
wrf_output_sh = wrf_output_results.variables['HFX'][:];
# wrf_output_gsw = wrf_output_results.variables['GSW'][:];
wrf_output_swupb = wrf_output_results.variables['SWUPB'][:];
wrf_output_swdnb = wrf_output_results.variables['SWDNB'][:];
wrf_output_gsw = - wrf_output_swupb + wrf_output_swdnb
wrf_output_lwupb = wrf_output_results.variables['LWUPB'][:];
wrf_output_lwdnb = wrf_output_results.variables['LWDNB'][:];
wrf_output_glw = - wrf_output_lwupb + wrf_output_lwdnb
wrf_output_t2 = wrf_output_results.variables['T2'][:] - 273.15;
wrf_output_sst = wrf_output_results.variables['SST'][:] - 273.15;
wrf_output_q2 = wrf_output_results.variables['Q2'][:];
wrf_output_uwnd = wrf_output_results.variables['U10'][:];
wrf_output_vwnd = wrf_output_results.variables['V10'][:];
wrf_output_wind = (wrf_output_uwnd**2+wrf_output_vwnd**2)**0.5
wrf_output_uoce = wrf_output_results.variables['UOCE'][:];
wrf_output_voce = wrf_output_results.variables['VOCE'][:];
wrf_output_current = (wrf_output_uoce**2+wrf_output_voce**2)**0.5
wrf_output_evap = wrf_output_results.variables['SFCEVP'][:];
wrf_output_rainc = wrf_output_results.variables['RAINC'][:];
wrf_output_rainnc = wrf_output_results.variables['RAINNC'][:];
wrf_output_rainsh = wrf_output_results.variables['RAINSH'][:];
wrf_output_rain = wrf_output_rainc+wrf_output_rainnc+wrf_output_rainsh
wrf_lon = wrf_output_results.variables['XLONG'][:]+360;
wrf_lat = wrf_output_results.variables['XLAT'][:];
parallels = np.arange(35.,55.1,5.)
meridians = np.arange(210.,240.1,10.)

fieldString = ['LH','SH','GSW','GLW','T2','SST','Q2',\
               'current','wind','evap','precip','waveheight','wavelength']
fieldName = [wrf_output_lh,wrf_output_sh,wrf_output_gsw,\
             wrf_output_glw,wrf_output_t2,wrf_output_sst,\
             wrf_output_q2,wrf_output_current,wrf_output_wind,wrf_output_evap,\
             wrf_output_rain]
clevsList = [np.arange(-205,205.01,10),np.arange(-20.5,20.51,1),np.arange(0,2001.01,200),\
             np.arange(-205,205.01,10),np.arange(-10,30.01,1),np.arange(0,16.01,0.1),\
             np.arange(0,0.02001,0.0005),np.arange(0,2.01,0.1),np.arange(0,20.01,1),\
             np.arange(0,200.01,1), np.arange(0,200.01,1),\
             np.arange(0,2.005,0.02),np.arange(0,40.1,1)]
cmapList = [cmocean.cm.balance,cmocean.cm.balance,cmocean.cm.thermal,\
            cmocean.cm.balance,cmocean.cm.thermal,cmocean.cm.thermal,\
            cmocean.cm.turbid,cmocean.cm.speed,cmocean.cm.speed,\
            cmocean.cm.dense, cmocean.cm.dense,\
            cmocean.cm.deep,cmocean.cm.deep]
tickList = [np.arange(-200,201,100),np.arange(-20,20.01,10),np.arange(0,2001.01,500),\
            np.arange(-200,201,100),np.arange(-10,30.01,5),np.arange(0,16.01,1),\
            np.arange(0,0.02001,0.002),np.arange(0,2.01,0.4),np.arange(0,20.01,4),\
            np.arange(0,200.01,10), np.arange(0,200.01,10),\
            np.arange(0.00001,2.001,0.4),np.arange(0,40.1,10)]
nFigures = 11
  
print "plot WRF..."
  
for iStep in [0,1,2,3,10,60]:
  print " plot step: " + str(iStep)
  for i in range(nFigures):
    print "  plot " + fieldString[i] + ' field'
    fig = plt.figure()
    f, axarr = plt.subplots(1,1)
    
    # PLOT CPL T2 after 12H
    m = Basemap(projection='cyl', llcrnrlat=35,urcrnrlat=55,\
                llcrnrlon=210,urcrnrlon=240,resolution='c')
    m.drawcoastlines()
    m.drawcountries()
    clevs = clevsList[i]
    m.drawparallels(parallels,labels=[1,0,0,0],fontsize=14,linewidth=0)
    m.drawmeridians(meridians,labels=[0,0,0,1],fontsize=14,linewidth=0)

    if (fieldString[i] == 'precip'):
      timeStep = 1200.0
      precip = (fieldName[i][iStep+1,:,:] - fieldName[i][iStep,:,:])*86400.0/timeStep
      cs = m.contourf(wrf_lon[0,:,:],wrf_lat[0,:,:],precip,\
                      clevs,extend='both',cmap=cmapList[i])
    if (fieldString[i] == 'evap'):
      timeStep = 1200.0
      evap = (fieldName[i][iStep+1,:,:] - fieldName[i][iStep,:,:])*86400.0/timeStep/1000
      cs = m.contourf(wrf_lon[0,:,:],wrf_lat[0,:,:],evap,\
                      clevs,extend='both',cmap=cmapList[i])
    else:
      cs = m.contourf(wrf_lon[0,:,:],wrf_lat[0,:,:],fieldName[i][iStep,:,:],\
                      clevs,extend='both',cmap=cmapList[i])
      

    if (fieldString[i] == 'wind'):
      nQ = 8
      cs_quiver = m.quiver(wrf_lon[0,::nQ,::nQ],wrf_lat[0,::nQ,::nQ],\
                           wrf_output_uwnd[iStep,::nQ,::nQ],wrf_output_vwnd[iStep,::nQ,::nQ])
    elif (fieldString[i] == 'current'):
      nQ = 8
      cs_quiver = m.quiver(wrf_lon[0,::nQ,::nQ],wrf_lat[0,::nQ,::nQ],\
                           wrf_output_uoce[iStep,::nQ,::nQ],wrf_output_voce[iStep,::nQ,::nQ],\
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
    fileName = 'wrf_' + fieldString[i] + '_' + str(iStep).zfill(4) + '.png'
    plt.savefig(fileName);
    plt.close('all')
