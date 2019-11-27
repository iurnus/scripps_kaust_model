#!/home/x_sunr/anaconda2/bin/python
import matplotlib
matplotlib.use('Agg')
from mpl_toolkits.basemap import Basemap, cm
import cmocean
import sys, os, os.path
import matplotlib.pyplot as plt
from netCDF4 import Dataset
import numpy as np
import MITgcmutils

# iStep = 1

p = [];

print "plot WRF..."
for iStep in [1,2,3,4]:
  print " plot step: " + str(iStep)
  mitgcm_pickup_file = '../../runCase/pickup.'+str(iStep).zfill(10)
  mitgcm_pickup_results = MITgcmutils.rdmds(mitgcm_pickup_file)
  mitgcm_diag_file = '../../runCase/diag2dKPP.'+str(iStep).zfill(10)
  mitgcm_diag_results = MITgcmutils.rdmds(mitgcm_diag_file)
  mitgcm_meshX   = MITgcmutils.rdmds('../../runCase/XC')
  mitgcm_meshY   = MITgcmutils.rdmds('../../runCase/YC')
  # latent/sensible heat into ocean
  mitgcm_sh = mitgcm_diag_results[3,:,:]
  mitgcm_lh = mitgcm_diag_results[4,:,:]
  # net upward long/short wave radiation
  mitgcm_swnet = mitgcm_diag_results[6,:,:]
  mitgcm_lwnet = mitgcm_diag_results[5,:,:]
  mitgcm_qnet = mitgcm_diag_results[7,:,:]
  mitgcm_uwind = mitgcm_diag_results[8,:,:]
  mitgcm_vwind = mitgcm_diag_results[9,:,:]
  mitgcm_wind = mitgcm_diag_results[10,:,:]
  mitgcm_t2 = mitgcm_diag_results[11,:,:] - 273.15
  mitgcm_q2 = mitgcm_diag_results[12,:,:]
  mitgcm_precip = mitgcm_diag_results[13,:,:]
  mitgcm_evap = mitgcm_diag_results[14,:,:]
  mitgcm_sst = mitgcm_pickup_results[80,:,:]
  mitgcm_uoce = mitgcm_pickup_results[0,:,:]
  mitgcm_voce = mitgcm_pickup_results[40,:,:]
  mitgcm_current = (mitgcm_uoce**2+mitgcm_voce**2)**0.5
  
  parallels = np.arange(35.,55.1,5.)
  meridians = np.arange(210.,240.1,10.)
  
  fieldString = ['LH','SH','GSW','GLW','T2','SST','Q2',\
                 'current','wind','precip','evap']
  # plot the lh/sh out of ocean; 
  # plot downward long/short wave radiation
  fieldName = [-mitgcm_lh,-mitgcm_sh,-mitgcm_swnet,\
               -mitgcm_lwnet,mitgcm_t2,mitgcm_sst,\
               mitgcm_q2,mitgcm_current,mitgcm_wind,\
               mitgcm_precip*1000*86400,mitgcm_evap*1000*86400]
  clevsList = [np.arange(-205,205.01,10),np.arange(-20.5,20.51,1),np.arange(0,2001.01,200),\
               np.arange(-205,205.01,10),np.arange(-10,30.01,1),np.arange(0,16.01,0.1),\
               np.arange(0,0.02001,0.0005),np.arange(0,2.01,0.1),np.arange(0,20.01,1),\
               np.arange(0,2.01,0.1),np.arange(0,2.01,0.1)]
  cmapList = [plt.cm.seismic,plt.cm.seismic,plt.cm.jet,\
              plt.cm.seismic,plt.cm.jet,plt.cm.jet,\
              plt.cm.jet,cmocean.cm.speed,plt.cm.jet,\
              plt.cm.jet,cmocean.cm.speed,plt.cm.jet]
  tickList = [np.arange(-200,201,100),np.arange(-20,20.01,10),np.arange(0,2001.01,500),\
              np.arange(-200,201,100),np.arange(-10,30.01,5),np.arange(0,16.01,1),\
              np.arange(0,0.02001,0.002),np.arange(0,2.01,0.4),np.arange(0,20.01,4),\
              np.arange(0,2.01,0.5),np.arange(0,2.01,0.5)]
  nFigures = 11
  
  print "plot mitgcm..."
    
  for i in range(nFigures):
    print "  plot " + fieldString[i] + ' field'
    fig = plt.figure()
    f, axarr = plt.subplots(1,1)
    
    # PLOT CPL T2 after 12H
    m = Basemap(projection='cyl', llcrnrlat=35,urcrnrlat=55,\
                llcrnrlon=210,urcrnrlon=240,resolution='c')
    m.drawcoastlines()
    m.drawcountries()
    m.drawparallels(parallels,labels=[1,0,0,0],fontsize=14,linewidth=0)
    m.drawmeridians(meridians,labels=[0,0,0,1],fontsize=14,linewidth=0)
    clevs = clevsList[i]
    cs = m.contourf(mitgcm_meshX,mitgcm_meshY,fieldName[i],\
                    clevs,extend='both',cmap=cmapList[i])
    print 'mean is: ', np.mean(fieldName[i])
    if (fieldString[i] == 'wind'):
      nQ = 8
      cs_quiver = m.quiver(mitgcm_meshX[::nQ,::nQ],mitgcm_meshY[::nQ,::nQ],\
                           mitgcm_uwind[::nQ,::nQ],mitgcm_vwind[::nQ,::nQ])
    elif (fieldString[i] == 'current'):
      nQ = 8
      cs_quiver = m.quiver(mitgcm_meshX[::nQ,::nQ],mitgcm_meshY[::nQ,::nQ],\
                           mitgcm_uoce[::nQ,::nQ],mitgcm_voce[::nQ,::nQ],\
                           scale=2,scale_units='inches')
    captionStr = 'Time Step: ' + str(iStep-1).zfill(4) + ', ' + fieldString[i]
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
    fileName = 'mitgcm_' + fieldString[i] + '_' + str(iStep-1).zfill(4) + '.png'
    plt.savefig(fileName);
    plt.close('all')
