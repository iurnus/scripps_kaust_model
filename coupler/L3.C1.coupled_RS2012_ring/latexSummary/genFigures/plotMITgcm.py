#!/home/rus043/anaconda2/bin/python
from mpl_toolkits.basemap import Basemap, cm
import cmocean
import sys, os, os.path
import matplotlib.pyplot as plt
from netCDF4 import Dataset
import numpy as np
import MITgcmutils

# iStep = 1

p = [];

for iStep in [1,2,3,10,60]:
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
  mitgcm_sst = mitgcm_pickup_results[80,:,:]
  mitgcm_uoce = mitgcm_pickup_results[0,:,:]
  mitgcm_voce = mitgcm_pickup_results[40,:,:]
  mitgcm_current = (mitgcm_uoce**2+mitgcm_voce**2)**0.5
  mitgcm_precip = mitgcm_diag_results[13,:,:]
  mitgcm_evap = mitgcm_diag_results[14,:,:]
  
  parallels = np.arange(12.,28.1,4.)
  meridians = np.arange(30.,50.1,4.)
  
  fieldString = ['LH','SH','GSW','GLW','T2','SST','Q2',\
                 'current','wind','precip','evap']
  # plot the lh/sh out of ocean; 
  # plot downward long/short wave radiation
  fieldName = [-mitgcm_lh,-mitgcm_sh,-mitgcm_swnet,\
               -mitgcm_lwnet,mitgcm_t2,mitgcm_sst,\
               mitgcm_q2,mitgcm_current,mitgcm_wind,\
               mitgcm_precip,mitgcm_evap]
  clevsList = [np.arange(-205,205.01,10),np.arange(-20.5,20.51,1),np.arange(0,2001.01,200),\
               np.arange(-205,205.01,10),np.arange(20,50.01,1),np.arange(24,32.01,0.1),\
               np.arange(0,0.02001,0.0005),np.arange(0,2.01,0.1),np.arange(0,20.01,1),\
               np.arange(0,1.001e-7,0.02e-7),np.arange(0,1.001e-7,0.02e-7)]
  cmapList = [cmocean.cm.balance,cmocean.cm.balance,cmocean.cm.thermal,\
              cmocean.cm.balance,cmocean.cm.thermal,cmocean.cm.thermal,\
              cmocean.cm.turbid,cmocean.cm.speed,cmocean.cm.speed,\
              cmocean.cm.speed,cmocean.cm.speed]
  tickList = [np.arange(-200,201,100),np.arange(-20,20.01,10),np.arange(0,2001.01,500),\
              np.arange(-200,201,100),np.arange(20,50.01,5),np.arange(24,32.01,1),\
              np.arange(0,0.02001,0.002),np.arange(0,2.01,0.4),np.arange(0,20.01,4),\
              np.arange(0,1.001e-7,0.2e-7),np.arange(0,1.001e-7,0.2e-7)]
  nFigures = 11
  
  print "plot mitgcm..."
  ## for i in range(128):
  ##   for j in range(128):
  ##     print i,j
  ##     print mitgcm_sh[i,j]
  ##     print mitgcm_lh[i,j]
  ##     print mitgcm_swnet[i,j]
  ##     print mitgcm_lwnet[i,j]
  ##     print mitgcm_qnet[i,j]
  ##     print - mitgcm_sh[i,j] - mitgcm_lh[i,j] + mitgcm_swnet[i,j] + mitgcm_lwnet[i,j]
    
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
    cs = m.contourf(mitgcm_meshX,mitgcm_meshY,fieldName[i],\
                    clevs,extend='both',cmap=cmapList[i])
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
