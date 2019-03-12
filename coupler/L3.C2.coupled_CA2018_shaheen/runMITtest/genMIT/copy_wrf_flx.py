#!/sw/xc40cle6/python/2.7.14/sles12.3_gnu7.2.0/bin/python
import sys, os, os.path
from netCDF4 import Dataset
import numpy as np
from scipy import interpolate

nXX = 432
nYY = 256

wrf_file = '../../runCase/wrfout_d01_2018-01-27_00:00:00';
wrf_results = Dataset(wrf_file,'r',format='NETCDF4');

wrf_namelist = ['T2','Q2','LH','HFX','U10','V10',\
                'SFCEVP',\
                'RAINC','RAINNC','RAINSH',\
                'LWDNB','LWUPB','SWDNB','SWUPB'];
mitgcm_namelist = ['atemp','aqh','hl','hs','uwind','vwind','evap','precip','lwflux','swflux'];
deltaT = 120.0;

for ni in range(10):
  mitgcm_fields_name = 'wrf_' + mitgcm_namelist[ni] + '_2min_2018'
  print 'writing item: ', mitgcm_fields_name

  if (mitgcm_namelist[ni] == 'evap'):
    evap_fields = wrf_results.variables['SFCEVP'][:];
    nTT = np.shape(evap_fields)[0]

    clean_array = np.zeros((nTT,nYY,nXX))
    for i in range(nXX):
      for j in range(nYY):
        for k in range(nTT-1):
          clean_array[k+1,j,i] = (evap_fields[k+1,j,i]-evap_fields[k,j,i])/deltaT/1000.0;

  elif (mitgcm_namelist[ni] == 'precip'):
    wrf_fields1 = wrf_results.variables['RAINC'][:];
    wrf_fields2 = wrf_results.variables['RAINNC'][:];
    wrf_fields3 = wrf_results.variables['RAINSH'][:];
    total_rain = wrf_fields1+wrf_fields2+wrf_fields3
    nTT = np.shape(wrf_fields1)[0]

    clean_array = np.zeros((nTT,nYY,nXX))
    for i in range(nXX):
      for j in range(nYY):
        for k in range(nTT-1):
          clean_array[k+1,j,i] = (total_rain[k+1,j,i]-total_rain[k,j,i])/deltaT/1000.0;

  elif (mitgcm_namelist[ni] == 'lwflux'):
    lwup_fields = wrf_results.variables['LWUPB'][:];
    lwdown_fields = wrf_results.variables['LWDNB'][:];

    nTT = np.shape(lwup_fields)[0]

    clean_array = np.zeros((nTT,nYY,nXX))
    for i in range(nXX):
      for j in range(nYY):
        for k in range(nTT):

          clean_array[k,j,i] = lwup_fields[k,j,i]-lwdown_fields[k,j,i]

  elif (mitgcm_namelist[ni] == 'swflux'):
    swup_fields = wrf_results.variables['SWUPB'][:];
    swdown_fields = wrf_results.variables['SWDNB'][:];

    nTT = np.shape(swup_fields)[0]

    clean_array = np.zeros((nTT,nYY,nXX))
    for i in range(nXX):
      for j in range(nYY):
        for k in range(nTT):

          clean_array[k,j,i] = swup_fields[k,j,i]-swdown_fields[k,j,i]

  else:
    wrf_fields = wrf_results.variables[wrf_namelist[ni]][:];
    nTT = np.shape(wrf_fields)[0]
    clean_array = np.zeros((nTT,nYY,nXX))
    for i in range(nXX):
      for j in range(nYY):
        for k in range(nTT):
          if (mitgcm_namelist[ni] == 'hl' or mitgcm_namelist[ni] == 'hs'):
            clean_array[k,j,i] = -wrf_fields[k,j,i]
          else:
            clean_array[k,j,i] = wrf_fields[k,j,i]

  clean_array.astype('>f4').tofile(mitgcm_fields_name)
