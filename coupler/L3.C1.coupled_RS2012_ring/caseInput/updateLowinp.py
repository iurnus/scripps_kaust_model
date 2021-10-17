#!/home/rus043/anaconda2/bin/python
import sys, os, os.path
import matplotlib.pyplot as plt
from netCDF4 import Dataset
import numpy as np
from scipy import interpolate

nXX = 128
nYY = 128

hycomSSTFile = './hycom_T_ccs_newtopo_01Jun2012.bin'
hycomUFile   = './hycom_U_ccs_newtopo_01Jun2012.bin'
hycomVFile   = './hycom_V_ccs_newtopo_01Jun2012.bin'

sstRaw = np.fromfile(hycomSSTFile,dtype='>f4');
uoceRaw = np.fromfile(hycomUFile,dtype='>f4');
voceRaw = np.fromfile(hycomVFile,dtype='>f4');
ini_sst = np.reshape(sstRaw,(40,nYY,nXX));
ini_uoce = np.reshape(uoceRaw,(40,nYY,nXX));
ini_voce = np.reshape(voceRaw,(40,nYY,nXX));
ini_sst_ = ini_sst[0,:,:]+273.15
ini_uoce_ = ini_uoce[0,:,:]
ini_voce_ = ini_voce[0,:,:]

wrf_results = Dataset('wrfinput_d01','r+',format='NETCDF4');
wrf_sst = wrf_results.variables['SST'][:];
wrf_ssti = wrf_results.variables['SST_INPUT'][:];
wrf_tsk = wrf_results.variables['TSK'][:];
wrf_uoce = wrf_results.variables['UOCE'][:];
wrf_voce = wrf_results.variables['VOCE'][:];
wrf_landmask = wrf_results.variables['LANDMASK'][:];

sst_write = np.zeros((nYY,nXX))
ssti_write = np.zeros((nYY,nXX))
tsk_write = np.zeros((nYY,nXX))
uoce_write = np.zeros((nYY,nXX))
voce_write = np.zeros((nYY,nXX))
for j in range(nYY):
  for i in range(nXX):
    if wrf_landmask[0,j,i] < 0.5:
      sst_write[j,i]  = ini_sst_[j,i]
      ssti_write[j,i] = ini_sst_[j,i]
      tsk_write[j,i]  = ini_sst_[j,i]
      uoce_write[j,i] = ini_uoce_[j,i]
      voce_write[j,i] = ini_voce_[j,i]
    else:
      sst_write[j,i]  = wrf_sst[0,j,i]
      ssti_write[j,i] = wrf_ssti[0,j,i]
      tsk_write[j,i]  = wrf_tsk[0,j,i]

wrf_results.variables['SST'][0,:,:] = sst_write;
wrf_results.variables['SST_INPUT'][0,:,:] = ssti_write;
wrf_results.variables['TSK'][0,:,:] = tsk_write;
wrf_results.variables['UOCE'][0,:,:] = uoce_write;
wrf_results.variables['VOCE'][0,:,:] = voce_write;

wrf_results.close()
print("mitgcm sst is: ", np.mean(ini_sst_))
print("old wrf sst is: ", np.mean(wrf_sst))
print("new wrf sst is: ", np.mean(sst_write))

wrf_results_low = Dataset('wrflowinp_d01','r+',format='NETCDF4');
wrf_sst_low = wrf_results_low.variables['SST'][:];
wrf_ssti_low = wrf_results_low.variables['SST_INPUT'][:];

nt,ny,nx = np.shape(wrf_ssti_low)
ssti_write_low = np.zeros((nt,ny,nx))
for k in range(nt):
  for j in range(ny):
    for i in range(nx):
      ssti_write_low[k,j,i] = wrf_ssti_low[k,j,i]

  print("wrf sst input at step ", nt, " is: ", np.mean(sst_write))

wrf_results_low.variables['SST_INPUT'][:,:] = ssti_write_low;
wrf_results_low.variables['SST'][:,:] = ssti_write_low;
wrf_results_low.close()

