#!/sw/xc40cle6/python/2.7.14/sles12.3_gnu7.2.0/bin/python
import sys, os, os.path
import matplotlib.pyplot as plt
from netCDF4 import Dataset
import numpy as np
from scipy import interpolate

nXX = 432
nYY = 256

hycomSSTFile = './hycom_T_ccs_newtopo_27Jan2018.bin'
hycomUFile = './hycom_U_ccs_newtopo_27Jan2018.bin'
hycomVFile = './hycom_V_ccs_newtopo_27Jan2018.bin'
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
wrf_tsk = wrf_results.variables['TSK'][:];
wrf_uoce = wrf_results.variables['UOCE'][:];
wrf_voce = wrf_results.variables['VOCE'][:];
wrf_landmask = wrf_results.variables['LANDMASK'][:];

sst_write = np.zeros((nYY,nXX))
tsk_write = np.zeros((nYY,nXX))
uoce_write = np.zeros((nYY,nXX))
voce_write = np.zeros((nYY,nXX))
for j in range(nXX):
  for i in range(nYY):
    if wrf_landmask[0,i,j] == 0:
      sst_write[i,j] = ini_sst_[i,j]
      tsk_write[i,j] = ini_sst_[i,j]
      uoce_write[i,j] = ini_uoce_[i,j]
      voce_write[i,j] = ini_voce_[i,j]

wrf_results.variables['SST'][0,:,:] = sst_write;
wrf_results.variables['TSK'][0,:,:] = tsk_write;
wrf_results.variables['UOCE'][0,:,:] = uoce_write;
wrf_results.variables['VOCE'][0,:,:] = voce_write;

wrf_results.close()

