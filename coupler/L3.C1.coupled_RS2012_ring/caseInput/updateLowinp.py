#!/home/rus043/miniconda3/bin/python
import sys, os, os.path
import matplotlib.pyplot as plt
from netCDF4 import Dataset
import numpy as np
from scipy import interpolate

nXX = 128
nYY = 128

bathymetry   = './bathymetry_rs.bin'
hycomSSTFile = './hycom_T_ccs_newtopo_01Jun2012.bin'
hycomUFile   = './hycom_U_ccs_newtopo_01Jun2012.bin'
hycomVFile   = './hycom_V_ccs_newtopo_01Jun2012.bin'

deepRaw = np.fromfile(bathymetry,dtype='>f4');
sstRaw  = np.fromfile(hycomSSTFile,dtype='>f4');
uoceRaw = np.fromfile(hycomUFile,dtype='>f4');
voceRaw = np.fromfile(hycomVFile,dtype='>f4');
ini_deep = np.reshape(deepRaw,(nYY,nXX));
ini_sst  = np.reshape(sstRaw,(40,nYY,nXX));
ini_uoce = np.reshape(uoceRaw,(40,nYY,nXX));
ini_voce = np.reshape(voceRaw,(40,nYY,nXX));
ini_deep_ = ini_deep[:,:]
ini_sst_  = ini_sst[0,:,:]+273.15
ini_uoce_ = ini_uoce[0,:,:]
ini_voce_ = ini_voce[0,:,:]

wrf_results = Dataset('wrfinput_d01','r+',format='NETCDF4');
wrf_sst = wrf_results.variables['SST'][:];
wrf_ssti = wrf_results.variables['SST_INPUT'][:];
wrf_tsk = wrf_results.variables['TSK'][:];
wrf_uoce = wrf_results.variables['UOCE'][:];
wrf_voce = wrf_results.variables['VOCE'][:];
wrf_ocnmask = wrf_results.variables['OCNMASK'][:];

sst_write = np.zeros((nYY,nXX))
ssti_write = np.zeros((nYY,nXX))
tsk_write = np.zeros((nYY,nXX))
uoce_write = np.zeros((nYY,nXX))
voce_write = np.zeros((nYY,nXX))
ocnmask_write = np.zeros((nYY,nXX))
for j in range(nYY):
  for i in range(nXX):
    if ini_deep_[j,i] < -0.01:
      print(ini_deep_[j,i])
      sst_write[j,i]  = ini_sst_[j,i]
      ssti_write[j,i] = ini_sst_[j,i]
      tsk_write[j,i]  = ini_sst_[j,i]
      uoce_write[j,i] = ini_uoce_[j,i]
      voce_write[j,i] = ini_voce_[j,i]
      ocnmask_write[j,i] = 1.0
    else:
      sst_write[j,i]  = wrf_tsk[0,j,i]
      ssti_write[j,i] = wrf_tsk[0,j,i]
      tsk_write[j,i]  = wrf_tsk[0,j,i]
      ocnmask_write[j,i] = 0.0

print(np.mean(ocnmask_write))
wrf_results.variables['SST'][0,:,:] = sst_write;
wrf_results.variables['SST_INPUT'][0,:,:] = ssti_write;
wrf_results.variables['TSK'][0,:,:] = tsk_write;
wrf_results.variables['UOCE'][0,:,:] = uoce_write;
wrf_results.variables['VOCE'][0,:,:] = voce_write;
# wrf_results.variables['OCNMASK'][0,:,:] = ocnmask_write;

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
      ssti_write_low[k,j,i] = wrf_sst_low[k,j,i]

  print("wrf sst at step ", k, " is: ", np.mean(wrf_sst_low[k,:,:]))
  print("wrf sst_input at step ", k, " is: ", np.mean(ssti_write_low[k,:,:]))

wrf_results_low.variables['SST_INPUT'][:,:,:] = ssti_write_low;
wrf_results_low.variables['SST'][:,:] = ssti_write_low;
wrf_results_low.close()

