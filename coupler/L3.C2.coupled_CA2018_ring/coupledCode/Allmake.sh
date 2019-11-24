#!/bin/sh

WRF_DIR=/home/rus043/scripps_kaust_model_github/coupler/L3.C2.coupled_CA2018_ring/../../WRFV412_AO_01/

make distclean

ln -s ../../L3.C1.coupled_RS2012_ring/coupledCode/mod_*.F90 .
ln -s ../../L3.C1.coupled_RS2012_ring/coupledCode/mitgcm_wrf*.F90 .

ln -s ../build/*.mod .
ln -s ../build/mmout/*.a .
ln -s ../build/setrlstk.o .
ln -s ../build/sigreg.o .

ln -s $WRF_DIR/main/wrf_ESMFMod.o .
ln -s $WRF_DIR/main/module_wrf_top.o .
ln -s $WRF_DIR/main/libwrflib.a .

make
