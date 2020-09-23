#!/bin/sh

WRF_DIR=${SKRIPS_DIR}/WRFV413_AO/

make distclean

ln -s ../../L3.C1.coupled_RS2012_ring/coupledCode/mod_*.F90 .
ln -s ../../L3.C1.coupled_RS2012_ring/coupledCode/mitgcm_wrf*.F90 .

ln -s ../build/*.mod .
ln -s ../build/mmout/*.a .
ln -s ../build/setrlstk.o .
ln -s ../build/sigreg.o .

make
