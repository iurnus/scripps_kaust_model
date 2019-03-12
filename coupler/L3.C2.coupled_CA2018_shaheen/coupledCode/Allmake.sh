#!/bin/sh

WRF_DIR=/home/x_sunr/coupleCOAW/WRFV3911_AO/
ESMF_DIR=/home/x_sunr/coupleCOAW/esmf/


export ESMFMKFILE=$ESMF_DIR/lib/libg/Unicos.intel.64.mpi.default/esmf.mk

make distclean

ln -s ../../L3.C1.coupled_RS2012_ring/coupledCode/mod_* .
ln -s ../../L3.C1.coupled_RS2012_ring/coupledCode/mitgcm_wrf* .

sed -i s/#include/include/g mod_esmf_atm.F90

ln -s ../build/*.mod .
ln -s ../build/mmout/*.a .
ln -s ../build/setrlstk.o .
ln -s ../build/sigreg.o .

ln -s $WRF_DIR/main/wrf_ESMFMod.o .
ln -s $WRF_DIR/main/module_wrf_top.o .
ln -s $WRF_DIR/main/libwrflib.a .

make
