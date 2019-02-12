#!/bin/sh

export ESMFMKFILE=../../../esmf/lib/libg/Unicos.intel.64.mpi.default/esmf.mk

make distclean

ln -s ../../L3.C1.coupled_RS2012_ring/coupledCode/mod_* .
ln -s ../../L3.C1.coupled_RS2012_ring/coupledCode/mitgcm_wrf* .
ln -s ../../L3.C1.coupled_RS2012_ring/coupledCode/namelist* .

sed -i s/#include/include/g mod_esmf_atm.F90

ln -s ../build/*.mod .
ln -s ../build/mmout/*.a .
ln -s ../build/setrlstk.o .
ln -s ../build/sigreg.o .

# ln -s ../../../WRFV36_AO/main/wrf_test_ESMF.o .
ln -s ../../../WRFV3911_AO/main/wrf_ESMFMod.o .
ln -s ../../../WRFV3911_AO/main/module_wrf_top.o .
ln -s ../../../WRFV3911_AO/main/libwrflib.a .

make
