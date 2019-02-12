#!/bin/sh

export ESMFMKFILE=../../../esmf/lib/libg/Linux.pgi.64.openmpi.default/esmf.mk

make distclean

ln -s ../../L3.C1.coupled_RS2012_ring/coupledCode/mod_*.F90 .
ln -s ../../L3.C1.coupled_RS2012_ring/coupledCode/mitgcm_wrf*.F90 .
ln -s ../../L3.C1.coupled_RS2012_ring/coupledCode/namelist* .

ln -s ../build/*.mod .
ln -s ../build/mmout/*.a .
ln -s ../build/setrlstk.o .
ln -s ../build/sigreg.o .

ln -s ../../../WRFV3911_AO/main/wrf_ESMFMod.o .
ln -s ../../../WRFV3911_AO/main/module_wrf_top.o .
ln -s ../../../WRFV3911_AO/main/libwrflib.a .

make
