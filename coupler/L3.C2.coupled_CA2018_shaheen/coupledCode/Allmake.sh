#!/bin/sh

make distclean

ln -s $SKRIPS_DIR/coupler/L3.C1.coupled_RS2012_ring/coupledCode/mod_* .
ln -s $SKRIPS_DIR/coupler/L3.C1.coupled_RS2012_ring/coupledCode/mitgcm_wrf* .

sed -i s/#include/include/g mod_esmf_atm.F90

ln -s ../build/*.mod .
ln -s ../build/mmout/*.a .
ln -s ../build/setrlstk.o .
ln -s ../build/sigreg.o .

make
