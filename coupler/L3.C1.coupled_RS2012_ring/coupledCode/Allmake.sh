#!/bin/sh

WRF_DIR=/home/rus043/scripps_coupled_model_github/coupler/L3.C1.coupled_RS2012_ring/../../WRFV412_AO_01/

make distclean

ln -s ../build/*.mod .
ln -s ../build/mmout/*.a .
ln -s ../build/setrlstk.o .
ln -s ../build/sigreg.o .

ln -s $WRF_DIR/main/wrf_ESMFMod.o .
ln -s $WRF_DIR/main/module_wrf_top.o .
ln -s $WRF_DIR/main/libwrflib.a .

make
