#!/bin/sh

WRF_DIR=${SKRIPS_DIR}/WRFV412_AO/

make distclean

ln -s ../build/*.mod .
ln -s ../build/mmout/*.a .
ln -s ../build/setrlstk.o .
ln -s ../build/sigreg.o .

ln -s $WRF_DIR/main/wrf_ESMFMod.o .
ln -s $WRF_DIR/main/module_wrf_top.o .
ln -s $WRF_DIR/main/libwrflib.a .

make
