#!/bin/sh

WRF_DIR=${SKRIPS_DIR}/WRFV413_AO/

make distclean

ln -s ../build/*.mod .
ln -s ../build/mmout/*.a .
ln -s ../build/setrlstk.o .
ln -s ../build/sigreg.o .

make
