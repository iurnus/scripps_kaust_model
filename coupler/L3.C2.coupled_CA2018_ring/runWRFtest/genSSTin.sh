#!/bin/bash

# NCL_ADD
export PATH=/home/rus043/anaconda2/bin:$PATH
source activate ncl_stable

rm cplFlux wrfout.nc
ln -sf ../runCase/wrfout_d01_2018-01-27_00:00:00 wrfout.nc

ncks -v Times wrfout.nc cplFlux
printf 'a\n1\n' | ncks -v SST wrfout.nc cplFlux
printf 'a\n1\n' | ncks -v UOCE wrfout.nc cplFlux
printf 'a\n1\n' | ncks -v VOCE wrfout.nc cplFlux
printf 'a\n1\n' | ncks -v ALBBCK wrfout.nc cplFlux
printf 'a\n1\n' | ncks -v VEGFRA wrfout.nc cplFlux
