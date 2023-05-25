#!/bin/bash

# NCL_ADD
export PATH=/home/rus043/anaconda2/bin:$PATH
source activate ncl_stable

rm -rf cplFlux wrfout.nc
ln -sf ../runCase/wrfout_d01_2012-06-01_00:00:00 wrfout.nc

ncks -v Times wrfout.nc cplFlux
printf 'a\n1\n' | ncks -v SST wrfout.nc cplFlux
printf 'a\n1\n' | ncks -v SST_INPUT wrfout.nc cplFlux
printf 'a\n1\n' | ncks -v UOCE wrfout.nc cplFlux
printf 'a\n1\n' | ncks -v VOCE wrfout.nc cplFlux
printf 'a\n1\n' | ncks -v ALBBCK wrfout.nc cplFlux
printf 'a\n1\n' | ncks -v VEGFRA wrfout.nc cplFlux
printf 'a\n1\n' | ncks -v LAI wrfout.nc cplFlux
printf 'a\n1\n' | ncks -v SEAICE wrfout.nc cplFlux
printf 'a\n1\n' | ncks -v OCNMASK wrfout.nc cplFlux

cp cplFlux cplFlux_save
ncdump cplFlux > log1
