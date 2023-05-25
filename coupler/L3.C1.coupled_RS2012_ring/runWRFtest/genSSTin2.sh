#!/bin/bash

# NCL_ADD
export PATH=/home/rus043/anaconda2/bin:$PATH
source activate ncl_stable

rm -rf cplFlux2 wrfout2.nc
ln -sf ./wrfout_d01_2012-06-01_00:00:00 wrfout2.nc

ncks -v Times wrfout2.nc cplFlux2
printf 'a\n1\n' | ncks -v SST wrfout2.nc cplFlux2
printf 'a\n1\n' | ncks -v SST_INPUT wrfout2.nc cplFlux2
printf 'a\n1\n' | ncks -v UOCE wrfout2.nc cplFlux2
printf 'a\n1\n' | ncks -v VOCE wrfout2.nc cplFlux2
printf 'a\n1\n' | ncks -v ALBBCK wrfout2.nc cplFlux2
printf 'a\n1\n' | ncks -v VEGFRA wrfout2.nc cplFlux2
printf 'a\n1\n' | ncks -v SEAICE wrfout2.nc cplFlux2
printf 'a\n1\n' | ncks -v OCNMASK wrfout2.nc cplFlux2

ncdump cplFlux2 > log2
