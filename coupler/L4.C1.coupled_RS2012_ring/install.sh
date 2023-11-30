#!/bin/sh
read -e -p "The ocean-atmosphere model (no wave model) location? :" -i "${SKRIPS_DIR}/coupler/L3.C1.coupled_RS2012_ring/" OALocation
read -e -p "WRF413 (with wave model) location? :" -i "${WRF_DIR}/" wrfLocation
read -e -p "ESMF location? :" -i "${ESMF_DIR}" esmfLocation

read -e -p "Using PGI compiler? (Y/N) :" -i "Y" pgiFlag
if [ $pgiFlag == 'Y' ]; then
  echo "Using PGI compiler"
  export MITGCM_OPT=mitgcm_optfile.pgi
else 
  echo "Using default intel compiler"
  export MITGCM_OPT=mitgcm_optfile.ifort
fi
echo "The option file is: $MITGCM_OPT"

# build the MITGCM as a library
mkdir build code
cp ${OALocation}/utils/* build/ # copy the scripts to install MITGCM
cp ${OALocation}/mitCode/* code/ # copy the scripts to install MITGCM
cp utils/* build/ # copy the scripts to install MITGCM
cp mitCode/* code/ # copy the scripts to install MITGCM
cp mitSettingRS/* code/ # copy the scripts to install MITGCM
cd build
./makescript_fwd.sh # install MITGCM, generate *.f files

cp ${MPI_INC}/mpif* . 
./mkmod.sh ocn # install MITGCM as a library, generate *.mod files
cd ..

# build the test coupler
cd coupledCode
./Allmake.sh
cd ..

if [ -f ./coupledCode/esmf_application ]; then
  echo "Installation is successful!"
  echo The coupled model is installed as ./coupledCode/esmf_application
else 
  echo ERROR! Installation is NOT successful!
fi
