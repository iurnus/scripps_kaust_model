#!/bin/sh
read -e -p "The ocean-atmosphere model (no wave model) location? :" -i "${SKRIPS_DIR}/coupler/L3.C1.coupled_RS2012_ring/" OALocation
read -e -p "WRF452 (with wave model) location? :" -i "${WRF_DIR}/" wrfLocation
read -e -p "WW3 (with RWND switch off) location? :" -i "${WW3_DIR}/" ww3Location
read -e -p "ESMF location? :" -i "${ESMF_DIR}" esmfLocation

export MITGCM_COMPILER=$ESMF_COMPILER
read -e -p "Using default ESMF compiler $MITGCM_COMPILER? (Y/N): " -i "Y" defaultFlag
if [ $defaultFlag == 'Y' ]; then
  echo "Using $MITGCM_COMPILER compiler"
  export MITGCM_OPT=mitgcm_optfile.$MITGCM_COMPILER
else 
  read -e -p "Which compiler do you want to use? (ifort/pgi/gfortran): " -i "pgi" CUSTOM_COMPILER
  export MITGCM_OPT=mitgcm_optfile.$CUSTOM_COMPILER
fi
echo "The option file is: $MITGCM_OPT"

read -e -p "Continue? (Y/N) :" -i "Y" continueFlag
if [ $continueFlag == 'Y' ]; then
  echo "continue"
else 
  echo "stop"
  exit
fi

# build the MITGCM as a library
mkdir build code
cp ${OALocation}/utils/* build/ # copy the scripts to install MITGCM
cp ${OALocation}/mitCode/* code/ # copy the scripts to install MITGCM
cp utils/* build/ # copy the scripts to install MITGCM
cp mitCode/* code/ # copy the scripts to install MITGCM
cp mitSettingRS/* code/ # copy the scripts to install MITGCM
cd build
./makescript_fwd.sh # install MITGCM, generate *.f files

cp ${SKRIPS_MPI_INC}/mpif* . 
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
