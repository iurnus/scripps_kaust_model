#!/bin/sh
export MPI_HOME="/project_shared/Libraries/openmpi-2.1.1_pgi_fortran_17.5-0/include"
export MITGCM_DIR="/home/rus043/scripps_coupled_model_github/MITgcm_c67m"

read -e -p "WRF412 (with OA coupling) location? :" -i "$PWD/../../WRFV412_AO_01/" wrfLocation
read -e -p "ESMF location? :" -i "$PWD/../../esmf/" esmfLocation
sed -i "1s@.*@WRF_DIR=$wrfLocation@" coupledCode/wrflib.mk
sed -i "2s@.*@ESMF_DIR=$esmfLocation@" coupledCode/wrflib.mk

sed -i "3s@.*@WRF_DIR=$wrfLocation@" coupledCode/Allmake.sh
sed -i "3s@.*@WRF_DIR=$wrfLocation@" runCase.init/Allrun
sed -i "3s@.*@WRF_DIR=$wrfLocation@" runCase/Allrun
sed -i "3s@.*@WRF_DIR=$wrfLocation@" runWRFtest/Allrun

# build the MITGCM as an executable
mkdir build_mit code_mit
cp utils/* build_mit/ # copy the scripts to install MITGCM
cp mitCode/* code_mit/ # copy the scripts to install MITGCM
cp mitSettingCA/* code_mit/ # copy the scripts to install MITGCM
rm code_mit/exf_get* # remove the exf_get file so that MITGCM read the file input
cd build_mit
sed -i s/code/code_mit/g makescript_fwd.sio.ring
./makescript_fwd.sio.ring ${MITGCM_DIR} # install MITGCM, generate *.f files
cd ..

# build the MITGCM as a library
mkdir build code
cp utils/* build/ # copy the scripts to install MITGCM
cp mitCode/* code/ # copy the scripts to install MITGCM
cp mitSettingCA/* code/ # copy the scripts to install MITGCM
cd build
./makescript_fwd.sio.ring ${MITGCM_DIR} # install MITGCM, generate *.f files

cp ${MPI_HOME}/mpif* . 
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
