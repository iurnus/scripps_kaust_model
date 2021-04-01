#!/bin/sh
export MITGCM_DIR=${SKRIPS_DIR}/MITgcm_c67m


read -e -p "WRF413 (with OA coupling) location? :" -i "$SKRIPS_DIR/WRFV413_AO/" wrfLocation
read -e -p "ESMF location? :" -i "$SKRIPS_DIR/esmf/" esmfLocation
read -e -p "MITgcm location? :" -i "$SKRIPS_DIR/MITgcm_c67m/" MITGCM_DIR
read -e -p "COUPLER location? :" -i "$SKRIPS_DIR/coupler/" couplerLocation
# sed -i "1s@.*@WRF_DIR=$wrfLocation@" coupledCode/wrflib.mk
# sed -i "2s@.*@ESMF_DIR=$esmfLocation@" coupledCode/wrflib.mk
# sed -i "3s@.*@WRF_DIR=$wrfLocation@" coupledCode/Allmake.sh
# sed -i "4s@.*@ESMF_DIR=$esmfLocation@" coupledCode/Allmake.sh
# sed -i "5s@.*@COUPLER_DIR=$couplerLocation@" coupledCode/Allmake.sh
# sed -i "3s@.*@WRF_DIR=$wrfLocation@" runCase.init/Allrun
# sed -i "3s@.*@WRF_DIR=$wrfLocation@" runCase/Allrun
# sed -i "3s@.*@WRF_DIR=$wrfLocation@" runWRFtest/Allrun

# build the MITGCM as an executable
mkdir build_mit code_mit
cp utils/* build_mit/ # copy the scripts to install MITGCM
cp mitCode/* code_mit/ # copy the scripts to install MITGCM
cp mitSettingCA/* code_mit/ # copy the scripts to install MITGCM
rm code_mit/exf_get* # remove the exf_get file so that MITGCM read the file input
cd build_mit
sed -i s/code/code_mit/g makescript_fwd.sio.shaheen
./makescript_fwd.sio.shaheen ${MITGCM_DIR} # install MITGCM, generate *.f files
cd ..

# build the MITGCM as a library
cp -rf ${couplerLocation}/L3.C1.coupled_RS2012_ring/mitCode . 
mkdir build
mkdir code
cp utils/* build/ # copy the scripts to install MITGCM
cp mitCode/* code/ # copy the scripts to install MITGCM
cp mitSettingCA/* code/ # copy the scripts to install MITGCM
cd build
./makescript_fwd.sio.shaheen ${MITGCM_DIR} # install MITGCM, generate *.f files

cp ${MPI_DIR}/include/mpif* . 
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
