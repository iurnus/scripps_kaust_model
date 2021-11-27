#!/bin/sh

echo "ESMF location? : " ${ESMF_DIR}
echo "WRF413 (with OA coupling) location? : " ${WRF_DIR}
echo "MITgcm (source code) location? : " ${MITGCM_DIR}

read -e -p "Using Intel compiler? (Y/N) :" -i "N" intelFlag
if [ $intelFlag == 'Y' ]; then
  if [ $ESMF_OS == 'Linux' ]; then
    echo "Using Intel compiler"
    export MITGCM_OPT=mitgcm_optfile.ifort
  elif [ $ESMF_OS == 'Unicos' ]; then
    echo "Using Intel compiler for Cray"
    export MITGCM_OPT=mitgcm_optfile.cray
  fi
else 
  echo "Using PGI compiler"
  export MITGCM_OPT=mitgcm_optfile.pgi
fi
echo "The option file is: $MITGCM_OPT"

read -e -p "Continue? (Y/N) :" -i "Y" continueFlag
if [ $continueFlag == 'Y' ]; then
  echo "continue"
else 
  echo "stop"
  exit
fi

# # build the MITGCM as an executable
# mkdir build_mit code_mit
# cp utils/* build_mit/ # copy the scripts to install MITGCM
# cp mitCode/* code_mit/ # copy the scripts to install MITGCM
# cp mitSettingRS/* code_mit/ # copy the scripts to install MITGCM
# rm code_mit/exf_get* # remove the exf_get file so that MITGCM read the file input
# rm code_mit/main.F # remove the main file
# cd build_mit
# sed -i s/code/code_mit/g makescript_fwd.sh
# ./makescript_fwd.sh # install MITGCM, generate *.f files
# cd ..

# build the MITGCM as a library
mkdir build code
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
