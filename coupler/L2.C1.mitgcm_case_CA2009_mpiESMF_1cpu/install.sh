#!/bin/sh

# build MITGCM
echo "building MITgcm..."
echo "WARNING:: NEED MITGCM SOURCE FILE in $MITGCM_DIR"

export MITGCM_COMPILER=$ESMF_COMPILER
read -e -p "Using default ESMF compiler $MITGCM_COMPILER? (Y/N): " -i "Y" defaultFlag
# read -e -p "Using PGI compiler? (Y/N) :" -i "Y" pgiFlag
if [ $defaultFlag == 'Y' ]; then
  echo "Using $MITGCM_COMPILER compiler"
  export MITGCM_OPT=mitgcm_optfile.$MITGCM_COMPILER
else 
  read -e -p "Which compiler do you want to use? (ifort/pgi/gfortran): " -i "pgi" CUSTOM_COMPILER
  export MITGCM_OPT=mitgcm_optfile.$CUSTOM_COMPILER
fi

echo "The option file is: $MITGCM_OPT"

# build the MITGCM as a library
mkdir build/
cp utils/* build/ # copy the scripts to install MITGCM
cp -rf mitCode/ code/ # copy the scripts to install MITGCM
cd build
./makescript_fwd.sio.ring # install MITGCM, generate *.f files

cp ${SKRIPS_MPI_INC}/mpif* . 
./mkmod.sh ocn # install MITGCM as a library, generate *.mod files
cd ..

# build the test coupler
cd coupledSolver
# set the path of ESMF installation
ln -s ../build/*.mod . # link the mod file to the coupler
ln -s ../build/mmout/*.a . # link the library to the coupler
ln -s ../build/setrlstk.o .
ln -s ../build/sigreg.o .
make
cd ..

if [ -f ./coupledSolver/esmf_application ]; then
  echo Installation is successful!
  echo The coupled model is installed as ./coupledSolver/esmf_application
else 
  echo ERROR! Installation is NOT successful!
fi
