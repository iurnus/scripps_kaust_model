#!/bin/sh

# build MITGCM
echo "building MITgcm..."
echo "WARNING:: NEED MITGCM SOURCE FILE in $MITGCM_DIR"

read -e -p "Using PGI compiler? (Y/N) :" -i "Y" pgiFlag
if [ $pgiFlag == 'Y' ]; then
  echo "Using PGI compiler"
  export MITGCM_OPT=mitgcm_optfile.pgi
else 
  echo "Using default intel compiler"
  export MITGCM_OPT=mitgcm_optfile.ifort
fi

echo "The option file is: $MITGCM_OPT"

export L2C1_DIR=${SKRIPS_DIR}/coupler/L2.C1.mitgcm_case_CA2009_mpiESMF_1cpu

# build the MITGCM as a library
mkdir build/
cp ${L2C1_DIR}/utils/* build/ # copy the scripts to install MITGCM
cp -rf ${L2C1_DIR}/mitCode/ code/ # copy the scripts to install MITGCM
rm -rf code/SIZE.h
cp patches/SIZE.h code/ # update from patch
cd build
./makescript_fwd.sio.ring # install MITGCM, generate *.f files

cp ${SKRIPS_MPI_INC}/mpif* . 
./mkmod.sh ocn # install MITGCM as a library, generate *.mod files
cd ..

# build the test coupler
cp -rf ${L2C1_DIR}/coupledSolver/ coupledSolver/ # copy the coupledSolver DIR
rm -rf coupledSolver/mod_esmf_atm.F90
cp -rf patches/*.F90 coupledSolver/ # update from patch
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
