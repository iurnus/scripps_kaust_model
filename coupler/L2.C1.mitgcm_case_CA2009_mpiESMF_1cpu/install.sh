#!/bin/sh
export MITGCM_DIR=${SKRIPS_DIR}/MITgcm_c67m

export MITGCM_OPT=mitgcm_optfile.pgi
echo "The option file is: $MITGCM_OPT"

# build the MITGCM as a library
mkdir build/
cp utils/* build/ # copy the scripts to install MITGCM
cp -rf mitCode/ code/ # copy the scripts to install MITGCM
cd build
./makescript_fwd.sio.ring ${MITGCM_DIR}# install MITGCM, generate *.f files

cp ${SKRIPS_MPI_DIR}/mpif* . 
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

if ( -f ./coupledSolver/esmf_application ) then
  echo Installation is successful!
  echo The coupled model is installed as ./coupledSolver/esmf_application
else 
  echo ERROR! Installation is NOT successful!
endif
