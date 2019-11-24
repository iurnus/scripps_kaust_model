#!/bin/csh -f
setenv MPI_HOME "/project_shared/Libraries/openmpi-2.1.1_pgi_fortran_17.5-0/include"
setenv MITGCM_DIR "/home/rus043/scripps_kaust_model_github/MITgcm_c67m"
setenv L2C1_DIR "../L2.C1.mitgcm_case_CA2009_mpiESMF_1cpu/"

# build the MITGCM as a library
mkdir build/
cp ${L2C1_DIR}/utils/* build/ # copy the scripts to install MITGCM
cp -rf ${L2C1_DIR}/mitCode/ code/ # copy the scripts to install MITGCM
rm -rf code/SIZE.h
cp patches/SIZE.h code/ # update from patch
cd build
./makescript_fwd.sio.ring ${MITGCM_DIR}# install MITGCM, generate *.f files

cp ${MPI_HOME}/mpif* . 
./mkmod.sh ocn # install MITGCM as a library, generate *.mod files
cd ..

# build the test coupler
cp -rf ${L2C1_DIR}/coupledSolver/ coupledSolver/ # copy the coupledSolver DIR
rm -rf coupledSolver/mod_esmf_atm.F90
rm -rf coupledSolver/mod_esmf_ocn.F90
cp -rf patches/*.F90 coupledSolver/ # update from patch
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
