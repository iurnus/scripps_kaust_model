#!/bin/csh -f

# build the MITGCM as a library
mkdir build/
cp utils/* build/ # copy the scripts to install MITGCM
cp -rf mitCode/ code/ # copy the scripts to install MITGCM
cd build
./makescript_fwd.sio.ring # install MITGCM, generate *.f files

setenv MPI_HOME "/project_shared/Libraries/openmpi-2.1.1_pgi_fortran_17.5-0/include"
cp ${MPI_HOME}/mpif* . 
./mkmod.sh ocn # install MITGCM as a library, generate *.mod files
cd ..

# build the test coupler
cd coupledSolver
# set the path of ESMF installation
setenv ESMFMKFILE "../../../esmf/lib/libg/Linux.pgi.64.openmpi.default/esmf.mk"
ln -s ../build/*.mod . # link the mod file to the coupler
ln -s ../build/mmout/*.a . # link the library to the coupler
ln -s ../build/setrlstk.o .
ln -s ../build/sigreg.o .
make
cd ..

cd mitRun
./Allrun
cd ..
