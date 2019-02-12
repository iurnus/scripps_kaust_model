#!/bin/csh -f

# build the test coupler
cd coupledSolver
# set the path of ESMF installation
setenv ESMFMKFILE "../../../esmf/lib/libg/Linux.pgi.64.openmpi.default/esmf.mk"
./Allmake.sh
cd ..

cd run
./Allrun
