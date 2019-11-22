#!/bin/csh -f

# build the test coupler
cd coupledSolver
# set the path of ESMF installation
./Allmake.sh
cd ..

cd run
./Allrun
