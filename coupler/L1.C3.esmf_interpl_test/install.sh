#!/bin/csh -f

# build the test coupler
cd coupledSolver
# set the path of ESMF installation
./Allmake.sh
cd ..

# run the test coupler
cd run
./Allrun
