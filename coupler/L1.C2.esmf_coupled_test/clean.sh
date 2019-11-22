#!/bin/csh -f

rm -rf build

cd run
./Allclean
cd ..

cd coupledSolver
make distclean
cd ..
