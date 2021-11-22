#!/bin/csh -f

rm -rf build
rm -rf plot_data/*.png

cd run
./Allclean
cd ..

cd coupledSolver
make distclean
cd ..
