#!/bin/csh -f

rm -rf build*
rm -rf code*

cd coupledCode
make distclean
cd ..
