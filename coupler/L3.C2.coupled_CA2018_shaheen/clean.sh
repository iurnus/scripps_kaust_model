#!/bin/sh -f

export ESMFMKFILE=/home/x_sunr/kaust_project/regionalcoam/esmf/lib/libg/Unicos.intel.64.mpi.default/esmf.mk

rm -rf build build_mit
rm -rf code code_mit

cd coupledCode
make distclean
cd ..
