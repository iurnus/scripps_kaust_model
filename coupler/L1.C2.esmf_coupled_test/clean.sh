#!/bin/csh -f

setenv ESMFMKFILE "../../../esmf/lib/libg/Linux.pgi.64.openmpi.default/esmf.mk"

rm -rf build

cd run
./Allclean
cd ..

cd coupledSolver
make distclean
cd ..
