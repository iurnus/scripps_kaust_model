#!/bin/csh -f

setenv ESMFMKFILE "../../../esmf/lib/libg/Linux.pgi.64.openmpi.default/esmf.mk"

rm -rf build
rm -rf code

cd mitRun
./Allclean
cd ..

cd coupledSolver
make distclean
cd ..
