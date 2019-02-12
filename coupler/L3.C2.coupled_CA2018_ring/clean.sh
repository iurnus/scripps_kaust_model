#!/bin/csh -f

rm -rf build*
rm -rf code*

cd coupledCode
setenv ESMFMKFILE "../../../esmf/lib/libg/Linux.pgi.64.openmpi.default/esmf.mk"
make distclean
cd ..
