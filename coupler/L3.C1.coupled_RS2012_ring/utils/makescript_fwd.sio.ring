#!/bin/csh -f

rm -f *.o
rm -f *.f
make CLEAN
rm Makefile

${MITGCM_DIR}/tools/genmake2 "-rootdir" "${MITGCM_DIR}" "-mpi" "-mods" "../code" "-optfile" "$MITGCM_OPT"
make depend
make -j8
