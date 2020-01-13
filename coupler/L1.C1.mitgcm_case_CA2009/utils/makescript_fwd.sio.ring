#!/bin/bash

rm -f *.o
rm -f *.f
make CLEAN
rm Makefile

${SKRIPS_DIR}/MITgcm_c67m/tools/genmake2 "-rootdir" "${SKRIPS_DIR}/MITgcm_c67m" "-mpi" "-mods" "../code" "-optfile" "$MITGCM_OPT"
make depend
make -j8
