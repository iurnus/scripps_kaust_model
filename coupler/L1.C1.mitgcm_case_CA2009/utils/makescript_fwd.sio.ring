#!/bin/bash

rm -f *.o
rm -f *.f
make CLEAN
rm Makefile

../../../MITgcm_c67m/tools/genmake2 "-rootdir" "../../../MITgcm_c67m" "-mpi" "-mods" "../code" "-optfile" "./mitgcm_optfile"
make depend
make -j8
