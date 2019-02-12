#!/bin/bash

MPI_HOME="/project_shared/Libraries/openmpi-2.1.1_pgi_fortran_17.5-0/include"

rm -f *.o
rm -f *.f
make CLEAN
rm Makefile

../../../MITgcm_c66h/tools/genmake2 "-rootdir" "../../../MITgcm_c66h" "-mpi" "-mods" "../code" "-optfile" "./mitgcm_optfile"
make depend
make -j8
