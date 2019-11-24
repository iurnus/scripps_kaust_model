#!/bin/csh -f

echo $1

if ( $# == 1 ) then
set MITGCM_DIR = ( $1 )
else
set MITGCM_DIR = ( "../../../MITgcm_c67m" )
endif

rm -f *.o
rm -f *.f
make CLEAN
rm Makefile

${MITGCM_DIR}/tools/genmake2 "-rootdir" "${MITGCM_DIR}" "-mpi" "-mods" "../code" "-optfile" "./mitgcm_optfile"
make depend
make -j8
