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

${SKRIPS_DIR}/MITgcm_c67m/tools/genmake2 "-rootdir" "${SKRIPS_DIR}/MITgcm_c67m" "-mpi" "-mods" "../code" "-optfile" "$MITGCM_OPT"
make depend
make -j8
