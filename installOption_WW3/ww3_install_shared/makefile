mpi_mod=yes
export mpi_mod

WW3_DIR=${SKRIPS_WAVE_DIR}/ww3-516/
aPb=${WW3_DIR}/bin
aPo=${WW3_DIR}/obj
aPm=${WW3_DIR}/mod
aPe=${WW3_DIR}/exe

ww3_esmf.o : ww3_esmf.ftn $(WW3_DIR)/ftn/w3macros.h  $(aPo)/w3initmd.o $(aPo)/w3wavemd.o $(aPo)/w3wdasmd.o $(aPo)/w3fldsmd.o $(aPo)/w3gdatmd.o $(aPo)/w3wdatmd.o $(aPo)/w3adatmd.o $(aPo)/w3odatmd.o $(aPo)/w3idatmd.o $(aPo)/w3iogrmd.o $(aPo)/w3iogomd.o $(aPo)/w3iopomd.o $(aPo)/w3tidemd.o $(aPo)/w3servmd.o $(aPo)/w3timemd.o
	@$(aPb)/ad3 ww3_esmf
