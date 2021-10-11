mpi_mod=yes
export mpi_mod

aPb=${WW3_DIR}/model/bin
aPo=${WW3_DIR}/model/obj
aPm=${WW3_DIR}/model/mod
aPe=${WW3_DIR}/model/exe

ww3_esmf.o : ww3_esmf.ftn $(WW3_DIR)/model/ftn/w3macros.h  $(aPo)/w3adatmd.o $(aPo)/w3fldsmd.o $(aPo)/w3gdatmd.o $(aPo)/w3idatmd.o $(aPo)/w3initmd.o $(aPo)/w3iogomd.o $(aPo)/w3iogrmd.o $(aPo)/w3iopomd.o $(aPo)/w3nmlshelmd.o $(aPo)/w3odatmd.o $(aPo)/w3servmd.o $(aPo)/w3tidemd.o $(aPo)/w3timemd.o $(aPo)/w3wavemd.o $(aPo)/w3wdasmd.o $(aPo)/w3wdatmd.o
	@$(aPb)/ad3 ww3_esmf
