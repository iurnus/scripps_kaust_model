WW3_MOD=${WW3_DIR}/model/mod
WW3_OBJ=${WW3_DIR}/model/obj

WW3_INC = \
   -I${WW3_MOD}

WW3_LIB = \
    $(WW3_OBJ)/constants.o \
    $(WW3_OBJ)/w3servmd.o \
    $(WW3_OBJ)/w3timemd.o \
    $(WW3_OBJ)/w3tidemd.o \
    $(WW3_OBJ)/w3arrymd.o \
    $(WW3_OBJ)/w3dispmd.o \
    $(WW3_OBJ)/w3cspcmd.o \
    $(WW3_OBJ)/w3gsrumd.o \
    $(WW3_OBJ)/w3nmlshelmd.o \
    $(WW3_OBJ)/w3fldsmd.o \
    $(WW3_OBJ)/w3initmd.o \
    $(WW3_OBJ)/w3wavemd.o \
    $(WW3_OBJ)/w3wdasmd.o \
    $(WW3_OBJ)/w3updtmd.o \
    $(WW3_OBJ)/wmmdatmd.o \
    $(WW3_OBJ)/w3gdatmd.o \
    $(WW3_OBJ)/w3wdatmd.o \
    $(WW3_OBJ)/w3adatmd.o \
    $(WW3_OBJ)/w3idatmd.o \
    $(WW3_OBJ)/w3odatmd.o \
    $(WW3_OBJ)/w3profsmd.o \
    $(WW3_OBJ)/w3pro3md.o \
    $(WW3_OBJ)/w3uqckmd.o \
    $(WW3_OBJ)/w3triamd.o \
    $(WW3_OBJ)/w3srcemd.o \
    $(WW3_OBJ)/w3flx1md.o \
    $(WW3_OBJ)/w3sln1md.o \
    $(WW3_OBJ)/w3src4md.o \
    $(WW3_OBJ)/w3snl1md.o \
    $(WW3_OBJ)/w3sbt4md.o \
    $(WW3_OBJ)/w3sic2md.o \
    $(WW3_OBJ)/w3sis2md.o \
    $(WW3_OBJ)/w3sdb1md.o \
    $(WW3_OBJ)/w3ref1md.o \
    $(WW3_OBJ)/w3parall.o \
    $(WW3_OBJ)/w3iogrmd.o \
    $(WW3_OBJ)/w3iogomd.o \
    $(WW3_OBJ)/w3iopomd.o \
    $(WW3_OBJ)/w3iotrmd.o \
    $(WW3_OBJ)/w3iorsmd.o \
    $(WW3_OBJ)/w3iobcmd.o \
    $(WW3_OBJ)/w3iosfmd.o \
    $(WW3_OBJ)/w3partmd.o \
    $(WW3_OBJ)/ww3_esmf.o
