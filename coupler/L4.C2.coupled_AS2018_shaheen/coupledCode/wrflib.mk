WRF_SRC_ROOT_DIR=${WRF_DIR}
WRF_INC = \
   -I${WRF_DIR}dyn_em \
   -I${WRF_DIR}dyn_nmm \
   -I${WRF_DIR}main \
   -I${WRF_DIR}external/io_esmf \
   -I${WRF_DIR}io_netcdf \
   -I${WRF_DIR}io_int \
   -I${WRF_DIR}frame \
   -I${WRF_DIR}share \
   -I${WRF_DIR}phys \
   -I${WRF_DIR}chem \
   -I${WRF_DIR}inc \
   -I${WRF_DIR}wrftladj \

include ${WRF_DIR}/configure.wrf_cpl
WRF_LIB = \
  ${WRF_DIR}/main/wrf_ESMFMod.o \
  ${WRF_DIR}/main/module_wrf_top.o \
  ${WRF_DIR}/main/libwrflib.a \
  $(LDFLAGS) \
  $(LIB)

