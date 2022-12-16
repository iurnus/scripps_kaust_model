WRF_SRC_ROOT_DIR=${PWRF_DIR}
WRF_INC = \
   -I${PWRF_DIR}dyn_em \
   -I${PWRF_DIR}dyn_nmm \
   -I${PWRF_DIR}main \
   -I${PWRF_DIR}external/io_esmf \
   -I${PWRF_DIR}io_netcdf \
   -I${PWRF_DIR}io_int \
   -I${PWRF_DIR}frame \
   -I${PWRF_DIR}share \
   -I${PWRF_DIR}phys \
   -I${PWRF_DIR}chem \
   -I${PWRF_DIR}inc \
   -I${PWRF_DIR}wrftladj \

include ${PWRF_DIR}/configure.wrf_cpl
WRF_LIB = \
  ${PWRF_DIR}/main/wrf_ESMFMod.o \
  ${PWRF_DIR}/main/module_wrf_top.o \
  ${PWRF_DIR}/main/libwrflib.a \
  $(LDFLAGS) \
  $(LIB)

