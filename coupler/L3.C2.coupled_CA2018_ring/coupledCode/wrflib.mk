WRF_DIR=/home/rus043/scripps_kaust_model_github/coupler/L3.C2.coupled_CA2018_ring/../../WRFV412_AO_01/
ESMF_DIR=/home/rus043/scripps_kaust_model_github/coupler/L3.C2.coupled_CA2018_ring/../../esmf/

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

WRF_LIB = \
	wrf_ESMFMod.o module_wrf_top.o libwrflib.a \
    ${WRF_DIR}external/fftpack/fftpack5/libfftpack.a \
	${WRF_DIR}external/io_grib1/libio_grib1.a \
	${WRF_DIR}external/io_grib_share/libio_grib_share.a \
	${WRF_DIR}external/io_int/libwrfio_int.a \
   -L${ESMF_DIR}/lib/libg/Linux.pgi.64.openmpi.default \
   -L/project_shared/Libraries/netcdf-fortran-4.4.4_pgi_fortran_17.5-0/lib \
   -L/project_shared/Libraries/netcdf-4.4.1.1_pgi_fortran_17.5-0/lib \
   -L/project_shared/Libraries/netcdf-cxx4-4.3.0_pgi_fortran_17.5-0/lib \
   -L/opt/pgi/linux86-64/17.5/libso \
   -lesmf  -lmpi -pgc++libs -ldl -lnetcdff -lnetcdf \
   -L${WRF_DIR}external/io_esmf \
   -lwrfio_esmf \
   ${WRF_DIR}external/RSL_LITE/librsl_lite.a \
   ${WRF_DIR}frame/module_internal_header_util.o \
   ${WRF_DIR}frame/pack_utils.o  \
   -L${WRF_DIR}external/io_netcdf \
   -lwrfio_nf
