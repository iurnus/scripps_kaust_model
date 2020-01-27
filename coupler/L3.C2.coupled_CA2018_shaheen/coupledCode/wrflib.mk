WRF_DIR=${SKRIPS_DIR}/WRFV413_AO/
ESMF_DIR=${SKRIPS_DIR}/esmf/

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

WRF_LIB = \
 wrf_ESMFMod.o ${WRF_DIR}/main/module_wrf_top.o libwrflib.a /home/x_sunr/scripps_kaust_model-1.1/WRFV413_AO/external/fftpack/fftpack5/libfftpack.a /home/x_sunr/scripps_kaust_model-1.1/WRFV413_AO/external/io_grib1/libio_grib1.a /home/x_sunr/scripps_kaust_model-1.1/WRFV413_AO/external/io_grib_share/libio_grib_share.a /home/x_sunr/scripps_kaust_model-1.1/WRFV413_AO/external/io_int/libwrfio_int.a -L/home/x_sunr/scripps_kaust_model-1.1//esmf//lib/libg/Unicos.intel.64.mpi.default -L/opt/cray/pe/netcdf/4.6.3.2/INTEL/19.0/lib/, -L/opt/cray/pe/parallel-netcdf/1.11.1.1/intel/19.0/lib  -lesmf   -cxxlib -lrt -ldl -lnetcdff -lnetcdf -L/home/x_sunr/scripps_kaust_model-1.1/WRFV413_AO/external/io_esmf -L/opt/intel/compilers_and_libraries_2019.5.281/linux/mpi/intel64/lib/ -lwrfio_esmf  /home/x_sunr/scripps_kaust_model-1.1/WRFV413_AO/external/RSL_LITE/librsl_lite.a /home/x_sunr/scripps_kaust_model-1.1/WRFV413_AO/frame/module_internal_header_util.o /home/x_sunr/scripps_kaust_model-1.1/WRFV413_AO/frame/pack_utils.o  /home/x_sunr/scripps_kaust_model-1.1//esmf//lib/libg/Unicos.intel.64.mpi.default/libesmf.a -L/home/x_sunr/scripps_kaust_model-1.1/WRFV413_AO/external/io_netcdf -lwrfio_nf -L/opt/cray/pe/netcdf/4.6.3.2/INTEL/19.0/lib -lnetcdff -lnetcdf  -L/home/x_sunr/scripps_kaust_model-1.1/WRFV413_AO/external/io_pnetcdf -lwrfio_pnf -L/opt/cray/pe/parallel-netcdf/1.11.1.1/INTEL/19.0/lib -lpnetcdf          
