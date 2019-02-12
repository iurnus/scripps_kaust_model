WRF_DIR=/home/x_sunr/coupleCOAW/coupler/L3.C6.coupled_CA_2018_shaheen/../../WRFV3911_AO/
ESMF_DIR=/home/x_sunr/coupleCOAW/coupler/L3.C6.coupled_CA_2018_shaheen/../../esmf/

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
    ${WRF_DIR}/external/fftpack/fftpack5/libfftpack.a \
    ${WRF_DIR}/external/io_grib1/libio_grib1.a \
    ${WRF_DIR}/external/io_grib_share/libio_grib_share.a \
    ${WRF_DIR}/external/io_int/libwrfio_int.a \
    -L${ESMF_DIR}/lib/libg/Unicos.intel.64.mpi.default \
    -L/opt/cray/pe/netcdf/4.4.1.1.6/INTEL/16.0/lib/ \
    -L/opt/cray/pe/parallel-netcdf/1.8.1.3/intel/16.0/lib \
    -L/opt/cray/pe/libsci/17.12.1/INTEL/16.0/x86_64/lib \
    -L/opt/cray/pe/parallel-netcdf/1.8.1.3/INTEL/16.0/lib \
    -L/opt/cray/pe/netcdf/4.4.1.1.6/INTEL/16.0/lib -L/opt/cray/dmapp/default/lib64 \
    -L/opt/cray/pe/mpt/7.7.0/gni/mpich-intel/16.0/lib \
    -L/opt/cray/dmapp/default/lib64 \
    -L/opt/cray/pe/mpt/7.7.0/gni/mpich-intel/16.0/lib \
    -L/opt/cray/pe/hdf5/1.10.1.1/INTEL/16.0/lib \
    -L/opt/cray/rca/2.2.16-6.0.5.0_15.34__g5e09e6d.ari/lib64 \
    -L/opt/cray/alps/6.5.28-6.0.5.0_18.6__g13a91b6.ari/lib64 \
    -L/opt/cray/xpmem/2.2.4-6.0.5.1_8.18__g35d5e73.ari/lib64 \
    -L/opt/cray/pe/pmi/5.0.13/lib64 \
    -L/opt/cray/ugni/6.0.14-6.0.5.0_16.9__g19583bb.ari/lib64 \
    -L/opt/cray/udreg/2.3.2-6.0.5.0_13.12__ga14955a.ari/lib64 \
    -L/sw/xc40cle6/darshan/3.1.4/sles12.3_gcc7.2.0/lib \
    -L/opt/cray/pe/atp/2.1.1/libApp -L/lib64 \
    -L/opt/cray/wlm_detect/1.3.2-6.0.5.0_3.1__g388ccd5.ari/lib64 \
    -L/opt/intel/compilers_and_libraries_2018.1.163/linux/ipp/lib/intel64 \
    -L/opt/intel/compilers_and_libraries_2018.1.163/linux/compiler/lib/intel64_lin \
    -L/opt/intel/compilers_and_libraries_2018.1.163/linux/mkl/lib/intel64_lin \
    -L/opt/intel/compilers_and_libraries_2018.1.163/linux/tbb/lib/intel64/gcc4.7 \
    -L/opt/intel/compilers_and_libraries_2018.1.163/linux/tbb/lib/intel64/gcc4.7 \
    -L/opt/intel/compilers_and_libraries_2018.1.163/linux/daal/lib/intel64_lin \
    -L/opt/intel/compilers_and_libraries_2018.1.163/linux/compiler/lib/intel64_lin \
    -L/usr/lib64/gcc/x86_64-suse-linux/4.8/ \
    -L/usr/lib64/gcc/x86_64-suse-linux/4.8/../../../../lib64 \
    -L/usr/lib64/gcc/x86_64-suse-linux/4.8/../../../../lib64/ -L/lib/../lib64 \
    -L/lib/../lib64/ -L/usr/lib/../lib64 -L/usr/lib/../lib64/ \
    -L/opt/intel/compilers_and_libraries_2018.1.163/linux/ipp/lib/intel64/ \
    -L/opt/intel/compilers_and_libraries_2018.1.163/linux/compiler/lib/intel64_lin/ \
    -L/opt/intel/compilers_and_libraries_2018.1.163/linux/mkl/lib/intel64_lin/ \
    -L/opt/intel/compilers_and_libraries_2018.1.163/linux/tbb/lib/intel64/gcc4.7/ \
    -L/opt/intel/compilers_and_libraries_2018.1.163/linux/daal/lib/intel64_lin/ \
    -L/usr/lib64/gcc/x86_64-suse-linux/4.8/../../../../x86_64-suse-linux/lib/ \
    -L/usr/lib64/gcc/x86_64-suse-linux/4.8/../../../ -L/lib64 -L/lib/ -L/usr/lib64 \
    -L/usr/lib \
    -L/opt/intel/compilers_and_libraries_2018.1.163/linux/compiler/lib/intel64_lin \
    -L/opt/intel/compilers_and_libraries_2018.1.163/linux/compiler/lib/intel64_lin \
    -L/opt/intel/compilers_and_libraries_2018.1.163/linux/compiler/lib/intel64_lin \
    -L/opt/intel/compilers_and_libraries_2018.1.163/linux/compiler/lib/intel64_lin \
    -L/opt/intel/compilers_and_libraries_2018.1.163/linux/compiler/lib/intel64_lin \
    -L/opt/intel/compilers_and_libraries_2018.1.163/linux/compiler/lib/intel64_lin \
    -L/opt/intel/compilers_and_libraries_2018.1.163/linux/compiler/lib/intel64_lin \
    -lesmf   -lhugetlbfs -lpnetcdf -lAtpSigHandler -lAtpSigHCommData -lpthread \
    -ldarshan -ldarshan-stubs -lz -lnetcdf_c++4 -limf -lm -ldl -lnetcdf -lhdf5_hl \
    -ldl -limf -lm -lz -lhdf5 -lrt -lz -ldl -limf -lm -lsci_intel -limf -lm -ldl \
    -lsci_intel -limf -lm -lpthread -ldl -lhugetlbfs -lrt -lugni -lpthread -lpmi \
    -limf -lm -ldl -lrt -lugni -lpthread -lpmi -limf -lm -ldl -lpmi -lpthread \
    -lalpslli -lpthread -lwlm_detect -lalpsutil -lpthread -lrca -lugni -lpthread \
    -lxpmem -ludreg -lstdc++ -limf -lm -lifport -lpthread -limf -lsvml -lirng \
    -lstdc++ -lm -lipgo -ldecimal -lstdc++ -lgcc -lgcc_eh -lirc -lsvml -lgcc \
    -lgcc_eh -lirc_s -ldl -lrt -ldl -lnetcdff -lnetcdf \
    -L${WRF_DIR}/external/io_esmf \
    -L${WRF_DIR}/external/io_netcdf \
    -L${WRF_DIR}/external/io_pnetcdf \
    ${WRF_DIR}/external/RSL_LITE/librsl_lite.a \
    ${WRF_DIR}/frame/module_internal_header_util.o \
    ${WRF_DIR}/frame/pack_utils.o \
    -L${ESMF_DIR}/lib/libg/Unicos.intel.64.mpi.default \
    -lwrfio_nf -L/opt/cray/pe/netcdf/4.4.1.1.6/INTEL/16.0/lib -lnetcdff -lnetcdf \
    -L/opt/cray/pe/mpt/7.7.0/gni/mpich-intel/16.0/lib -lwrfio_esmf \
    -lwrfio_pnf -L/opt/cray/pe/parallel-netcdf/1.8.1.3/INTEL/16.0/lib -lpnetcdf     
