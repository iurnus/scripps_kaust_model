## # BASH OPTIONS FOR THE COUPLER
#
# Update the following directories from line 4 to 13
export SKRIPS_DIR=/home/rus043/scripps_kaust_model-1.1/
export ESMF_NETCDF_INCLUDE="/project_shared/Libraries/netcdf-fortran-4.4.4_pgi_fortran_17.5-0/include -I/project_shared/Libraries/netcdf-4.4.1.1_pgi_fortran_17.5-0/include -I/project_shared/Libraries/netcdf-cxx4-4.3.0_pgi_fortran_17.5-0/include"
export ESMF_NETCDF_LIBPATH="/project_shared/Libraries/netcdf-fortran-4.4.4_pgi_fortran_17.5-0/lib -L/project_shared/Libraries/netcdf-4.4.1.1_pgi_fortran_17.5-0/lib -L/project_shared/Libraries/netcdf-cxx4-4.3.0_pgi_fortran_17.5-0/lib"
export ESMF_NETCDF_LIBPATH_PREFIX="-Wl,-rpath,/project_shared/Libraries/netcdf-fortran-4.4.4_pgi_fortran_17.5-0/lib -Wl,-rpath,/project_shared/Libraries/netcdf-4.4.1.1_pgi_fortran_17.5-0/lib -Wl,-rpath,/project_shared/Libraries/netcdf-cxx4-4.3.0_pgi_fortran_17.5-0/lib"
export SKRIPS_NETCDF_INCLUDE=-I$ESMF_NETCDF_INCLUDE
export SKRIPS_NETCDF_LIB=-L$ESMF_NETCDF_LIBPATH
export SKRIPS_MPI_DIR=/project_shared/Libraries/openmpi-2.1.1_pgi_fortran_17.5-0/
export ESMF_DIR=$SKRIPS_DIR/esmf/
export ESMF_LIB=$ESMF_DIR/lib/libg/Linux.pgi.64.openmpi.default/

export ESMF_OS=Linux
export ESMF_COMM=openmpi
export ESMF_NETCDF=split
export ESMF_OPENMP=OFF
export ESMF_LAPACK=internal
export ESMF_BOPT=g
export ESMF_ABI=64
export ESMF_COMPILER=pgi
export ESMF_SITE=default

export ESMF_TESTEXHAUSTIVE=ON
export ESMF_TESTMPMD=OFF
export ESMF_TESTHARNESS_ARRAY=RUN_ESMF_TestHarnessArray_default
export ESMF_TESTHARNESS_FIELD=RUN_ESMF_TestHarnessField_default
export ESMF_TESTWITHTHREADS=OFF

export ESMFMKFILE=$ESMF_LIB/esmf.mk
export LD_LIBRARY_PATH=$ESMF_LIB/${LD_LIBRARY_PATH:+:$LD_LIBRARY_PATH}
