
MPI_HOME="/project_shared/Libraries/openmpi-2.1.1_pgi_fortran_17.5-0/include"

export ESMF_DIR=$(pwd)/
echo "ESMF DIR is: $ESMF_DIR"
export ESMF_OS=Linux
export ESMF_COMM=openmpi
export ESMF_NETCDF=split

export ESMF_NETCDF_INCLUDE="/project_shared/Libraries/netcdf-fortran-4.4.4_pgi_fortran_17.5-0/include -I/project_shared/Libraries/netcdf-4.4.1.1_pgi_fortran_17.5-0/include -I/project_shared/Libraries/netcdf-cxx4-4.3.0_pgi_fortran_17.5-0/include"

export ESMF_NETCDF_LIBPATH="/project_shared/Libraries/netcdf-fortran-4.4.4_pgi_fortran_17.5-0/lib -L/project_shared/Libraries/netcdf-4.4.1.1_pgi_fortran_17.5-0/lib -L/project_shared/Libraries/netcdf-cxx4-4.3.0_pgi_fortran_17.5-0/lib"

export ESMF_NETCDF_LIBPATH_PREFIX="-Wl,-rpath,/project_shared/Libraries/netcdf-fortran-4.4.4_pgi_fortran_17.5-0/lib -Wl,-rpath,/project_shared/Libraries/netcdf-4.4.1.1_pgi_fortran_17.5-0/lib -Wl,-rpath,/project_shared/Libraries/netcdf-cxx4-4.3.0_pgi_fortran_17.5-0/lib"

export ESMFMKFILE=${ESMF_DIR}lib/libg/Linux.pgi.64.openmpi.default/esmf.mk
echo "ESMFMKFILE is: $ESMFMKFILE"


export ESMF_OPENMP=OFF
export ESMF_TESTMPMD=OFF
export ESMF_TESTHARNESS_ARRAY=RUN_ESMF_TestHarnessArray_default
export ESMF_TESTHARNESS_FIELD=RUN_ESMF_TestHarnessField_default
export ESMF_TESTWITHTHREADS=OFF
export ESMF_LAPACK=internal
export ESMF_TESTEXHAUSTIVE=ON
export ESMF_BOPT=g
export ESMF_SITE=default
export ESMF_ABI=64
export ESMF_COMPILER=pgi
