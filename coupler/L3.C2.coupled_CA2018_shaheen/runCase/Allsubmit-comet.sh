#!/bin/sh
#SBATCH -p compute
#SBATCH --nodes=2
#SBATCH --ntasks-per-node=16
#SBATCH --account=csg102
#SBATCH -t 00:20:00
#SBATCH -J ar_test
#SBATCH -o job_%j
#SBATCH -e job_%j
#SBATCH --export=ALL

module purge
module load intel
module load intelmpi
module load netcdf
export NETCDF="/opt/netcdf/4.6.1/intel/intelmpi/"

~/anaconda3/bin/python updateLowinp.py
cp namelist.input.set namelist.input
mpirun -np 32 ./wrf.exe

~/anaconda3/bin/python updateHFlux.py
cp namelist.input.run namelist.input
echo "running coupled MITgcm--WRF simulation.."
mpirun -np 32 ./esmf_application
