#!/bin/sh
#SBATCH -p debug
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

mpirun -np 32 ./wrf.exe
