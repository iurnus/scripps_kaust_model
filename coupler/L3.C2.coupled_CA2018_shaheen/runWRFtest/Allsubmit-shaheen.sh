#!/bin/sh
#SBATCH -A k1325
#SBATCH --partition=debug
#SBATCH -J mod_test
#SBATCH -N 2
#SBATCH -t 00:10:00
#SBATCH --mem=MaxMemPerNode
#SBATCH -o job_%j
#SBATCH -e job_%j

module unload PrgEnv-cray
module unload PrgEnv-intel
module unload PrgEnv-pgi
module unload PrgEnv-gnu
module load PrgEnv-intel
# module load wrf/3.9.1.1
module load craype-haswell
module unload cray-hdf5-parallel
module load cray-netcdf
module load cray-parallel-netcdf
module load craype-hugepages4M
module load grib2

srun -n 32 --hint=nomultithread --ntasks-per-node=16 ./wrf.exe
