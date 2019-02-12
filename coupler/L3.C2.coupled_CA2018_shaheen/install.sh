
export MPI_HOME=/opt/cray/pe/mpt/7.7.0/gni/mpich-intel/16.0/include/

read -e -p "WRF3911 (with OA coupling) location? :" -i "$PWD/../../WRFV3911_AO/" wrfLocation
read -e -p "ESMF location? :" -i "$PWD/../../esmf/" esmfLocation
sed -i "1s@.*@WRF_DIR=$wrfLocation@" coupledCode/wrflib.mk
sed -i "2s@.*@ESMF_DIR=$esmfLocation@" coupledCode/wrflib.mk

# build the MITGCM as an executable
mkdir build_mit code_mit
cp utils/* build_mit/ # copy the scripts to install MITGCM
cp mitCode/* code_mit/ # copy the scripts to install MITGCM
cp mitSettingCA/* code_mit/ # copy the scripts to install MITGCM
rm code_mit/exf_get* # remove the exf_get file so that MITGCM read the file input
cd build_mit
sed -i s/code/code_mit/g makescript_fwd.sio.shaheen
./makescript_fwd.sio.shaheen # install MITGCM, generate *.f files
cd ..

# build the MITGCM as a library
cp -rf ../L3.C1.coupled_RS2012_ring/mitCode . 

mkdir build
mkdir code
cp utils/* build/ # copy the scripts to install MITGCM
cp mitCode/* code/ # copy the scripts to install MITGCM
cp mitSettingCA/* code/ # copy the scripts to install MITGCM
cd build
./makescript_fwd.sio.shaheen # install MITGCM, generate *.f files

cp $MPI_HOME/mpif* . 
./mkmod.sh ocn # install MITGCM as a library, generate *.mod files
cd ..

# build the test coupler
cd coupledCode
./Allmake.sh
cd ..
