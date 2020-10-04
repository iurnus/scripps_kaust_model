#!/bin/sh

# build MITGCM
echo "building MITgcm..."
echo "WARNING:: NEED MITGCM SOURCE FILE in $MITGCM_DIR"

read -e -p "Using PGI compiler? (Y/N) :" -i "Y" pgiFlag
if [ $pgiFlag == 'Y' ]; then
  echo "Using PGI compiler"
  export MITGCM_OPT=mitgcm_optfile.pgi
else 
  echo "Using default intel compiler"
  export MITGCM_OPT=mitgcm_optfile.ifort
fi

echo "The option file is: $MITGCM_OPT"

mkdir build/
cp utils/* build/ # copy the scripts to install MITGCM
cp -rf mitCode code # copy the code to install MITGCM
cd build
./makescript_fwd.sio.ring # install MITGCM, generate *.f files
cd ..

# run mitgcm
echo "running MITgcm..."
cd mitRun
./Allrun
cd ..
