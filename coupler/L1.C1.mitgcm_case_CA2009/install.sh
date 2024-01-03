#!/bin/sh

# build MITGCM
echo "building MITgcm..."
echo "WARNING:: NEED MITGCM SOURCE FILE in $MITGCM_DIR"

export MITGCM_COMPILER=$ESMF_COMPILER
read -e -p "Using default ESMF compiler $MITGCM_COMPILER? (Y/N): " -i "Y" defaultFlag
# read -e -p "Using PGI compiler? (Y/N) :" -i "Y" pgiFlag
if [ $defaultFlag == 'Y' ]; then
  echo "Using $MITGCM_COMPILER compiler"
  export MITGCM_OPT=mitgcm_optfile.$MITGCM_COMPILER
else 
  exit
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
