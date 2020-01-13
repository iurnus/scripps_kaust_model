#!/bin/sh

# build the MITGCM as a library
echo "building MITgcm..."
echo "WARNING:: NEED MITGCM SOURCE FILE in ../../MITgcm_c67m/"

export MITGCM_OPT=mitgcm_optfile.pgi
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
