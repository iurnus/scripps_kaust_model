#!/bin/csh -f

./clean.sh

cd genFigures
./plotWRF.py
./plotMITgcm.py
./plotCompT2.py
./plotCompSST.py
cd ..
cp -rf genFigures figures/

make
