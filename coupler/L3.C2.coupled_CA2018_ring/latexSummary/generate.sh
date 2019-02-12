#!/bin/csh -f

./clean.sh

cd genFigures
./plotWRF.py
./plotMITgcm.py
cd ..
cp -rf genFigures figures

make
