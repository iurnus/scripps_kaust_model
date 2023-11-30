#!/bin/csh -f

./clean.sh

cd genFigures
./plotWRF.py
./plotMITgcm.py
./plotWW3.py
cd ..
mkdir figures
cp genFigures/*.png figures/

make
