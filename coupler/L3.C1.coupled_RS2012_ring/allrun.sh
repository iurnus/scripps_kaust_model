#!/bin/csh -f

# ./install.sh
# ./install_mit.sh

cd runCase.init/
./Allrun
cd ..

cd runCase/
./Allrun
cd ..

cd runMITtest/
./Allrun
cd ..

cd runWRFtest/
./Allrun
cd ..

cd latexSummary
./generate.sh
cd ..

evince latexSummary/report.pdf
