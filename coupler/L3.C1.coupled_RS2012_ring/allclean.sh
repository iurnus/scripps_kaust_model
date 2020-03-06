#!/bin/csh -f

./clean.sh

cd runCase/
./Allclean
cd ..

cd runCase.restart/
./Allclean
cd ..

cd runCase.init/
./Allclean
cd ..

cd runMITtest/
./Allclean
cd ..

cd runWRFtest/
./Allclean
cd ..

cd latexSummary
./clean.sh
cd ..
