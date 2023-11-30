#!/bin/csh -f

rm -rf build code
rm -rf build_mit code_mit

cd coupledCode
make distclean
cd ..

# cd runCase
# ./Allclean
# cd ..

# cd runCase.init
# ./Allclean
# cd ..

# cd latexSummary
# ./clean.sh
# cd ..
