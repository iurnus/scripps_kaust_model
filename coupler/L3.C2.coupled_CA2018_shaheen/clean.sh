#!/bin/sh -f

rm -rf build build_mit
rm -rf code code_mit

cd coupledCode
make distclean
cd ..
