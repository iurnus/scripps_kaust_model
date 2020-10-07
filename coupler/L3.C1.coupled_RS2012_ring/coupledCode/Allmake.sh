#!/bin/sh

make distclean

ln -s ../build/*.mod .
ln -s ../build/mmout/*.a .
ln -s ../build/setrlstk.o .
ln -s ../build/sigreg.o .

make
