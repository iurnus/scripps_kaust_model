#!/bin/bash -l

DATE1=20120525
DATE2=20120605
YY1=`echo $DATE1 | cut -c1-4`
MM1=`echo $DATE1 | cut -c5-6`
DD1=`echo $DATE1 | cut -c7-8`
YY2=`echo $DATE2 | cut -c1-4`
MM2=`echo $DATE2 | cut -c5-6`
DD2=`echo $DATE2 | cut -c7-8`
Nort=32
Sout=8
West=28
East=52

sed -e "s/DATE1/${DATE1}/g;s/DATE2/${DATE2}/g;s/Nort/${Nort}/g;s/West/${West}/g;s/Sout/${Sout}/g;s/East/${East}/g;" getERA5-sl.py > getERA5-${DATE1}-${DATE2}-sl.py
~/anaconda2/bin/python getERA5-${DATE1}-${DATE2}-sl.py

~/anaconda2/bin/python processERA5.py

exit 0
