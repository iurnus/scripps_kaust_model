#! /bin/csh -f
cd ./$1

ln -sf ../../installOption_seaice/code_pwrf/$1/$2.F.PWRF4.1.3 $2.F
ls  $2.F*

