# Readme
# IMPORTANT!!
# ADD the bash_setup to the ~/.bashrc file if necessary
read -e -p "GRIB files? :" -i "/home/rus043/ECMWF/download_rs/ecmf_2012060[1-2]_an_*" gribLocation
echo "grid location: $gribLocation"

echo "compiling WPS"
cp ../installOption_OTH/wps_ecmwf_rs/{ecmwf*,Vtable,*.TBL,namelist*} .
rm ./metgrid/METGRID.TBL
cp METGRID.TBL ./metgrid/
rm ./geogrid/GEOGRID.TBL
cp GEOGRID.TBL ./geogrid/

./link_grib.csh $gribLocation
./ungrib.exe
./util/calc_ecmwf_p.exe
./geogrid.exe
./metgrid.exe

echo the I.C. and B.C. are generated using WPS!
