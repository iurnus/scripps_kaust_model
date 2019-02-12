# Readme
# IMPORTANT!!
# ADD the bash_setup to the ~/.bashrc file if necessary
read -e -p "GRIB files? :" -i "/project/rus043_data/ar_2018_forecast/save_nc/fine_loop/2018_0127/gfs.0p25.2018012700.f00*.grib2" gribLocation
echo "grid location: $gribLocation"

echo "compiling WPS"
cp ../installOption_OTH/wps_gfs_ca/{Vtable,namelist*} .

./link_grib.csh $gribLocation
./ungrib.exe
./geogrid.exe
./metgrid.exe

echo the I.C. and B.C. are generated using WPS!
