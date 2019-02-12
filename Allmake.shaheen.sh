# Readme
# IMPORTANT!!
# ADD the bash_setup to the ~/.bashrc file if necessary
echo -n "Have you updated the ~/.bashrc file (y/n)? "
read answer
if [ "$answer" != "${answer#[Yy]}" ] ;then
  echo "bashrc file updated!"
else
  echo "Please update the bashrc file!"
  echo "There are a few bashrc options in installOption_OTH/bashrc_setup_*"
  exit
fi

# IMPORTANT!!
# check if the tar files are downloaded
echo -n "Have you downloaded the tar files (y/n)? "
read answer
if [ "$answer" != "${answer#[Yy]}" ] ;then
  echo "tar files downloaded!"
  read -e -p "The location of the tar files? (default: $HOME/tarfiles/):" -i "$HOME/tarfiles/" tarLocation
  echo "tar files are located at: $tarLocation"
else
  echo "please download the tar files via: git clone git@bitbucket.org:iurnus/regionalcoam_tarfiles.git"
  echo "required tar files: MITgcm (version c66h), WRF (version 3.9.1.1), ESMF (version 7.0.0)"
  echo "required tar file names: MITgcm_c66h.tar.gz, WRFV3.9.1.1.TAR.gz, esmf_7_0_0_src.tar"
  exit
fi

read -e -p "ESMF location? :" -i "$PWD/esmf/" esmfLocation
echo "esmf location: $esmfLocation"
read -e -p "WRF location? :" -i "$PWD/WRFV3911_AO/" wrfLocation
echo "wrf location: $wrfLocation"

# How to install MITgcm
tar -xvf $tarLocation/MITgcm_c66h.tar.gz -C .

# How to make ESMF
tar -xvf $tarLocation/esmf_7_0_0_src.tar -C .
# IMPORTANT!!!
# Make sure the ESMF options are correct in configure.esmf
# Make sure the NETCDF options are correct in configure.esmf
cp installOption_OTH/esmfInstallOptions/configure.esmf.shaheen esmf/configure.esmf
cd esmf
. configure.esmf
echo "installing esmf, it may take about 30 minutes"
gmake info &> log.info
gmake &> log.gmake
cd ..

# How to make WRF3
tar -xvf $tarLocation/WRFV3.9.1.1.TAR.gz -C .
echo "compiling stand-alone WRF"
cp -rf WRFV3 WRFV3911
cd WRFV3911
echo "choosing 50th option to compile WRF"
printf '50\n1\n' | ./configure &> log.configure
./compile em_real &> log.em_real0
cd ../

echo "compiling WRF for ocean-atmosphere coupling"
rm -rf WRFV3911_AO
cp -r WRFV3 WRFV3911_AO
ln -sf installOption_WRF/installWRF3911_ao_shaheen.sh .
# IMPORTANT!!!
# Make sure the ESMF_DIR in configure.wrf is correct
# Make sure the ESMF_DIR, MAIN_DIR, CURRENT_DIR in makefile.io_esmf are correct
sed -i "17s@.*@ESMF_DIR=$esmfLocation@" installOption_WRF/wrfAO3911Implementations_shaheen/configure.wrf
sed -i "4s@.*@ESMF_DIR=$esmfLocation@" installOption_WRF/wrfAO3911Implementations_shaheen/makefile.io_esmf
sed -i "7s@.*@MAIN_DIR=$PWD/@" installOption_WRF/wrfAO3911Implementations_shaheen/makefile.io_esmf
sed -i "8s@.*@CURRENT_DIR=$PWD/WRFV3911_AO/external/io_esmf/@" installOption_WRF/wrfAO3911Implementations_shaheen/makefile.io_esmf
./installWRF3911_ao_shaheen.sh

echo "compiling WPS for Red Sea case"
tar -xvf $tarLocation/WPSV3.9.1.TAR.gz -C .
cp -rf WPS WPS_RedSea
cd WPS_RedSea
echo "choosing 7 option to compile WPS"
printf '5\n' | ./configure &> log.configure
sed -i "35s@.*@WRF_DIR         =   $wrfLocation@" ./configure.wps
cp ../installOption_OTH/patch.WPS_configure .
cp ../installOption_OTH/wps_ecmwf_rs/*.sh .
patch < patch.WPS_configure
./compile &> log.compile
cd ..

echo "compiling WPS for California Case"
tar -xvf $tarLocation/WPSV3.9.1.TAR.gz -C .
cp -rf WPS WPS_California
cd WPS_California
echo "choosing 7 option to compile WPS"
printf '5\n' | ./configure &> log.configure
sed -i "35s@.*@WRF_DIR         =   $wrfLocation@" ./configure.wps
cp ../installOption_OTH/patch.WPS_configure .
cp ../installOption_OTH/wps_gfs_ca/*.sh .
patch < patch.WPS_configure
./compile &> log.compile
cd ..

echo Finished!
echo "To test the installation, try running 'allrun.ring.sh in /coupler'"
