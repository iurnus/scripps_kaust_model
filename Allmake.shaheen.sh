# Readme
# IMPORTANT!!
# ADD the bash_setup to the ~/.bashrc file if necessary
echo -n "Have you updated the ~/.bashrc file (y/n)? "
read answer
if [ "$answer" != "${answer#[Yy]}" ] ;then
  echo "bashrc file updated!"
else
  echo "Please update the ~/.bashrc file!"
  echo "Please update netcdf path in ~/.bashrc file for ESMF!"
  echo "My ~/.bashrc file is available in installOption_OTH/bashrc_setup_shaheen"
  echo "On shaheen@kaust.edu.sa, we used Intel compiler."
  exit
fi

# IMPORTANT!!
# check if the tar files are downloaded
echo -n "Have you downloaded the tar files (y/n)? "
read answer
if [ "$answer" != "${answer#[Yy]}" ] ;then
  echo "tar files downloaded!"
  read -e -p "The location of the tar files? :" -i "$PWD/tarfiles/" tarLocation
  echo "tar files are located at: $tarLocation"
else
  echo "please download the tar files via: git clone git@bitbucket.org:iurnus/regionalcoam_tarfiles.git"
  echo "required tar files: MITgcm (version c67m), WRF (version 4.1.2), ESMF (version 8.0.0)"
  echo "required tar file names: MITgcm_c67m.tar.gz, WRFV4.1.2.TAR.gz, esmf_8_0_0_src.tar"
  exit
fi

read -e -p "ESMF location? :" -i "$PWD/esmf/" esmfLocation
echo "esmf location: $esmfLocation"
read -e -p "WRF location? :" -i "$PWD/WRFV412_AO/" wrfLocation
echo "wrf location: $wrfLocation"

#  =1: compile
# !=1: do not compile
ifMITgcm=1
ifESMF=1
ifWRF=1
ifWPS=0

if [ $ifMITgcm == "1" ]; then
  # How to install MITgcm
  unzip $tarLocation/MITgcm-checkpoint67m.zip
  mv MITgcm-checkpoint67m MITgcm_c67m
fi

if [ $ifESMF == "1" ]; then
  # How to make ESMF
  tar -xvf $tarLocation/esmf_8_0_0_src.tar -C .
  # IMPORTANT!!!
  # Make sure the NETCDF options are correct in configure.esmf
  cp installOption_OTH/esmfInstallOptions/configure.esmf.shaheen esmf/configure.esmf
  cd esmf
  . configure.esmf
  echo "installing esmf, it may take about 30 minutes"
  gmake info &> log.info
  gmake &> log.gmake
  cd ..
fi

if [ $ifWRF == "1" ]; then
  # How to make WRF3
  unzip $tarLocation/WRF-master.zip
  echo "compiling stand-alone WRF"
  mv WRF-master WRFV412.org
  cp -rf WRFV412.org WRFV412
  cd WRFV412
  echo "choosing 50th option to compile WRF"
  printf '50\n1\n' | ./configure &> log.configure
  # ./compile em_real &> log.em_real0
  cd ../
  
  echo "compiling WRF for ocean-atmosphere coupling"
  WRF_CPL_DIR=WRFV412_AO_01
  rm -rf $WRF_CPL_DIR
  cp -r WRFV412.org $WRF_CPL_DIR
  ln -sf installOption_WRF/installWRF412_ao_shaheen.sh .
  # IMPORTANT!!!
  # Make sure the ESMF_DIR in configure.wrf is correct
  # Make sure the ESMF_DIR, MAIN_DIR, CURRENT_DIR in makefile.io_esmf are correct
  sed -i "17s@.*@ESMF_DIR=$esmfLocation@" installOption_WRF/wrfAO412_shaheen/configure.wrf
  sed -i "4s@.*@ESMF_DIR=$esmfLocation@" installOption_WRF/wrfAO412_shaheen/makefile.io_esmf
  sed -i "6s@.*@CURRENT_DIR=$PWD/$WRF_CPL_DIR/external/io_esmf/@" installOption_WRF/wrfAO412_shaheen/makefile.io_esmf
  sed -i "3s@.*@cd $WRF_CPL_DIR@" ./installWRF412_ao_shaheen.sh
  ./installWRF412_ao_shaheen.sh
fi

# TODO: update the WPS installer
# if [ $ifWPS == "1" ]; then
#   echo "compiling WPS for Red Sea case"
#   tar -xvf $tarLocation/WPSV3.9.1.TAR.gz -C .
#   cp -rf WPS WPS_RedSea
#   cd WPS_RedSea
#   echo "choosing 7 option to compile WPS"
#   printf '5\n' | ./configure &> log.configure
#   sed -i "35s@.*@WRF_DIR         =   $wrfLocation@" ./configure.wps
#   cp ../installOption_OTH/patch.WPS_configure .
#   cp ../installOption_OTH/wps_ecmwf_rs/*.sh .
#   patch < patch.WPS_configure
#   ./compile &> log.compile
#   cd ..
#   
#   echo "compiling WPS for California Case"
#   tar -xvf $tarLocation/WPSV3.9.1.TAR.gz -C .
#   cp -rf WPS WPS_California
#   cd WPS_California
#   echo "choosing 7 option to compile WPS"
#   printf '5\n' | ./configure &> log.configure
#   sed -i "35s@.*@WRF_DIR         =   $wrfLocation@" ./configure.wps
#   cp ../installOption_OTH/patch.WPS_configure .
#   cp ../installOption_OTH/wps_gfs_ca/*.sh .
#   patch < patch.WPS_configure
#   ./compile &> log.compile
#   cd ..
# fi

echo Finished!
echo "To test the installation, try running 'allrun.shaheen.sh in /coupler'"
