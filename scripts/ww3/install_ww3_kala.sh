echo "installing ww3"
cd ${WW3_DIR}

./model/bin/w3_setup model

# FTP is still working, but no longer support WW3 6.07 version
# ./model/bin/ww3_from_ftp.sh

# ./model/bin/w3_setup ./model -c Portland -s Ifremer1
./model/bin/w3_setup ./model -c Gnu

WW3_DIR0=${SKRIPS_DIR}/scripts/ww3/ww3_install_shared/
WW3_DIR1=${SKRIPS_DIR}/scripts/ww3/ww3_install_kala/

ln -sf ${WW3_DIR1}/comp.Gnu ./model/bin/comp
ln -sf ${WW3_DIR1}/link.Gnu ./model/bin/link

ln -sf ${WW3_DIR0}/switch ./model/bin/switch
ln -sf ${WW3_DIR0}/ww3_esmf.ftn ./model/ftn/
ln -sf ${WW3_DIR0}/w3adatmd.ftn ./model/ftn/
ln -sf ${WW3_DIR0}/w3idatmd.ftn ./model/ftn/
ln -sf ${WW3_DIR0}/w3initmd.ftn ./model/ftn/
ln -sf ${WW3_DIR0}/w3iogomd.ftn ./model/ftn/
ln -sf ${WW3_DIR0}/w3odatmd.ftn ./model/ftn/
ln -sf ${WW3_DIR0}/w3wavemd.ftn ./model/ftn/

cd $WW3_DIR0
./install.sh

