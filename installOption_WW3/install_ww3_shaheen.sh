echo "installing ww3"
WW3_PWD_DIR=${PWD}
cd ww3-516

WW3_DIR0=${WW3_PWD_DIR}/installOption_ww3/ww3_install_shared/
WW3_DIR1=${WW3_PWD_DIR}/installOption_ww3/ww3_install_shaheen/

ln -sf ${WW3_DIR1}/comp ./bin/comp
ln -sf ${WW3_DIR1}/link ./bin/link
ln -sf ${WW3_DIR1}/w3profsmd.ftn ./ftn/
ln -sf ${WW3_DIR0}/switch ./bin/switch
ln -sf ${WW3_DIR0}/ww3_esmf.ftn ./ftn/
ln -sf ${WW3_DIR0}/w3adatmd.ftn ./ftn/
ln -sf ${WW3_DIR0}/w3idatmd.ftn ./ftn/
ln -sf ${WW3_DIR0}/w3initmd.ftn ./ftn/
ln -sf ${WW3_DIR0}/w3iogomd.ftn ./ftn/
ln -sf ${WW3_DIR0}/w3odatmd.ftn ./ftn/
ln -sf ${WW3_DIR0}/w3wavemd.ftn ./ftn/

cd $WW3_DIR0
./install.sh

