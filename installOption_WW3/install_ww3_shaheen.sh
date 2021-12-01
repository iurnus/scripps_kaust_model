echo "installing ww3"
cd $WW3_DIR/model/

WW3_DIR0=${SKRIPS_DIR}/installOption_WW3/ww3_install_shared/
WW3_DIR1=${SKRIPS_DIR}/installOption_WW3/ww3_install_shaheen/

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

