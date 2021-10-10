echo "installing ww3"
cd ${WW3_DIR}

WW3_DIR0=${SKRIPS_WAVE_DIR}/installOption_ww3/ww3_install_shared/

cp ${WW3_DIR}/bin/comp.Intel ./bin/comp
cp ${WW3_DIR}/bin/link.Intel ./bin/link

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

