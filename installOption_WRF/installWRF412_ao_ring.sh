echo "installing WRF"
WRF_PWD_DIR=${PWD}
cd WRFV412_AO
echo "WRF_PWD_DIR is: ${WRF_PWD_DIR}"
WRF_UPDATE_DIR0=${WRF_PWD_DIR}/installOption_WRF/wrfAO412_ring/
WRF_UPDATE_DIR1=${WRF_PWD_DIR}/installOption_WRF/wrfAO412_shared/

echo "Deleting old configure file..."
rm -rf configure.wrf

# WRF configure=54, then nesting=1
echo "choosing 54th option to compile WRF"
echo "nesting option is 1 (normal)"
printf '54\n1\n' | ./configure &> log.configure

echo "copying other files to compile ESMF--WRF"
ln -sf ${WRF_UPDATE_DIR0}/Makefile.wrf Makefile
ln -sf ${WRF_UPDATE_DIR0}/module_domain.F frame/
ln -sf ${WRF_UPDATE_DIR0}/module_diag_rasm.F phys/
ln -sf ${WRF_UPDATE_DIR0}/input_wrf.F share/

ln -sf ${WRF_UPDATE_DIR0}/ext_esmf_write_field.F90 external/io_esmf/
ln -sf ${WRF_UPDATE_DIR0}/ext_esmf_read_field.F90 external/io_esmf/
ln -sf ${WRF_UPDATE_DIR0}/ext_esmf_open_for_read.F90 external/io_esmf/
ln -sf ${WRF_UPDATE_DIR0}/ext_esmf_open_for_write.F90 external/io_esmf/
ln -sf ${WRF_UPDATE_DIR0}/module_esmf_extensions.F90 external/io_esmf/
ln -sf ${WRF_UPDATE_DIR0}/io_esmf.F90 external/io_esmf/
ln -sf ${WRF_UPDATE_DIR0}/wrf_ESMFMod.F main/

ln -sf ${WRF_UPDATE_DIR0}/Registry.EM Registry/Registry.EM
ln -sf ${WRF_UPDATE_DIR0}/Registry.EM_COMMON Registry/Registry.EM_COMMON

ln -sf ${WRF_UPDATE_DIR1}/configure.wrf configure.wrf
ln -sf ${WRF_UPDATE_DIR1}/makefile.io_esmf external/io_esmf/makefile

echo "compiling WRFv4.1.2"
./compile em_real &> log.em_real1
echo "need to compile WRF twice..."
./compile em_real &> log.em_real2
cd ../
