echo "installing WRF"
WRF_PWD_DIR=${PWD}
cd WRFV413_AO
echo "WRF_PWD_DIR is: ${WRF_PWD_DIR}"
WRF_UPDATE_DIR0=${WRF_PWD_DIR}/installOption_WRF/wrfAO413_shared/
WRF_UPDATE_DIR1=${WRF_PWD_DIR}/installOption_WRF/wrfAO413_shaheen/

echo "Deleting old configure file..."
rm -rf configure.wrf

# WRF configure=50, then nesting=1
echo "choosing 50th option to compile WRF"
echo "nesting option is 1 (normal)"
printf '50\n1\n' | ./configure &> log.configure

echo "copying other files to compile ESMF--WRF"
ln -sf ${WRF_UPDATE_DIR0}/Makefile.wrf Makefile
ln -sf ${WRF_UPDATE_DIR0}/module_domain.F frame/
ln -sf ${WRF_UPDATE_DIR0}/module_diag_rasm.F phys/
ln -sf ${WRF_UPDATE_DIR0}/module_ltng_iccg.F phys/
ln -sf ${WRF_UPDATE_DIR0}/input_wrf.F share/

ln -sf ${WRF_UPDATE_DIR0}/ext_esmf_write_field.F90 external/io_esmf/
ln -sf ${WRF_UPDATE_DIR0}/ext_esmf_read_field.F90 external/io_esmf/
ln -sf ${WRF_UPDATE_DIR0}/ext_esmf_open_for_read.F90 external/io_esmf/
ln -sf ${WRF_UPDATE_DIR0}/ext_esmf_open_for_write.F90 external/io_esmf/
ln -sf ${WRF_UPDATE_DIR0}/module_esmf_extensions.F90 external/io_esmf/
ln -sf ${WRF_UPDATE_DIR0}/io_esmf.F90 external/io_esmf/
ln -sf ${WRF_UPDATE_DIR0}/wrf_ESMFMod.F main/

ln -sf ${WRF_UPDATE_DIR1}/configure.wrf configure.wrf

echo "compiling WRFv4.1.3"
./compile em_real &> log.em_real1

echo "finished copying"
linenumber=$(grep -n "bundled:" configure.wrf | cut -d : -f 1)
echo "linenumber is: " $linenumber
head -n $((linenumber-1)) configure.wrf > configure.wrf_cpl

cd ..
