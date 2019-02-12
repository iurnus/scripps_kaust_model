echo "installing WRF"
WRF_PWD_DIR=${PWD}
cd WRFV3911_AO
echo "WRF_PWD_DIR is: ${WRF_PWD_DIR}"
WRF_OPTION_DIR=${WRF_PWD_DIR}/installOption_WRF/wrfAO3911Implementations_shaheen/

echo "Deleting old configure file..."
rm -rf configure.wrf

# WRF configure=50, then nesting=1
echo "choosing 50th option to compile WRF"
echo "nesting option is 1 (normal)"
printf '50\n1\n' | ./configure &> log.configure

echo "copying other files to compile ESMF--WRF"
ln -sf ${WRF_OPTION_DIR}/Makefile.main main/Makefile
ln -sf ${WRF_OPTION_DIR}/wrf_test_ESMF.F main/
ln -sf ${WRF_OPTION_DIR}/wrf_ESMFMod.F main/
ln -sf ${WRF_OPTION_DIR}/Makefile.wrf Makefile
# ln -sf ${WRF_OPTION_DIR}/makefile.io_netcdf external/io_netcdf/makefile
ln -sf ${WRF_OPTION_DIR}/makefile.io_esmf external/io_esmf/makefile
ln -sf ${WRF_OPTION_DIR}/module_domain.F frame/
ln -sf ${WRF_OPTION_DIR}/ext_esmf_write_field.F90 external/io_esmf/
ln -sf ${WRF_OPTION_DIR}/ext_esmf_read_field.F90 external/io_esmf/
ln -sf ${WRF_OPTION_DIR}/ext_esmf_open_for_read.F90 external/io_esmf/
ln -sf ${WRF_OPTION_DIR}/ext_esmf_open_for_write.F90 external/io_esmf/
ln -sf ${WRF_OPTION_DIR}/module_esmf_extensions.F90 external/io_esmf/
ln -sf ${WRF_OPTION_DIR}/module_ltng_iccg.F phys/
ln -sf ${WRF_OPTION_DIR}/module_diag_rasm.F phys/
ln -sf ${WRF_OPTION_DIR}/module_diag_cl.F phys/
ln -sf ${WRF_OPTION_DIR}/module_diagnostics_driver.F phys/
ln -sf ${WRF_OPTION_DIR}/io_esmf.F90 external/io_esmf/
ln -sf ${WRF_OPTION_DIR}/Registry.EM Registry/Registry.EM
ln -sf ${WRF_OPTION_DIR}/Registry.EM_COMMON_direct Registry/Registry.EM_COMMON
ln -sf ${WRF_OPTION_DIR}/configure.wrf configure.wrf

echo "compiling WRFv3.9.1.1"
./compile em_real &> log.em_real0
echo "need to compile WRF twice..."
./compile em_real &> log.em_real1
cd ../
