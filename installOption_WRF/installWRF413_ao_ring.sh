echo "installing WRF"
echo "WRF DIR is: ${SKRIPS_DIR}/WRFV413_AO"
cd ${SKRIPS_DIR}/WRFV413_AO
WRF_UPDATE_DIR0=${SKRIPS_DIR}/installOption_WRF/wrfAO413_shared/
WRF_UPDATE_DIR1=${SKRIPS_DIR}/installOption_WRF/wrfAO413_ring/

echo "Deleting old configure file..."
rm -rf configure.wrf

# WRF configure=54, then nesting=1
echo "choosing 54th option to compile WRF"
echo "nesting option is 1 (normal)"
printf '54\n1\n' | ./configure &> log.configure

echo "copying other files to compile ESMF--WRF"
ln -sf ${WRF_UPDATE_DIR1}/configure.wrf configure.wrf
ln -sf ${WRF_UPDATE_DIR0}/Makefile.wrf Makefile
ln -sf ${WRF_UPDATE_DIR0}/Registry.EM Registry/

ln -sf ${WRF_UPDATE_DIR0}/ext_esmf_write_field.F90 external/io_esmf/
ln -sf ${WRF_UPDATE_DIR0}/ext_esmf_read_field.F90 external/io_esmf/
ln -sf ${WRF_UPDATE_DIR0}/ext_esmf_open_for_read.F90 external/io_esmf/
ln -sf ${WRF_UPDATE_DIR0}/ext_esmf_open_for_write.F90 external/io_esmf/
ln -sf ${WRF_UPDATE_DIR0}/module_esmf_extensions.F90 external/io_esmf/
ln -sf ${WRF_UPDATE_DIR0}/io_esmf.F90 external/io_esmf/

ln -sf ${WRF_UPDATE_DIR0}/module_diag_rasm.F phys/
ln -sf ${WRF_UPDATE_DIR0}/module_ltng_iccg.F phys/
ln -sf ${WRF_UPDATE_DIR0}/module_sf_ruclsm.F phys/
ln -sf ${WRF_UPDATE_DIR0}/module_sf_sfclayrev.F phys/
ln -sf ${WRF_UPDATE_DIR0}/module_surface_driver.F phys/
ln -sf ${WRF_UPDATE_DIR0}/module_sf_mynn.F phys/

ln -sf ${WRF_UPDATE_DIR0}/input_wrf.F share/
ln -sf ${WRF_UPDATE_DIR0}/module_domain.F frame/
ln -sf ${WRF_UPDATE_DIR0}/module_first_rk_step_part1.F dyn_em/
ln -sf ${WRF_UPDATE_DIR0}/wrf_ESMFMod.F main/


echo "compiling WRFv4.1.3"
./compile em_real &> log.em_real1

echo "finished copying"
linenumber=$(grep -n "bundled:" configure.wrf | cut -d : -f 1)
echo "linenumber is: " $linenumber
head -n $((linenumber-1)) configure.wrf > configure.wrf_cpl

cd ..
