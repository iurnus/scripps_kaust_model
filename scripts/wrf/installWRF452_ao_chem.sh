echo "installing WRF"
echo "WRF DIR is: ${WRF_CHEM_DIR}"
read -e -p "Continue with this WRF DIR? (Y/N): " -i "Y" defaultFlag
if [ $defaultFlag == 'Y' ]; then
  echo "continue"
else 
  echo "stop"
  exit
fi

cd ${WRF_CHEM_DIR}
export WRF_ESMF=1
export WRF_CHEM=1
WRF_UPDATE_DIR0=${SKRIPS_DIR}/scripts/wrf/wrfAO452_shared/
WRF_UPDATE_DIR1=${SKRIPS_DIR}/scripts/wrf/wrfAO452_chem_shared/

echo "Deleting old configure file..."
rm -rf configure.wrf

ln -sf ${WRF_UPDATE_DIR0}/Config.pl arch/
ln -sf ${WRF_UPDATE_DIR0}/preample arch/
ln -sf ${WRF_UPDATE_DIR0}/postample arch/

# for kala, WRF configure=34, then nesting=1
./configure

echo "copying other files to compile ESMF--WRF"
# ln -sf ${WRF_UPDATE_DIR1}/configure.wrf configure.wrf
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

ln -sf ${WRF_UPDATE_DIR1}/Registry.EM_CHEM Registry/
ln -sf ${WRF_UPDATE_DIR1}/chemics_init.F chem/

echo "compiling WRFv4.5.2"
./compile em_real &> log.em_real1

echo "finished copying"
linenumber=$(grep -n "bundled:" configure.wrf | cut -d : -f 1)
echo "linenumber is: " $linenumber
head -n $((linenumber-1)) configure.wrf > configure.wrf_cpl

cd ..

if [ -f $WRF_CHEM_DIR/main/wrf.exe ]; then
  echo The WRF-chem model is installed successfully
else 
  echo ERROR! Installation is NOT successful!
  echo Please check the log file in $WRF_CHEM_DIR/log.em_real1
fi

