echo "installing WRF"
echo "Polar-WRF DIR is: ${PWRF_DIR}"
read -e -p "Continue with this WRF DIR? (Y/N): " -i "Y" defaultFlag
if [ $defaultFlag == 'Y' ]; then
  echo "continue"
else 
  echo "stop"
  exit
fi

cd ${PWRF_DIR}
export WRF_ESMF=1
WRF_UPDATE_DIR0=${SKRIPS_DIR}/scripts/wrf/wrfAO471_shared/
WRF_UPDATE_DIR1=${SKRIPS_DIR}/code_pwrf/
WRF_UPDATE_DIR2=${SKRIPS_DIR}/scripts/polar-wrf/pwrfAOI471_shared

echo "Deleting old configure file..."
rm -rf configure.wrf

ln -sf ${WRF_UPDATE_DIR0}/Config.pl arch/
ln -sf ${WRF_UPDATE_DIR0}/preamble arch/
ln -sf ${WRF_UPDATE_DIR0}/postamble arch/

# for kala, WRF configure=34, then nesting=1
# for shaheen, WRF configure=50, then nesting=1
./configure

echo "copying other files to compile ESMF--WRF"
ln -sf ${WRF_UPDATE_DIR0}/Makefile.wrf Makefile
# ln -sf ${WRF_UPDATE_DIR0}/Registry.EM Registry/

ln -sf ${WRF_UPDATE_DIR0}/ext_esmf_write_field.F90 external/io_esmf/
# ln -sf ${WRF_UPDATE_DIR0}/ext_esmf_read_field.F90 external/io_esmf/
ln -sf ${WRF_UPDATE_DIR0}/ext_esmf_open_for_read.F90 external/io_esmf/
ln -sf ${WRF_UPDATE_DIR0}/ext_esmf_open_for_write.F90 external/io_esmf/
ln -sf ${WRF_UPDATE_DIR0}/module_esmf_extensions.F90 external/io_esmf/
ln -sf ${WRF_UPDATE_DIR0}/io_esmf.F90 external/io_esmf/

ln -sf ${WRF_UPDATE_DIR0}/module_diag_rasm.F phys/
ln -sf ${WRF_UPDATE_DIR0}/module_ltng_iccg.F phys/
ln -sf ${WRF_UPDATE_DIR0}/module_sf_ruclsm.F phys/
ln -sf ${WRF_UPDATE_DIR0}/module_sf_sfclayrev.F phys/
# ln -sf ${WRF_UPDATE_DIR0}/module_surface_driver.F phys/
# ln -sf ${WRF_UPDATE_DIR0}/module_sf_mynn.F phys/
ln -sf ${WRF_UPDATE_DIR0}/sf_sfclayrev.F90 phys/physics_mmm/

ln -sf ${WRF_UPDATE_DIR0}/input_wrf.F share/
ln -sf ${WRF_UPDATE_DIR0}/module_domain.F frame/
# ln -sf ${WRF_UPDATE_DIR0}/module_first_rk_step_part1.F dyn_em/
ln -sf ${WRF_UPDATE_DIR0}/wrf_ESMFMod.F main/

ln -sf ${WRF_UPDATE_DIR1}/module_mp_morr_two_moment.F phys/
ln -sf ${WRF_UPDATE_DIR1}/module_mp_p3.F phys/
ln -sf ${WRF_UPDATE_DIR1}/module_sf_noahmp_glacier.F  phys/noahmp/src/
ln -sf ${WRF_UPDATE_DIR1}/module_sf_noahmplsm.F  phys/noahmp/src/
ln -sf ${WRF_UPDATE_DIR1}/module_sf_noahmpdrv.F  phys/noahmp/drivers/wrf/
ln -sf ${WRF_UPDATE_DIR1}/module_sf_noah_seaice.F  phys/
ln -sf ${WRF_UPDATE_DIR1}/module_sf_noah_seaice_drv.F  phys/
ln -sf ${WRF_UPDATE_DIR1}/module_sf_noahlsm.F  phys/
ln -sf ${WRF_UPDATE_DIR1}/module_sf_noahdrv.F  phys/
ln -sf ${WRF_UPDATE_DIR1}/module_sf_noahlsm_glacial_only.F  phys/
ln -sf ${WRF_UPDATE_DIR1}/module_initialize_real.F dyn_em/
ln -sf ${WRF_UPDATE_DIR1}/module_soil_pre.F  share/

# There are three files in Polar WRF in conflict with SKRIPS
ln -sf ${WRF_UPDATE_DIR2}/Registry.EM Registry/
ln -sf ${WRF_UPDATE_DIR2}/Registry.EM_COMMON Registry/
ln -sf ${WRF_UPDATE_DIR2}/ext_esmf_read_field.F90 external/io_esmf/
ln -sf ${WRF_UPDATE_DIR2}/module_surface_driver.F  phys/
ln -sf ${WRF_UPDATE_DIR2}/module_first_rk_step_part1.F  dyn_em/
ln -sf ${WRF_UPDATE_DIR2}/module_sf_mynn.F  phys/
ln -sf ${WRF_UPDATE_DIR2}/module_sf_noahdrv.F  phys/
ln -sf ${WRF_UPDATE_DIR2}/module_sf_noahlsm.F  phys/
ln -sf ${WRF_UPDATE_DIR2}/module_sf_noahlsm_glacial_only.F  phys/
ln -sf ${WRF_UPDATE_DIR2}/module_sf_noah_seaice_drv.F  phys/
ln -sf ${WRF_UPDATE_DIR2}/module_sf_noah_seaice.F  phys/

read -e -p "Replace icc option with icx? (N/Y): " -i "N" defaultFlag
if [ $defaultFlag == 'N' ]; then
  echo "Using icc to compile WRF"
else 
  echo "Use icx to compile WRF (for Shaheen)" 
  # need to use icx instead of icc on Shaheen
  sed -i s/icc/icx/g configure.wrf
fi
echo "The option file is: $MITGCM_OPT"

echo "compiling WRFv4.7.1"
./compile em_real &> log.em_real1

echo "finished copying"
linenumber=$(grep -n "bundled:" configure.wrf | cut -d : -f 1)
echo "linenumber is: " $linenumber
head -n $((linenumber-1)) configure.wrf > configure.wrf_cpl

cd ..

if [ -f $PWRF_DIR/main/wrf.exe ]; then
  echo The Polar WRF model is installed successfully
else 
  echo ERROR! Installation is NOT successful!
  echo Please check the log file in $PWRF_DIR/log.em_real1
fi

