echo "installing Polar WRF"
echo "PWRF DIR is: ${PWRF_DIR}"
cd $PWRF_DIR
WRF_UPDATE_DIR0=${SKRIPS_DIR}/installOption_WRF/wrfAO413_shared/
WRF_UPDATE_DIR1=${SKRIPS_DIR}/installOption_WRF/wrfAO413_ring/
WRF_UPDATE_DIR2=${SKRIPS_DIR}/installOption_seaice/pwrfAOI413/

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

ln -sf ${WRF_UPDATE_DIR2}/ln.csh .

./ln.csh dyn_em module_big_step_utilities_em
# ./ln.csh dyn_em module_first_rk_step_part1
./ln.csh dyn_em module_initialize_real
./ln.csh dyn_em nest_init_utils

./ln.csh phys module_bl_mynn
./ln.csh phys module_fddagd_driver
./ln.csh phys module_fdda_psufddagd
./ln.csh phys module_fdda_spnudging
./ln.csh phys module_mp_morr_two_moment
./ln.csh phys module_mp_p3
./ln.csh phys module_sf_mynn
./ln.csh phys module_sf_noahdrv
./ln.csh phys module_sf_noahlsm
./ln.csh phys module_sf_noahlsm_glacial_only
./ln.csh phys module_sf_noahmpdrv
./ln.csh phys module_sf_noahmp_glacier
./ln.csh phys module_sf_noahmp_groundwater
./ln.csh phys module_sf_noahmplsm
./ln.csh phys module_sf_noah_seaice_drv
./ln.csh phys module_sf_noah_seaice
# ./ln.csh phys module_surface_driver

./ln.csh share module_soil_pre

ln -sf ${WRF_UPDATE_DIR2}/module_surface_driver.F phys/module_surface_driver.F
ln -sf ${WRF_UPDATE_DIR2}/ext_esmf_read_field.F90 external/io_esmf/
ln -sf ${WRF_UPDATE_DIR2}/module_first_rk_step_part1.F dyn_em/module_first_rk_step_part1.F
ln -sf ${WRF_UPDATE_DIR2}/Registry.EM Registry/Registry.EM
ln -sf ${WRF_UPDATE_DIR2}/Registry.EM_COMMON Registry/Registry.EM_COMMON

echo "compiling WRFv4.1.3"
./compile em_real &> log.em_real1

echo "finished copying"
linenumber=$(grep -n "bundled:" configure.wrf | cut -d : -f 1)
echo "linenumber is: " $linenumber
head -n $((linenumber-1)) configure.wrf > configure.wrf_cpl

cd ../
