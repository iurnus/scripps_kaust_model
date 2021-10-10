## # BASH OPTIONS FOR THE COUPLER
#
# Update the following directories from line 4 to 10
export SKRIPS_WAVE_DIR=$HOME/scripps_kaust_model_wave/
export WRF_AOW_DIR=$SKRIPS_WAVE_DIR/WRFV413_AOW/
export WW3_DIR=$SKRIPS_WAVE_DIR/ww3-516/

export PATH=$WW3_DIR/bin:${PATH}
export PATH=$WW3_DIR/exe:${PATH}
export WWATCH3_ENV=$WW3_DIR/wwatch3.env
export WWATCH3_NETCDF=NC4
export NETCDF_CONFIG='/project_shared/Libraries/netcdf-4.4.1.1_pgi_fortran_17.5-0/bin/nc-config'
export NETCDF_CONFIG_C='/project_shared/Libraries/netcdf-cxx4-4.3.0_pgi_fortran_17.5-0/bin/ncxx4-config'
export NETCDF_CONFIG_F='/project_shared/Libraries/netcdf-fortran-4.4.4_pgi_fortran_17.5-0/bin/nf-config'
