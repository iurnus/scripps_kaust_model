!-----------------------------------------------------------------------
! Earth System Modeling Framework
! Copyright 2002-2017, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
#define FILENAME "mod_types.F90" 
!
!-----------------------------------------------------------------------
!     Module for user defined types 
!-----------------------------------------------------------------------
!
      module mod_types
!
!-----------------------------------------------------------------------
!     Used module declarations 
!-----------------------------------------------------------------------
!
      use ESMF
      use NUOPC
!
      implicit none
!
!-----------------------------------------------------------------------
!     Constants 
!     cf3 - 1/rhow (rhow is reference density of seawater in kg/m3)
!-----------------------------------------------------------------------
!
      real*8, parameter :: cp = 3985.0d0
      real*8, parameter :: rho0 = 1025.0d0
      real*8, parameter :: cf1 = rho0*cp
      real*8, parameter :: cf2 = 1.0d0/cf1
      real*8, parameter :: cf3 = 1.0d0/rho0
      real*8, parameter :: day2s = 1.0d0/86400.0d0
      real*8, parameter :: mm2m = 1.0d0/1000.0d0
      real*8, parameter :: pi = 4.0d0*atan(1.0d0)
      real*8, parameter :: pi2 = 2.0d0*pi
      real*8, parameter :: phi = 0.5d0*pi
      real*8, parameter :: D2R = PI/180.0d0
      real*8, parameter :: R2D = 1.0d0/D2R
      real*8, parameter :: RADIUS = 6371.0d3
!
      real(ESMF_KIND_R8), parameter :: MISSING_R8 = 1.0d20
      real(ESMF_KIND_R4), parameter :: MISSING_R4 = 1.0e20
      real(ESMF_KIND_R8), parameter :: TOL_R8 = MISSING_R8/2.0d0
      real(ESMF_KIND_R4), parameter :: TOL_R4 = MISSING_R4/2.0
!
      real(ESMF_KIND_I4), parameter :: ZERO_I4 = 0
      real(ESMF_KIND_R4), parameter :: ZERO_R4 = 0.0
      real(ESMF_KIND_R8), parameter :: ZERO_R8 = 0.0d0
      real(ESMF_KIND_R8), parameter :: ONE_R8 = 1.0d0
!
      integer(ESMF_KIND_I4), parameter :: MAPPED_MASK = 99
      integer(ESMF_KIND_I4), parameter :: UNMAPPED_MASK = 98
!
      integer, parameter :: MAX_MAPPED_GRID = 1000
!
      character(ESMF_MAXSTR) :: config_fname="namelist.rc"
!
      integer :: debugLevel = 1

      integer :: start_year
      integer :: start_month
      integer :: start_day
      integer :: start_hour
      integer :: start_minute
      integer :: start_second
      integer :: stop_year
      integer :: stop_month
      integer :: stop_day
      integer :: stop_hour
      integer :: stop_minute
      integer :: stop_second

      integer :: esm_step_seconds
      integer :: atm_step_seconds
      integer :: ocn_step_seconds

      ! 1: sqeuential; 2: concurrent
      integer :: coupleMode
      integer :: cpuOCN
      integer :: cpuATM

      real(ESMF_KIND_R4) :: esm_wall_time = 0.d0
      real(ESMF_KIND_R4) :: atm_wall_time = 0.d0
      real(ESMF_KIND_R4) :: ocn_wall_time = 0.d0

      type(ESMF_Calendar) :: esmCal
      type(ESMF_Time) :: esmStartTime
      type(ESMF_Time) :: esmStopTime
      type(ESMF_TimeInterval) :: esmTimeStep
      type(ESMF_TimeInterval) :: ocnTimeStep
      type(ESMF_TimeInterval) :: atmTimeStep
      integer :: currentTimeStep = 0

      integer :: nList = 21
      character(ESMF_MAXSTR), dimension(1:21) :: nuopc_entryNameList=&
                   (/'XLAT_VALUE','XLONG_VALUE',&
                     'LATENT_HEAT','SENSIBLE_HEAT',&
                     'SHORTWAVE_UP_FLUX','SHORTWAVE_DOWN_FLUX',&
                     'LONGWAVE_UP_FLUX','LONGWAVE_DOWN_FLUX',&
                     'U_VELOCITY_AT_10M','V_VELOCITY_AT_10M',&
                     'TEMPERATURE_AT_2M','SPECIFIC_HUMIDITY_AT_2M',&
                     'SURFACE_EVAPORATION','PRECIP_CONVECTIVE',&
                     'PRECIP_SH_CONVECTIVE','PRECIP_NON_CONVECTIVE',&
                     'REANALYSIS_SEA_SURFACE_TEMPERATURE',&
                     'OCEAN_MASK_VALUE',&
                     'ACTIVE_SEA_SURFACE_TEMPERATURE',&
                     'OCEAN_SURFACE_U','OCEAN_SURFACE_V'/)
      character(ESMF_MAXSTR), dimension(1:21) :: wrf_nameList=&
                   (/'XLAT','XLONG',&
                     'LH','HFX','SWUPB','SWDNB','LWUPB','LWDNB',&
                     'U10','V10','T2','Q2',&
                     'QFX','RAINCV','RAINSHV','RAINNCV','SST_INPUT',&
                     'OCNMASK','SST','UOCE','VOCE'/)
      character(ESMF_MAXSTR), dimension(1:21) :: nuopc_entryUnitList=&
                   (/'1','1',&
                     'w/m^2','w/m^2','w/m^2','w/m^2','w/m^2','w/m^2',&
                     'm/s','m/s','degree','kg/kg',&
                     'kg/m2','mm','mm','mm','degree',&
                     '1','degree','m/s','m/s'/)
      logical, dimension(1:21) :: OCNtoATM=&
                   (/.False.,.False.,.False.,.False.,.False.,.False.,&
                     .False.,.False.,.False.,.False.,.False.,.False.,&
                     .False.,.False.,.False.,.False.,.False.,&
                     .True.,.True.,.True.,.True./)
      logical, dimension(1:21) :: ATMtoOCN=&
                   (/.True.,.True.,.True.,.True.,.True.,.True.,&
                     .True.,.True.,.True.,.True.,.True.,.True.,&
                     .True.,.True.,.True.,.True.,.True.,&
                     .False.,.False.,.False.,.False./)
!
      end module mod_types
