! Earth System Modeling Framework
! Copyright 2002-2019, University Corporation for Atmospheric Research,
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
  type ESM_Conn
    character(ESMF_MAXSTR) :: name
    character(ESMF_MAXSTR), allocatable :: srcFields(:)
    character(ESMF_MAXSTR), allocatable :: dstFields(:)
  end type ESM_Conn
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
  real(ESMF_KIND_R8), parameter :: ZERO_R4 = 0.0
  real(ESMF_KIND_R8), parameter :: ZERO_R8 = 0.0d0
  real(ESMF_KIND_R8), parameter :: ONE_R8 = 1.0d0
!
  integer(ESMF_KIND_I4), parameter :: MAPPED_MASK = 99
  integer(ESMF_KIND_I4), parameter :: UNMAPPED_MASK = 98
!
  integer, parameter :: MAX_MAPPED_GRID = 1000
!
  character(ESMF_MAXSTR) :: config_fname="namelist.rc"

  character(ESMF_MAXSTR) :: ocnExportField(1)=(/'sst'/)
  character(ESMF_MAXSTR) :: ocnImportField(1)=(/'pmsl'/)
  character(ESMF_MAXSTR) :: atmExportField(1)=(/'pmsl'/)
  character(ESMF_MAXSTR) :: atmImportField(1)=(/'sst'/)
!
  integer :: debugLevel = 1

  type(ESM_Conn), allocatable, target :: connectors(:)
!
end module mod_types
