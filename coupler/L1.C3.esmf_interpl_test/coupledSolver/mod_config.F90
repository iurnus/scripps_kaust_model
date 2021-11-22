! Earth System Modeling Framework
! Copyright 2002-2019, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
#define FILENAME "mod_config.F90" 
!
!-----------------------------------------------------------------------
!     Module for ESM configuration file 
!-----------------------------------------------------------------------
!
module mod_config
!
!-----------------------------------------------------------------------
!     Used module declarations 
!-----------------------------------------------------------------------
!
  use ESMF
  use NUOPC
!
  use mod_types
!
  implicit none
  contains
!
  subroutine read_config(vm, rc)
  implicit none
!
!-----------------------------------------------------------------------
!     Imported variable declarations 
!-----------------------------------------------------------------------
!
  type(ESMF_VM), intent(in) :: vm
  integer, intent(out) :: rc 
!
!-----------------------------------------------------------------------
!     Local variable declarations 
!-----------------------------------------------------------------------
!
  integer :: time(6)
  integer :: i, j, k, p, np, dumm
  integer :: localPet, petCount, lineCount, columnCount
  integer, allocatable :: petList(:)
  logical :: file_exists
  character(100) :: fmt_123, str
!
  type(ESMF_Config) :: cf
  type(ESMF_CalKind_Flag) :: cflag
!
  rc = ESMF_SUCCESS
!
!-----------------------------------------------------------------------
!     Query gridded component
!-----------------------------------------------------------------------
!
  call ESMF_VMGet(vm, localPet=localPet, petCount=petCount, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
      line=__LINE__, file=FILENAME)) return
!
!-----------------------------------------------------------------------
!     Read configuration file 
!-----------------------------------------------------------------------
!
  inquire(file=trim(config_fname), exist=file_exists)
!
  if (file_exists) then
!
  cf = ESMF_ConfigCreate(rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
      line=__LINE__, file=FILENAME)) return
!
  call ESMF_ConfigLoadFile(cf, trim(config_fname), rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
      line=__LINE__, file=FILENAME)) return
!
!-----------------------------------------------------------------------
!     Set debug level 
!-----------------------------------------------------------------------
!
  call ESMF_ConfigGetAttribute(cf, debugLevel, label='debugLevel:', rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
      line=__LINE__, file=FILENAME)) return
  call ESMF_ConfigGetAttribute(cf, interp_option, label='interpolationOption:', rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
      line=__LINE__, file=FILENAME)) return
  PRINT *, 'interpolation option is: ', interp_option
!
  end if
!
!-----------------------------------------------------------------------
!     Added fields to connect
!-----------------------------------------------------------------------
!
  if (.not. allocated(connectors)) then
    allocate(connectors(2))
  end if

  connectors(1)%name = 'ATO' 
  connectors(1)%srcFields = atmExportField
  connectors(1)%dstFields = ocnImportField
  connectors(2)%name = 'OTA' 
  connectors(2)%srcFields = ocnExportField
  connectors(2)%dstFields = atmExportField

  end subroutine read_config
!
end module mod_config
