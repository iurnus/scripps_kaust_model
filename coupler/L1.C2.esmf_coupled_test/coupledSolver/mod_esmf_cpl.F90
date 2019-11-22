! Earth System Modeling Framework
! Copyright 2002-2019, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
#define FILENAME "mod_esmf_cpl.F90"
!
!-----------------------------------------------------------------------
!     CPL gridded component code 
!-----------------------------------------------------------------------
!
module mod_esmf_cpl
!
!-----------------------------------------------------------------------
!     Used module declarations 
!-----------------------------------------------------------------------
!
  use ESMF
  use NUOPC
  use NUOPC_Connector, only :                                       &
      NUOPC_SetServices     => SetServices,                         &
      NUOPC_Label_ComputeRH => label_ComputeRouteHandle,            &
      NUOPC_Label_ExecuteRH => label_ExecuteRouteHandle,            &
      NUOPC_Label_ReleaseRH => label_ReleaseRouteHandle,            &
      NUOPC_ConnectorGet, NUOPC_ConnectorSet
!
  implicit none
  private
!
!-----------------------------------------------------------------------
!     Public subroutines 
!-----------------------------------------------------------------------
!
  public :: CPL_SetServices
!
  contains
!
  subroutine CPL_SetServices(ccomp, rc)
  implicit none
!
!-----------------------------------------------------------------------
!     Imported variable declarations 
!-----------------------------------------------------------------------
!
  type(ESMF_CplComp) :: ccomp
  integer, intent(out) :: rc
!
  rc = ESMF_SUCCESS
!
  call NUOPC_CompDerive(ccomp, NUOPC_SetServices, rc = rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
      line=__LINE__, file=FILENAME)) return

  call NUOPC_CompSpecialize(ccomp, specLabel=NUOPC_Label_ComputeRH, &
                               specRoutine=CPL_ComputeRH, rc = rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
      line=__LINE__, file=FILENAME)) return

  call NUOPC_CompSpecialize(ccomp, specLabel=NUOPC_Label_ExecuteRH, &
                               specRoutine=CPL_ExecuteRH, rc = rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
      line=__LINE__, file=FILENAME)) return

  call NUOPC_CompSpecialize(ccomp, specLabel=NUOPC_Label_ReleaseRH, &
                               specRoutine=CPL_ReleaseRH, rc = rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
      line=__LINE__, file=FILENAME)) return

  end subroutine CPL_SetServices
!
!-----------------------------------------------------------------------
!     Initialize the coupler
!-----------------------------------------------------------------------
!
  subroutine CPL_ComputeRH(ccomp, rc)
  implicit none

  type(ESMF_CplComp) :: ccomp
  integer, intent(out) :: rc
!
  integer   :: localrc
  type(ESMF_State)              :: state
  type(ESMF_FieldBundle)        :: dstFields, srcFields
  type(ESMF_FieldBundle)        :: interDstFields
  type(ESMF_Field), allocatable :: fields(:)
  integer                       :: fieldCount, i
  type(ESMF_Grid)               :: Grid
  type(ESMF_TypeKind_Flag)      :: typekind
  type(ESMF_Field)              :: field
  type(ESMF_RouteHandle)        :: rh1, rh2
  type(ESMF_RouteHandle)        :: rh

  rc = ESMF_SUCCESS
  PRINT *, "calling empty CPL_ComputeRH..."

  end subroutine
!
!-----------------------------------------------------------------------
!     Run the coupler
!-----------------------------------------------------------------------
!
  subroutine CPL_ExecuteRH(ccomp, rc)
  implicit none

  type(ESMF_CplComp) :: ccomp
  integer, intent(out) :: rc

  integer                       :: localrc
  type(ESMF_FieldBundle)        :: interDstFields
  type(ESMF_RouteHandle)        :: rh1, rh2
  type(ESMF_State)              :: state
  type(ESMF_FieldBundle)        :: dstFields, srcFields

!
  rc = ESMF_SUCCESS
!
  PRINT *, "running empty CPL_ExecuteRH..."

  end subroutine
!
!-----------------------------------------------------------------------
!     Finalize the coupler
!-----------------------------------------------------------------------
!
  subroutine CPL_ReleaseRH(ccomp, rc)
  implicit none

  type(ESMF_CplComp) :: ccomp
  integer, intent(out) :: rc

  integer                       :: localrc
  type(ESMF_State)              :: state
  type(ESMF_FieldBundle)        :: interDstFields
  type(ESMF_RouteHandle)        :: rh1, rh2

!
  rc = ESMF_SUCCESS

  PRINT *, "calling empty CPL_ReleaseRH..."

  end subroutine

end module mod_esmf_cpl
