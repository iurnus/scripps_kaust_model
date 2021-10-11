! Earth System Modeling Framework
! Copyright 2002-2017, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
#define FILENAME "mitgcm_wrf.F90"
!-----------------------------------------------------------------------
!     Main function
!-----------------------------------------------------------------------
!
program esmf_application

  ! modules
  use ESMF
  use NUOPC
  use mod_esmf_esm, only : ESM_SetServices 
  use mod_config, only : read_config
  use mod_types
  ! use mod_config, only : set_field_dir
  
  implicit none
  
  ! local variables
  integer :: rc, urc
  type(ESMF_GridComp) :: esmComp
  type(ESMF_VM) :: vm
!
!-----------------------------------------------------------------------
!     Initialize ESMF framework
!-----------------------------------------------------------------------
!
  call ESMF_Initialize(logkindflag=ESMF_LOGKIND_MULTI,              &
                       defaultCalkind=ESMF_CALKIND_GREGORIAN,       &
                       vm=vm, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
      line=__LINE__, file=__FILE__))                                &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)
!
!-----------------------------------------------------------------------
!     Create component 
!-----------------------------------------------------------------------
!
  esmComp = ESMF_GridCompCreate(name="NEW_GRID", rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
      line=__LINE__, file=__FILE__))                                &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)
!
!-----------------------------------------------------------------------
!     Read main configuration file 
!-----------------------------------------------------------------------
!
  call read_config(vm, rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
      line=__LINE__, file=__FILE__))                                &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)
  PRINT *, "DEBUGLEVEL ESMF is: ", debugLevel
!
!-----------------------------------------------------------------------
!     Register component 
!-----------------------------------------------------------------------
!
  call ESMF_GridCompSetServices(esmComp, ESM_SetServices,           &
                                userRc=urc, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
      line=__LINE__, file=__FILE__))                                &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)
  if (ESMF_LogFoundError(rcToCheck=urc, msg=ESMF_LOGERR_PASSTHRU,   &
      line=__LINE__, file=__FILE__))                                &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)
!
!-----------------------------------------------------------------------
!     Wait for finishing initialize phase
!-----------------------------------------------------------------------
!
  call ESMF_VMBarrier(vm, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
      line=__LINE__, file=__FILE__))                                &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)
!
!-----------------------------------------------------------------------
!     Initialize component
!-----------------------------------------------------------------------
!
  call ESMF_GridCompInitialize(esmComp, userRc=urc, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
      line=__LINE__, file=__FILE__))                                &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)
  if (ESMF_LogFoundError(rcToCheck=urc, msg=ESMF_LOGERR_PASSTHRU,   &
      line=__LINE__, file=__FILE__))                                &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)
!
!-----------------------------------------------------------------------
!     Run component 
!-----------------------------------------------------------------------
!
  print *, "calling main run function"
  call ESMF_GridCompRun(esmComp, userRc=urc, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
      line=__LINE__, file=__FILE__))                                &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)
  if (ESMF_LogFoundError(rcToCheck=urc, msg=ESMF_LOGERR_PASSTHRU,   &
      line=__LINE__, file=__FILE__))                                &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)
!
!-----------------------------------------------------------------------
!     Finalize component 
!-----------------------------------------------------------------------
!
  print *, "calling main finalize function"
  call ESMF_GridCompFinalize(esmComp, userRc=urc, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
      line=__LINE__, file=__FILE__))                                &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)
  if (ESMF_LogFoundError(rcToCheck=urc, msg=ESMF_LOGERR_PASSTHRU,   &
      line=__LINE__, file=__FILE__))                                &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)
! 
!-----------------------------------------------------------------------
!     Destroy the earth system Component
!-----------------------------------------------------------------------
! 
  call ESMF_GridCompDestroy(esmComp, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
      line=__LINE__, file=__FILE__))                                &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)
! 
!-----------------------------------------------------------------------
!     Finalize ESMF framework 
!-----------------------------------------------------------------------
!
  call ESMF_Finalize(rc=rc)
      
end program esmf_application
