!-----------------------------------------------------------------------
! Earth System Modeling Framework
! Copyright 2002-2017, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
#define FILENAME "mod_esmf_esm.F90"
!
!-----------------------------------------------------------------------
!     ESM gridded component code 
!-----------------------------------------------------------------------
!
      module mod_esmf_esm
!
!-----------------------------------------------------------------------
!     Used module declarations 
!-----------------------------------------------------------------------
!
      use ESMF
      use NUOPC
      use NUOPC_Driver,                                                 &
          NUOPC_SetServices            => SetServices,                  &
          NUOPC_Label_SetModelServices => label_SetModelServices,       &
          NUOPC_Label_SetRunSequence   => label_SetRunSequence
!
      use mod_types
      use mod_esmf_atm, only: ATM_SetServices
      use mod_esmf_ocn, only: OCN_SetServices
      use mod_esmf_cpl, only: CPL_SetServices
!
      implicit none
      private
!
!-----------------------------------------------------------------------
!     Public subroutines 
!-----------------------------------------------------------------------
!
      public :: ESM_SetServices
!
      contains
!
      subroutine ESM_SetServices(gcomp, rc)
      implicit none
!
!-----------------------------------------------------------------------
!     Imported variable declarations 
!-----------------------------------------------------------------------
!
      type(ESMF_GridComp) :: gcomp
      integer, intent(out) :: rc
!
      rc = ESMF_SUCCESS
!
!-----------------------------------------------------------------------
!     Register generic methods 
!-----------------------------------------------------------------------
!
      print *, "calling ESM_SetServices function"
      call NUOPC_CompDerive(gcomp, NUOPC_SetServices, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
          line=__LINE__, file=FILENAME)) return
!
!-----------------------------------------------------------------------
!     Attach specializing methods 
!-----------------------------------------------------------------------
!
      print *, "calling NUOPC_CompSpecialize function"
      call NUOPC_CompSpecialize(gcomp,                                  &
                                specLabel=NUOPC_Label_SetModelServices, &
                                specRoutine=ESM_SetModelServices, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
          line=__LINE__, file=FILENAME)) return
!
      print *, "calling NUOPC_CompSpecialize function"
      call NUOPC_CompSpecialize(gcomp,                                  &
                                specLabel=NUOPC_Label_SetRunSequence,   &
                                specRoutine=ESM_SetRunSequence, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
          line=__LINE__, file=FILENAME)) return
!
      print *, "calling NUOPC_CompSpecialize function finished"
      end subroutine ESM_SetServices
!
      subroutine ESM_SetModelServices(gcomp, rc)
      implicit none
!
!-----------------------------------------------------------------------
!     Imported variable declarations 
!-----------------------------------------------------------------------
!
      type(ESMF_GridComp) :: gcomp
      integer, intent(out) :: rc
!     
!-----------------------------------------------------------------------
!     Local variable declarations 
!-----------------------------------------------------------------------
!
      integer :: i, j
!
      type(ESMF_GridComp) :: child
      type(ESMF_CplComp) :: connector

      type(ESMF_Calendar) :: esmCal
      type(ESMF_Clock) :: esmClock
!
      rc = ESMF_SUCCESS
      print *, "calling ESM_SetModelServices function"
!
!-----------------------------------------------------------------------
!     SetServices for model components 
!-----------------------------------------------------------------------
!
      print *, "setting ATM services"
      call NUOPC_DriverAddComp(gcomp, "ATM", ATM_SetServices,           &
                               comp=child, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
      call ESMF_AttributeSet(child, name="Verbosity", value="high",     &
                             rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return

      print *, "setting OCN services"
      call NUOPC_DriverAddComp(gcomp, "OCN", OCN_SetServices,           &
                               comp=child, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
      call ESMF_AttributeSet(child, name="Verbosity", value="high",     &
                             rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
!
!-----------------------------------------------------------------------
!     SetServices for connector components 
!-----------------------------------------------------------------------
!
      call NUOPC_DriverAddComp(gcomp,                                   &
                           srcCompLabel="ATM",                          &
                           dstCompLabel="OCN",                          &
                           compSetServicesRoutine=CPL_SetServices,      &
                           comp=connector, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
      call ESMF_AttributeSet(connector, name="Verbosity", value="high", &
                             rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return

      call NUOPC_DriverAddComp(gcomp,                                   &
                           srcCompLabel="OCN",                          &
                           dstCompLabel="ATM",                          &
                           compSetServicesRoutine=CPL_SetServices,      &
                           comp=connector, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
      call ESMF_AttributeSet(connector, name="Verbosity", value="high", &
                             rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
!
!-----------------------------------------------------------------------
!     Set internal clock for application (gcomp). The time step must be 
!     set to the slowest time interval of the connector components
!-----------------------------------------------------------------------
!
      print *, "setting clock services according to namelist.rc"
!
      esmClock = ESMF_ClockCreate(name="ESM_Clock",                     &
                                  timeStep=esmTimeStep,                 &
                                  startTime=esmStartTime,               &
                                  stopTime=esmStopTime,                 &
                                  rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
          line=__LINE__, file=FILENAME)) return
!
      call ESMF_GridCompSet(gcomp, clock=esmClock, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
          line=__LINE__, file=FILENAME)) return
      print *, "setting clock services finished!"
!
      end subroutine ESM_SetModelServices 
!
      subroutine ESM_SetRunSequence(gcomp, rc)
      implicit none
!
!-----------------------------------------------------------------------
!     Imported variable declarations 
!-----------------------------------------------------------------------
!
      type(ESMF_GridComp) :: gcomp
      integer, intent(out) :: rc
!     
!-----------------------------------------------------------------------
!     Local variable declarations 
!-----------------------------------------------------------------------
!
      integer :: i, j, maxdiv, runid, localPet, petCount
      character(ESMF_MAXSTR) :: cname
!
      type(ESMF_VM) :: vm
      type(ESMF_Time) :: startTime
      type(ESMF_Time) :: stopTime
      type(ESMF_TimeInterval) :: timeStep
      type(ESMF_Clock) :: internalClock
!
      type(ESMF_Clock) :: localclock
      integer :: urc
!
      rc = ESMF_SUCCESS

      PRINT *, "Running coupled solver..."

      call ESMF_GridCompGet(gcomp, vm=vm, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
          line=__LINE__, file=FILENAME)) return

      call ESMF_VMGet(vm, localPet=localPet, petCount=petCount, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
          line=__LINE__, file=FILENAME)) return

      call NUOPC_DriverNewRunSequence(gcomp, slotCount=1, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
          line=__LINE__, file=FILENAME)) return

      call NUOPC_DriverAddRunElement(gcomp, slot=1,                     &
                                     srcCompLabel="ATM",                &
                                     dstCompLabel="OCN",                &
                                     rc=rc)

      call NUOPC_DriverAddRunElement(gcomp, slot=1,                     &
                                     srcCompLabel="OCN",                &
                                     dstCompLabel="ATM",                &
                                     rc=rc)

      call NUOPC_DriverAddRunElement(gcomp, slot=1, compLabel="OCN",    &
                                     rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
          line=__LINE__, file=FILENAME)) return

      call NUOPC_DriverAddRunElement(gcomp, slot=1, compLabel="ATM",    &
                                     rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
          line=__LINE__, file=FILENAME)) return

      end subroutine ESM_SetRunSequence
!
      end module mod_esmf_esm
