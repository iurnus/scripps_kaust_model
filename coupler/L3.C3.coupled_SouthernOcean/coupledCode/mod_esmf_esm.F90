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
      subroutine ESM_SetServices(driver, rc)
      implicit none
!
!-----------------------------------------------------------------------
!     Imported variable declarations 
!-----------------------------------------------------------------------
!
      type(ESMF_GridComp) :: driver
      integer, intent(out) :: rc
!
      rc = ESMF_SUCCESS
!
!-----------------------------------------------------------------------
!     Register generic methods 
!-----------------------------------------------------------------------
!
      call NUOPC_CompDerive(driver, NUOPC_SetServices, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
          line=__LINE__, file=FILENAME)) return
!
!-----------------------------------------------------------------------
!     Attach specializing methods 
!-----------------------------------------------------------------------
!
      call NUOPC_CompSpecialize(driver,                                  &
                                specLabel=NUOPC_Label_SetModelServices, &
                                specRoutine=ESM_SetModelServices, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
          line=__LINE__, file=FILENAME)) return
!
      call NUOPC_CompSpecialize(driver,                                  &
                                specLabel=NUOPC_Label_SetRunSequence,   &
                                specRoutine=ESM_SetRunSequence, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
          line=__LINE__, file=FILENAME)) return
!
      end subroutine ESM_SetServices
!
      subroutine ESM_SetModelServices(driver, rc)
      implicit none
!
!-----------------------------------------------------------------------
!     Imported variable declarations 
!-----------------------------------------------------------------------
!
      type(ESMF_GridComp) :: driver
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
      integer, allocatable :: petList(:)
!
      rc = ESMF_SUCCESS
!
!-----------------------------------------------------------------------
!     SetServices for model components 
!-----------------------------------------------------------------------
!
      allocate(petList(cpuATM))
      do i=1,cpuATM
        petList(i) = i-1
      enddo
      call NUOPC_DriverAddComp(driver, "ATM", ATM_SetServices,           &
                               petList=petList,comp=child, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
      call ESMF_AttributeSet(child, name="Verbosity", value="high",     &
                             rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
      deallocate(petList)

      allocate(petList(cpuOCN))
      if (coupleMode .eq. 1) then
        do i=1,cpuOCN
          petList(i) = i-1
        enddo
      elseif (coupleMode .eq. 2) then
        do i=1,cpuOCN
          petList(i) = i-1 + cpuATM
        enddo
      endif
      call NUOPC_DriverAddComp(driver, "OCN", OCN_SetServices,           &
                               petList=petList,comp=child, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
      call ESMF_AttributeSet(child, name="Verbosity", value="high",    &
                             rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
      deallocate(petList)
!
!-----------------------------------------------------------------------
!     SetServices for connector components 
!-----------------------------------------------------------------------
!
      call NUOPC_DriverAddComp(driver,                                   &
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

      call NUOPC_DriverAddComp(driver,                                   &
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
!     Set internal clock for application (driver). The time step must be 
!     set to the slowest time interval of the connector components
!-----------------------------------------------------------------------
!
      esmClock = ESMF_ClockCreate(name="ESM_Clock",                     &
                                  timeStep=esmTimeStep,                 &
                                  startTime=esmStartTime,               &
                                  stopTime=esmStopTime,                 &
                                  rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
          line=__LINE__, file=FILENAME)) return
!
      call ESMF_GridCompSet(driver, clock=esmClock, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
          line=__LINE__, file=FILENAME)) return
!
      end subroutine ESM_SetModelServices 
!
      subroutine ESM_SetRunSequence(driver, rc)
      implicit none
!
!-----------------------------------------------------------------------
!     Imported variable declarations 
!-----------------------------------------------------------------------
!
      type(ESMF_GridComp) :: driver
      integer, intent(out) :: rc
      type(NUOPC_FreeFormat)              :: runSeqFF
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
      type(ESMF_Clock) :: localclock
      real(ESMF_KIND_R8) :: timeStart, timeEnd
      character(160)  :: msgString

      integer :: urc
!
      rc = ESMF_SUCCESS

      call ESMF_VMWtime(timeStart)
      write (msgString,*) "START TIME: ", timeStart
      call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)

      call ESMF_GridCompGet(driver, vm=vm, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
          line=__LINE__, file=FILENAME)) return


      ! read free format run sequence from config
      runSeqFF = NUOPC_FreeFormatCreate(stringList=(/ &
        " @*            ",    &
        "   OCN -> ATM  ",    &
        "   ATM -> OCN  ",    &
        "   OCN         ",    &
        "   ATM         ",    &
        " @             " /), &
        rc=rc)

      call NUOPC_FreeFormatLog(runSeqFF, rc=rc)
      call NUOPC_DriverIngestRunSequence(driver, runSeqFF, &
        autoAddConnectors=.true., rc=rc)
      call NUOPC_DriverPrint(driver, orderflag=.true., rc=rc)
      call NUOPC_FreeFormatDestroy(runSeqFF, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
          line=__LINE__, file=FILENAME)) return
      call ESMF_VMWtime(timeEnd)
      write (msgString,*) "END TIME: ", timeEnd
      call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)

      end subroutine ESM_SetRunSequence
!
      end module mod_esmf_esm
