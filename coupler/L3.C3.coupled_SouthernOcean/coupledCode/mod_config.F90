!-----------------------------------------------------------------------
! Earth System Modeling Framework
! Copyright 2002-2017, University Corporation for Atmospheric Research,
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
      integer :: localPet, petCount, iEntry
      character(ESMF_MAXSTR) :: entryName, entryUnit
      logical :: file_exists
!
      type(ESMF_Config) :: cf
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
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,  &
            line=__LINE__, file=FILENAME)) return
!
        call ESMF_ConfigLoadFile(cf, trim(config_fname), rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,  &
            line=__LINE__, file=FILENAME)) return

        ! Set debug level 
        call ESMF_ConfigGetAttribute(cf, debugLevel,                    &
                                     label='DebugLevel:', rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,  &
            line=__LINE__, file=FILENAME)) return

        PRINT *, "DebugLevel now is: ", debugLevel

        ! Set simulation start/stop time 
        call ESMF_ConfigGetAttribute(cf, start_year,                    &
                                     label='StartYear:', rc=rc)
        call ESMF_ConfigGetAttribute(cf, start_month,                   &
                                     label='StartMonth:', rc=rc)
        call ESMF_ConfigGetAttribute(cf, start_day,                     &
                                     label='StartDay:', rc=rc)
        call ESMF_ConfigGetAttribute(cf, start_hour,                    &
                                     label='StartHour:', rc=rc)
        call ESMF_ConfigGetAttribute(cf, start_minute,                  &
                                     label='StartMinute:', rc=rc)
        call ESMF_ConfigGetAttribute(cf, start_second,                  &
                                     label='StartSecond:', rc=rc)
        call ESMF_TimeSet(esmStartTime,                                 &
                          yy=start_year, mm=start_month, dd=start_day,  &
                          h=start_hour, m=start_minute, s=start_second, &
                          calkindflag=ESMF_CALKIND_GREGORIAN, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,  &
            line=__LINE__, file=FILENAME)) return

        call ESMF_ConfigGetAttribute(cf, stop_year,                     &
                                     label='StopYear:', rc=rc)
        call ESMF_ConfigGetAttribute(cf, stop_month,                    &
                                     label='StopMonth:', rc=rc)
        call ESMF_ConfigGetAttribute(cf, stop_day,                      &
                                     label='StopDay:', rc=rc)
        call ESMF_ConfigGetAttribute(cf, stop_hour,                     &
                                     label='StopHour:', rc=rc)
        call ESMF_ConfigGetAttribute(cf, stop_minute,                   &
                                     label='StopMinute:', rc=rc)
        call ESMF_ConfigGetAttribute(cf, stop_second,                   &
                                     label='StopSecond:', rc=rc)
        call ESMF_TimeSet(esmStopTime,                                  &
                          yy=stop_year, mm=stop_month, dd=stop_day,     &
                          h=stop_hour, m=stop_minute, s=stop_second,    &
                          calkindflag=ESMF_CALKIND_GREGORIAN, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,  &
            line=__LINE__, file=FILENAME)) return

        ! Set simulation time step
        call ESMF_ConfigGetAttribute(cf, esm_step_seconds,              &
                                     label='EsmStepSeconds:', rc=rc)
        call ESMF_TimeIntervalSet(esmTimeStep,                          &
                                  s=esm_step_seconds, rc=rc)

        call ESMF_ConfigGetAttribute(cf, atm_step_seconds,              &
                                     label='ATMStepSeconds:', rc=rc)
        call ESMF_TimeIntervalSet(atmTimeStep,                          &
                                  s=atm_step_seconds, rc=rc)

        call ESMF_ConfigGetAttribute(cf, ocn_step_seconds,              &
                                     label='OCNStepSeconds:', rc=rc)
        call ESMF_TimeIntervalSet(ocnTimeStep,                          &
                                  s=ocn_step_seconds, rc=rc)
!
        call ESMF_ConfigGetAttribute(cf, coupleMode,                    &
                                     label='coupleMode:', rc=rc)
        call ESMF_ConfigGetAttribute(cf, cpuOCN,                        &
                                     label='cpuOCN:', rc=rc)
        call ESMF_ConfigGetAttribute(cf, cpuATM,                        &
                                     label='cpuATM:', rc=rc)
      end if

      currentTimeStep = 0
!
!-----------------------------------------------------------------------
!     Set field dictionary entries
!-----------------------------------------------------------------------
!
      do iEntry = 1, nList
        entryName = trim(nuopc_entryNameList(iEntry));
        entryUnit = trim(nuopc_entryUnitList(iEntry));
        PRINT *, "entry name is: ", trim(entryName)
        call NUOPC_FieldDictionaryAddEntry(entryName, &
                canonicalUnits=entryUnit, rc=rc)
      end do
    

      end subroutine read_config
!
      end module mod_config
