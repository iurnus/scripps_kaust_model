!-----------------------------------------------------------------------
! Earth System Modeling Framework
! Copyright 2002-2017, University Corporation for Atmospheric Research,
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

      use mod_types
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
      integer                       :: iterI

      rc = ESMF_SUCCESS
      call NUOPC_ConnectorGet(ccomp, srcFields=srcFields,               &
                              dstFields=dstFields, state=state, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
          line=__LINE__, file=FILENAME)) return

      call ESMF_FieldBundleGet(dstFields, fieldCount=fieldCount, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
          line=__LINE__, file=FILENAME)) return

      allocate(fields(fieldCount))

      call ESMF_FieldBundleGet(dstFields, fieldList=fields, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
          line=__LINE__, file=FILENAME)) return

      interDstFields = ESMF_FieldBundleCreate(name="interDstFields",    &
                                              rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
          line=__LINE__, file=FILENAME)) return

      do i=1, fieldCount
        call ESMF_FieldGet(fields(i), grid=grid, typekind=typekind,     &
                           rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,  &
          line=__LINE__, file=__FILE__)) return  ! bail out
        field = ESMF_FieldCreate(grid, typekind, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,  &
            line=__LINE__, file=FILENAME)) return
        call ESMF_FieldBundleAdd(interDstFields, (/field/), rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,  &
            line=__LINE__, file=FILENAME)) return
      enddo

      ! add interDstFields to the state member
      call ESMF_StateAdd(state, (/interDstFields/), rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
          line=__LINE__, file=FILENAME)) return

      ! compute the first RouteHandle for srcFields->interDstFields (Regrid)
      ! TODO::interpolation method is NEAREST POINT now (1st order)
      call ESMF_FieldBundleRegridStore(srcFields, interDstFields,       &
        unmappedaction=ESMF_UNMAPPEDACTION_IGNORE, &
        regridMethod=ESMF_REGRIDMETHOD_NEAREST_STOD, &
        !! extrapMethod=ESMF_EXTRAPMETHOD_NONE, &
        routehandle=rh1, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
          line=__LINE__, file=FILENAME)) return
      call ESMF_RouteHandleSet(rh1, name="src2interDstRH", rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
          line=__LINE__, file=FILENAME)) return

      ! compute the second RouteHandle for interDstFields->dstFields (Redist)
      call ESMF_FieldBundleRedistStore(interDstFields, dstFields,       &
                                       routehandle=rh2, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
          line=__LINE__, file=FILENAME)) return
      call ESMF_RouteHandleSet(rh2, name="interDst2dstRH", rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
          line=__LINE__, file=FILENAME)) return

      ! add rh1, rh2 to the state member
      call ESMF_StateAdd(state, (/rh1, rh2/), rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
          line=__LINE__, file=FILENAME)) return
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
      real(ESMF_KIND_R8) :: timeStart, timeEnd
      character(160)  :: msgString

!
      rc = ESMF_SUCCESS
!
      call ESMF_VMWtime(timeStart)
      write (msgString,*) "Total time spent on the coupled run: ", timeStart
      call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)

      call NUOPC_ConnectorGet(ccomp, srcFields=srcFields,               &
                              dstFields=dstFields, state=state, rc=rc)

      call ESMF_StateGet(state, "interDstFields", interDstFields, rc=rc)

      call ESMF_StateGet(state, "src2interDstRH", rh1, rc=rc)

      call ESMF_StateGet(state, "interDst2dstRH", rh2, rc=rc)

      call ESMF_FieldBundleRegrid(srcFields, interDstFields,            &
                                  routehandle=rh1, rc=rc)

      call ESMF_FieldBundleRegrid(interDstFields, dstFields,            &
                                  routehandle=rh2, rc=rc)

      call ESMF_VMWtime(timeEnd)
       
      esm_wall_time = esm_wall_time + timeEnd - timeStart

      write (msgString,*) "  Time spent on ESMF coupler: ", esm_wall_time
      call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)
      write (msgString,*) "  Time spent on running ATM model: ", atm_wall_time
      call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)
      write (msgString,*) "  Time spent on running OCN model: ", ocn_wall_time
      call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)
      write (msgString,*) "  Time spent on other ESMF processes : ", &
        timeEnd - atm_wall_time - ocn_wall_time - esm_wall_time
      call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)

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

      call NUOPC_ConnectorGet(ccomp, state=state, rc=rc)

      call ESMF_StateGet(state, "interDstFields", interDstFields, rc=rc)

      call ESMF_StateGet(state, "src2interDstRH", rh1, rc=rc)

      call ESMF_StateGet(state, "interDst2dstRH", rh2, rc=rc)

      call ESMF_FieldBundleRegridRelease(rh1, rc=rc)

      call ESMF_FieldBundleRegridRelease(rh2, rc=rc)

      end subroutine

      end module mod_esmf_cpl
