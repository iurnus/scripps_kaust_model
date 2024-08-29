! $Id$
!
! Earth System Modeling Framework
! Copyright 2002-2017, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!------------------------------------------------------------------------------
!BOE
!
! \subsection{Top level Gridded Component source CoupledFlowDemo.F90}
!
! !DESCRIPTION:
! ESMF Coupled Flow Demo - A Gridded Component which can be called either 
!   directly from an Application Driver or nested in a larger application.
!   It contains 2 nested subcomponents and 1 Coupler Component which does 
!   two-way coupling between the subcomponents.
!
!EOE
!------------------------------------------------------------------------------
!
!

    module CoupledFlowMod

    ! ESMF module, defines all ESMF data types and procedures
    use ESMF
    
    ! User Component registration routines
    use   InjectorMod, only : Injector_register
    use FlowSolverMod, only : FlowSolver_register
    use    CouplerMod, only : Coupler_register

    implicit none
    
    private
    
    ! Subcomponents
    type(ESMF_GridComp), save :: INcomp, FScomp
    type(ESMF_CplComp), save :: cpl

    ! States
    type(ESMF_State), save :: INimp, INexp, FSimp, FSexp

    ! Public entry point
    public CoupledFlow_register

!------------------------------------------------------------------------------

    contains
        
!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: CoupledFlow_register - Externally visible registration routine

! !INTERFACE:
      subroutine CoupledFlow_register(comp, rc)
!
! !ARGUMENTS:
     type(ESMF_GridComp)  :: comp
     integer, intent(out) :: rc
!
! !DESCRIPTION:
!     User-supplied setservices routine.
!     The register routine sets the subroutines to be called
!     as the init, run, and finalize routines.  Note that these are
!     private to the module.
!
!     The arguments are:
!     \begin{description}
!     \item[comp] 
!          Component.
!     \item[{[rc]}] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors,
!          otherwise {\tt ESMF\_FAILURE}.
!     \end{description}
!
!EOPI

    rc = ESMF_SUCCESS ! initialize

!BOE
!  !DESCRIPTION:
! \subsubsection{Example of Set Services Usage}
!
!   The following code registers with ESMF 
!   the subroutines to be called to Init, Run, and Finalize this component.
!BOC
    ! Register the callback routines.

    call ESMF_GridCompSetEntryPoint(comp, ESMF_METHOD_INITIALIZE, &
      userRoutine=coupledflow_init, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
    call ESMF_GridCompSetEntryPoint(comp, ESMF_METHOD_RUN, &
      userRoutine=coupledflow_run, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
      
    call ESMF_GridCompSetEntryPoint(comp, ESMF_METHOD_FINALIZE, &
      userRoutine=coupledflow_final, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

!EOC
!EOE
     print *, "CoupledFlowDemo: Registered Initialize, Run, and Finalize routines"

    end subroutine

!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: coupledflow_init - initialization routine

! !INTERFACE:
      subroutine coupledflow_init(gcomp, importState, exportState, clock, rc)
!
! !ARGUMENTS:
     type(ESMF_GridComp)  :: gcomp
     type(ESMF_State)     :: importState, exportState
     type(ESMF_Clock)     :: clock
     integer, intent(out) :: rc
!
! !DESCRIPTION:
!     User-supplied init routine.
!
!     The arguments are:
!     \begin{description}
!     \item[comp] 
!          Component.
!     \item[importState]
!          Importstate.
!     \item[exportState]
!          Exportstate.
!     \item[clock] 
!          External clock.
!     \item[rc] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors,
!          otherwise {\tt ESMF\_FAILURE}.
!     \end{description}
!
!EOPI
    integer :: petCount, localrc, urc
    integer :: mid, by2, quart, by4

    type(ESMF_Grid) :: gridTop, gridIN, gridFS
    type(ESMF_DistGrid) :: distgridIN, distgridFS
#define WRITECOORD__disable
#ifdef WRITECOORD
    type(ESMF_Array) :: coordXa, coordYa
#endif

    rc = ESMF_SUCCESS ! initialize

    ! Get petCount from the component
    call ESMF_GridCompGet(gcomp, petCount=petCount, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__, &
      rcToReturn=rc)) return  ! bail out

    ! Sanity check the number of PETs we were started on.
    if (petCount .eq. 1) then
      mid = 1
      by2 = 1
      quart = 1
      by4 = 1
    else
      if ((petCount .lt. 4) .or. (petCount .gt. 16) &
        .or. (mod(petCount, 4) .ne. 0)) then
        print *, "This demo needs to run at least 4-way and no more "
        print *, "than 16-way, on a multiple of 4 processors."
        print *, "The requested number of processors was ", petCount
        call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_BAD, &
          msg="The petCount does not match the application requirements.", &
          line=__LINE__, &
          file=__FILE__, &
          rcToReturn=rc)
        return  ! bail out
      endif
      mid = petCount/2
      by2 = 2
      quart = petCount/4
      by4 = 4
    endif

    ! Create the 2 model components and coupler. 

!BOE
!
! \subsubsection{Example of Component Creation}
!
!   The following code creates 2 Gridded Components on the same set of PETs 
!   (persistent execution threads) as the top level Component, but each 
!   of the Grids useds by these Components will have a different connectivity.
!   It also creates a Coupler Component on the same PET set. Each gridded
!   component has a Grid attached internally.
!
!BOC
    INcomp = ESMF_GridCompCreate(name="Injector model", rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__, &
      rcToReturn=rc)) return  ! bail out
      
    FScomp = ESMF_GridCompCreate(name="Flow Solver model", rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__, &
      rcToReturn=rc)) return  ! bail out

    cpl = ESMF_CplCompCreate(name="Two-way coupler", rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__, &
      rcToReturn=rc)) return  ! bail out
!EOC
!EOE

    print *, "Comp Creates finished"

!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!  Register section for subcomponents
!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
    call ESMF_GridCompSetServices(INcomp, Injector_register, userRc=urc, &
      rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__, &
      rcToReturn=rc)) return  ! bail out

    call ESMF_GridCompSetServices(FScomp, FlowSolver_register, userRc=urc, &
      rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__, &
      rcToReturn=rc)) return  ! bail out

    call ESMF_CplCompSetServices(cpl, Coupler_register, userRc=urc, &
      rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__, &
      rcToReturn=rc)) return  ! bail out

!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!  Init section for subcomponents.  Create subgrids on separate DELayouts,
!    and create import/export states for subcomponents.
!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
 
    ! Get grid from the component
    call ESMF_GridCompGet(gcomp, grid=gridTop, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__, &
      rcToReturn=rc)) return  ! bail out

    ! 
    ! Create and attach subgrids to the subcomponents.
    !

    ! Injector Flow Grid
!BOE
! Create the Injector Grid to match the "demo grid" from the top, but decompose
! differently:
!BOC

    gridIN = ESMF_GridCreate(grid=gridTop, name="Injector grid", &
      regDecomp=(/ mid, by2 /), rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__, &
      rcToReturn=rc)) return  ! bail out

!EOC
!EOE

#ifdef WRITECOORD
    ! write center stagger x and y coordinates to file
    call ESMF_GridGetCoord(gridIN, staggerLoc=ESMF_STAGGERLOC_CENTER, &
      coordDim=1, array=coordXa, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__, &
      rcToReturn=rc)) return  ! bail out
    call ESMF_ArrayWrite(coordXa, file="coordXa-Injector.nc", rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__, &
      rcToReturn=rc)) return  ! bail out
    
    call ESMF_GridGetCoord(gridIN, staggerLoc=ESMF_STAGGERLOC_CENTER, &
      coordDim=2, array=coordYa, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__, &
      rcToReturn=rc)) return  ! bail out
    call ESMF_ArrayWrite(coordYa, file="coordYa-Injector.nc", rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__, &
      rcToReturn=rc)) return  ! bail out
#endif

!BOE
! Set the Injector Grid in the Injector Component
!BOC
    call ESMF_GridCompSet(INcomp, grid=gridIN, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__, &
      rcToReturn=rc)) return  ! bail out
!EOC
!EOE

    ! FlowSolver Grid
!BOE
! Create the FlowSolver Grid to match the "demo grid" from the top, but decompose
! differently
!BOC

    gridFS = ESMF_GridCreate(grid=gridTop, name="Flow Solver grid", &
      regDecomp=(/ quart, by4 /), rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__, &
      rcToReturn=rc)) return  ! bail out

!EOC
!EOE

#ifdef WRITECOORD
    ! write center stagger x and y coordinates to file
    call ESMF_GridGetCoord(gridFS, staggerLoc=ESMF_STAGGERLOC_CENTER, &
      coordDim=1, array=coordXa, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__, &
      rcToReturn=rc)) return  ! bail out
    call ESMF_ArrayWrite(coordXa, file="coordXa-FlowSolver.nc", rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__, &
      rcToReturn=rc)) return  ! bail out
    
    call ESMF_GridGetCoord(gridFS, staggerLoc=ESMF_STAGGERLOC_CENTER, &
      coordDim=2, array=coordYa, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__, &
      rcToReturn=rc)) return  ! bail out
    call ESMF_ArrayWrite(coordYa, file="coordYa-FlowSolver.nc", rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__, &
      rcToReturn=rc)) return  ! bail out
#endif

!BOE
! Set the FlowSolver Grid in the FlowSolver Component
!BOC
    call ESMF_GridCompSet(FScomp, grid=gridFS, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__, &
      rcToReturn=rc)) return  ! bail out
!EOC
!EOE

    !
    ! Create import/export states for Injection Component
    !
!BOE
!
! \subsubsection{Example of State Creation}
!
!   The following code creates Import and Export States for the
!   Injection subcomponent.  All information being passed between
!   subcomponents will be described by these States.
!
!BOC
    INimp = ESMF_StateCreate(name="Injection Input", stateintent=ESMF_STATEINTENT_IMPORT, &
      rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__, &
      rcToReturn=rc)) return  ! bail out
    INexp = ESMF_StateCreate(name="Injection Feedback", stateintent=ESMF_STATEINTENT_EXPORT, &
      rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__, &
      rcToReturn=rc)) return  ! bail out
!EOC
!EOE
    ! 
    ! Initialize the injector component, first phase 
    !
    call ESMF_GridCompInitialize(INcomp, importState=INimp, exportState=INexp, &
      clock=clock, phase=1, rc=rc, userRc=urc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
    if(urc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=urc)
    print *, "Injection Model Initialize finished, rc =", rc
 
    !
    ! Create import/export states for FlowSolver Component
    !
    FSimp = ESMF_StateCreate(name="FlowSolver Input", stateintent=ESMF_STATEINTENT_IMPORT, &
        rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
    FSexp = ESMF_StateCreate(name="FlowSolver Feedback ", stateintent=ESMF_STATEINTENT_EXPORT, &
        rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

    !
    ! Initialize the flow solver component, first phase
    !
    call ESMF_GridCompInitialize(FScomp, importState=FSimp, exportState=FSexp, &
      clock=clock, phase=1, rc=rc, userRc=urc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
    if(urc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=urc)
    print *, "Flow Model Initialize finished, rc =", rc

    !
    ! Initialize the coupler, once for each export state
    ! (note this is not 2 phases - it is calling the same code each time.)
    !
    call ESMF_CplCompInitialize(cpl, importState=FSexp, exportState=INimp, &
      clock=clock, rc=rc, userRc=urc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
    if(urc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=urc)
    call ESMF_CplCompInitialize(cpl, importState=INexp, exportState=FSimp, &
      clock=clock, rc=rc, userRc=urc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
    if(urc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=urc)
    print *, "Coupler Initialize finished, rc =", rc
 
    !
    ! Second phase of init
    !
    call ESMF_GridCompInitialize(INcomp, importState=INimp, exportState=INexp, &
      clock=clock, phase=2, rc=rc, userRc=urc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
    if(urc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=urc)
    print *, "Injection Model Initialize finished, rc =", rc
 
    call ESMF_GridCompInitialize(FScomp, importState=FSimp, exportState=FSexp, &
      clock=clock, phase=2, rc=rc, userRc=urc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
    if(urc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=urc)
    print *, "Flow Model Initialize finished, rc =", rc

    end subroutine coupledflow_init

!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: coupledflow_run - run routine

! !INTERFACE:
      subroutine coupledflow_run(comp, importState, exportState, clock, rc)
!
! !ARGUMENTS:
     type(ESMF_GridComp)  :: comp
     type(ESMF_State)     :: importState, exportState
     type(ESMF_Clock)     :: clock
     integer, intent(out) :: rc
!
! !DESCRIPTION:
!     User-supplied run routine.
!
!     The arguments are:
!     \begin{description}
!     \item[comp] 
!          Component.
!     \item[importState]
!          Importstate.
!     \item[exportState]
!          Exportstate.
!     \item[clock] 
!          External clock.
!     \item[{[rc]}] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors,
!          otherwise {\tt ESMF\_FAILURE}.
!     \end{description}
!
!EOPI

     ! Local variables
     type(ESMF_Clock) :: localclock
     integer          :: urc


!BOE
!
! !DESCRIPTION:
! \subsubsection{Example of Time Stepping Loop}
!
! Advancing in time with ESMF clock, the coupled flow component calls
! the run methods of the gridded components and coupler component sequentially:
!BOC
     ! Make our own local copy of the clock
     localclock = ESMF_ClockCreate(clock, rc=rc)
     if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

     print *, "Run Loop Start time"
     call ESMF_ClockPrint(localclock, options="currtime string", rc=rc)
     if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

     do while (.not. ESMF_ClockIsStopTime(localclock, rc=rc))

        ! Run FlowSolver Component
        call ESMF_GridCompRun(FScomp, importState=FSimp, exportState=FSexp, &
          clock=localclock, rc=rc, userRc=urc)
        if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
        if(urc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=urc)

        ! Couple export state of FlowSolver to import of Injector
        call ESMF_CplCompRun(cpl, importState=FSexp, exportState=INimp, &
          clock=localclock, rc=rc, userRc=urc)
        if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
        if(urc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=urc)
  
        ! Run Injector Component
        call ESMF_GridCompRun(INcomp, importState=INimp, exportState=INexp, &
          clock=localclock, rc=rc, userRc=urc)
        if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
        if(urc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=urc)
  
        ! Couple export state of Injector to import of FlowSolver
        call ESMF_CplCompRun(cpl, importState=INexp, exportState=FSimp, &
          clock=localclock, rc=rc, userRc=urc)
        if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
        if(urc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=urc)
  
        ! Advance the time
        call ESMF_ClockAdvance(localclock, rc=rc)
        if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
      
        ! This demo runs a lot of time steps and only outputs files
        ! every N iterations.  This print statement, if commented in,
        ! generates a lot of output.
        !call ESMF_ClockPrint(localclock, "currtime string", rc)

     enddo

     print *, "Run Loop End time"
     call ESMF_ClockPrint(localclock, options="currtime string", rc=rc)
     if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
!EOC
!EOE
 
!BOE
!
! !DESCRIPTION:
! \subsubsection{Example of Clock Destruction}
!
! At the end of run method, destroy the clock used to iterate through time:
!BOC
     call ESMF_ClockDestroy(localclock, rc=rc)
     if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
!EOC
!EOE

end subroutine coupledflow_run


!------------------------------------------------------------------------------
!BOPI
! !IROUTINE:  coupledflow_final - user supplied finalize routine

! !INTERFACE:
      subroutine coupledflow_final(comp, importState, exportState, clock, rc)
!
! !ARGUMENTS:
     type(ESMF_GridComp)  :: comp
     type(ESMF_State)     :: importState, exportState
     type(ESMF_Clock)     :: clock
     integer, intent(out) :: rc
!
! !DESCRIPTION:
!     User-supplied finalize routine.
!
!     The arguments are:
!     \begin{description}
!     \item[comp] 
!          Component.
!     \item[importState]
!          Importstate.
!     \item[exportState]
!          Exportstate.
!     \item[clock] 
!          External clock.
!     \item[{[rc]}] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors,
!          otherwise {\tt ESMF\_FAILURE}.
!     \end{description}
!
!EOPI

      integer :: urc

      ! First finalize all subcomponents

      ! Finalize Injector Component    
      call ESMF_GridCompFinalize(INcomp, importState=INimp, exportState=INexp, &
        clock=clock, rc=rc, userRc=urc)
      if (rc .ne. ESMF_SUCCESS .or. urc .ne. ESMF_SUCCESS) then
          print *, "Injector Component Finalize routine returned error"
          return
      endif

      ! Finalize FlowSolver Component
      call ESMF_GridCompFinalize(FScomp, importState=FSimp, exportState=FSimp, &
        clock=clock, rc=rc, userRc=urc)
      if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
      if(urc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=urc)

      ! Finalize Coupler
      call ESMF_CplCompFinalize(cpl, importState=INexp, exportState=FSimp, &
        clock=clock, rc=rc, userRc=urc)
      if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
      if(urc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=urc)

      print *, "CoupledFlowMod finished calling all subcomponent Finalize routines"

      ! Then clean them up

      print *, "ready to destroy all states"
      call ESMF_StateDestroy(INimp, rc=rc)
      if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
      call ESMF_StateDestroy(INexp, rc=rc)
      if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
      call ESMF_StateDestroy(FSimp, rc=rc)
      if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
      call ESMF_StateDestroy(FSexp, rc=rc)
      if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

      print *, "ready to destroy all components"
      call ESMF_GridCompDestroy(INcomp, rc=rc)
      if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
      call ESMF_GridCompDestroy(FScomp, rc=rc)
      if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)
      call ESMF_CplCompDestroy(cpl, rc=rc)
      if(rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=rc)

      print *, "end of CoupledFlowMod Finalization routine"
      rc = ESMF_SUCCESS

    end subroutine coupledflow_final


    end module CoupledFlowMod
    
    
