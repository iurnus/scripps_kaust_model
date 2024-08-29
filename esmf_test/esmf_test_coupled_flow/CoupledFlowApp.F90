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
! \subsection{Main program source file for demo CoupledFlowApp.F90}
!
! !DESCRIPTION:
! ESMF Application Wrapper for Coupled Flow Demo.  This file contains the
!  main program, and creates a top level ESMF Gridded Component to contain
!  all other Components.
!
!
!EOE

    program ESMF_CoupledFlow

    ! ESMF module, defines all ESMF data types and methods
    use ESMF
    
    ! Flow Component registration routines
    use CoupledFlowMod, only : CoupledFlow_register

    implicit none
    
    ! Component, and State
    type(ESMF_GridComp) :: coupledFlowComp  ! the coupled flow Component
    type(ESMF_State)    :: coupledFlowState ! the coupled flow State

    ! Clock, TimeInterval, and Times
    type(ESMF_Clock)        :: clock
    type(ESMF_TimeInterval) :: timeStep
    type(ESMF_Time)         :: startTime
    type(ESMF_Time)         :: stopTime

    ! Grid and related variables
    type(ESMF_Grid) :: grid
    integer :: i, j, imin_t, imax_t, jmin_t, jmax_t
    real(ESMF_KIND_R8) :: dx, dy
    real(ESMF_KIND_R8), pointer :: CoordX(:), CoordY(:)
#define WRITECOORD
#ifdef WRITECOORD
    type(ESMF_Array) :: coordXa, coordYa
#endif
    
    ! Namelist and related variables
    integer :: fileunit
    integer :: i_max, j_max
    real(ESMF_KIND_R8) :: x_min, x_max, y_min, y_max
    integer :: s_month, s_day, s_hour, s_min
    integer :: e_month, e_day, e_hour, e_min
    namelist /input/ i_max, j_max, x_min, x_max, y_min, y_max, &
                     s_month, s_day, s_hour, s_min, &
                     e_month, e_day, e_hour, e_min
!BOE
!
! !DESCRIPTION:
! \subsubsection{Namelist Input Parameters for CoupledFlowApp}
!     The following variables must be input to the CoupledFlow Application to
!     run.  They are located in a file called "coupled\_app\_input."
!
!     The variables are:
!     \begin{description}
!     \item [i\_max]
!           Global number of cells in the first grid direction.
!     \item [j\_max]
!           Global number of cells in the second grid direction.
!     \item [x\_min]
!           Minimum grid coordinate in the first direction.
!     \item [x\_max]
!           Maximum grid coordinate in the first direction.
!     \item [y\_min]
!           Minimum grid coordinate in the second direction.
!     \item [y\_max]
!           Maximum grid coordinate in the second direction.
!     \item [s\_month]
!           Simulation start time month (integer).
!     \item [s\_day]
!           Simulation start time day (integer).
!     \item [s\_hour]
!           Simulation start time hour (integer).
!     \item [s\_min]
!           Simulation start time minute (integer).
!     \item [e\_month]
!           Simulation end time month (integer).
!     \item [e\_day]
!           Simulation end time day (integer).
!     \item [e\_hour]
!           Simulation end time hour (integer).
!     \item [e\_min]
!           Simulation end time minute (integer).
!     \end{description}
!
!EOE

    ! Return codes for error checks
    integer :: rc, urc
        
!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!    ESMF_Initialize
!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!

!BOE
!
! !DESCRIPTION:
! \subsubsection{Example of Initializing the Framework}
!
!     The first call to ESMF must be the initialize method.   As part of
!     initialization the default Calendar can be specified and options
!     for logging can be set.
!     Here we are setting the default Calendar to be Gregorian, and 
!     request default logging into seperate files for each PET:
!EOE

!BOC
    ! Initialize ESMF, set the default calendar and log type.
    call ESMF_Initialize(defaultCalKind=ESMF_CALKIND_GREGORIAN, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)
!EOC 

!------------------------------------------------------------------------------
!------------------------------------------------------------------------------

    print *, "Coupled Flow Demo Application Start"

!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!   Read in configuration data - could be replaced by Config routines
!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!
    !
    ! Read in input file
    !
    call ESMF_UtilIOUnitGet(unit=fileunit, rc=rc) ! get an available Fortran unit number
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)
    open(fileunit, status="old", file="./data/coupled_app_input", &
      action="read", iostat=rc)
    if (rc .ne. 0) then
      call ESMF_LogSetError(rcToCheck=ESMF_RC_FILE_OPEN, &
        msg="Failed to open namelist file 'coupled_app_input'", &
        line=__LINE__, &
        file=__FILE__)
      call ESMF_Finalize(endflag=ESMF_END_ABORT)
    endif
    read(fileunit, input, end=20)
 20 continue
    close(fileunit)

!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!    Create section
!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!

!BOE
! Clreate the top level Gridded Component called "Coupled Flow Demo"
!EOE
!BOC
    ! Create the top level Gridded Component.
    coupledFlowComp = ESMF_GridCompCreate(name="Coupled Flow Demo", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)
!EOC 

    print *, "Comp Creates finished"


!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!  Register section
!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
    call ESMF_GridCompSetServices(coupledFlowComp, CoupledFlow_register, &
      userRc=urc, rc=rc)
    print *, "Coupled Flow Component SetServices finished, rc =", rc, urc
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)

!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!  Create and initialize a clock, and a grid.
!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!BOE
!
! !DESCRIPTION:
! \subsubsection{Example of Calendar and Clock Creation and Usage}
!
!     The following piece of code provides an example of Clock creation used in
!     the Demo.  Note that the Gregorian calendar was set as the default in
!     the ESMF\_Initialize() call above.  As shown in this example, we first
!     initialize a time interval (timestep) to 2 seconds:
!EOE

!BOC
    call ESMF_TimeIntervalSet(timeStep, s=2, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)
    !And then we set the start time and stop time to input values for the month,
    !day, and hour (assuming the year to be 2003):
    call ESMF_TimeSet(startTime, yy=2003, mm=s_month, dd=s_day, &
                      h=s_hour, m=s_min, s=0, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)

    call ESMF_TimeSet(stopTime, yy=2003, mm=e_month, dd=e_day, &
      h=e_hour, m=e_min, s=0, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)
    !With the time interval, start time, and stop time set above, the Clock can
    !now be created:
    clock = ESMF_ClockCreate(timeStep=timeStep, startTime=startTime, &
      stopTime=stopTime, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)
    !Subsequent calls to ESMF_ClockAdvance with this clock will increment the
    !current time from the start time by the timestep.
!EOC 

!
! Create the Grid and attach it to the Component:
!

!BOE
!
! !DESCRIPTION:
! \subsubsection{Example of Grid Creation}
!
!  The following piece of code provides an example of Grid creation used in
!  the Demo.  The extents of the Grid were previously read in from an input
!  file, but the rest of the Grid parameters are set here by default.  The
!  Grid spans the Application's PET list, while the type of the Grid is 
!  assumed to be horizontal and Cartesian x-y with an Arakawa C staggering.  
!  The Grid name is set to "demo grid":
!EOE

!BOC

    dx = (x_max-x_min)/i_max
    dy = (y_max-y_min)/j_max

    grid = ESMF_GridCreateNoPeriDim(maxIndex=(/i_max,j_max/), &
      coordDep1=(/1/), coordDep2=(/2/), &
      gridEdgeLWidth=(/0,0/), gridEdgeUWidth=(/0,0/), &
      indexflag=ESMF_INDEX_GLOBAL, name="demo grid", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)

    ! add center stagger for: sie, p, q, rho, flag
    call ESMF_GridAddCoord(grid, staggerLoc=ESMF_STAGGERLOC_CENTER, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)
    call ESMF_GridGetCoord(grid, localDE=0, &
      staggerLoc=ESMF_STAGGERLOC_CENTER, &
      coordDim=1, farrayPtr=CoordX, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)
    call ESMF_GridGetCoord(grid, localDE=0, &
      staggerLoc=ESMF_STAGGERLOC_CENTER, &
      coordDim=2, farrayPtr=CoordY, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)

    ! compute center stagger coordinate values
    imin_t = lbound(CoordX,1)
    imax_t = ubound(CoordX,1)
    jmin_t = lbound(CoordY,1)
    jmax_t = ubound(CoordY,1)
      
    coordX(imin_t) = (imin_t-1)*dx + 0.5*dx
    do i = imin_t+1, imax_t
      coordX(i) = coordX(i-1) + dx
    enddo
    coordY(jmin_t) = (jmin_t-1)*dy + 0.5*dy
    do j = jmin_t+1, jmax_t
      coordY(j) = coordY(j-1) + dy
    enddo
      
!EOC
      
#ifdef WRITECOORD
    ! write center stagger x and y coordinates to file
    call ESMF_GridGetCoord(grid, staggerLoc=ESMF_STAGGERLOC_CENTER, &
      coordDim=1, array=coordXa, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)
    call ESMF_ArrayWrite(coordXa, fileName="coordXa.nc", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)
    
    call ESMF_GridGetCoord(grid, staggerLoc=ESMF_STAGGERLOC_CENTER, &
      coordDim=2, array=coordYa, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)
    call ESMF_ArrayWrite(coordYa, fileName="coordYa.nc", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)

    call ESMF_GridWriteVTK(grid, filename="mesh", &
      staggerLoc=ESMF_STAGGERLOC_CENTER, rc=rc)

#endif
      
!BOC
    ! add east stagger for: u and rhou
    call ESMF_GridAddCoord(grid, staggerLoc=ESMF_STAGGERLOC_EDGE1, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)
    call ESMF_GridGetCoord(grid, localDE=0, &
      staggerLoc=ESMF_STAGGERLOC_EDGE1, &
      coordDim=1, farrayPtr=CoordX, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)
    call ESMF_GridGetCoord(grid, localDE=0, &
      staggerLoc=ESMF_STAGGERLOC_EDGE1, &
      coordDim=2, farrayPtr=CoordY, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)

    ! compute east stagger coordinate values
    imin_t = lbound(CoordX,1)
    imax_t = ubound(CoordX,1)
    jmin_t = lbound(CoordY,1)
    jmax_t = ubound(CoordY,1)
    
    coordX(imin_t) = (imin_t-1)*dx + dx
    do i = imin_t+1, imax_t
      coordX(i) = coordX(i-1) + dx
    enddo
    coordY(jmin_t) = (jmin_t-1)*dy + 0.5*dy
    do j = jmin_t+1, jmax_t
      coordY(j) = coordY(j-1) + dy
    enddo
    
    ! add north stagger for: v and rhov
    call ESMF_GridAddCoord(grid, staggerLoc=ESMF_STAGGERLOC_EDGE2, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)
    call ESMF_GridGetCoord(grid, localDE=0, &
      staggerLoc=ESMF_STAGGERLOC_EDGE2, &
      coordDim=1, farrayPtr=CoordX, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)
    call ESMF_GridGetCoord(grid, localDE=0, &
      staggerLoc=ESMF_STAGGERLOC_EDGE2, &
      coordDim=2, farrayPtr=CoordY, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)

    ! compute north stagger coordinate values
    imin_t = lbound(CoordX,1)
    imax_t = ubound(CoordX,1)
    jmin_t = lbound(CoordY,1)
    jmax_t = ubound(CoordY,1)
    
    coordX(imin_t) = (imin_t-1)*dx + 0.5*dx
    do i = imin_t+1, imax_t
      coordX(i) = coordX(i-1) + dx
    enddo
    coordY(jmin_t) = (jmin_t-1)*dy + dy
    do j = jmin_t+1, jmax_t
      coordY(j) = coordY(j-1) + dy
    enddo

    ! Set Grid in Gridded Component
    call ESMF_GridCompSet(coupledFlowComp, grid=grid, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)
!EOC

!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!BOE
!
! !DESCRIPTION:
! \subsubsection{Example of State Creation}
!
!  Create and initialize a dummy State to use for both import and export.
!EOE
!------------------------------------------------------------------------------
!------------------------------------------------------------------------------

!BOC
    coupledFlowState = ESMF_StateCreate(Name="Coupled Flow State", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)
!EOC
     
!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!BOE
!
! !DESCRIPTION:
! \subsubsection{Example of Initialize, Run, and Finalize}
!
! Init, Run, and Finalize sections of the Coupled Flow Component:
!EOE
!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
 
!BOC
    call ESMF_GridCompInitialize(coupledFlowComp, &
      importState=coupledFlowState, exportState=coupledFlowState, &
      clock=clock, userRc=urc, rc=rc)
    print *, "Coupled Flow Component Initialize finished, rc =", rc, urc
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)

    call ESMF_GridCompRun(coupledFlowComp, &
      importState=coupledFlowState, exportState=coupledFlowState, &
      clock=clock, userRc=urc, rc=rc)
    print *, "Coupled Flow Component Run finished, rc =", rc, urc
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)
 
    call ESMF_GridCompFinalize(coupledFlowComp, &
      importState=coupledFlowState, exportState=coupledFlowState, &
      clock=clock, userRc=urc, rc=rc)
    print *, "Coupled Flow Component Finalize finished, rc =", rc, urc
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)
!EOC
 
!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!BOE
!
! !DESCRIPTION:
! \subsubsection{Example of Object Destruction}
!
!     Near the end of the application, call object destroy methods to 
!     clean up the objects previously created:
!EOE
!------------------------------------------------------------------------------
!------------------------------------------------------------------------------

!BOC
    call ESMF_StateDestroy(coupledFlowState, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)

    call ESMF_GridCompDestroy(coupledFlowComp, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)

    call ESMF_GridDestroy(grid, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)

    call ESMF_ClockDestroy(clock, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)

!EOC

!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
    ! This output goes to stdout
    print *, "**********************************************************"
    print *, "SUCCESS!  Your ESMF Coupled Flow Application Demo ", &
             "ran to completion!"
    print *, "See the output files in the source directory for ", &
             "the generated data."
    print *, "**********************************************************"

!BOE
!
! !DESCRIPTION:
! \subsubsection{Example of ESMF Finalize}
!
!EOE
!BOC
    !Call ESMF_Finalize at the end of an ESMF application:
    call ESMF_Finalize()
!EOC

    end program ESMF_CoupledFlow
    
