! Earth System Modeling Framework
! Copyright 2002-2017, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
#define FILENAME "mod_esmf_atm.F90"
!
!-----------------------------------------------------------------------
!     ATM gridded component code 
!-----------------------------------------------------------------------
!
module mod_esmf_atm
!
!-----------------------------------------------------------------------
!     Used module declarations 
!-----------------------------------------------------------------------
!
  use ESMF
  use NUOPC
  use NUOPC_Model,                                                  &
      NUOPC_SetServices          => SetServices,                    &
      NUOPC_Label_SetClock       => label_SetClock,                 &
      NUOPC_Label_Advance        => label_Advance,                  &
      NUOPC_Label_DataInitialize => label_DataInitialize
!
  use mod_types
!
  implicit none
  private
!
!-----------------------------------------------------------------------
!     Public subroutines 
!-----------------------------------------------------------------------
!
  public :: ATM_SetServices
!
  contains
!
  subroutine ATM_SetServices(gcomp, rc)
!
!-----------------------------------------------------------------------
!     Imported variable declarations 
!-----------------------------------------------------------------------
!
  type(ESMF_GridComp) :: gcomp
  integer, intent(inout) :: rc
!
  rc = ESMF_SUCCESS

  call NUOPC_CompDerive(gcomp, NUOPC_SetServices, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
      line=__LINE__, file=FILENAME)) return

  ! set entry point for methods that require specific implementation
  call NUOPC_CompSetEntryPoint(gcomp, ESMF_METHOD_INITIALIZE,       &
                               phaseLabelList=(/"IPDv00p1"/),       &
                               userRoutine=ATM_Init1, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
      line=__LINE__, file=FILENAME)) return

  call NUOPC_CompSetEntryPoint(gcomp, ESMF_METHOD_INITIALIZE,       &
                               phaseLabelList=(/"IPDv00p2"/),       &
                               userRoutine=ATM_Init2, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
      line=__LINE__, file=FILENAME)) return

  call NUOPC_CompSpecialize(gcomp, specLabel=NUOPC_Label_SetClock,  &
                            specRoutine=ATM_SetClock, rc = rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
      line=__LINE__, file=FILENAME)) return

  call NUOPC_CompSpecialize(gcomp, specLabel=NUOPC_Label_Advance,   &
                            specRoutine=ATM_Run, rc = rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
      line=__LINE__, file=FILENAME)) return

  end subroutine ATM_SetServices
!
!-----------------------------------------------------------------------
!     Initialization phase 1, set import/export fields
!-----------------------------------------------------------------------
!
  subroutine ATM_Init1(gcomp, importState, exportState, clock, rc)

  TYPE(ESMF_GridComp), TARGET, INTENT(INOUT) :: gcomp
  TYPE(ESMF_State),    TARGET, INTENT(INOUT) :: importState
  TYPE(ESMF_State),    TARGET, INTENT(INOUT) :: exportState
  TYPE(ESMF_Clock),    TARGET, INTENT(INOUT) :: clock

  TYPE(ESMF_VM) :: vm
  INTEGER :: mpicomtmp
  INTEGER,                     INTENT(  OUT) :: rc

  ! Local variables
  TYPE(ESMF_GridComp), POINTER :: p_gcomp
  TYPE(ESMF_State),    POINTER :: p_importState
  TYPE(ESMF_State),    POINTER :: p_exportState
  TYPE(ESMF_Clock),    POINTER :: p_clock
  ! Time hackery
  TYPE(ESMF_Time) :: startTime
  TYPE(ESMF_Time) :: stopTime
  TYPE(ESMF_TimeInterval) :: couplingInterval
  ! decomposition hackery
  INTEGER :: ids, ide, jds, jde, kds, kde
  INTEGER :: ims, ime, jms, jme, kms, kme
  INTEGER :: ips, ipe, jps, jpe, kps, kpe
  INTEGER :: domdesc
  LOGICAL :: bdy_mask(4)
  CHARACTER(LEN=256) :: couplingIntervalString
  
  rc = ESMF_SUCCESS

  call NUOPC_Advertise(importState,                                 &
      StandardName="sea_surface_temperature", name="sst", rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
      line=__LINE__, file=FILENAME)) return

  call NUOPC_Advertise(exportState,                                 &
      StandardName="air_pressure_at_sea_level", name="pmsl", rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
      line=__LINE__, file=FILENAME)) return

  call NUOPC_Advertise(exportState,                                 &
      StandardName="surface_net_downward_shortwave_flux",           &
      name="rsns", rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
      line=__LINE__, file=FILENAME)) return

  end subroutine
!
!-----------------------------------------------------------------------
!     Initialization phase 2 
!-----------------------------------------------------------------------
!
  subroutine ATM_Init2(gcomp, importState, exportState, clock, rc)

  type(ESMF_GridComp)  :: gcomp
  type(ESMF_State)     :: importState, exportState
  type(ESMF_Clock)     :: clock
  integer, intent(out) :: rc
!
!-----------------------------------------------------------------------
!     Local variable declaration
!-----------------------------------------------------------------------
!
  type(ESMF_Field) :: field
  type(ESMF_DistGrid) :: distGrid
  type(ESMF_Grid) :: atmGridIn
  type(ESMF_Grid) :: atmGridOut

  integer :: myThid = 1
  integer :: comm, localPet, petCount
  character(ESMF_MAXSTR) :: gname
!
  type(ESMF_VM) :: vm
  
  rc = ESMF_SUCCESS

  call ESMF_GridCompGet(gcomp, name=gname, vm=vm, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
         line=__LINE__, file=FILENAME)) return
  call ESMF_VMGet(vm, localPet=localPet, petCount=petCount,         &
                  mpiCommunicator=comm, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
              line=__LINE__, file=FILENAME)) return

  call ATM_SetGridArrays(gcomp, petCount, localPet, atmGridIn,rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
      line=__LINE__, file=FILENAME)) return
  atmGridOut = atmGridIn

  field = ESMF_FieldCreate(name="sst", grid=atmGridIn,              &
    typekind=ESMF_TYPEKIND_R8, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
      line=__LINE__, file=FILENAME)) return
  call NUOPC_Realize(importState, field=field, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
      line=__LINE__, file=FILENAME)) return

  field = ESMF_FieldCreate(name="pmsl", grid=atmGridOut,            &
    typekind=ESMF_TYPEKIND_R8, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
      line=__LINE__, file=FILENAME)) return
  call NUOPC_Realize(exportState, field=field, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
      line=__LINE__, file=FILENAME)) return

  field = ESMF_FieldCreate(name="rsns", grid=atmGridOut,            &
    typekind=ESMF_TYPEKIND_R8, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
      line=__LINE__, file=FILENAME)) return
  call NUOPC_Realize(exportState, field=field, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
      line=__LINE__, file=FILENAME)) return

  !! call ATM_SetInitData(gcomp, atmGridIn, atmGridOut, rc)
  !! if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
  !!     line=__LINE__, file=FILENAME)) return

  end subroutine
!
!-----------------------------------------------------------------------
!     Atmosphere Check Import Fields
!-----------------------------------------------------------------------
!
  subroutine ATM_CheckImport(gcomp, rc)

  type(ESMF_GridComp)  :: gcomp
  integer, intent(out) :: rc

  type(ESMF_Clock)     :: modelClock, driverClock
  type(ESMF_TimeInterval) :: atmTimeStep
  type(ESMF_Time) :: atmStartTime
  type(ESMF_Time) :: atmEndTime
  
  rc = ESMF_SUCCESS

  end subroutine
!
!-----------------------------------------------------------------------
!     Atmosphere Set Clock  
!-----------------------------------------------------------------------
!
  subroutine ATM_SetClock(gcomp, rc)

  type(ESMF_GridComp)  :: gcomp
  integer, intent(out) :: rc

  type(ESMF_Clock)     :: clock
  type(ESMF_Clock)     :: modelClock, driverClock
  type(ESMF_TimeInterval) :: atmTimeStep
  type(ESMF_Time) :: atmStartTime
  type(ESMF_Time) :: atmEndTime
  type(ESMF_TimeInterval) :: stabilityTimeStep
  
  rc = ESMF_SUCCESS

  call NUOPC_ModelGet(gcomp, modelClock=clock, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
      line=__LINE__, file=FILENAME)) return

  call ESMF_TimeIntervalSet(stabilityTimeStep, h=1, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
      line=__LINE__, file=FILENAME)) return

  call NUOPC_CompSetClock(gcomp, clock, stabilityTimeStep, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
      line=__LINE__, file=FILENAME)) return

  end subroutine
!
!-----------------------------------------------------------------------
!     Run
!-----------------------------------------------------------------------
!
  subroutine ATM_Run(gcomp, rc)

  TYPE(ESMF_GridComp), TARGET, INTENT(INOUT) :: gcomp
  INTEGER,                     INTENT(  OUT) :: rc

  type(ESMF_State),    TARGET:: importState
  type(ESMF_State),    TARGET:: exportState
  type(ESMF_Clock),    TARGET:: clock

  TYPE(ESMF_GridComp), POINTER :: p_gcomp
  TYPE(ESMF_State),    POINTER :: p_importState
  TYPE(ESMF_State),    POINTER :: p_exportState
  TYPE(ESMF_Clock),    POINTER :: p_clock

  ! Local variables
  TYPE(ESMF_Time) :: currentTime, nextTime
  TYPE(ESMF_TimeInterval) :: runLength     ! how long to run in this call
  CHARACTER(LEN=256) :: timeStr
  TYPE(ESMF_StateIntent_Flag) :: stateintent
  INTEGER :: itemCount
  INTEGER :: iLoopATM = 1
  
  rc = ESMF_SUCCESS

  print *, "ATM iLoop is: ", iLoopATM

  call ATM_Get(gcomp, iLoopATM, rc)

  iLoopATM = iLoopATM + 1

  end subroutine

  subroutine ATM_SetGridArrays(gcomp, petCount, localPet, gridIn,rc)
!
!-----------------------------------------------------------------------
!     Used module declarations 
!-----------------------------------------------------------------------
!
  implicit none
!
!-----------------------------------------------------------------------
!     Imported variable declarations 
!-----------------------------------------------------------------------
!
  type(ESMF_GridComp), intent(inout) :: gcomp
  integer, intent(in) :: localPet 
  integer, intent(in) :: petCount 
  type(ESMF_Grid) :: gridIn
  integer, intent(inout) :: rc
  type(ESMF_VM) :: vm
  character(ESMF_MAXSTR) :: cname

  integer :: myThid = 1
  integer :: k, m, n, p, iG, jG
  character(ESMF_MAXSTR) :: name
  real(ESMF_KIND_R8), pointer :: ptrX(:,:), ptrY(:,:)
  type(ESMF_Array) :: arrX, arrY
  type(ESMF_StaggerLoc) :: staggerLoc
  type(ESMF_DistGrid) :: distGrid
  integer, allocatable :: meshType(:)
  character(ESMF_MAXSTR), allocatable :: meshTypeName(:)
  integer, allocatable :: deBlockList(:,:,:)
  integer :: localDECount, j
  character(ESMF_MAXSTR) ::  ofile

  integer :: nx = 10
  integer :: ny = 10
  integer :: localN
!
!-----------------------------------------------------------------------
!     Local variable declarations 
!-----------------------------------------------------------------------
!
  rc = ESMF_SUCCESS
!
!-----------------------------------------------------------------------
!     Get gridded component 
!-----------------------------------------------------------------------
!
  call ESMF_GridCompGet(gcomp, vm=vm, name=cname, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
      line=__LINE__, file=FILENAME)) return

  !! TODO:: make the deblocklist more general
  if (.not.allocated(deBlockList)) then
    allocate(deBlockList(2,2,0:0))
  end if

  deBlockList(:,1,0) = (/ 1, 1/)
  deBlockList(:,2,0) = (/nx, ny/)

  !! if (.not.allocated(deBlockList)) then
  !!   allocate(deBlockList(2,2,0:1))
  !! end if

  !! deBlockList(:,1,0) = (/ 1, 1/)
  !! deBlockList(:,2,0) = (/ 5, ny/)
  !! deBlockList(:,1,1) = (/ 6, 1/)
  !! deBlockList(:,2,1) = (/nx, ny/)
!
!-----------------------------------------------------------------------
!     Create ESMF DistGrid based on model domain decomposition
!-----------------------------------------------------------------------
!
  distGrid = ESMF_DistGridCreate(minIndex=(/ 1, 1 /),               &
                                 maxIndex=(/ nx, ny /),             &
                                 deBlockList=deBlockList,           &
                                 rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                         line=__LINE__, file=FILENAME)) return
!
!-----------------------------------------------------------------------
!     Set staggering type 
!-----------------------------------------------------------------------
!
!
  staggerLoc = ESMF_STAGGERLOC_CENTER ! Icross
!
!-----------------------------------------------------------------------
!     Create ESMF Grid
!-----------------------------------------------------------------------
!
  ! Icross
  gridIn = ESMF_GridCreate(distgrid=distGrid,                       &
                           indexflag=ESMF_INDEX_GLOBAL,             &
                           name="atm_grid",                         &
                           rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                         line=__LINE__, file=FILENAME)) return
!
!-----------------------------------------------------------------------
!     Allocate coordinates 
!-----------------------------------------------------------------------
!
  call ESMF_GridAddCoord(gridIn, staggerLoc=staggerLoc, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                         line=__LINE__, file=FILENAME)) return
!
!-----------------------------------------------------------------------
!     Allocate items for masking (not for cell corners)
!-----------------------------------------------------------------------
!
  call ESMF_GridAddItem(gridIn, staggerLoc=staggerLoc,              &
                        itemflag=ESMF_GRIDITEM_MASK,                &
                        rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                         line=__LINE__, file=FILENAME)) return
!
!-----------------------------------------------------------------------
!     Allocate items for grid area (only for cell center)
!-----------------------------------------------------------------------
!
  call ESMF_GridAddItem(gridIn, staggerLoc=staggerLoc,              &
                        itemflag=ESMF_GRIDITEM_AREA,                &
                        rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                         line=__LINE__, file=FILENAME)) return
!
!-----------------------------------------------------------------------
!     Get pointers and set coordinates for the grid 
!-----------------------------------------------------------------------
! 
  call ESMF_GridGet(gridIn, localDECount=localDECount, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                         line=__LINE__, file=FILENAME)) return
  print *, "localDECount is: ", localDECount, " localPet is: ", localPet

  do j = 0, localDECount-1

    print *, "j is: ", j
    call ESMF_GridGetCoord(gridIn, staggerLoc=staggerLoc, localDE=j,&
                         coordDim=1, farrayPtr=ptrX, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,  &
                         line=__LINE__, file=FILENAME)) return
!
    call ESMF_GridGetCoord(gridIn, staggerLoc=staggerLoc, localDE=j,&
                         coordDim=2, farrayPtr=ptrY, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,  &
                         line=__LINE__, file=FILENAME)) return
!
!-----------------------------------------------------------------------
!     Fill the pointers    
!-----------------------------------------------------------------------
!
    do n = 1, nx/2
      do m = 1, ny
        !! TODO: pseudo mesh
        localN = localPet*5 + n
        print *, "loop n is: ", n, " m is: ", m
        print *, "atmMesh n is: ", n, " m is: ", m
        ptrX(localN,m) = 228 + (20.0d0/nx)*(localN-0.5d0)
        ptrY(localN,m) = 26 + (16.0d0/ny)*(m-0.5d0)
        !! print *, "atmosphere meshX is: ", ptrX(localN,m)
        !! print *, "atmosphere meshY is: ", ptrY(localN,m)
      end do
    end do
    !! do n = 1, nx
    !!   do m = 1, ny
    !!     !! TODO: pseudo mesh
    !!     localN = n
    !!     print *, "loop n is: ", n, " m is: ", m
    !!     print *, "atmMesh n is: ", n, " m is: ", m
    !!     ptrX(localN,m) = 228 + (20.0d0/nx)*(localN-0.5d0)
    !!     ptrY(localN,m) = 26 + (16.0d0/ny)*(m-0.5d0)
    !!     print *, "atmosphere meshX is: ", ptrX(localN,m)
    !!     print *, "atmosphere meshY is: ", ptrY(localN,m)
    !!   end do
    !! end do
    print *, "atmosphere array filled!"
!
!-----------------------------------------------------------------------
!     Nullify pointers 
!-----------------------------------------------------------------------
!
    if (associated(ptrY)) then
      nullify(ptrY)
    end if
    if (associated(ptrX)) then
      nullify(ptrX)
    end if
    print *, "ptr nullified!"
!
!-----------------------------------------------------------------------
!     Assign grid to gridded component 
!-----------------------------------------------------------------------
!
    if (debugLevel >= 1) then

      call ESMF_GridCompSet(gcomp, grid=gridIn, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,&
                             line=__LINE__, file=FILENAME)) return
      call ESMF_GridGetCoord(gridIn,                                &
                             staggerLoc=ESMF_STAGGERLOC_CENTER,     &
                             coordDim=1, array=arrX, rc=rc)
      call ESMF_GridGetCoord(gridIn,                                &
                             staggerLoc=ESMF_STAGGERLOC_CENTER,     &
                             coordDim=2, array=arrY, rc=rc)
      call ESMF_ArrayWrite(arrX, filename='atm_xa.nc',              &
                           status=ESMF_FILESTATUS_NEW, rc=rc)
      call ESMF_ArrayWrite(arrY, filename='atm_ya.nc',              &
                           status=ESMF_FILESTATUS_NEW, rc=rc)
    end if
  end do
  print *, "debug finished!"
!
  end subroutine ATM_SetGridArrays
!
!-----------------------------------------------------------------------
!     Set the initial data value
!-----------------------------------------------------------------------
!
!!       subroutine ATM_SetInitData(gcomp, gridIn, gridOut, rc)
!! !
!! !-----------------------------------------------------------------------
!! !     Used module declarations 
!! !-----------------------------------------------------------------------
!! !
!!       implicit none
!! !
!! !-----------------------------------------------------------------------
!! !     Imported variable declarations 
!! !-----------------------------------------------------------------------
!! !
!!       type(ESMF_GridComp), intent(inout) :: gcomp
!!       type(ESMF_Grid) :: gridIn
!!       type(ESMF_Grid) :: gridOut
!!       integer, intent(inout) :: rc
!!       type(ESMF_VM) :: vm
!! 
!!       type(ESMF_State) :: exportState
!!       type(ESMF_State) :: importState
!!       integer :: myThid = 1
!!       integer :: k, m, n, p, iG, jG
!!       character(ESMF_MAXSTR) :: name
!!       real(ESMF_KIND_R8), pointer :: ptrX(:,:), ptrY(:,:)
!!       real(ESMF_KIND_R8), pointer :: ptrIn(:,:)
!!       real(ESMF_KIND_R8), pointer :: ptrOut(:,:)
!!       real(ESMF_KIND_R8), pointer :: ptrOut2(:,:)
!!       type(ESMF_Array) :: arrX, arrY
!!       type(ESMF_StaggerLoc) :: staggerLoc
!!       type(ESMF_DistGrid) :: distGrid
!!       integer, allocatable :: meshType(:)
!!       character(ESMF_MAXSTR), allocatable :: meshTypeName(:)
!!       type(ESMF_Field) :: fieldIn
!!       type(ESMF_Field) :: fieldOut
!!       type(ESMF_Field) :: fieldOut2
!! 
!!       integer :: ii, jj
!!       integer :: nx = 10
!!       integer :: ny = 10
!!       character(ESMF_MAXSTR) :: cname, ofile
!! !
!! !-----------------------------------------------------------------------
!! !     Local variable declarations 
!! !-----------------------------------------------------------------------
!! !
!!       rc = ESMF_SUCCESS
!! !
!! !-----------------------------------------------------------------------
!! !     Get gridded component 
!! !-----------------------------------------------------------------------
!! !
!!       call ESMF_GridCompGet(gcomp, vm=vm, name=cname,                   &
!!                             exportState=exportState,                    &
!!                             importState=importState,                    &
!!                             rc=rc)
!!       if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
!!           line=__LINE__, file=FILENAME)) return
!! 
!!       call ESMF_GridGet(gridIn, rc=rc)
!!       if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
!!           line=__LINE__, file=FILENAME)) return
!! 
!!       call ESMF_StateGet(importState, "sst", fieldIn, rc=rc)
!!       if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
!!           line=__LINE__, file=FILENAME)) return
!! 
!!       call ESMF_FieldGet(fieldIn, farrayPtr=ptrIn, rc=rc)
!!       if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
!!           line=__LINE__, file=FILENAME)) return
!! 
!!       do ii = 1, Nx
!!         do jj = 1, Ny
!!           ptrIn(ii,jj) = 0.001d0*ii + 1.0d0*ii
!!         end do
!!       end do
!! 
!!       call ESMF_StateGet(exportState, "pmsl", fieldOut, rc=rc)
!!       if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
!!           line=__LINE__, file=FILENAME)) return
!! 
!!       call ESMF_FieldGet(fieldOut, farrayPtr=ptrOut, rc=rc)
!!       if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
!!           line=__LINE__, file=FILENAME)) return
!! 
!!       do ii = 1, Nx
!!         do jj = 1, Ny
!!           ptrOut(ii,jj) = 0.001d0*ii + 1.0d0*ii
!!         end do
!!       end do
!! 
!!       call ESMF_StateGet(exportState, "rsns", fieldOut2, rc=rc)
!!       if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
!!           line=__LINE__, file=FILENAME)) return
!! 
!!       call ESMF_FieldGet(fieldOut2, farrayPtr=ptrOut2, rc=rc)
!!       if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
!!           line=__LINE__, file=FILENAME)) return
!! 
!!       do ii = 1, Nx
!!         do jj = 1, Ny
!!           ptrOut2(ii,jj) = 0.00001d0*ii + 100.0d0*ii
!!         end do
!!       end do
!! 
!!       if (debugLevel >= 1) then
!!         write (ofile, "(A9)") "rsnsATM.nc"
!!         call ESMF_FieldWrite(fieldOut2, trim(ofile), rc=rc)
!!       end if
!! 
!!       end subroutine ATM_SetInitData

  subroutine ATM_Get(gcomp, iLoop, rc)
!
!-----------------------------------------------------------------------
!     Used module declarations 
!-----------------------------------------------------------------------
!
  implicit none
!
!-----------------------------------------------------------------------
!     Imported variable declarations 
!-----------------------------------------------------------------------
!
  type(ESMF_GridComp) :: gcomp
  integer :: iLoop
  integer, intent(out) :: rc
!
!-----------------------------------------------------------------------
!     Local variable declarations 
!-----------------------------------------------------------------------
!
  character(ESMF_MAXSTR) ::  ofile
  type(ESMF_Field) :: field
  type(ESMF_State) :: importState
!
  rc = ESMF_SUCCESS
!
!-----------------------------------------------------------------------
!     Get gridded component 
!-----------------------------------------------------------------------
!
  call ESMF_GridCompGet(gcomp, importState=importState, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                         line=__LINE__, file=FILENAME)) return
!
!-----------------------------------------------------------------------
!     Get field
!-----------------------------------------------------------------------
!
  call ESMF_StateGet(importState, 'sst', field, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                         line=__LINE__, file=FILENAME)) return
!
  if (debugLevel >= 1) then
    write (ofile, "(A6,I2.2,A3)") "sstATM", iLoop, ".nc"
    call ESMF_FieldWrite(field, trim(ofile), rc=rc)
  end if

  end subroutine ATM_Get

end module mod_esmf_atm
