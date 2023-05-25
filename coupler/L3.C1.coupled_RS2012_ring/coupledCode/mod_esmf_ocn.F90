!-----------------------------------------------------------------------
! Earth System Modeling Framework
! Copyright 2002-2017, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
#define FILENAME "mod_esmf_ocn.F90"
!
!-----------------------------------------------------------------------
!     OCN gridded component code 
!-----------------------------------------------------------------------
!
module mod_esmf_ocn
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
  use mitgcm_org_ocn, only : mit_init
  use mitgcm_org_ocn, only : mit_getclock
  use mitgcm_org_ocn, only : mit_run
  use mitgcm_org_ocn, only : get_domain_size
  use ieee_arithmetic, only : ieee_is_nan
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
  public :: OCN_SetServices
!
  contains
!
  subroutine OCN_SetServices(gcomp, rc)
!
!-------------------------------------------------------------------
! Imported variable declarations 
!-------------------------------------------------------------------
!
  type(ESMF_GridComp) :: gcomp
  integer, intent(out) :: rc
!
  rc = ESMF_SUCCESS

  call NUOPC_CompDerive(gcomp, NUOPC_SetServices, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
      line=__LINE__, file=FILENAME)) return

  ! set entry point for methods that require specific implementation
  call NUOPC_CompSetEntryPoint(gcomp, ESMF_METHOD_INITIALIZE,       &
                               phaseLabelList=(/"IPDv00p1"/),       &
                               userRoutine=OCN_Init1, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
      line=__LINE__, file=FILENAME)) return

  call NUOPC_CompSetEntryPoint(gcomp, ESMF_METHOD_INITIALIZE,       &
                               phaseLabelList=(/"IPDv00p2"/),       &
                               userRoutine=OCN_Init2, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
      line=__LINE__, file=FILENAME)) return

  call NUOPC_CompSpecialize(gcomp, specLabel=NUOPC_Label_SetClock,  &
                            specRoutine=OCN_SetClock, rc = rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
      line=__LINE__, file=FILENAME)) return

  call NUOPC_CompSpecialize(gcomp, specLabel=NUOPC_Label_Advance,   &
                            specRoutine=OCN_Run, rc = rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
      line=__LINE__, file=FILENAME)) return

  call ESMF_GridCompSetEntryPoint(gcomp,                            &
                               methodflag=ESMF_METHOD_FINALIZE,     &
                               userRoutine=OCN_Final, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
      line=__LINE__, file=FILENAME)) return

  end subroutine OCN_SetServices
!
!-----------------------------------------------------------------------
!     Initialization phase 1, set import/export fields
!-----------------------------------------------------------------------
!
  subroutine OCN_Init1(gcomp, importState, exportState, clock, rc)

  type(ESMF_GridComp)  :: gcomp
  type(ESMF_State)     :: importState, exportState
  type(ESMF_Clock)     :: clock
  integer, intent(out) :: rc

  character(ESMF_MAXSTR) :: entryNameNUOPC, entryNameWRF
  integer :: iEntry
  logical :: exportEntry
  
  rc = ESMF_SUCCESS

  ! register the MITgcm entries in ESMF
  do iEntry = 1, nList
    entryNameNUOPC = trim(nuopc_entryNameList(iEntry));
    entryNameWRF = trim(wrf_nameList(iEntry));
    exportEntry = OCNtoATM(iEntry);
    if (exportEntry == .True.) then
      Call NUOPC_Advertise(exportState, &
        StandardName=entryNameNUOPC, name=entryNameWRF, rc=rc)
    else
      Call NUOPC_Advertise(importState, &
        StandardName=entryNameNUOPC, name=entryNameWRF, rc=rc)
    end if
  end do
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
      line=__LINE__, file=FILENAME)) return

  end subroutine
!
!-----------------------------------------------------------------------
!     Initialization phase 2 
!-----------------------------------------------------------------------
!
  subroutine OCN_Init2(gcomp, importState, exportState, clock, rc)

  type(ESMF_GridComp)  :: gcomp
  type(ESMF_State)     :: importState, exportState
  type(ESMF_Clock)     :: clock
  integer, intent(out) :: rc
!
!-------------------------------------------------------------------
! Local variable declaration
!-------------------------------------------------------------------
!
  integer :: myThid = 1
  integer :: comm, localPet, petCount
  character(ESMF_MAXSTR) :: gname
  type(ESMF_Field) :: field
  type(ESMF_Grid) :: ocnGridIn
  type(ESMF_Grid) :: ocnGridOut
!
  type(ESMF_VM) :: vm
  
  rc = ESMF_SUCCESS
!
!-------------------------------------------------------------------
! Get gridded component
!-------------------------------------------------------------------
!
  call ESMF_GridCompGet(gcomp, name=gname, vm=vm, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
      line=__LINE__, file=FILENAME)) return

  call ESMF_VMGet(vm, localPet=localPet, petCount=petCount,         &
                  mpiCommunicator=comm, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
      line=__LINE__, file=FILENAME)) return
!
!-------------------------------------------------------------------
! Initialize the gridded component
!-------------------------------------------------------------------
!
  call MIT_INIT(myThid, comm)
!
!-------------------------------------------------------------------
! Set-up grid and load coordinate data
!-------------------------------------------------------------------
!
  call OCN_SetGridArrays(gcomp, petCount, localPet, ocnGridIn, rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
      line=__LINE__, file=FILENAME)) return
  ocnGridOut = ocnGridIn

  call OCN_SetStates(gcomp, ocnGridIn, ocnGridOut, rc)
!
  end subroutine
!
!-----------------------------------------------------------------------
!     Ocean Set Clock  
!-----------------------------------------------------------------------
!
  subroutine OCN_SetClock(gcomp, rc)

  type(ESMF_GridComp)  :: gcomp
  integer, intent(out) :: rc

  type(ESMF_Clock)     :: clock
  type(ESMF_VM) :: vm
  
  rc = ESMF_SUCCESS

  call NUOPC_ModelGet(gcomp, modelClock=clock, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
      line=__LINE__, file=FILENAME)) return

  call NUOPC_CompSetClock(gcomp, clock, ocnTimeStep, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
      line=__LINE__, file=FILENAME)) return

  ! Run 0 time step to initialize data (maybe not useful)
  call OCN_Put(gcomp, 0, rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
      line=__LINE__, file=FILENAME)) return

  end subroutine
!
!-----------------------------------------------------------------------
!     Run
!-----------------------------------------------------------------------
!
  subroutine OCN_Run(gcomp, rc)

  type(ESMF_GridComp)  :: gcomp
  integer, intent(out) :: rc

  type(ESMF_Clock)     :: clock
  type(ESMF_State)     :: importState, exportState
  
  integer :: i, j, maxdiv, runid, localPet, petCount
  integer :: myThid
  integer :: myIter = 0
  real*8 :: myTime = 0d0
  integer :: iLoop_ocn = 1
  integer :: nTimeStepsIn = 1
  character(ESMF_MAXSTR) :: cname
  
  type(ESMF_VM) :: vm
  type(ESMF_Time) :: startTime
  type(ESMF_Time) :: stopTime
  type(ESMF_Time) :: currTime
  type(ESMF_TimeInterval) :: timeStep
  type(ESMF_Clock) :: internalClock
  real(ESMF_KIND_R8) :: wTimeStart, wTimeEnd

  call ESMF_VMWtime(wTimeStart)
  
  rc = ESMF_SUCCESS

  call NUOPC_ModelGet(gcomp, modelClock=clock,                      &
                      importState=importState,                      &
                      exportState=exportState, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
      line=__LINE__, file=FILENAME)) return

  call OCN_Get(gcomp, iLoop_ocn, rc)

  if (currentTimeStep == 0) then
    call mit_getclock(myTime, myIter)
    nTimeStepsIn = INT( esm_step_seconds/ocn_step_seconds )
  end if

  ! print *, "calling OCN_Run function"
  ! print *, "iLoop_ocn is: ", iLoop_ocn
  ! print *, "myTime is: ", myTime
  ! print *, "myIter is: ", myIter
  ! print *, "nTimeStepsIn is: ", nTimeStepsIn

  call mit_run(iLoop_ocn, myTime, myIter, nTimeStepsIn, myThid)

  call OCN_Put(gcomp, iLoop_ocn, rc)

  call ESMF_ClockPrint(clock, options="currTime", &
         preString="------>Advancing OCN from: ", rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
      line=__LINE__, file=__FILE__)) return  ! bail out
  
  call ESMF_ClockGet(clock, currTime=currTime, timeStep=timeStep,   &
                     rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
      line=__LINE__, file=__FILE__)) return  ! bail out
  
  call ESMF_TimePrint(currTime + timeStep,                          &
         preString="--------------------------------> to: ", rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
      line=__LINE__, file=__FILE__)) return  ! bail out

  iLoop_ocn = iLoop_ocn + 1
  currentTimeStep = currentTimeStep + 1

  call ESMF_VMWtime(wTimeEnd)
  ocn_wall_time = ocn_wall_time + wTimeEnd - wTimeStart

  end subroutine
!
!-----------------------------------------------------------------------
!     Call Ocean model finalize routines
!-----------------------------------------------------------------------
!
  subroutine OCN_Final(gcomp, importState, exportState, clock, rc)

  type(ESMF_GridComp)  :: gcomp
  integer, intent(out) :: rc

  type(ESMF_Clock)     :: clock
  type(ESMF_State)     :: importState, exportState
  
  rc = ESMF_SUCCESS

  end subroutine

  subroutine OCN_SetGridArrays(gcomp, petCount, localPet, gridIn,rc)
!
!-------------------------------------------------------------------
! Used module declarations 
!-------------------------------------------------------------------
!
  use mitgcm_org_ocn, only : get_grid_parameters
!
  implicit none
!
!-------------------------------------------------------------------
! Imported variable declarations 
!-------------------------------------------------------------------
!
  type(ESMF_GridComp), intent(inout) :: gcomp
  integer, intent(in) :: localPet 
  integer, intent(in) :: petCount 
  type(ESMF_Grid) :: gridIn
  integer, intent(inout) :: rc
  type(ESMF_VM) :: vm
  character(ESMF_MAXSTR) :: cname

  INTEGER sNx, sNy, OLx, OLy, nSx, nSy, nPx, nPy
  INTEGER Nx, Ny, Nr, i, j, bi, bj
  INTEGER myXGlobalLo, myYGlobalLo
  integer :: myThid = 1
  REAL*8, DIMENSION(:,:,:,:), ALLOCATABLE :: rA_ESMF, xC_ESMF
  REAL*8, DIMENSION(:,:,:,:), ALLOCATABLE :: xG_ESMF, yC_ESMF
  REAL*8, DIMENSION(:,:,:,:), ALLOCATABLE :: yG_ESMF
  REAL*8, DIMENSION(:,:,:,:,:), ALLOCATABLE :: maskC_ESMF
  REAL*8, DIMENSION(:,:,:,:,:), ALLOCATABLE :: maskS_ESMF
  REAL*8, DIMENSION(:,:,:,:,:), ALLOCATABLE :: maskW_ESMF

  integer :: k, m, n, p, iG, jG
  integer :: localDECount, tile
  character(ESMF_MAXSTR) :: name
  integer(ESMF_KIND_I4), pointer :: ptrM(:,:)
  real(ESMF_KIND_R8), pointer :: ptrX(:,:), ptrY(:,:), ptrA(:,:)
  type(ESMF_Array) :: arrX, arrY, arrM, arrA
  type(ESMF_StaggerLoc) :: staggerLoc
  type(ESMF_DistGrid) :: distGrid
  integer, allocatable :: deBlockList(:,:,:)
  integer, allocatable :: meshType(:)
  character(ESMF_MAXSTR), allocatable :: meshTypeName(:)
  integer, allocatable :: mpi_myXGlobalLo(:), mpi_myYGlobalLo(:)
!
!-------------------------------------------------------------------
! Local variable declarations 
!-------------------------------------------------------------------
!
  rc = ESMF_SUCCESS
  call get_domain_size(sNx, sNy, OLx, OLy,                          &
                       nSx, nSy, nPx, nPy, Nx, Ny, Nr,              &
                       myXGlobalLo, myYGlobalLo)
  ALLOCATE(rA_ESMF(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy))
  ALLOCATE(xC_ESMF(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy))
  ALLOCATE(xG_ESMF(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy))
  ALLOCATE(yC_ESMF(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy))
  ALLOCATE(yG_ESMF(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy))
  ALLOCATE(maskC_ESMF(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy))
  ALLOCATE(maskS_ESMF(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy))
  ALLOCATE(maskW_ESMF(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy))

  call get_grid_parameters(rA_ESMF, xC_ESMF, xG_ESMF,               &
                           yC_ESMF, yG_ESMF, maskC_ESMF,            &
                           maskS_ESMF, maskW_ESMF, myThid)
!
!-------------------------------------------------------------------
! Get gridded component 
!-------------------------------------------------------------------
!
  call ESMF_GridCompGet(gcomp, vm=vm, name=cname, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
      line=__LINE__, file=FILENAME)) return
!
  if (.not. allocated(mpi_myXGlobalLo)) then
    allocate(mpi_myXGlobalLo(nPx*nPy))
    allocate(mpi_myYGlobalLo(nPx*nPy))
  end if
!  
  call ESMF_VMAllGatherV(vm, sendData=(/ myXGlobalLo /),            &
                         sendCount=1, recvData=mpi_myXGlobalLo,     &
                         recvCounts=(/ (1, k = 0, petCount-1) /),   &
                         recvOffsets=(/ (k, k = 0, petCount-1) /),  &
                         rc=rc)
!
  call ESMF_VMAllGatherV(vm, sendData=(/ myYGlobalLo /),            &
                         sendCount=1, recvData=mpi_myYGlobalLo,     &
                         recvCounts=(/ (1, k = 0, petCount-1) /),   &
                         recvOffsets=(/ (k, k = 0, petCount-1) /),  &
                         rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
      line=__LINE__, file=FILENAME)) return
!
!-------------------------------------------------------------------
! Get limits of the grid arrays (based on PET and nest level)
!-------------------------------------------------------------------
!
  if (.not.allocated(deBlockList)) then
    allocate(deBlockList(2,2,1:nPx*nPy))
  end if
!
  do tile = 1, (nPx*nPy)
    deBlockList(1,1,tile) = mpi_myXGlobalLo(tile)
    deBlockList(1,2,tile) = mpi_myXGlobalLo(tile)+sNx-1
    deBlockList(2,1,tile) = mpi_myYGlobalLo(tile) 
    deBlockList(2,2,tile) = mpi_myYGlobalLo(tile)+sNy-1
  end do
!
!-------------------------------------------------------------------
! Create ESMF DistGrid based on model domain decomposition
!-------------------------------------------------------------------
!
  distGrid = ESMF_DistGridCreate(minIndex=(/ 1, 1 /),               &
                                 maxIndex=(/ Nx, Ny /),             &
                                 deBlockList=deBlockList,           &
                                 rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
      line=__LINE__, file=FILENAME)) return
!
!-------------------------------------------------------------------
! Set staggering type 
!-------------------------------------------------------------------
!
  staggerLoc = ESMF_STAGGERLOC_CENTER ! Icross
!
!-------------------------------------------------------------------
! Create ESMF Grid
!-------------------------------------------------------------------
!
  ! Icross
  gridIn = ESMF_GridCreate(distgrid=distGrid,                       &
                           indexflag=ESMF_INDEX_GLOBAL,             &
                           name="ocn_grid", rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
      line=__LINE__, file=FILENAME)) return
!
!-------------------------------------------------------------------
! Allocate coordinates 
!-------------------------------------------------------------------
!
  call ESMF_GridAddCoord(gridIn, staggerLoc=staggerLoc, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
      line=__LINE__, file=FILENAME)) return
!
  call ESMF_GridAddItem(gridIn, staggerLoc=staggerLoc,              &
                        itemflag=ESMF_GRIDITEM_MASK, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
      line=__LINE__, file=FILENAME)) return
!
  call ESMF_GridAddItem(gridIn, staggerLoc=staggerLoc,              &
                        itemflag=ESMF_GRIDITEM_AREA, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
      line=__LINE__, file=FILENAME)) return
!
  call ESMF_GridGet(gridIn, localDECount=localDECount, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
      line=__LINE__, file=FILENAME)) return
!
  do j = 0, localDECount-1
    call ESMF_GridGetCoord(gridIn, localDE=j, staggerLoc=staggerLoc,&
                           coordDim=1, farrayPtr=ptrX, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,  &
        line=__LINE__, file=FILENAME)) return
!
    call ESMF_GridGetCoord(gridIn, localDE=j, staggerLoc=staggerLoc,&
                           coordDim=2, farrayPtr=ptrY, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,  &
        line=__LINE__, file=FILENAME)) return
!
    call ESMF_GridGetItem(gridIn, localDE=j, staggerLoc=staggerLoc, &
                          itemflag=ESMF_GRIDITEM_MASK,              &
                          farrayPtr=ptrM, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,  &
        line=__LINE__, file=FILENAME)) return
!
    call ESMF_GridGetItem(gridIn, localDE=j, staggerLoc=staggerLoc, &
                          itemflag=ESMF_GRIDITEM_AREA, &
                          farrayPtr=ptrA, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,  &
        line=__LINE__, file=FILENAME)) return
!
    bj = 1
    bi = 1
    do n = 1, sNy
      do m = 1, sNx
        iG = myXGlobalLo-1+(bi-1)*sNx+m
        jG = myYGlobalLo-1+(bj-1)*sNy+n
        ptrX(iG,jG) = nint(xC_ESMF(m,n,1,1)*100000)/100000.0
        ptrY(iG,jG) = nint(yC_ESMF(m,n,1,1)*100000)/100000.0
        ptrM(iG,jG) = nint(maskC_ESMF(m,n,1,1,1))
        ptrA(iG,jG) = nint(rA_ESMF(m,n,1,1)*100000)/100000.0
      end do
    end do
!
!   ! Nullify pointers 
    if (associated(ptrY)) then
      nullify(ptrY)
    end if
    if (associated(ptrX)) then
      nullify(ptrX)
    end if
    if (associated(ptrM)) then
      nullify(ptrM)
    end if
    if (associated(ptrA)) then
      nullify(ptrA)
    end if
  end do
!
!-------------------------------------------------------------------
! Write the grid to debug
!-------------------------------------------------------------------
! 
  if (debugLevel >= 1) then
    call ESMF_GridGetCoord(gridIn,                                  &
                           staggerLoc=ESMF_STAGGERLOC_CENTER,       &
                           coordDim=1, array=arrX, rc=rc)
    call ESMF_GridGetCoord(gridIn,                                  &
                           staggerLoc=ESMF_STAGGERLOC_CENTER,       &
                           coordDim=2, array=arrY, rc=rc)
    call ESMF_ArrayWrite(arrX, filename="ocean_xa_1.nc",            &
                         status=ESMF_FILESTATUS_NEW, rc=rc)
    call ESMF_ArrayWrite(arrY, filename="ocean_ya_1.nc",            &
                         status=ESMF_FILESTATUS_NEW, rc=rc)
  end if
!
!-------------------------------------------------------------------
! Assign grid to gridded component 
!-------------------------------------------------------------------
!
  call ESMF_GridCompSet(gcomp, grid=gridIn, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
      line=__LINE__, file=FILENAME)) return
!
  DEALLOCATE(rA_ESMF)
  DEALLOCATE(xC_ESMF)
  DEALLOCATE(xG_ESMF)
  DEALLOCATE(yC_ESMF)
  DEALLOCATE(yG_ESMF)
  DEALLOCATE(maskC_ESMF)
  DEALLOCATE(maskS_ESMF)
  DEALLOCATE(maskW_ESMF)
!
  end subroutine OCN_SetGridArrays
!
  subroutine OCN_SetStates(gcomp, gridIn, gridOut, rc)
  implicit none
!
!-----------------------------------------------------------------------
!     Imported variable declarations 
!-----------------------------------------------------------------------
!
  type(ESMF_GridComp), intent(in) :: gcomp
  type(ESMF_Grid) :: gridIn
  type(ESMF_Grid) :: gridOut
  integer, intent(out) :: rc
!
!-------------------------------------------------------------------
! Local variable declarations 
!-------------------------------------------------------------------
!
  integer :: i, j, k, itemCount, localDECount, localPet, petCount
  character(ESMF_MAXSTR), allocatable :: itemNameList(:)
  real*8, dimension(:,:), pointer :: ptr2d_tmp
!
  INTEGER sNx, sNy, OLx, OLy, nSx, nSy, nPx, nPy, Nx, Ny, Nr
  INTEGER myXGlobalLo, myYGlobalLo
!
  type(ESMF_VM) :: vm
  type(ESMF_Field) :: field_tmp
  type(ESMF_ArraySpec) :: arraySpec
  type(ESMF_StaggerLoc) :: staggerLoc 
  type(ESMF_State) :: importState, exportState

  character(ESMF_MAXSTR) :: entryNameNUOPC, entryNameWRF
  integer :: iEntry
  logical :: exportEntry
  
  rc = ESMF_SUCCESS

!
!-------------------------------------------------------------------
! Get information about gridded component 
!-------------------------------------------------------------------
!
  call ESMF_GridCompGet(gcomp, importState=importState,             &
                        exportState=exportState, vm=vm, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
      line=__LINE__, file=FILENAME)) return
!
  call ESMF_VMGet(vm, localPet=localPet, petCount=petCount, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
      line=__LINE__, file=FILENAME)) return
!
!-------------------------------------------------------------------
! Set array descriptor
!-------------------------------------------------------------------
!
  call ESMF_ArraySpecSet(arraySpec, typekind=ESMF_TYPEKIND_R8,      &
                         rank=2, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
      line=__LINE__, file=FILENAME)) return
!
!-------------------------------------------------------------------
! Get number of local DEs
!-------------------------------------------------------------------
! 
  call ESMF_GridGet(gridIn, localDECount=localDECount, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
      line=__LINE__, file=FILENAME)) return
!
!-----------------------------------------------------------------------
!     Set staggering type 
!-----------------------------------------------------------------------
!
  staggerLoc = ESMF_STAGGERLOC_CENTER
!
!-------------------------------------------------------------------
! Create export field 
!-------------------------------------------------------------------
!
  ! register the MITgcm entries in ESMF
  do iEntry = 1, nList
    entryNameNUOPC = trim(nuopc_entryNameList(iEntry));
    entryNameWRF = trim(wrf_nameList(iEntry));
    exportEntry = OCNtoATM(iEntry);

    if (exportEntry == .True.) then
      field_tmp = ESMF_FieldCreate(gridOut, arraySpec,&
                 staggerloc=staggerLoc,                &
                 indexflag=ESMF_INDEX_GLOBAL,          &
                 name=entryNameWRF, rc=rc)
    else
      field_tmp = ESMF_FieldCreate(gridOut, arraySpec,&
                 staggerloc=staggerLoc,                &
                 indexflag=ESMF_INDEX_GLOBAL,          &
                 name=entryNameWRF, rc=rc)
    end if

    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,  &
        line=__LINE__, file=FILENAME)) return

    !Put initial data into state 
    do j = 0, localDECount-1
    ! Get pointer from field 
      call ESMF_FieldGet(field_tmp, localDe=j, farrayPtr=ptr2d_tmp, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,  &
          line=__LINE__, file=FILENAME)) return
     
      ! Initialize pointer 
      ptr2d_tmp = 0.d0
      ! Nullify the pointer
      if (associated(ptr2d_tmp)) then
        nullify(ptr2d_tmp)
      end if
    end do

    if (exportEntry == .True.) then
      call NUOPC_Realize(exportState, field=field_tmp, rc=rc) 
    else
      call NUOPC_Realize(importState, field=field_tmp, rc=rc) 
    end if
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,  &
        line=__LINE__, file=FILENAME)) return
  end do
!
  end subroutine OCN_SetStates 
!
!-----------------------------------------------------------------------
!     Ocean model get data from external
!-----------------------------------------------------------------------
!
  subroutine OCN_Get(gcomp, iLoop, rc)
!
!-------------------------------------------------------------------
! Used module declarations 
!-------------------------------------------------------------------
!
  use mitgcm_org_ocn, only : get_field_parameters
!
  implicit none
!
!-------------------------------------------------------------------
! Imported variable declarations 
!-------------------------------------------------------------------
!
  type(ESMF_GridComp) :: gcomp
  integer :: iLoop
  integer, intent(out) :: rc
!
!-------------------------------------------------------------------
! Local variable declarations 
!-------------------------------------------------------------------
!
  integer :: i, j, ii, jj, bi, bj, iG, jG, imax, jmax
  integer :: imin, jmin
  integer :: localPet, petCount, itemCount, localDECount
  character(ESMF_MAXSTR) :: cname, ofile
  real(ESMF_KIND_R8) :: sfac, addo
  real(ESMF_KIND_R8), pointer :: ptr_lwup(:,:), ptr_lwdn(:,:)
  real(ESMF_KIND_R8), pointer :: ptr_swup(:,:), ptr_swdn(:,:)
  real(ESMF_KIND_R8), pointer :: ptr_hl(:,:), ptr_hs(:,:)
  real(ESMF_KIND_R8), pointer :: ptr_u10(:,:), ptr_v10(:,:)
  real(ESMF_KIND_R8), pointer :: ptr_t2(:,:), ptr_q2(:,:)
  real(ESMF_KIND_R8), pointer :: ptr_evap(:,:), ptr_raincv(:,:)
  real(ESMF_KIND_R8), pointer :: ptr_rainshv(:,:), ptr_rainncv(:,:)
!
  type(ESMF_VM) :: vm
  type(ESMF_Clock) :: clock
  type(ESMF_Grid) :: gridIn
  type(ESMF_Field) :: field
  type(ESMF_Field) :: field_lwup, field_lwdn, field_swup, field_swdn
  type(ESMF_Field) :: field_hl, field_hs
  type(ESMF_Field) :: field_u10, field_v10
  type(ESMF_Field) :: field_t2, field_q2
  type(ESMF_Field) :: field_evap, field_raincv
  type(ESMF_Field) :: field_rainshv, field_rainncv
  type(ESMF_State) :: importState
  INTEGER sNx, sNy, OLx, OLy, nSx, nSy, nPx, nPy, Nx, Ny, Nr
  INTEGER myXGlobalLo, myYGlobalLo
  REAL*8, DIMENSION(:,:,:,:), ALLOCATABLE :: swdown_ESMF
  REAL*8, DIMENSION(:,:,:,:), ALLOCATABLE :: lwdown_ESMF
  REAL*8, DIMENSION(:,:,:,:), ALLOCATABLE :: hl_ESMF
  REAL*8, DIMENSION(:,:,:,:), ALLOCATABLE :: hs_ESMF
  REAL*8, DIMENSION(:,:,:,:), ALLOCATABLE :: uwind_ESMF
  REAL*8, DIMENSION(:,:,:,:), ALLOCATABLE :: vwind_ESMF
  REAL*8, DIMENSION(:,:,:,:), ALLOCATABLE :: atemp_ESMF
  REAL*8, DIMENSION(:,:,:,:), ALLOCATABLE :: aqh_ESMF
  REAL*8, DIMENSION(:,:,:,:), ALLOCATABLE :: evap_ESMF
  REAL*8, DIMENSION(:,:,:,:), ALLOCATABLE :: precip_ESMF
  integer :: myThid = 1
!
  rc = ESMF_SUCCESS
!
!-------------------------------------------------------------------
! Get gridded component 
!-------------------------------------------------------------------
!
  call get_domain_size(sNx, sNy, OLx, OLy,                          &
        nSx, nSy, nPx, nPy, Nx, Ny, Nr,                             &
        myXGlobalLo, myYGlobalLo)

  ALLOCATE(swdown_ESMF(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy))
  ALLOCATE(lwdown_ESMF(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy))
  ALLOCATE(hl_ESMF(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy))
  ALLOCATE(hs_ESMF(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy))
  ALLOCATE(uwind_ESMF(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy))
  ALLOCATE(vwind_ESMF(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy))
  ALLOCATE(atemp_ESMF(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy))
  ALLOCATE(aqh_ESMF(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy))
  ALLOCATE(evap_ESMF(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy))
  ALLOCATE(precip_ESMF(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy))

  call ESMF_GridCompGet(gcomp, name=cname, clock=clock, grid=gridIn,&
                        importState=importState, vm=vm, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
      line=__LINE__, file=FILENAME)) return
!
  call ESMF_VMGet(vm, localPet=localPet, petCount=petCount, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
      line=__LINE__, file=FILENAME)) return
!
!-------------------------------------------------------------------
! Get number of local DEs
!-------------------------------------------------------------------
! 
  call ESMF_GridGet(gridIn, localDECount=localDECount, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
      line=__LINE__, file=FILENAME)) return
!
!-------------------------------------------------------------------
! Get field
!-------------------------------------------------------------------
!
  call ESMF_StateGet(importState, "LWUPB", field_lwup, rc=rc)
  call ESMF_StateGet(importState, "LWDNB", field_lwdn, rc=rc)
  call ESMF_StateGet(importState, "SWUPB", field_swup, rc=rc)
  call ESMF_StateGet(importState, "SWDNB", field_swdn, rc=rc)
  call ESMF_StateGet(importState, "LH", field_hl, rc=rc)
  call ESMF_StateGet(importState, "HFX", field_hs, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
      line=__LINE__, file=FILENAME)) return

  call ESMF_StateGet(importState, "U10", field_u10, rc=rc)
  call ESMF_StateGet(importState, "V10", field_v10, rc=rc)
  call ESMF_StateGet(importState, "T2", field_t2, rc=rc)
  call ESMF_StateGet(importState, "Q2", field_q2, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
      line=__LINE__, file=FILENAME)) return

  call ESMF_StateGet(importState, "QFX", field_evap, rc=rc)
  call ESMF_StateGet(importState, "RAINCV", field_raincv, rc=rc)
  call ESMF_StateGet(importState, "RAINSHV", field_rainshv, rc=rc)
  call ESMF_StateGet(importState, "RAINNCV", field_rainncv, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
      line=__LINE__, file=FILENAME)) return
!
!-------------------------------------------------------------------
! Loop over decomposition elements (DEs) 
!-------------------------------------------------------------------
!
  do j = 0, localDECount-1
!
!   ! Get pointer /from field
    call ESMF_FieldGet(field_lwup, localDE=j, farrayPtr=ptr_lwup, rc=rc)
    call ESMF_FieldGet(field_lwdn, localDE=j, farrayPtr=ptr_lwdn, rc=rc)
    call ESMF_FieldGet(field_swup, localDE=j, farrayPtr=ptr_swup, rc=rc)
    call ESMF_FieldGet(field_swdn, localDE=j, farrayPtr=ptr_swdn, rc=rc)
    call ESMF_FieldGet(field_hl, localDE=j, farrayPtr=ptr_hl, rc=rc)
    call ESMF_FieldGet(field_hs, localDE=j, farrayPtr=ptr_hs, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,  &
        line=__LINE__, file=FILENAME)) return

    call ESMF_FieldGet(field_u10, localDE=j, farrayPtr=ptr_u10, rc=rc)
    call ESMF_FieldGet(field_v10, localDE=j, farrayPtr=ptr_v10, rc=rc)
    call ESMF_FieldGet(field_t2, localDE=j, farrayPtr=ptr_t2, rc=rc)
    call ESMF_FieldGet(field_q2, localDE=j, farrayPtr=ptr_q2, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,  &
        line=__LINE__, file=FILENAME)) return

    call ESMF_FieldGet(field_evap, localDE=j, farrayPtr=ptr_evap, rc=rc)
    call ESMF_FieldGet(field_raincv, localDE=j, farrayPtr=ptr_raincv, rc=rc)
    call ESMF_FieldGet(field_rainshv, localDE=j, farrayPtr=ptr_rainshv, rc=rc)
    call ESMF_FieldGet(field_rainncv, localDE=j, farrayPtr=ptr_rainncv, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,  &
        line=__LINE__, file=FILENAME)) return

!   ! Put data to OCN component variable
    sfac = 1.0d0
    addo = 0.0d0
!
    bi = 1
    bj = 1
!
    ! where (ieee_is_nan(ptr_lwup)) ptr_lwup = MISSING_R8
    ! where (ieee_is_nan(ptr_lwdn)) ptr_lwdn = MISSING_R8
    ! where (ieee_is_nan(ptr_swup)) ptr_swup = MISSING_R8
    ! where (ieee_is_nan(ptr_swdn)) ptr_swdn = MISSING_R8
    ! where (ieee_is_nan(ptr_hl)) ptr_hl = MISSING_R8
    ! where (ieee_is_nan(ptr_hs)) ptr_hs = MISSING_R8
    ! where (ieee_is_nan(ptr_u10)) ptr_u10 = MISSING_R8
    ! where (ieee_is_nan(ptr_v10)) ptr_v10 = MISSING_R8
    ! where (ieee_is_nan(ptr_t2)) ptr_t2 = MISSING_R8
    ! where (ieee_is_nan(ptr_q2)) ptr_q2 = MISSING_R8
    ! where (ieee_is_nan(ptr_evap)) ptr_evap = MISSING_R8
    ! where (ieee_is_nan(ptr_raincv)) ptr_raincv = MISSING_R8
    ! where (ieee_is_nan(ptr_rainshv)) ptr_rainshv = MISSING_R8
    ! where (ieee_is_nan(ptr_rainncv)) ptr_rainncv = MISSING_R8
    ! ptr = MISSING_R8
!
    do jj = 1-OLy, sNy+OLy
      do ii = 1-OLx, sNx+OLx
        imin = myXGlobalLo-1+(bi-1)*sNx
        imax = myXGlobalLo-1+(bi-1)*sNx+sNx + 1
        jmin = myYGlobalLo-1+(bj-1)*sNy
        jmax = myYGlobalLo-1+(bj-1)*sNy+sNy + 1
        iG = myXGlobalLo-1+(bi-1)*sNx+ii
        jG = myYGlobalLo-1+(bj-1)*sNy+jj
        lwdown_ESMF(ii,jj,1,1)       =   0.0d0
        swdown_ESMF(ii,jj,1,1)       =   0.0d0
        hl_ESMF(ii,jj,1,1)           =   0.0d0
        hs_ESMF(ii,jj,1,1)           =   0.0d0
        uwind_ESMF(ii,jj,1,1)        =   0.0d0
        vwind_ESMF(ii,jj,1,1)        =   0.0d0
        atemp_ESMF(ii,jj,1,1)        =   0.0d0
        aqh_ESMF(ii,jj,1,1)          =   0.0d0
        evap_ESMF(ii,jj,1,1)         =   0.0d0
        precip_ESMF(ii,jj,1,1)       =   0.0d0

        if ((iG > imin .and. iG < imax) .and. (jG > jmin .and. jG < jmax)) then
          lwdown_ESMF(ii,jj,1,1) = (ptr_lwup(iG,jG)-ptr_lwdn(iG,jG))*sfac+addo
          swdown_ESMF(ii,jj,1,1) = (ptr_swup(iG,jG)-ptr_swdn(iG,jG))*sfac+addo
          hl_ESMF(ii,jj,1,1)     = -ptr_hl(iG,jG)*sfac+addo
          hs_ESMF(ii,jj,1,1)     = -ptr_hs(iG,jG)*sfac+addo
          uwind_ESMF(ii,jj,1,1)  = ptr_u10(iG,jG)*sfac+addo
          vwind_ESMF(ii,jj,1,1)  = ptr_v10(iG,jG)*sfac+addo
          atemp_ESMF(ii,jj,1,1)  = (ptr_t2(iG,jG)*sfac)+addo
          aqh_ESMF(ii,jj,1,1)    = (ptr_q2(iG,jG)*sfac)+addo
          evap_ESMF(ii,jj,1,1)   = (ptr_evap(iG,jG)/1000d0*sfac)+addo
          !! need to divide the 'time-step' precipitation
          !! by WRF time step
          precip_ESMF(ii,jj,1,1) = ((ptr_raincv(iG,jG)           &
                                    +ptr_rainshv(iG,jG)          &
                                    +ptr_rainncv(iG,jG))*sfac    &
                                    +addo) /(atm_step_seconds+0.d0)     &
                                   /1000d0
        endif
      end do
    end do
!
!   ! Nullify the pointer
    if (associated(ptr_lwup)) then
      nullify(ptr_lwup)
    end if
    if (associated(ptr_lwdn)) then
      nullify(ptr_lwdn)
    end if
    if (associated(ptr_hl)) then
      nullify(ptr_hl)
    end if
    if (associated(ptr_hs)) then
      nullify(ptr_hs)
    end if
    if (associated(ptr_swup)) then
      nullify(ptr_swup)
    end if
    if (associated(ptr_swdn)) then
      nullify(ptr_swdn)
    end if
    if (associated(ptr_u10)) then
      nullify(ptr_u10)
    end if
    if (associated(ptr_v10)) then
      nullify(ptr_v10)
    end if
    if (associated(ptr_t2)) then
      nullify(ptr_t2)
    end if
    if (associated(ptr_q2)) then
      nullify(ptr_q2)
    end if
    if (associated(ptr_evap)) then
      nullify(ptr_evap)
    end if
    if (associated(ptr_raincv)) then
      nullify(ptr_raincv)
    end if
    if (associated(ptr_rainshv)) then
      nullify(ptr_rainshv)
    end if
    if (associated(ptr_rainncv)) then
      nullify(ptr_rainncv)
    end if
!
  end do       
!
  call get_field_parameters(lwdown_ESMF, swdown_ESMF, &
                            hl_ESMF, hs_ESMF, &
                            uwind_ESMF, vwind_ESMF, &
                            atemp_ESMF, aqh_ESMF, &
                            evap_ESMF, precip_ESMF, myThid)

  if (debugLevel >= 1) then
    write (ofile, "(A5,I6.6,A3)") "hlATM", iLoop, ".nc"
    call ESMF_FieldWrite(field_hl, trim(ofile), rc=rc)
  end if
!

  DEALLOCATE(swdown_ESMF)
  DEALLOCATE(lwdown_ESMF)
  DEALLOCATE(hl_ESMF)
  DEALLOCATE(hs_ESMF)
  DEALLOCATE(uwind_ESMF)
  DEALLOCATE(vwind_ESMF)
  DEALLOCATE(atemp_ESMF)
  DEALLOCATE(aqh_ESMF)
  DEALLOCATE(evap_ESMF)
  DEALLOCATE(precip_ESMF)

  end subroutine OCN_Get
!
!-----------------------------------------------------------------------
!     Ocean model put data to external
!-----------------------------------------------------------------------
!
  subroutine OCN_Put(gcomp, iLoop, rc)
!
!-------------------------------------------------------------------
! Used module declarations 
!-------------------------------------------------------------------
!
  use mitgcm_org_ocn, only : get_theta
  use mitgcm_org_ocn, only : get_uvoce
  use mitgcm_org_ocn, only : get_grid_parameters
!
  implicit none
!
!-------------------------------------------------------------------
! Imported variable declarations 
!-------------------------------------------------------------------
!
  type(ESMF_GridComp) :: gcomp
  type(ESMF_Grid) :: gridIn
  integer, intent(out) :: rc
  integer :: iLoop
!
!-------------------------------------------------------------------
! Local variable declarations 
!-------------------------------------------------------------------
!
  integer :: i, j, ii, jj, bi, bj, iG, jG, imax, jmax
  integer :: petCount, localPet, itemCount, localDECount
  character(ESMF_MAXSTR) :: cname, ofile
  real(ESMF_KIND_R8), pointer :: ptr_sst(:,:)
  real(ESMF_KIND_R8), pointer :: ptr_uoce(:,:)
  real(ESMF_KIND_R8), pointer :: ptr_voce(:,:)
  real(ESMF_KIND_R8), pointer :: ptr_sst_input(:,:)
  real(ESMF_KIND_R8), pointer :: ptr_ocnmask(:,:)
  integer(ESMF_KIND_I4), pointer :: ptr_mask(:,:)
!
  type(ESMF_VM) :: vm
  type(ESMF_Clock) :: clock
  type(ESMF_Field) :: field_sst
  type(ESMF_Field) :: field_uoce
  type(ESMF_Field) :: field_voce
  type(ESMF_Field) :: field_sst_input
  type(ESMF_Field) :: field_ocnmask
  type(ESMF_State) :: importState, exportState
  REAL*8, DIMENSION(:,:,:,:,:), ALLOCATABLE :: theta_ESMF
  REAL*8, DIMENSION(:,:,:,:,:), ALLOCATABLE :: uoce_ESMF
  REAL*8, DIMENSION(:,:,:,:,:), ALLOCATABLE :: voce_ESMF
!
  INTEGER sNx, sNy, OLx, OLy, nSx, nSy, nPx, nPy, Nx, Ny, Nr
  INTEGER myXGlobalLo, myYGlobalLo
  integer :: myThid = 1
!
  rc = ESMF_SUCCESS
!
!-------------------------------------------------------------------
! Get ESMF domain size info
!-------------------------------------------------------------------
!
  call get_domain_size(sNx, sNy, OLx, OLy,                          &
                       nSx, nSy, nPx, nPy, Nx, Ny, Nr,              &
                       myXGlobalLo, myYGlobalLo)

  ALLOCATE(theta_ESMF(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy))
  ALLOCATE(uoce_ESMF(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy))
  ALLOCATE(voce_ESMF(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy))

  call get_theta(theta_ESMF, myThid)
  call get_uvoce(uoce_ESMF, voce_ESMF, myThid)
!
!-------------------------------------------------------------------
! Get gridded component 
!-------------------------------------------------------------------
!
  call ESMF_GridCompGet(gcomp, name=cname, clock=clock, grid=gridIn,&
                        exportState=exportState,                    &
                        importState=importState, vm=vm, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
      line=__LINE__, file=FILENAME)) return
!
  call ESMF_VMGet(vm, localPet=localPet, petCount=petCount, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
      line=__LINE__, file=FILENAME)) return
!
!-------------------------------------------------------------------
! Get number of local DEs
!-------------------------------------------------------------------
! 
  !! TODO: check if gridin being used correctly
  call ESMF_GridGet(gridIn, localDECount=localDECount, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
      line=__LINE__, file=FILENAME)) return
!
!-------------------------------------------------------------------
! Get export field 
!-------------------------------------------------------------------
!
  call ESMF_StateGet(exportState, "SST", field_sst, rc=rc)
  call ESMF_StateGet(exportState, "UOCE", field_uoce, rc=rc)
  call ESMF_StateGet(exportState, "VOCE", field_voce, rc=rc)
  call ESMF_StateGet(exportState, "OCNMASK", field_ocnmask, rc=rc)
  call ESMF_StateGet(importState, "SST_INPUT", field_sst_input, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
      line=__LINE__, file=FILENAME)) return
!
!-------------------------------------------------------------------
! Loop over decomposition elements (DEs) 
!-------------------------------------------------------------------
!
  do j = 0, localDECount-1

!   ! Get pointer 
    call ESMF_GridGetItem(gridIn, ESMF_GRIDITEM_MASK,               &
                          staggerloc=ESMF_STAGGERLOC_CENTER,        &
                          localDe=j, farrayPtr=ptr_mask, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,  &
        line=__LINE__, file=FILENAME)) return

    call ESMF_FieldGet(field_sst, localDE=j, farrayPtr=ptr_sst, rc=rc)
    call ESMF_FieldGet(field_uoce, localDE=j, farrayPtr=ptr_uoce, rc=rc)
    call ESMF_FieldGet(field_voce, localDE=j, farrayPtr=ptr_voce, rc=rc)
    call ESMF_FieldGet(field_ocnmask, localDE=j, farrayPtr=ptr_ocnmask, rc=rc)
    call ESMF_FieldGet(field_sst_input, localDE=j,                    &
                       farrayPtr=ptr_sst_input, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,  &
        line=__LINE__, file=FILENAME)) return
!
!   ! Set initial value to missing 
    !! ptr_sst = MISSING_R8
    !! ptr_uoce = MISSING_R8
    !! ptr_voce = MISSING_R8
    ! DO NOT initialize the initial sst?
!
!   ! Put data to export field 
    bi = 1
    bj = 1
!
    do jj = 1, sNy
      do ii = 1, sNx
        iG = myXGlobalLo-1+(bi-1)*sNx+ii
        jG = myYGlobalLo-1+(bj-1)*sNy+jj
        !! Make sure that theta + 273.15 is the true SST number
        if (ptr_mask(iG,jG) > 0.01) then
          !! if ocean, use theta_ESMF in MITgcm
          ptr_sst(iG,jG) = theta_ESMF(ii,jj,1,1,1) + 273.15
          ptr_uoce(iG,jG) = uoce_ESMF(ii,jj,1,1,1)
          ptr_voce(iG,jG) = voce_ESMF(ii,jj,1,1,1)
          ptr_ocnmask(iG,jG) = 1.d0
        else
          !! if land, use initial SST and zero current
          ptr_sst(iG,jG) = ptr_sst_input(iG,jG)
          ptr_uoce(iG,jG) = 0.d0
          ptr_voce(iG,jG) = 0.d0
          ptr_ocnmask(iG,jG) = 0.d0
        end if
      end do
    end do
!
!   ! Write field to debug
    if (debugLevel >= 1) then
      write (ofile, "(A9,I6.6,A3)") "sstOCNput", iLoop, ".nc"
      call ESMF_FieldWrite(field_sst, trim(ofile), rc=rc)
    end if

!   ! Nullify the pointer
    if (associated(ptr_sst)) then
      nullify(ptr_sst)
    end if
    if (associated(ptr_uoce)) then
      nullify(ptr_uoce)
    end if
    if (associated(ptr_voce)) then
      nullify(ptr_voce)
    end if
    if (associated(ptr_mask)) then
      nullify(ptr_mask)
    end if
    if (associated(ptr_ocnmask)) then
      nullify(ptr_ocnmask)
    end if
!
  end do
!
  DEALLOCATE(theta_ESMF)
  DEALLOCATE(uoce_ESMF)
  DEALLOCATE(voce_ESMF)
!
  end subroutine OCN_Put
!
  end module mod_esmf_ocn

