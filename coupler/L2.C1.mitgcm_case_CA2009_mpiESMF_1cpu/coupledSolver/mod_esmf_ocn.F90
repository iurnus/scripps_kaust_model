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
      
      rc = ESMF_SUCCESS

      call NUOPC_Advertise(importState,                                 &
          StandardName="air_pressure_at_sea_level", name="pmsl", rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
          line=__LINE__, file=FILENAME)) return

      call NUOPC_Advertise(importState,                                 &
          StandardName="surface_net_downward_shortwave_flux",           &
          name="rsns", rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
          line=__LINE__, file=FILENAME)) return

      call NUOPC_Advertise(exportState,                                 &
          StandardName="sea_surface_temperature", name="sst", rc=rc)
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
!-----------------------------------------------------------------------
!     Local variable declaration
!-----------------------------------------------------------------------
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
!-----------------------------------------------------------------------
!     Get gridded component
!-----------------------------------------------------------------------
!
      call ESMF_GridCompGet(gcomp, name=gname, vm=vm, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
          line=__LINE__, file=FILENAME)) return

      call ESMF_VMGet(vm, localPet=localPet, petCount=petCount,         &
                      mpiCommunicator=comm, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
          line=__LINE__, file=FILENAME)) return
!
!-----------------------------------------------------------------------
!     Initialize the gridded component
!-----------------------------------------------------------------------
!
      call MIT_INIT(myThid, comm)
!
!-----------------------------------------------------------------------
!     Set-up grid and load coordinate data
!-----------------------------------------------------------------------
!
      call OCN_SetGridArrays(gcomp, petCount, localPet, ocnGridIn, rc)
      !! call OCN_PRINTGridArrays(gcomp, petCount, localPet, ocnGridIn, rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
          line=__LINE__, file=FILENAME)) return
      ocnGridOut = ocnGridIn

      field = ESMF_FieldCreate(name="pmsl", grid=ocnGridIn,             &
        typekind=ESMF_TYPEKIND_R8, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
          line=__LINE__, file=FILENAME)) return
      call NUOPC_Realize(importState, field=field, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
          line=__LINE__, file=FILENAME)) return

      field = ESMF_FieldCreate(name="rsns", grid=ocnGridIn,             &
        typekind=ESMF_TYPEKIND_R8, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
          line=__LINE__, file=FILENAME)) return
      call NUOPC_Realize(importState, field=field, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
          line=__LINE__, file=FILENAME)) return

      field = ESMF_FieldCreate(name="sst", grid=ocnGridOut,             &
        typekind=ESMF_TYPEKIND_R8, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
          line=__LINE__, file=FILENAME)) return
      call NUOPC_Realize(exportState, field=field, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
          line=__LINE__, file=FILENAME)) return
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
      type(ESMF_TimeInterval) :: stabilityTimeStep
      integer :: comm, localPet, petCount

      type(ESMF_VM) :: vm
      
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
      subroutine OCN_Run(gcomp, rc)

      type(ESMF_GridComp)  :: gcomp
      integer, intent(out) :: rc

      type(ESMF_Clock)     :: clock
      type(ESMF_State)     :: importState, exportState
      
      integer :: i, j, maxdiv, runid, localPet, petCount
      integer :: myThid
      integer :: iLoopOCN = 1
      integer :: myIter = 0
      real*8 :: myTime = 0d0
      integer :: nTimeStepsIn = 1
      character(ESMF_MAXSTR) :: cname
      
      type(ESMF_VM) :: vm
      type(ESMF_Time) :: startTime
      type(ESMF_Time) :: stopTime
      type(ESMF_TimeInterval) :: timeStep
      type(ESMF_Clock) :: internalClock
      
      rc = ESMF_SUCCESS

      call NUOPC_ModelGet(gcomp, modelClock=clock,                      &
                          importState=importState,                      &
                          exportState=exportState, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
          line=__LINE__, file=FILENAME)) return

      print *, "calling OCN_Run function"
      print *, "OCN iLoop is: ", iLoopOCN
      print *, "myTime is: ", myTime
      print *, "myIter is: ", myIter
      print *, "nTimeStepsIn is: ", nTimeStepsIn

      call OCN_Get(gcomp, iLoopOCN, rc)
      call mit_run(iLoopOCN, myTime, myIter, nTimeStepsIn, myThid)
      call OCN_Put(gcomp, iLoopOCN, rc)

      iLoopOCN = iLoopOCN + 1

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
!-----------------------------------------------------------------------
!     Used module declarations 
!-----------------------------------------------------------------------
!
      use mitgcm_org_ocn, only : get_grid_parameters
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
!-----------------------------------------------------------------------
!     Local variable declarations 
!-----------------------------------------------------------------------
!
      rc = ESMF_SUCCESS
      call get_domain_size(sNx, sNy, OLx, OLy,                          &
            nSx, nSy, nPx, nPy, Nx, Ny, Nr,                             &
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
        yC_ESMF, yG_ESMF, maskC_ESMF, maskS_ESMF, maskW_ESMF, myThid)
!
!-----------------------------------------------------------------------
!     Get gridded component 
!-----------------------------------------------------------------------
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
!-----------------------------------------------------------------------
!     Get limits of the grid arrays (based on PET and nest level)
!-----------------------------------------------------------------------
!
      if (.not.allocated(deBlockList)) then
        allocate(deBlockList(2,2,0:nPx*nPy-1))
      end if
!
      bj = 1
      bi = 1
      do tile = 0, (nPx*nPy)-1
        deBlockList(1,1,tile) = mpi_myXGlobalLo(tile+1)
        deBlockList(1,2,tile) = mpi_myXGlobalLo(tile+1)+sNx-1
        deBlockList(2,1,tile) = mpi_myYGlobalLo(tile+1) 
        deBlockList(2,2,tile) = mpi_myYGlobalLo(tile+1)+sNy-1
      end do
!
!-----------------------------------------------------------------------
!     Create ESMF DistGrid based on model domain decomposition
!-----------------------------------------------------------------------
!
      distGrid = ESMF_DistGridCreate(minIndex=(/ 1, 1 /),               &
                                     maxIndex=(/ Nx, Ny /),             &
                                     deBlockList=deBlockList,           &
                                     rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
!
!-----------------------------------------------------------------------
!     Set staggering type 
!-----------------------------------------------------------------------
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
                               name="ocn_grid",                         &
                               rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
!
!-----------------------------------------------------------------------
!     Allocate coordinates 
!-----------------------------------------------------------------------
!
      call ESMF_GridAddCoord(gridIn,                                    &
                             staggerLoc=staggerLoc,                     &
                             rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
!
      call ESMF_GridAddItem(gridIn,                                     &
                            staggerLoc=staggerLoc,                      &
                            itemflag=ESMF_GRIDITEM_MASK,                &
                            rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
!
      call ESMF_GridAddItem(gridIn,                                     &
                            staggerLoc=staggerLoc,                      &
                            itemflag=ESMF_GRIDITEM_AREA,                &
                            rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
!
      call ESMF_GridGet(gridIn, localDECount=localDECount, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
!
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
            ptrX(iG,jG) = xC_ESMF(m,n,1,1)
            ptrY(iG,jG) = yC_ESMF(m,n,1,1)
            ptrM(iG,jG) = int(maskC_ESMF(m,n,1,1,1))
            ptrA(iG,jG) = rA_ESMF(m,n,1,1)
            !! PRINT *, "iG, jG, m, n values are: ", iG, jG, m, n
            !! PRINT *, "ocean mesh ptrX(iG, jG) is: ", ptrX(iG, jG)
            !! PRINT *, "ocean mesh ptrY(iG, jG) is: ", ptrY(iG, jG)
          end do
        end do
!
!       ! Nullify pointers 
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
!-----------------------------------------------------------------------
!     Write the grid to debug
!-----------------------------------------------------------------------
! 
      if (debugLevel >= 1) then
        call ESMF_GridGetCoord(gridIn,                                  &
                               staggerLoc=ESMF_STAGGERLOC_CENTER,       &
                               coordDim=1, array=arrX, rc=rc)
        call ESMF_GridGetCoord(gridIn,                                  &
                               staggerLoc=ESMF_STAGGERLOC_CENTER,       &
                               coordDim=2, array=arrY, rc=rc)
        call ESMF_GridWriteVTK(gridIn, filename="ocean_mesh_1",         &
                               staggerLoc=staggerLoc, rc=rc)
        call ESMF_ArrayWrite(arrX, filename="ocean_xa_1.nc",            &
                             status=ESMF_FILESTATUS_NEW, rc=rc)
        call ESMF_ArrayWrite(arrY, filename="ocean_ya_1.nc",            &
                             status=ESMF_FILESTATUS_NEW, rc=rc)
      end if
!
!-----------------------------------------------------------------------
!     Assign grid to gridded component 
!-----------------------------------------------------------------------
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
!-----------------------------------------------------------------------
!     Local variable declarations 
!-----------------------------------------------------------------------
!
      integer :: i, j, k, itemCount, localDECount, localPet, petCount
      character(ESMF_MAXSTR), allocatable :: itemNameList(:)
      real*8, dimension(:,:), pointer :: ptr2d
!
      INTEGER sNx, sNy, OLx, OLy, nSx, nSy, nPx, nPy, Nx, Ny, Nr
      INTEGER myXGlobalLo, myYGlobalLo
!
      type(ESMF_VM) :: vm
      type(ESMF_Field) :: field
      type(ESMF_ArraySpec) :: arraySpec
      type(ESMF_StaggerLoc) :: staggerLoc 
      type(ESMF_State) :: importState, exportState
!
!-----------------------------------------------------------------------
!     Get information about gridded component 
!-----------------------------------------------------------------------
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
!-----------------------------------------------------------------------
!     Set array descriptor
!-----------------------------------------------------------------------
!
      call ESMF_ArraySpecSet(arraySpec, typekind=ESMF_TYPEKIND_R8,      &
                             rank=2, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
!
!-----------------------------------------------------------------------
!     Get number of local DEs
!-----------------------------------------------------------------------
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
!-----------------------------------------------------------------------
!     Create export field 
!-----------------------------------------------------------------------
!
      field = ESMF_FieldCreate(gridOut,                                 &
                               arraySpec,                               &
                               staggerloc=staggerLoc,                   &
                               indexflag=ESMF_INDEX_GLOBAL,             &
                               name="sst",                              &
                               rc=rc)

      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
!
!-----------------------------------------------------------------------
!     Put initial data into state 
!-----------------------------------------------------------------------
! 
      do j = 0, localDECount-1
!
!       ! Get pointer from field 
        call ESMF_FieldGet(field, localDe=j, farrayPtr=ptr2d, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,  &
                             line=__LINE__, file=FILENAME)) return
!
!       ! Initialize pointer 
        ptr2d = MISSING_R8
!
!       ! Nullify the pointer
        if (associated(ptr2d)) then
          nullify(ptr2d)
        end if
!
      end do
!
!-----------------------------------------------------------------------
!     Add field export state
!-----------------------------------------------------------------------
!
      call NUOPC_Realize(exportState, field=field, rc=rc) 
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
!
!-----------------------------------------------------------------------
!     Set staggering type 
!-----------------------------------------------------------------------
!
      staggerLoc = ESMF_STAGGERLOC_CENTER
!
!-----------------------------------------------------------------------
!     Create import field
!-----------------------------------------------------------------------
!
      call get_domain_size(sNx, sNy, OLx, OLy,                          &
            nSx, nSy, nPx, nPy, Nx, Ny, Nr,                             &
            myXGlobalLo, myYGlobalLo)

      !! TODO: check olx oly meaning
      field = ESMF_FieldCreate(gridIn,                                  &
                               arraySpec,                               &
                               totalLWidth=(/OLx,OLy/),                 &
                               totalUWidth=(/OLx,OLy/),                 &
                               staggerloc=staggerLoc,                   &
                               indexflag=ESMF_INDEX_GLOBAL,             &
                               name='pmsl',                             &
                               rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
!
!-----------------------------------------------------------------------
!     Put data into state 
!-----------------------------------------------------------------------
! 
      do j = 0, localDECount-1
!
!       ! Get pointer from field 
        call ESMF_FieldGet(field, localDe=j, farrayPtr=ptr2d, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,  &
                               line=__LINE__, file=FILENAME)) return
!
!       ! Initialize pointer 
        ptr2d = MISSING_R8
!
!       ! Nullify the pointer
        if (associated(ptr2d)) then
          nullify(ptr2d)
        end if
!
      end do
!
!-----------------------------------------------------------------------
!     Add field import state
!-----------------------------------------------------------------------
!
      call NUOPC_Realize(importState, field=field, rc=rc) 
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
!
      field = ESMF_FieldCreate(gridIn,                                  &
                               arraySpec,                               &
                               totalLWidth=(/OLx,OLy/),                 &
                               totalUWidth=(/OLx,OLy/),                 &
                               staggerloc=staggerLoc,                   &
                               indexflag=ESMF_INDEX_GLOBAL,             &
                               name='psns',                             &
                               rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
!
!-----------------------------------------------------------------------
!     Put data into state 
!-----------------------------------------------------------------------
! 
      do j = 0, localDECount-1
!
!       ! Get pointer from field 
        call ESMF_FieldGet(field, localDe=j, farrayPtr=ptr2d, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,  &
                               line=__LINE__, file=FILENAME)) return
!
!       ! Initialize pointer 
        ptr2d = MISSING_R8
!
!       ! Nullify the pointer
        if (associated(ptr2d)) then
          nullify(ptr2d)
        end if
!
      end do
!
!-----------------------------------------------------------------------
!     Add field import state
!-----------------------------------------------------------------------
!
      call NUOPC_Realize(importState, field=field, rc=rc) 
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
!
      end subroutine OCN_SetStates 
 
      subroutine OCN_Get(gcomp, iLoop, rc)
!
!-----------------------------------------------------------------------
!     Used module declarations 
!-----------------------------------------------------------------------
!
      use mitgcm_org_ocn, only : get_field_parameters
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
      integer :: i, j, ii, jj, bi, bj, iG, jG, imax, jmax
      integer :: id, iyear, iday, imonth, ihour, iunit
      integer :: LBi, UBi, LBj, UBj
      integer :: localPet, petCount, itemCount, localDECount
      character(ESMF_MAXSTR) :: cname, ofile
      real(ESMF_KIND_R8) :: sfac, addo
      real(ESMF_KIND_R8), pointer :: ptr(:,:)
!
      type(ESMF_VM) :: vm
      type(ESMF_Clock) :: clock
      type(ESMF_Grid) :: gridIn
      type(ESMF_Field) :: field
      type(ESMF_Field) :: field2
      type(ESMF_State) :: importState
      INTEGER sNx, sNy, OLx, OLy, nSx, nSy, nPx, nPy, Nx, Ny, Nr
      INTEGER myXGlobalLo, myYGlobalLo
      REAL*8, DIMENSION(:,:,:,:), ALLOCATABLE :: apressure_ESMF
      integer :: myThid = 1
!
      rc = ESMF_SUCCESS
!
!-----------------------------------------------------------------------
!     Get gridded component 
!-----------------------------------------------------------------------
!
      call get_domain_size(sNx, sNy, OLx, OLy,                          &
            nSx, nSy, nPx, nPy, Nx, Ny, Nr,                             &
            myXGlobalLo, myYGlobalLo)

      ALLOCATE(apressure_ESMF(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy))

      call ESMF_GridCompGet(gcomp, name=cname, clock=clock, grid=gridIn,&
                            importState=importState, vm=vm, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
!
      call ESMF_VMGet(vm, localPet=localPet, petCount=petCount, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
!
!-----------------------------------------------------------------------
!     Get number of local DEs
!-----------------------------------------------------------------------
! 
      call ESMF_GridGet(gridIn, localDECount=localDECount, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
!
!-----------------------------------------------------------------------
!     Get field
!-----------------------------------------------------------------------
!
      call ESMF_StateGet(importState, 'pmsl', field, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return

      call ESMF_StateGet(importState, 'rsns', field2, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
!
!-----------------------------------------------------------------------
!     Loop over decomposition elements (DEs) 
!-----------------------------------------------------------------------
!
      do j = 0, localDECount-1
!
!       ! Get pointer /from field
        call ESMF_FieldGet(field, localDE=j, farrayPtr=ptr, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,  &
                             line=__LINE__, file=FILENAME)) return
!
!       ! Put data to OCN component variable
        sfac = 1.0d0
        addo = 0.0d0
!
        bi = 1
        bj = 1
        imax = Nx+1
        jmax = Ny+1
!
        where (ieee_is_nan(ptr)) ptr = MISSING_R8
        ! ptr = MISSING_R8
!
        do jj = 1-OLy, sNy+OLy
          do ii = 1-OLx, sNx+OLx
            iG = myXGlobalLo-1+(bi-1)*sNx+ii
            jG = myYGlobalLo-1+(bj-1)*sNy+jj
            if ((iG > 0 .and. iG < imax) .and.                          &
                (jG > 0 .and. jG < jmax) .and. ptr(iG,jG) < TOL_R8) then
              !! apressure_ESMF(ii,jj,1,1) = (ptr(iG,jG)*sfac)+addo
              !! TODO:: not really reading data
              apressure_ESMF(ii,jj,1,1) = 0.0d0
              !! print *, "iG, jG, ptr(iG, jG) are: ", iG, jG, ptr(iG, jG)
            end if
          end do
        end do
!
!       ! Nullify the pointer
        if (associated(ptr)) then
          nullify(ptr)
        end if
!
      end do       
!
      ! call get_field_parameters(apressure_ESMF, myThid)
!
      if (debugLevel >= 1) then
        write (ofile, "(A12,I2.2,A3)") "apressureOCN", iLoop, ".nc"
        call ESMF_FieldWrite(field, trim(ofile), rc=rc)
        write (ofile, "(A7,I2.2,A3)") "rsnsOCN", iLoop, ".nc"
        call ESMF_FieldWrite(field2, trim(ofile), rc=rc)
      end if

      DEALLOCATE(apressure_ESMF)

      end subroutine OCN_Get

      subroutine OCN_Put(gcomp, iLoop, rc)
!
!-----------------------------------------------------------------------
!     Used module declarations 
!-----------------------------------------------------------------------
!
      use mitgcm_org_ocn, only : get_theta_parameters
      use mitgcm_org_ocn, only : get_grid_parameters
!
      implicit none
!
!-----------------------------------------------------------------------
!     Imported variable declarations 
!-----------------------------------------------------------------------
!
      type(ESMF_GridComp) :: gcomp
      type(ESMF_Grid) :: gridOut
      integer, intent(out) :: rc
      integer :: iLoop
!
!-----------------------------------------------------------------------
!     Local variable declarations 
!-----------------------------------------------------------------------
!
      integer :: bi, bj, iG, jG, imax, jmax
      integer :: i, j, ii, jj, iunit, iyear, iday, imonth, ihour
      integer :: petCount, localPet, itemCount, localDECount
      character(ESMF_MAXSTR) :: cname, ofile
      real(ESMF_KIND_R8), pointer :: ptr(:,:)
!
      type(ESMF_VM) :: vm
      type(ESMF_Clock) :: clock
      type(ESMF_Field) :: field
      type(ESMF_State) :: exportState
      REAL*8, DIMENSION(:,:,:,:,:), ALLOCATABLE :: theta_ESMF
!
      INTEGER sNx, sNy, OLx, OLy, nSx, nSy, nPx, nPy, Nx, Ny, Nr
      INTEGER myXGlobalLo, myYGlobalLo
      integer :: myThid = 1
!
      rc = ESMF_SUCCESS
!
!-----------------------------------------------------------------------
!     Get ESMF domain size info
!-----------------------------------------------------------------------
!
      call get_domain_size(sNx, sNy, OLx, OLy,                          &
            nSx, nSy, nPx, nPy, Nx, Ny, Nr,                             &
            myXGlobalLo, myYGlobalLo)

      ALLOCATE(theta_ESMF(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy))

      call get_theta_parameters(theta_ESMF, myThid)
!
!-----------------------------------------------------------------------
!     Get gridded component 
!-----------------------------------------------------------------------
!
      call ESMF_GridCompGet(gcomp, name=cname, clock=clock, &
                            grid=gridOut, &
                            exportState=exportState, vm=vm, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
!
      call ESMF_VMGet(vm, localPet=localPet, petCount=petCount, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
!
!-----------------------------------------------------------------------
!     Get number of local DEs
!-----------------------------------------------------------------------
! 
      !! TODO: check if gridin being used correctly
      call ESMF_GridGet(gridOut, localDECount=localDECount, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
!
!-----------------------------------------------------------------------
!     Get export field 
!-----------------------------------------------------------------------
!
      call ESMF_StateGet(exportState, "sst", field, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
!
!-----------------------------------------------------------------------
!     Loop over decomposition elements (DEs) 
!-----------------------------------------------------------------------
!
      do j = 0, localDECount-1

!       ! Get pointer 
        call ESMF_FieldGet(field, localDE=j, farrayPtr=ptr, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,  &
                             line=__LINE__, file=FILENAME)) return
!
!       ! Set initial value to missing 
        ptr = MISSING_R8
!
!       ! Put data to export field 
        bi = 1
        bj = 1
        imax = Nx+1
        jmax = Ny+1
!
        do jj = 1, sNy
          do ii = 1, sNx
            iG = myXGlobalLo-1+(bi-1)*sNx+ii
            jG = myYGlobalLo-1+(bj-1)*sNy+jj
            ptr(iG,jG) = theta_ESMF(ii,jj,1,1,1)
          end do
        end do
!
!       ! Write field to debug
        if (debugLevel >= 1) then
          write (ofile, "(A6,I2.2,A3)") "sstOCN", iLoop, ".nc"
          call ESMF_FieldWrite(field, trim(ofile), rc=rc)
        end if

!       ! Nullify the pointer
        if (associated(ptr)) then
          nullify(ptr)
        end if
!
      end do
!
      DEALLOCATE(theta_ESMF)
!
      end subroutine OCN_Put

      end module mod_esmf_ocn

