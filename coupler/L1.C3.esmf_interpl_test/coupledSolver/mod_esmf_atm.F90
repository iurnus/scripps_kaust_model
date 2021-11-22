! Earth System Modeling Framework
! Copyright 2002-2019, University Corporation for Atmospheric Research,
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
  integer, intent(out) :: rc
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

  TYPE(ESMF_GridComp) :: gcomp
  TYPE(ESMF_State)    :: importState
  TYPE(ESMF_State)    :: exportState
  TYPE(ESMF_Clock)    :: clock

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
  type(ESMF_ArraySpec) :: arrayspec
!
  type(ESMF_VM) :: vm
  
  rc = ESMF_SUCCESS

  call ESMF_GridCompGet(gcomp, name=gname, vm=vm, rc=rc)
  call ESMF_ArraySpecSet(arrayspec, 2, ESMF_TYPEKIND_R8, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
         line=__LINE__, file=FILENAME)) return
  call ESMF_VMGet(vm, localPet=localPet, petCount=petCount,         &
                  mpiCommunicator=comm, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
              line=__LINE__, file=FILENAME)) return

  PRINT *, "setting atm grid arrays..."
  call ATM_SetGridArrays(gcomp, petCount, localPet, atmGridIn,rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
      line=__LINE__, file=FILENAME)) return
  atmGridOut = atmGridIn

  field = ESMF_FieldCreate(name="sst", grid=atmGridIn, arrayspec=arrayspec, &
                           staggerLoc=ESMF_STAGGERLOC_CENTER, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
      line=__LINE__, file=FILENAME)) return
  call NUOPC_Realize(importState, field=field, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
      line=__LINE__, file=FILENAME)) return

  field = ESMF_FieldCreate(name="rsns", grid=atmGridOut, arrayspec=arrayspec, &
                            staggerLoc=ESMF_STAGGERLOC_CENTER, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
      line=__LINE__, file=FILENAME)) return
  call NUOPC_Realize(exportState, field=field, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
      line=__LINE__, file=FILENAME)) return

  PRINT *, "setting atm init data"
  call ATM_SetInitData(gcomp, atmGridIn, atmGridOut, localPet, rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
      line=__LINE__, file=FILENAME)) return

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

  call ESMF_TimeIntervalSet(stabilityTimeStep, m=1, rc=rc)
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

  TYPE(ESMF_GridComp)    :: gcomp
  INTEGER, INTENT(  OUT) :: rc

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
  real(ESMF_KIND_R8), pointer :: ptrXc(:,:), ptrYc(:,:)
  real(ESMF_KIND_R8), pointer :: ptrE1(:,:), ptrE2(:,:)
  type(ESMF_Array) :: arrX, arrY
  type(ESMF_StaggerLoc) :: staggerLoc
  type(ESMF_DistGrid) :: distGrid
  integer, allocatable :: meshType(:)
  character(ESMF_MAXSTR), allocatable :: meshTypeName(:)
  integer :: localDECount, j
  character(ESMF_MAXSTR) ::  ofile
  integer :: clbnd(2),cubnd(2)
  integer :: clbndc(2),cubndc(2)

  integer :: nx = 10
  integer :: ny = 10
  real(ESMF_KIND_R8) :: x_max = 179.0, y_max = 80.0
  real(ESMF_KIND_R8) :: x_min =-180.0, y_min =-80.0
  real(ESMF_KIND_R8) :: dst_dx, dst_dy
  integer :: localN 
  character(160)  :: msgString
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

!
!-----------------------------------------------------------------------
!     Create ESMF DistGrid based on model domain decomposition
!-----------------------------------------------------------------------
!
  distGrid = ESMF_DistGridCreate(minIndex=(/ 1, 1/),               &
                                 maxIndex=(/nx,ny/),             &
                                 rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                         line=__LINE__, file=FILENAME)) return
!
!-----------------------------------------------------------------------
!     Create ESMF Grid
!-----------------------------------------------------------------------
!
  ! Icross
  ! gridIn = ESMF_GridCreate(distgrid=distGrid,                       &
  !                          indexflag=ESMF_INDEX_GLOBAL,             &
  !                          ! coordSys=ESMF_COORDSYS_SPH_DEG,          &
  !                          name="atm_grid",                         &
  !                          rc=rc)
  gridIn = ESMF_GridCreateNoPeriDim(minIndex=(/1,1/),maxIndex=(/nx,ny/), &
                           regDecomp=(/petCount,1/), &
                           coordSys=ESMF_COORDSYS_SPH_DEG, &
                           indexflag=ESMF_INDEX_GLOBAL, rc = rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                         line=__LINE__, file=FILENAME)) return
!
!-----------------------------------------------------------------------
!     Allocate coordinates 
!-----------------------------------------------------------------------
!
  call ESMF_GridAddCoord(gridIn, staggerLoc=ESMF_STAGGERLOC_CENTER, rc=rc)
  call ESMF_GridAddCoord(gridIn, staggerLoc=ESMF_STAGGERLOC_CORNER, rc=rc)
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
                         computationalLBound=clbnd, computationalUBound=cubnd, & 
                         coordDim=1, farrayPtr=ptrX, rc=rc)
    call ESMF_GridGetCoord(gridIn, staggerLoc=staggerLoc, localDE=j,&
                         computationalLBound=clbnd, computationalUBound=cubnd, & 
                         coordDim=2, farrayPtr=ptrY, rc=rc)
    call ESMF_GridGetCoord(gridIn, staggerLoc=ESMF_STAGGERLOC_CORNER, localDE=j,&
                         computationalLBound=clbndc, computationalUBound=cubndc, & 
                         coordDim=1, farrayPtr=ptrXc, rc=rc)
    call ESMF_GridGetCoord(gridIn, staggerLoc=ESMF_STAGGERLOC_CORNER, localDE=j,&
                         computationalLBound=clbndc, computationalUBound=cubndc, & 
                         coordDim=2, farrayPtr=ptrYc, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,  &
                         line=__LINE__, file=FILENAME)) return
!
!-----------------------------------------------------------------------
!     Fill the pointers    
!-----------------------------------------------------------------------
!
    dst_dx = 359./nx
    dst_dy = 160./ny
    do n = clbnd(1),cubnd(1)
      do m = clbnd(2),cubnd(2)
        !! TODO: pseudo mesh
        ptrX(n,m) = -180. + (REAL(n-1)*dst_dx) + 0.5*dst_dx
        ptrY(n,m) =  -80. + (REAL(m-1)*dst_dy) + 0.5*dst_dy
        write (msgString,*) 'atmosphere center: ', n, m, dst_dx, dst_dy, ptrX(n,m), ptrY(n,m)
        call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)
      end do
    end do

    do n = clbndc(1),cubndc(1)
      do m = clbndc(2),cubndc(2)
        !! TODO: pseudo mesh
        ptrXc(n,m) = -180. + (REAL(n-1)*dst_dx)
        ptrYc(n,m) =  -80. + (REAL(m-1)*dst_dy)
        write (msgString,*) 'atmosphere corner: ', n, m, ptrXc(n,m), ptrYc(n,m)
        call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)
      end do
    end do
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
    if (associated(ptrYc)) then
      nullify(ptrYc)
    end if
    if (associated(ptrXc)) then
      nullify(ptrXc)
    end if
    if (associated(ptrE1)) then
      nullify(ptrE1)
    end if
    if (associated(ptrE2)) then
      nullify(ptrE2)
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
      call ESMF_ArrayWrite(arrX, fileName="atm_xa.nc",              &
                           status=ESMF_FILESTATUS_NEW, rc=rc)
      call ESMF_ArrayWrite(arrY, fileName="atm_ya.nc",              &
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
  subroutine ATM_SetInitData(gcomp, gridIn, gridOut, localPet, rc)
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
  type(ESMF_Grid) :: gridIn
  type(ESMF_Grid) :: gridOut
  integer, intent(inout) :: rc
  integer :: localPet
  type(ESMF_VM) :: vm

  type(ESMF_State) :: exportState
  type(ESMF_State) :: importState
  integer :: myThid = 1
  integer :: k, m, n, p, iG, jG
  character(ESMF_MAXSTR) :: name
  real(ESMF_KIND_R8), pointer :: ptrX(:,:), ptrY(:,:)
  real(ESMF_KIND_R8), pointer :: ptrIn(:,:)
  real(ESMF_KIND_R8), pointer :: ptrOut(:,:)
  type(ESMF_Array) :: arrX, arrY
  type(ESMF_StaggerLoc) :: staggerLoc
  type(ESMF_DistGrid) :: distGrid
  integer, allocatable :: meshType(:)
  character(ESMF_MAXSTR), allocatable :: meshTypeName(:)
  type(ESMF_Field) :: fieldIn
  type(ESMF_Field) :: fieldOut
  integer :: clbnd(2),cubnd(2)

  real(ESMF_KIND_R8) :: theta, phi, lat, lon, x, y, z
  real(ESMF_KIND_R8) :: DEG2RAD, RAD2DEG
  real(ESMF_KIND_R8) :: dst_dx, dst_dy
  integer :: ii, jj
  integer :: nx = 10
  integer :: ny = 10
  integer :: localN
  character(ESMF_MAXSTR) :: cname, ofile
  character(160)  :: msgString
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
  PRINT *, "setting atm init data..."
  call ESMF_GridCompGet(gcomp, vm=vm, name=cname,                   &
                        exportState=exportState,                    &
                        importState=importState,                    &
                        rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
      line=__LINE__, file=FILENAME)) return

  call ESMF_GridGet(gridIn, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
      line=__LINE__, file=FILENAME)) return

  call ESMF_StateGet(importState, "sst", fieldIn, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
      line=__LINE__, file=FILENAME)) return

  call ESMF_FieldGet(fieldIn, farrayPtr=ptrIn, &
              computationalLBound=clbnd, computationalUBound=cubnd, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
      line=__LINE__, file=FILENAME)) return

  do ii = clbnd(1),cubnd(1)
    do jj = clbnd(2),cubnd(2)
      ptrIn(ii,jj) = 0.0
    end do
  end do

  call ESMF_StateGet(exportState, "rsns", fieldOut, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
      line=__LINE__, file=FILENAME)) return

  call ESMF_FieldGet(fieldOut, farrayPtr=ptrOut, &
              computationalLBound=clbnd, computationalUBound=cubnd, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
      line=__LINE__, file=FILENAME)) return

  DEG2RAD = 3.14159265/180.0
  RAD2DEG = 1./DEG2RAD
  dst_dx = 359./nx
  dst_dy = 160./ny
  do ii = clbnd(1),cubnd(1)
    do jj = clbnd(2),cubnd(2)
      lon = -180. + (REAL(ii-1)*dst_dx) + 0.5*dst_dx
      lat =  -80. + (REAL(jj-1)*dst_dy) + 0.5*dst_dy
      theta = DEG2RAD*(lon)
      phi = DEG2RAD*(90.-lat)
      x = cos(theta)*sin(phi)
      y = sin(theta)*sin(phi)
      z = cos(phi)
      ptrOut(ii,jj) = x+y+z+15

      ! ptrOut(ii,jj) = 0.1d0*ii*ii + 1.0d0*ii
      write (msgString,*) 'atm out: ', ii, jj, ptrOut(ii,jj)
      call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)
    end do
  end do

  if (debugLevel >= 1) then
    write (ofile, "(A10)") "rsnsATM.nc"
    call ESMF_FieldWrite(fieldOut, trim(ofile), rc=rc)
  end if

  end subroutine ATM_SetInitData

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
    write (ofile, "(A6,I6.6,A3)") "sstATM", iLoop, ".nc"
    call ESMF_FieldWrite(field, trim(ofile), rc=rc)
  end if

  end subroutine ATM_Get

end module mod_esmf_atm
