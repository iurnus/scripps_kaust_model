!-----------------------------------------------------------------------
!
!     This file is part of ITU RegESM.
!
!     ITU RegESM is free software: you can redistribute it and/or modify
!     it under the terms of the GNU General Public License as published by
!     the Free Software Foundation, either version 3 of the License, or
!     (at your option) any later version.
!
!     ITU RegESM is distributed in the hope that it will be useful,
!     but WITHOUT ANY WARRANTY; without even the implied warranty of
!     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!     GNU General Public License for more details.
!
!     You should have received a copy of the GNU General Public License
!     along with ITU RegESM.  If not, see <http://www.gnu.org/licenses/>.
!
!-----------------------------------------------------------------------
#define FILENAME "mod_esmf_wav.F90"
!
!-----------------------------------------------------------------------
!     WAV gridded component code 
!-----------------------------------------------------------------------
!
      module mod_esmf_wav
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
      use ww3_esmf, only : w3_test
      use ww3_esmf, only : w3_init
      use ww3_esmf, only : w3_run
      use ieee_arithmetic, only : ieee_is_nan
!
      implicit none
      private
!
!-----------------------------------------------------------------------
!     Public subroutines 
!-----------------------------------------------------------------------
!
      public :: WAV_SetServices
!
      contains
!
      subroutine WAV_SetServices(gcomp, rc)
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
                                   userRoutine=WAV_Init1, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
          line=__LINE__, file=FILENAME)) return

      call NUOPC_CompSetEntryPoint(gcomp, ESMF_METHOD_INITIALIZE,       &
                                   phaseLabelList=(/"IPDv00p2"/),       &
                                   userRoutine=WAV_Init2, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
          line=__LINE__, file=FILENAME)) return

      call NUOPC_CompSpecialize(gcomp, specLabel=NUOPC_Label_SetClock,  &
                                specRoutine=WAV_SetClock, rc = rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
          line=__LINE__, file=FILENAME)) return

      call NUOPC_CompSpecialize(gcomp, specLabel=NUOPC_Label_Advance,   &
                                specRoutine=WAV_Run, rc = rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
          line=__LINE__, file=FILENAME)) return

      end subroutine WAV_SetServices
!
!-----------------------------------------------------------------------
!     Initialization phase 1, set import/export fields
!-----------------------------------------------------------------------
!
      subroutine WAV_Init1(gcomp, importState, exportState, clock, rc)

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
      character(ESMF_MAXSTR) :: entryNameNUOPC, entryNameWRF
      integer :: iEntry
      logical :: exportEntry
      
      rc = ESMF_SUCCESS

      ! register the WRF entries in ESMF
      do iEntry = 1, nList
        entryNameNUOPC = trim(nuopc_entryNameList(iEntry));
        entryNameWRF = trim(wrf_nameList(iEntry));
        exportEntry = fromWAV(iEntry);
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
      subroutine WAV_Init2(gcomp, importState, exportState, clock, rc)

      use ww3_esmf, only : get_domain_size

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
      type(ESMF_Field) :: field_tmp
      type(ESMF_DistGrid) :: distGrid
      type(ESMF_Grid) :: wavGridIn
      type(ESMF_Grid) :: wavGridOut

      integer :: myThid = 1
      integer :: comm, localPet, petCount
      character(ESMF_MAXSTR) :: gname
      INTEGER :: timeI(2)
      integer :: meshNX, meshNY, IP, NP, tile, iterI
      integer :: iX, iY, nX_per, nY_per, nX_rem, nY_rem
      character(ESMF_MAXSTR) :: entryNameNUOPC, entryNameWRF
      integer :: iEntry
      logical :: exportEntry
      type(ESMF_ArraySpec) :: arraySpec
      type(ESMF_StaggerLoc) :: staggerLoc
      TYPE(ESMF_StateIntent_Flag) :: stateintent
      INTEGER :: itemCount,i
      CHARACTER (ESMF_MAXSTR), ALLOCATABLE :: itemNames(:)
      TYPE(ESMF_StateItem_Flag), ALLOCATABLE :: itemTypes(:)

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

      ! PRINT *, 'running wave watch 3...'
      call w3_test
      call w3_init(comm)
      ! PRINT *, 'wave watch 3 run finished...'

      call get_domain_size(meshNX, meshNY, IP, NP)

      allocate(waveXLow(0:cpuWAV-1))
      allocate(waveYLow(0:cpuWAV-1))
      allocate(waveXHigh(0:cpuWAV-1))
      allocate(waveYHigh(0:cpuWAV-1))
      allocate(waveSNX(0:cpuWAV-1))
      allocate(waveSNY(0:cpuWAV-1))

      ! For a 433x257 wave domain using 8 processors
      ! waveXLow:     1 109 217 325   1 109 217 325 
      ! waveYLow:     1   1   1   1 129 129 129 129
      ! waveXHigh:  108 216 324 433 108 216 324 433 
      ! waveYHigh:  128 128 128 128 257 257 257 257
      ! waveSNX:    108 108 108 109 108 108 108 109
      ! waveSNY:    128 128 128 128 129 129 129 129

      nX_per = meshNX/waveNPX
      nY_per = meshNY/waveNPY
      nX_rem = meshNX - nX_per*waveNPX
      nY_rem = meshNY - nY_per*waveNPY

      DO tile = 0,NP
        iX = mod(tile,waveNPX)
        iY = tile/waveNPX
        ! PRINT *, "ix, iy: ", iX, iY
        waveXLow(tile) = iX*nX_per + 1 
        waveYLow(tile) = iY*nY_per + 1 

        waveXHigh(tile) = waveXLow(tile) + nX_per - 1
        waveYHigh(tile) = waveYLow(tile) + nY_per - 1

        if (iX .eq. waveNPX) then
          waveXHigh(tile) = waveXHigh(tile) + nX_rem
        endif
        if (iY .eq. waveNPY) then
          waveYHigh(tile) = waveYHigh(tile) + nY_rem
        endif

        waveSNX(tile) = waveXHigh(tile) - waveXLow(tile) + 1
        waveSNY(tile) = waveYHigh(tile) - waveYLow(tile) + 1
      ENDDO

      call WAV_SetGridArrays(gcomp, petCount, localPet, wavGridIn,rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
          line=__LINE__, file=FILENAME)) return
      wavGridOut = wavGridIn

      !! call ESMF_ArraySpecSet(arraySpec, typekind=ESMF_TYPEKIND_R8,      &
      !!                        rank=2, rc=rc)
      !! staggerLoc = ESMF_STAGGERLOC_CENTER
      !! ! register the MITgcm entries in ESMF
      !! do iEntry = 1, nList
      !!   entryNameNUOPC = trim(nuopc_entryNameList(iEntry));
      !!   entryNameWRF = trim(wrf_nameList(iEntry));
      !!   exportEntry = fromWAV(iEntry);

      !!   if (exportEntry == .True.) then
      !!     field_tmp = ESMF_FieldCreate(wavGridOut, arraySpec,&
      !!                staggerloc=staggerLoc, &
      !!                indexflag=ESMF_INDEX_GLOBAL,          &
      !!                name=entryNameWRF, rc=rc)
      !!   else
      !!     field_tmp = ESMF_FieldCreate(wavGridIn, arraySpec,&
      !!                staggerloc=staggerLoc, &
      !!                indexflag=ESMF_INDEX_GLOBAL,          &
      !!                name=entryNameWRF, rc=rc)
      !!   end if

      !!   if (exportEntry == .True.) then
      !!     call NUOPC_Realize(exportState, field=field_tmp, rc=rc) 
      !!   else
      !!     call NUOPC_Realize(importState, field=field_tmp, rc=rc) 
      !!   end if
      !! end do

      field = ESMF_FieldCreate(name="LANDMASK", grid=wavGridIn,         &
        typekind=ESMF_TYPEKIND_R8, rc=rc)
      call NUOPC_Realize(importState, field=field, rc=rc)
      field = ESMF_FieldCreate(name="XLAT", grid=wavGridIn,          &
        typekind=ESMF_TYPEKIND_R8, rc=rc)
      call NUOPC_Realize(importState, field=field, rc=rc)
      field = ESMF_FieldCreate(name="XLONG", grid=wavGridIn,          &
        typekind=ESMF_TYPEKIND_R8, rc=rc)
      call NUOPC_Realize(importState, field=field, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
          line=__LINE__, file=FILENAME)) return

      field = ESMF_FieldCreate(name="SST", grid=wavGridIn,          &
        typekind=ESMF_TYPEKIND_R8, rc=rc)
      call NUOPC_Realize(importState, field=field, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
          line=__LINE__, file=FILENAME)) return

      field = ESMF_FieldCreate(name="SWUPB", grid=wavGridIn,          &
        typekind=ESMF_TYPEKIND_R8, rc=rc)
      call NUOPC_Realize(importState, field=field, rc=rc)
      field = ESMF_FieldCreate(name="SWDNB", grid=wavGridIn,          &
        typekind=ESMF_TYPEKIND_R8, rc=rc)
      call NUOPC_Realize(importState, field=field, rc=rc)
      field = ESMF_FieldCreate(name="LWUPB", grid=wavGridIn,          &
        typekind=ESMF_TYPEKIND_R8, rc=rc)
      call NUOPC_Realize(importState, field=field, rc=rc)
      field = ESMF_FieldCreate(name="LWDNB", grid=wavGridIn,          &
        typekind=ESMF_TYPEKIND_R8, rc=rc)
      call NUOPC_Realize(importState, field=field, rc=rc)
      field = ESMF_FieldCreate(name="LH", grid=wavGridIn,          &
        typekind=ESMF_TYPEKIND_R8, rc=rc)
      call NUOPC_Realize(importState, field=field, rc=rc)
      field = ESMF_FieldCreate(name="HFX", grid=wavGridIn,          &
        typekind=ESMF_TYPEKIND_R8, rc=rc)
      call NUOPC_Realize(importState, field=field, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
          line=__LINE__, file=FILENAME)) return

      field = ESMF_FieldCreate(name="U10", grid=wavGridIn,          &
        typekind=ESMF_TYPEKIND_R8, rc=rc)
      call NUOPC_Realize(importState, field=field, rc=rc)
      field = ESMF_FieldCreate(name="V10", grid=wavGridIn,          &
        typekind=ESMF_TYPEKIND_R8, rc=rc)
      call NUOPC_Realize(importState, field=field, rc=rc)
      field = ESMF_FieldCreate(name="T2", grid=wavGridIn,          &
        typekind=ESMF_TYPEKIND_R8, rc=rc)
      call NUOPC_Realize(importState, field=field, rc=rc)
      field = ESMF_FieldCreate(name="Q2", grid=wavGridIn,          &
        typekind=ESMF_TYPEKIND_R8, rc=rc)
      call NUOPC_Realize(importState, field=field, rc=rc)
      field = ESMF_FieldCreate(name="SST_INPUT", grid=wavGridIn,        &
        typekind=ESMF_TYPEKIND_R8, rc=rc)
      call NUOPC_Realize(importState, field=field, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
          line=__LINE__, file=FILENAME)) return

      field = ESMF_FieldCreate(name="QFX", grid=wavGridIn,          &
        typekind=ESMF_TYPEKIND_R8, rc=rc)
      call NUOPC_Realize(importState, field=field, rc=rc)
      field = ESMF_FieldCreate(name="RAINCV", grid=wavGridIn,          &
        typekind=ESMF_TYPEKIND_R8, rc=rc)
      call NUOPC_Realize(importState, field=field, rc=rc)
      field = ESMF_FieldCreate(name="RAINSHV", grid=wavGridIn,          &
        typekind=ESMF_TYPEKIND_R8, rc=rc)
      call NUOPC_Realize(importState, field=field, rc=rc)
      field = ESMF_FieldCreate(name="RAINNCV", grid=wavGridIn,          &
        typekind=ESMF_TYPEKIND_R8, rc=rc)
      call NUOPC_Realize(importState, field=field, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
          line=__LINE__, file=FILENAME)) return

      field = ESMF_FieldCreate(name="UOCE", grid=wavGridIn,          &
        typekind=ESMF_TYPEKIND_R8, rc=rc)
      call NUOPC_Realize(importState, field=field, rc=rc)
      field = ESMF_FieldCreate(name="VOCE", grid=wavGridIn,          &
        typekind=ESMF_TYPEKIND_R8, rc=rc)
      call NUOPC_Realize(importState, field=field, rc=rc)
      field = ESMF_FieldCreate(name="OCNMLD", grid=wavGridIn,          &
        typekind=ESMF_TYPEKIND_R8, rc=rc)
      call NUOPC_Realize(importState, field=field, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
          line=__LINE__, file=FILENAME)) return

      field = ESMF_FieldCreate(name="WAVEHS", grid=wavGridOut,          &
        typekind=ESMF_TYPEKIND_R8, rc=rc)
      call NUOPC_Realize(exportState, field=field, rc=rc)
      field = ESMF_FieldCreate(name="WAVELP", grid=wavGridOut,          &
        typekind=ESMF_TYPEKIND_R8, rc=rc)
      call NUOPC_Realize(exportState, field=field, rc=rc)
      field = ESMF_FieldCreate(name="OCNMASK", grid=wavGridOut,        &
        typekind=ESMF_TYPEKIND_R8, rc=rc)
      call NUOPC_Realize(exportState, field=field, rc=rc)
      field = ESMF_FieldCreate(name="WAVESTOKESX", grid=wavGridOut,     &
        typekind=ESMF_TYPEKIND_R8, rc=rc)
      call NUOPC_Realize(exportState, field=field, rc=rc)
      field = ESMF_FieldCreate(name="WAVESTOKESY", grid=wavGridOut,     &
        typekind=ESMF_TYPEKIND_R8, rc=rc)
      call NUOPC_Realize(exportState, field=field, rc=rc)
      field = ESMF_FieldCreate(name="WAVESTOKESXH", grid=wavGridOut,     &
        typekind=ESMF_TYPEKIND_R8, rc=rc)
      call NUOPC_Realize(exportState, field=field, rc=rc)
      field = ESMF_FieldCreate(name="WAVESTOKESYH", grid=wavGridOut,     &
        typekind=ESMF_TYPEKIND_R8, rc=rc)
      call NUOPC_Realize(exportState, field=field, rc=rc)
      field = ESMF_FieldCreate(name="WAVELANGMUIR", grid=wavGridOut,    &
        typekind=ESMF_TYPEKIND_R8, rc=rc)
      call NUOPC_Realize(exportState, field=field, rc=rc)
      field = ESMF_FieldCreate(name="WAVELASL", grid=wavGridOut,    &
        typekind=ESMF_TYPEKIND_R8, rc=rc)
      call NUOPC_Realize(exportState, field=field, rc=rc)
      field = ESMF_FieldCreate(name="WAVEFP", grid=wavGridOut,          &
        typekind=ESMF_TYPEKIND_R8, rc=rc)
      call NUOPC_Realize(exportState, field=field, rc=rc)
      field = ESMF_FieldCreate(name="WAVECHA", grid=wavGridOut,          &
        typekind=ESMF_TYPEKIND_R8, rc=rc)
      call NUOPC_Realize(exportState, field=field, rc=rc)
      field = ESMF_FieldCreate(name="WAVETAUIX", grid=wavGridOut,    &
        typekind=ESMF_TYPEKIND_R8, rc=rc)
      call NUOPC_Realize(exportState, field=field, rc=rc)
      field = ESMF_FieldCreate(name="WAVETAUIY", grid=wavGridOut,    &
        typekind=ESMF_TYPEKIND_R8, rc=rc)
      call NUOPC_Realize(exportState, field=field, rc=rc)
      field = ESMF_FieldCreate(name="WAVETAUOX", grid=wavGridOut,    &
        typekind=ESMF_TYPEKIND_R8, rc=rc)
      call NUOPC_Realize(exportState, field=field, rc=rc)
      field = ESMF_FieldCreate(name="WAVETAUOY", grid=wavGridOut,    &
        typekind=ESMF_TYPEKIND_R8, rc=rc)
      call NUOPC_Realize(exportState, field=field, rc=rc)
      field = ESMF_FieldCreate(name="WAVENUMBER", grid=wavGridOut,    &
        typekind=ESMF_TYPEKIND_R8, rc=rc)
      call NUOPC_Realize(exportState, field=field, rc=rc)

      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
          line=__LINE__, file=FILENAME)) return

      timeI(1) = start_year*10000 + start_month*100 + start_day*1
      timeI(2) = start_hour*10000 + start_minute*100 + start_second*1

      call w3_run(comm, timeI)
      call WAV_Put(gcomp, 0, rc)

      end subroutine
!
!-----------------------------------------------------------------------
!     Wave Check Import Fields
!-----------------------------------------------------------------------
!
      subroutine WAV_CheckImport(gcomp, rc)

      type(ESMF_GridComp)  :: gcomp
      integer, intent(out) :: rc

      type(ESMF_Clock)     :: modelClock, driverClock
      type(ESMF_TimeInterval) :: wavTimeStep
      type(ESMF_Time) :: wavStartTime
      type(ESMF_Time) :: wavEndTime
      
      rc = ESMF_SUCCESS

      end subroutine
!
!-----------------------------------------------------------------------
!     Wave Set Clock  
!-----------------------------------------------------------------------
!
      subroutine WAV_SetClock(gcomp, rc)

      type(ESMF_GridComp)  :: gcomp
      integer, intent(out) :: rc

      type(ESMF_Clock)     :: clock
      type(ESMF_VM) :: vm
      
      rc = ESMF_SUCCESS

      call NUOPC_ModelGet(gcomp, modelClock=clock, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
          line=__LINE__, file=FILENAME)) return

      call NUOPC_CompSetClock(gcomp, clock, wavTimeStep, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
          line=__LINE__, file=FILENAME)) return

      end subroutine
!
!-----------------------------------------------------------------------
!     Run
!-----------------------------------------------------------------------
!
      subroutine WAV_Run(gcomp, rc)

      TYPE(ESMF_GridComp)     :: gcomp
      INTEGER,  INTENT(  OUT) :: rc

      type(ESMF_State),    TARGET:: importState
      type(ESMF_State),    TARGET:: exportState
      type(ESMF_Clock),    TARGET:: clock

      TYPE(ESMF_GridComp), POINTER :: p_gcomp
      TYPE(ESMF_State),    POINTER :: p_importState
      TYPE(ESMF_State),    POINTER :: p_exportState
      TYPE(ESMF_Clock),    POINTER :: p_clock
      type(ESMF_Time) :: currTime
      integer :: iyear, iday, imonth, ihour, iminute, isecond

      ! Local variables
      TYPE(ESMF_Time) :: currentTime, nextTime
      TYPE(ESMF_TimeInterval) :: runLength     ! how long to run in this call
      CHARACTER(LEN=256) :: timeStr
      TYPE(ESMF_StateIntent_Flag) :: stateintent
      INTEGER :: itemCount
      INTEGER :: iLoopWAV = 1
      INTEGER :: timeI(2)
      integer :: comm, localPet, petCount
      integer :: total_seconds
      character(ESMF_MAXSTR) :: gname
      type(ESMF_VM) :: vm
      integer :: iloop
      
      rc = ESMF_SUCCESS

      call ESMF_GridCompGet(gcomp, vm=vm, name=gname, clock=clock, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
          line=__LINE__, file=FILENAME)) return

      call ESMF_VMGet(vm, localPet=localPet, petCount=petCount,         &
                      mpiCommunicator=comm, rc=rc)

      call ESMF_ClockGet(clock, currTime=currTime, rc=rc)

      currTime = currTime + wavTimeStep
      call ESMF_TimeGet(currTime, yy=iyear, mm=imonth, dd=iday, &
                        h=ihour, m=iminute, s=isecond, rc=rc)
      ! print *, "start year is: ", start_year
      !! timeI(1) = 20120101
      !! timeI(2) = INT(iLoopWAV/3)*10000 + MOD(iLoopWAV,3)*2000
      timeI(1) = iyear*10000 + imonth*100 + iday*1
      timeI(2) = ihour*10000 + iminute*100 + isecond*1
      ! print *, "WAV iLoop is: ", iLoopWAV
      ! print *, "WAV timeI is: ", timeI

      call WAV_Get(gcomp, iLoopWAV, rc)
      call w3_run(comm, timeI)
      call WAV_Put(gcomp, iLoopWAV, rc)

      iLoopWAV = iLoopWAV + 1

      end subroutine

      subroutine WAV_SetGridArrays(gcomp, petCount, localPet, gridIn,rc)

      use ww3_esmf, only : get_domain_size
      use ww3_esmf, only : get_grid_parameters
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
      real(ESMF_KIND_R8), pointer :: ptrX(:,:), ptrY(:,:), ptrA(:,:)
      real(ESMF_KIND_I4), pointer :: ptrM(:,:)
      type(ESMF_Array) :: arrX, arrY
      type(ESMF_StaggerLoc) :: staggerLoc
      type(ESMF_DistGrid) :: distGrid
      integer, allocatable :: meshType(:)
      character(ESMF_MAXSTR), allocatable :: meshTypeName(:)
      integer, allocatable :: deBlockList(:,:,:)
      integer :: localDECount, j
      character(ESMF_MAXSTR) ::  ofile

      integer :: localN
      integer :: meshNX, meshNY, IP, NP, tile
      real*8, dimension(:,:), allocatable :: meshXGRD, meshYGRD
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
      call get_domain_size(meshNX, meshNY, IP, NP)

      allocate(meshXGRD(meshNY,meshNX))
      allocate(meshYGRD(meshNY,meshNX))

      call get_grid_parameters(meshXGRD, meshYGRD)

      call ESMF_GridCompGet(gcomp, vm=vm, name=cname, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
          line=__LINE__, file=FILENAME)) return

      ! PRINT *, "WW3 NX is, ", meshNX
      ! PRINT *, "WW3 NY is, ", meshNY
      ! PRINT *, "IP is, ", IP
      ! PRINT *, "NP is, ", NP

      !! TODO:: make the deblocklist more general
      if (.not.allocated(deBlockList)) then
        allocate(deBlockList(2,2,1:NP+1))
      end if

      do tile = 0, NP
        deBlockList(1,1,tile+1) = waveXLow(tile)
        deBlockList(2,1,tile+1) = waveYLow(tile)
        deBlockList(1,2,tile+1) = waveXHigh(tile)
        deBlockList(2,2,tile+1) = waveYHigh(tile)
      end do
!
!-----------------------------------------------------------------------
!     Create ESMF DistGrid based on model domain decomposition
!-----------------------------------------------------------------------
!
      distGrid = ESMF_DistGridCreate(minIndex=(/ 1, 1 /),               &
                                     maxIndex=(/ meshNX, meshNY /), &
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
                               name="wav_grid",                         &
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
      ! print *, "localDECount is: ", localDECount, " localPet is: ", localPet

      do j = 0, localDECount-1

        call ESMF_GridGetCoord(gridIn, staggerLoc=staggerLoc, localDE=j,&
                             coordDim=1, farrayPtr=ptrX, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,  &
                             line=__LINE__, file=FILENAME)) return
!
        call ESMF_GridGetCoord(gridIn, staggerLoc=staggerLoc, localDE=j,&
                             coordDim=2, farrayPtr=ptrY, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,  &
                             line=__LINE__, file=FILENAME)) return

        !! call ESMF_GridGetItem(gridIn, staggerLoc=staggerLoc, localDE=j,&
        !!                      itemflag=ESMF_GRIDITEM_MASK,               &
        !!                      farrayPtr=ptrM, rc=rc)
        !! call ESMF_GridGetItem(gridIn, staggerLoc=staggerLoc, localDE=j,&
        !!                      itemflag=ESMF_GRIDITEM_AREA,               &
        !!                      farrayPtr=ptrA, rc=rc)
!
!-----------------------------------------------------------------------
!     Fill the pointers    
!-----------------------------------------------------------------------
!
        do n = 1, waveSNX(IP-1)
          do m = 1, waveSNY(IP-1)
            !! TODO: pseudo mesh
            iG = waveXLow(IP-1) - 1 + n 
            jG = waveYLow(IP-1) - 1 + m
            ptrX(iG,jG) = meshXGRD(jG,iG)
            ptrY(iG,jG) = meshYGRD(jG,iG)
          end do
        end do

        call ESMF_GridCompSet(gcomp, grid=gridIn, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,  &
                             line=__LINE__, file=FILENAME)) return

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

!
!-----------------------------------------------------------------------
!     Assign grid to gridded component 
!-----------------------------------------------------------------------
!
        if (debugLevel >= 1) then

          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,&
                                 line=__LINE__, file=FILENAME)) return
          call ESMF_GridGetCoord(gridIn,                                &
                                 staggerLoc=ESMF_STAGGERLOC_CENTER,     &
                                 coordDim=1, array=arrX, rc=rc)
          call ESMF_GridGetCoord(gridIn,                                &
                                 staggerLoc=ESMF_STAGGERLOC_CENTER,     &
                                 coordDim=2, array=arrY, rc=rc)
          call ESMF_ArrayWrite(arrX, filename='wav_xa.nc',              &
                               status=ESMF_FILESTATUS_NEW, rc=rc)
          call ESMF_ArrayWrite(arrY, filename='wav_ya.nc',              &
                               status=ESMF_FILESTATUS_NEW, rc=rc)
        end if
      end do
!
      end subroutine WAV_SetGridArrays
!
!
      subroutine WAV_Get(gcomp, iLoop, rc)

      use ww3_esmf, only : get_domain_size
      use ww3_esmf, only : get_wind_fields
      use ww3_esmf, only : put_wind_fields
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
      integer :: bi, bj, iG, jG, imax, jmax
      integer :: i, j, ii, jj, iunit, iyear, iday, imonth, ihour
      type(ESMF_Grid) :: gridIn
      type(ESMF_VM) :: vm
      type(ESMF_Clock) :: clock
      type(ESMF_Field) :: field_windux, field_winduy
      type(ESMF_Field) :: field_currux, field_curruy
      type(ESMF_Field) :: field_mld
      real(ESMF_KIND_R8), pointer :: ptr_windux(:,:), ptr_winduy(:,:)
      real(ESMF_KIND_R8), pointer :: ptr_currux(:,:), ptr_curruy(:,:)
      real(ESMF_KIND_R8), pointer :: ptr_mld(:,:)
      type(ESMF_State) :: importState
      integer :: petCount, localPet, itemCount, localDECount
      character(ESMF_MAXSTR) :: cname, ofile
      REAL*8, DIMENSION(:,:), ALLOCATABLE :: windU0_OUT, windV0_OUT
      REAL*8, DIMENSION(:,:), ALLOCATABLE :: windUN_OUT, windVN_OUT
      REAL*8, DIMENSION(:,:), ALLOCATABLE :: currU0_OUT, currV0_OUT
      REAL*8, DIMENSION(:,:), ALLOCATABLE :: currUN_OUT, currVN_OUT
      REAL*8, DIMENSION(:,:), ALLOCATABLE :: mld_OUT
      integer :: meshNX, meshNY, IP, NP, tile
      integer :: comm
!
      rc = ESMF_SUCCESS
!
!-----------------------------------------------------------------------
!     Get gridded component 
!-----------------------------------------------------------------------
!
      call get_domain_size(meshNX, meshNY, IP, NP)

      ALLOCATE(windU0_OUT(meshNX, meshNY))
      ALLOCATE(windUN_OUT(meshNX, meshNY))
      ALLOCATE(windV0_OUT(meshNX, meshNY))
      ALLOCATE(windVN_OUT(meshNX, meshNY))
      ALLOCATE(currU0_OUT(meshNX, meshNY))
      ALLOCATE(currUN_OUT(meshNX, meshNY))
      ALLOCATE(currV0_OUT(meshNX, meshNY))
      ALLOCATE(currVN_OUT(meshNX, meshNY))
      ALLOCATE(mld_OUT(meshNX, meshNY))

      call ESMF_GridCompGet(gcomp, name=cname, clock=clock, grid=gridIn,&
                            importState=importState, vm=vm, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return

      call ESMF_VMGet(vm, localPet=localPet, petCount=petCount,         &
                      mpiCommunicator=comm, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                  line=__LINE__, file=FILENAME)) return
!
!-----------------------------------------------------------------------
!     Get field
!-----------------------------------------------------------------------
!
      call ESMF_GridGet(gridIn, localDECount=localDECount, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return

      call ESMF_StateGet(importState, 'U10', field_windux, rc=rc)
      call ESMF_StateGet(importState, 'V10', field_winduy, rc=rc)
      call ESMF_StateGet(importState, 'UOCE', field_currux, rc=rc)
      call ESMF_StateGet(importState, 'VOCE', field_curruy, rc=rc)
      call ESMF_StateGet(importState, 'OCNMLD', field_mld, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
!
      do j = 0, localDECount-1

!       ! Get pointer 
        call ESMF_FieldGet(field_windux, localDE=j, farrayPtr=ptr_windux, rc=rc)
        call ESMF_FieldGet(field_winduy, localDE=j, farrayPtr=ptr_winduy, rc=rc)
        call ESMF_FieldGet(field_currux, localDE=j, farrayPtr=ptr_currux, rc=rc)
        call ESMF_FieldGet(field_curruy, localDE=j, farrayPtr=ptr_curruy, rc=rc)
        call ESMF_FieldGet(field_mld, localDE=j, farrayPtr=ptr_mld, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,  &
                             line=__LINE__, file=FILENAME)) return
!
!       ! Set initial value to missing 
        !! ptr_windux = 0d0
        !! ptr_winduy = 0d0
        !! ptr_currux = 0d0
        !! ptr_curruy = 0d0
        !! ptr_mld    =10d0
!
!       ! Put data to export field 
        do ii = 1, waveSNX(IP-1)
          do jj = 1, waveSNY(IP-1)
            iG = waveXLow(IP-1) - 1 + ii
            jG = waveYLow(IP-1) - 1 + jj 
            windU0_OUT(iG,jG) = ptr_windux(iG,jG)
            windV0_OUT(iG,jG) = ptr_winduy(iG,jG)
            windUN_OUT(iG,jG) = ptr_windux(iG,jG)
            windVN_OUT(iG,jG) = ptr_winduy(iG,jG)
            currU0_OUT(iG,jG) = ptr_currux(iG,jG)
            currV0_OUT(iG,jG) = ptr_curruy(iG,jG)
            currUN_OUT(iG,jG) = ptr_currux(iG,jG)
            currVN_OUT(iG,jG) = ptr_curruy(iG,jG)
            mld_OUT(iG,jG) = ptr_mld(iG,jG)
            !! PRINT *, "ig, jg, ptr_ux is: ", iG, jG, ptr_ux(iG,jG)
            !! PRINT *, "ig, jg, ptr_uy is: ", iG, jG, ptr_uy(iG,jG)
            !! PRINT *, "ig, jg, windu0_out is: ", iG, jG, windU0_OUT(iG,jG)
            !! PRINT *, "ig, jg, winduN_out is: ", iG, jG, windUN_OUT(iG,jG)
            !! PRINT *, "ig, jg, windv0_out is: ", iG, jG, windV0_OUT(iG,jG)
            !! PRINT *, "ig, jg, windvN_out is: ", iG, jG, windVN_OUT(iG,jG)
          end do
        end do
!
!       ! Write field to debug
        if (debugLevel >= 1) then
          write (ofile, "(A6,I2.2,A3)") "wuxWAV", iLoop, ".nc"
          call ESMF_FieldWrite(field_windux, trim(ofile), rc=rc)
          write (ofile, "(A6,I2.2,A3)") "wuyWAV", iLoop, ".nc"
          call ESMF_FieldWrite(field_winduy, trim(ofile), rc=rc)
          write (ofile, "(A6,I2.2,A3)") "cuxWAV", iLoop, ".nc"
          call ESMF_FieldWrite(field_currux, trim(ofile), rc=rc)
          write (ofile, "(A6,I2.2,A3)") "cuyWAV", iLoop, ".nc"
          call ESMF_FieldWrite(field_curruy, trim(ofile), rc=rc)
        end if

!       ! Nullify the pointer
        if (associated(ptr_windux)) then
          nullify(ptr_windux)
        end if
        if (associated(ptr_winduy)) then
          nullify(ptr_winduy)
        end if
        if (associated(ptr_currux)) then
          nullify(ptr_currux)
        end if
        if (associated(ptr_curruy)) then
          nullify(ptr_curruy)
        end if
        if (associated(ptr_mld)) then
          nullify(ptr_mld)
        end if
!
      end do

      call put_wind_fields(comm, windU0_OUT, windV0_OUT, &
                           windUN_OUT, windVN_OUT, &
                           currU0_OUT, currV0_OUT, &
                           currUN_OUT, currVN_OUT, &
                           mld_OUT, &
                           cpuWAV, waveXLow, waveYLow, &
                           waveXHigh, waveYHigh, waveSNX, waveSNY)

      DEALLOCATE(windU0_OUT)
      DEALLOCATE(windUN_OUT)
      DEALLOCATE(windV0_OUT)
      DEALLOCATE(windVN_OUT)
      DEALLOCATE(currU0_OUT)
      DEALLOCATE(currUN_OUT)
      DEALLOCATE(currV0_OUT)
      DEALLOCATE(currVN_OUT)
      DEALLOCATE(mld_OUT)

      end subroutine WAV_Get

      subroutine WAV_Put(gcomp, iLoop, rc)

      use ww3_esmf, only : get_domain_size
      use ww3_esmf, only : get_hs
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
      integer :: comm, localPet, petCount
      integer :: itemCount, localDECount
      character(ESMF_MAXSTR) :: cname, ofile
      real(ESMF_KIND_R8), pointer :: ptr_hs(:,:), ptr_lp(:,:)
      real(ESMF_KIND_R8), pointer :: ptr_wn(:,:), ptr_mask(:,:)
      real(ESMF_KIND_R8), pointer :: ptr_stokesx(:,:), ptr_stokesy(:,:)
      real(ESMF_KIND_R8), pointer :: ptr_stokesxh(:,:), ptr_stokesyh(:,:)
      real(ESMF_KIND_R8), pointer :: ptr_langmuir(:,:), ptr_lasl(:,:)
      real(ESMF_KIND_R8), pointer :: ptr_fp(:,:), ptr_cha(:,:)
      real(ESMF_KIND_R8), pointer :: ptr_tauix(:,:), ptr_tauiy(:,:)
      real(ESMF_KIND_R8), pointer :: ptr_tauox(:,:), ptr_tauoy(:,:)
!
      type(ESMF_VM) :: vm
      type(ESMF_Field) :: field_hs, field_lp, field_wn, field_mask
      type(ESMF_Field) :: field_stokesx, field_stokesy
      type(ESMF_Field) :: field_stokesxh, field_stokesyh
      type(ESMF_Field) :: field_langmuir, field_lasl
      type(ESMF_Field) :: field_fp, field_cha
      type(ESMF_Field) :: field_tauix, field_tauiy
      type(ESMF_Field) :: field_tauox, field_tauoy
      type(ESMF_State) :: exportState
      REAL*8, DIMENSION(:,:), ALLOCATABLE :: hs_ESMF, lp_ESMF, wn_ESMF
      REAL*8, DIMENSION(:,:), ALLOCATABLE :: ocnmask_ESMF
      REAL*8, DIMENSION(:,:), ALLOCATABLE :: stokesx_ESMF
      REAL*8, DIMENSION(:,:), ALLOCATABLE :: stokesy_ESMF
      REAL*8, DIMENSION(:,:), ALLOCATABLE :: stokesxh_ESMF
      REAL*8, DIMENSION(:,:), ALLOCATABLE :: stokesyh_ESMF
      REAL*8, DIMENSION(:,:), ALLOCATABLE :: langmuir_ESMF
      REAL*8, DIMENSION(:,:), ALLOCATABLE :: lasl_ESMF
      REAL*8, DIMENSION(:,:), ALLOCATABLE :: fp_ESMF
      REAL*8, DIMENSION(:,:), ALLOCATABLE :: cha_ESMF
      REAL*8, DIMENSION(:,:), ALLOCATABLE :: tauix_ESMF
      REAL*8, DIMENSION(:,:), ALLOCATABLE :: tauiy_ESMF
      REAL*8, DIMENSION(:,:), ALLOCATABLE :: tauox_ESMF
      REAL*8, DIMENSION(:,:), ALLOCATABLE :: tauoy_ESMF
!
      integer :: meshNX, meshNY, IP, NP, tile
      integer :: myThid = 1
!
      rc = ESMF_SUCCESS
!
!-----------------------------------------------------------------------
!     Get ESMF domain size info
!-----------------------------------------------------------------------
!
      call get_domain_size(meshNX, meshNY, IP, NP)

      ALLOCATE(hs_ESMF(meshNX, meshNY))
      ALLOCATE(lp_ESMF(meshNX, meshNY))
      ALLOCATE(wn_ESMF(meshNX, meshNY))
      ALLOCATE(ocnmask_ESMF(meshNX, meshNY))
      ALLOCATE(stokesx_ESMF(meshNX, meshNY))
      ALLOCATE(stokesy_ESMF(meshNX, meshNY))
      ALLOCATE(stokesxh_ESMF(meshNX, meshNY))
      ALLOCATE(stokesyh_ESMF(meshNX, meshNY))
      ALLOCATE(langmuir_ESMF(meshNX, meshNY))
      ALLOCATE(lasl_ESMF(meshNX, meshNY))
      ALLOCATE(fp_ESMF(meshNX, meshNY))
      ALLOCATE(cha_ESMF(meshNX, meshNY))
      ALLOCATE(tauix_ESMF(meshNX, meshNY))
      ALLOCATE(tauiy_ESMF(meshNX, meshNY))
      ALLOCATE(tauox_ESMF(meshNX, meshNY))
      ALLOCATE(tauoy_ESMF(meshNX, meshNY))

!-----------------------------------------------------------------------
!     Get gridded component 
!-----------------------------------------------------------------------
!
      call ESMF_GridCompGet(gcomp, name=cname, &
                            grid=gridOut, &
                            exportState=exportState, vm=vm, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return

      call ESMF_VMGet(vm, localPet=localPet, petCount=petCount,         &
                      mpiCommunicator=comm, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                  line=__LINE__, file=FILENAME)) return
!
!-----------------------------------------------------------------------
!     Get number of local DEs
!-----------------------------------------------------------------------
! 
      call get_hs(comm, localPet, hs_ESMF, lp_ESMF, wn_ESMF, &
                  ocnmask_ESMF, stokesx_ESMF, stokesy_ESMF, &
                  stokesxh_ESMF, stokesyh_ESMF, &
                  langmuir_ESMF, lasl_ESMF, &
                  fp_ESMF, cha_ESMF, &
                  tauix_ESMF, tauiy_ESMF, &
                  tauox_ESMF, tauoy_ESMF, &
                  cpuWAV, waveXLow, waveYLow, &
                  waveXHigh, waveYHigh, waveSNX, waveSNY)
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
      call ESMF_StateGet(exportState, "WAVEHS", field_hs, rc=rc)
      call ESMF_StateGet(exportState, "WAVELP", field_lp, rc=rc)
      call ESMF_StateGet(exportState, "WAVENUMBER", field_wn, rc=rc)
      call ESMF_StateGet(exportState, "OCNMASK", field_mask, rc=rc)
      call ESMF_StateGet(exportState, "WAVESTOKESX", field_stokesx, rc=rc)
      call ESMF_StateGet(exportState, "WAVESTOKESY", field_stokesy, rc=rc)
      call ESMF_StateGet(exportState, "WAVESTOKESXH", field_stokesxh, rc=rc)
      call ESMF_StateGet(exportState, "WAVESTOKESYH", field_stokesyh, rc=rc)
      call ESMF_StateGet(exportState, "WAVELANGMUIR", field_langmuir, rc=rc)
      call ESMF_StateGet(exportState, "WAVELASL", field_lasl, rc=rc)
      call ESMF_StateGet(exportState, "WAVEFP", field_fp, rc=rc)
      call ESMF_StateGet(exportState, "WAVECHA", field_cha, rc=rc)
      call ESMF_StateGet(exportState, "WAVETAUIX", field_tauix, rc=rc)
      call ESMF_StateGet(exportState, "WAVETAUIY", field_tauiy, rc=rc)
      call ESMF_StateGet(exportState, "WAVETAUOX", field_tauox, rc=rc)
      call ESMF_StateGet(exportState, "WAVETAUOY", field_tauoy, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                             line=__LINE__, file=FILENAME)) return
!
!-----------------------------------------------------------------------
!     Loop over decomposition elements (DEs) 
!-----------------------------------------------------------------------
!
      do j = 0, localDECount-1

!       ! Get pointer 
        call ESMF_FieldGet(field_hs, localDE=j, farrayPtr=ptr_hs, rc=rc)
        call ESMF_FieldGet(field_lp, localDE=j, farrayPtr=ptr_lp, rc=rc)
        call ESMF_FieldGet(field_wn, localDE=j, farrayPtr=ptr_wn, rc=rc)
        call ESMF_FieldGet(field_mask, localDE=j, farrayPtr=ptr_mask, rc=rc)
        call ESMF_FieldGet(field_stokesx, localDE=j, farrayPtr=ptr_stokesx, rc=rc)
        call ESMF_FieldGet(field_stokesy, localDE=j, farrayPtr=ptr_stokesy, rc=rc)
        call ESMF_FieldGet(field_stokesxh, localDE=j, farrayPtr=ptr_stokesxh, rc=rc)
        call ESMF_FieldGet(field_stokesyh, localDE=j, farrayPtr=ptr_stokesyh, rc=rc)
        call ESMF_FieldGet(field_langmuir, localDE=j, farrayPtr=ptr_langmuir, rc=rc)
        call ESMF_FieldGet(field_lasl, localDE=j, farrayPtr=ptr_lasl, rc=rc)
        call ESMF_FieldGet(field_fp, localDE=j, farrayPtr=ptr_fp, rc=rc)
        call ESMF_FieldGet(field_cha, localDE=j, farrayPtr=ptr_cha, rc=rc)
        call ESMF_FieldGet(field_tauix, localDE=j, farrayPtr=ptr_tauix, rc=rc)
        call ESMF_FieldGet(field_tauiy, localDE=j, farrayPtr=ptr_tauiy, rc=rc)
        call ESMF_FieldGet(field_tauox, localDE=j, farrayPtr=ptr_tauox, rc=rc)
        call ESMF_FieldGet(field_tauoy, localDE=j, farrayPtr=ptr_tauoy, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,  &
                             line=__LINE__, file=FILENAME)) return
!
!       ! Set initial value to missing 
        ptr_hs = 0d0
        ptr_lp = 0d0
        ptr_wn = 0d0
        ptr_mask = 0d0
        ptr_stokesx = 0d0
        ptr_stokesy = 0d0
        ptr_stokesxh = 0d0
        ptr_stokesyh = 0d0
        ptr_langmuir = 0d0
        ptr_lasl = 0d0
        ptr_fp = 0d0
        ptr_cha = 0d0
        ptr_tauix = 0d0
        ptr_tauiy = 0d0
        ptr_tauox = 0d0
        ptr_tauoy = 0d0
!
!       ! Put data to export field 
        do ii = 1, waveSNX(IP-1)
          do jj = 1, waveSNY(IP-1)
            iG = waveXLow(IP-1) - 1 + ii
            jG = waveYLow(IP-1) - 1 + jj 
            ptr_hs(iG,jG) = hs_esmf(iG,jG)
            ptr_lp(iG,jG) = lp_esmf(iG,jG)
            ptr_wn(iG,jG) = wn_esmf(iG,jG)
            ptr_mask(iG,jG) = ocnmask_esmf(iG,jG)
            ptr_stokesx(iG,jG) = stokesx_ESMF(iG,jG)
            ptr_stokesy(iG,jG) = stokesy_ESMF(iG,jG)
            ptr_stokesxh(iG,jG) = stokesxh_ESMF(iG,jG)
            ptr_stokesyh(iG,jG) = stokesyh_ESMF(iG,jG)
            ptr_langmuir(iG,jG) = langmuir_ESMF(iG,jG)
            ptr_lasl(iG,jG) = lasl_ESMF(iG,jG)
            ptr_fp(iG,jG) = fp_ESMF(iG,jG)
            ptr_cha(iG,jG) = cha_ESMF(iG,jG)
            ptr_tauix(iG,jG) = tauix_ESMF(iG,jG)
            ptr_tauiy(iG,jG) = tauiy_ESMF(iG,jG)
            ptr_tauox(iG,jG) = tauox_ESMF(iG,jG)
            ptr_tauoy(iG,jG) = tauoy_ESMF(iG,jG)
          end do
        end do
!
!       ! Write field to debug
        if (debugLevel >= 1) then
          write (ofile, "(A6,I2.2,A3)") "hhsWAV", iLoop, ".nc"
          call ESMF_FieldWrite(field_hs, trim(ofile), rc=rc)
          write (ofile, "(A6,I2.2,A3)") "llpWAV", iLoop, ".nc"
          call ESMF_FieldWrite(field_lp, trim(ofile), rc=rc)
          write (ofile, "(A6,I2.2,A3)") "wwnWAV", iLoop, ".nc"
          call ESMF_FieldWrite(field_wn, trim(ofile), rc=rc)
          write (ofile, "(A6,I2.2,A3)") "wmaWAV", iLoop, ".nc"
          call ESMF_FieldWrite(field_mask, trim(ofile), rc=rc)
          write (ofile, "(A6,I2.2,A3)") "stxWAV", iLoop, ".nc"
          call ESMF_FieldWrite(field_stokesx, trim(ofile), rc=rc)
          write (ofile, "(A6,I2.2,A3)") "styWAV", iLoop, ".nc"
          call ESMF_FieldWrite(field_stokesy, trim(ofile), rc=rc)
          write (ofile, "(A6,I2.2,A3)") "shxWAV", iLoop, ".nc"
          call ESMF_FieldWrite(field_stokesxh, trim(ofile), rc=rc)
          write (ofile, "(A6,I2.2,A3)") "shyWAV", iLoop, ".nc"
          call ESMF_FieldWrite(field_stokesyh, trim(ofile), rc=rc)
          write (ofile, "(A6,I2.2,A3)") "lamWAV", iLoop, ".nc"
          call ESMF_FieldWrite(field_langmuir, trim(ofile), rc=rc)
          write (ofile, "(A6,I2.2,A3)") "alpWAV", iLoop, ".nc"
          call ESMF_FieldWrite(field_lasl, trim(ofile), rc=rc)
          write (ofile, "(A6,I2.2,A3)") "ffpWAV", iLoop, ".nc"
          call ESMF_FieldWrite(field_fp, trim(ofile), rc=rc)
          write (ofile, "(A6,I2.2,A3)") "chaWAV", iLoop, ".nc"
          call ESMF_FieldWrite(field_cha, trim(ofile), rc=rc)
          write (ofile, "(A6,I2.2,A3)") "tixWAV", iLoop, ".nc"
          call ESMF_FieldWrite(field_tauix, trim(ofile), rc=rc)
          write (ofile, "(A6,I2.2,A3)") "tiyWAV", iLoop, ".nc"
          call ESMF_FieldWrite(field_tauiy, trim(ofile), rc=rc)
          write (ofile, "(A6,I2.2,A3)") "toxWAV", iLoop, ".nc"
          call ESMF_FieldWrite(field_tauox, trim(ofile), rc=rc)
          write (ofile, "(A6,I2.2,A3)") "toyWAV", iLoop, ".nc"
          call ESMF_FieldWrite(field_tauoy, trim(ofile), rc=rc)
        end if

!       ! Nullify the pointer
        if (associated(ptr_hs)) then
          nullify(ptr_hs)
        end if
        if (associated(ptr_lp)) then
          nullify(ptr_lp)
        end if
        if (associated(ptr_wn)) then
          nullify(ptr_wn)
        end if
        if (associated(ptr_mask)) then
          nullify(ptr_mask)
        end if
        if (associated(ptr_stokesx)) then
          nullify(ptr_stokesx)
        end if
        if (associated(ptr_stokesy)) then
          nullify(ptr_stokesy)
        end if
        if (associated(ptr_stokesxh)) then
          nullify(ptr_stokesxh)
        end if
        if (associated(ptr_stokesyh)) then
          nullify(ptr_stokesyh)
        end if
        if (associated(ptr_langmuir)) then
          nullify(ptr_langmuir)
        end if
        if (associated(ptr_lasl)) then
          nullify(ptr_lasl)
        end if
        if (associated(ptr_fp)) then
          nullify(ptr_fp)
        end if
        if (associated(ptr_cha)) then
          nullify(ptr_cha)
        end if
        if (associated(ptr_tauix)) then
          nullify(ptr_tauix)
        end if
        if (associated(ptr_tauiy)) then
          nullify(ptr_tauiy)
        end if
        if (associated(ptr_tauox)) then
          nullify(ptr_tauox)
        end if
        if (associated(ptr_tauoy)) then
          nullify(ptr_tauoy)
        end if
!
      end do
!
      DEALLOCATE(hs_ESMF)
      DEALLOCATE(lp_ESMF)
      DEALLOCATE(wn_ESMF)
      DEALLOCATE(ocnmask_ESMF)
      DEALLOCATE(stokesx_ESMF)
      DEALLOCATE(stokesy_ESMF)
      DEALLOCATE(stokesxh_ESMF)
      DEALLOCATE(stokesyh_ESMF)
      DEALLOCATE(langmuir_ESMF)
      DEALLOCATE(lasl_ESMF)
      DEALLOCATE(fp_ESMF)
      DEALLOCATE(cha_ESMF)
      DEALLOCATE(tauix_ESMF)
      DEALLOCATE(tauiy_ESMF)
      DEALLOCATE(tauox_ESMF)
      DEALLOCATE(tauoy_ESMF)
!
      end subroutine WAV_Put

      end module mod_esmf_wav
