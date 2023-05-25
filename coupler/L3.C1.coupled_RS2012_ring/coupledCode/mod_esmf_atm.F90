!-----------------------------------------------------------------------
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
      use module_wrf_top, only : wrf_init, wrf_run, wrf_finalize
      use module_domain, only : head_grid, get_ijk_from_grid
      use module_state_description
      use module_streams

      use module_esmf_extensions
      use module_metadatautils, only : AttachTimesToState
      use module_metadatautils, only : AttachDecompToState

      USE module_ext_esmf, only : ESMF_GridHandle => grid
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

      TYPE(ESMF_GridComp)  :: gcomp
      TYPE(ESMF_State)     :: importState, exportState
      TYPE(ESMF_Clock)     :: clock

      TYPE(ESMF_VM) :: vm
      INTEGER :: mpicomtmp
      INTEGER,                     INTENT(  OUT) :: rc

      ! Local variables
      TYPE(ESMF_GridComp), TARGET :: t_gcomp
      TYPE(ESMF_State),    TARGET :: t_importState, t_exportState
      TYPE(ESMF_Clock),    TARGET :: t_clock
      TYPE(ESMF_GridComp), POINTER :: p_gcomp
      TYPE(ESMF_State),    POINTER :: p_importState, p_exportState
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
      CHARACTER(LEN=256) :: ofile
      type(ESMF_Field) :: esmffield
      character(ESMF_MAXSTR) :: entryNameNUOPC, entryNameWRF
      integer :: iEntry
      logical :: exportEntry
      
      rc = ESMF_SUCCESS

      t_gcomp = gcomp
      t_importState = importState
      t_exportState = exportState
      t_clock = clock

      p_gcomp => t_gcomp
      p_importState => t_importState
      p_exportState => t_exportState
      p_clock => t_clock

      CALL ESMF_SetCurrent( gcomp=p_gcomp, importState=p_importState, &
                            exportState=p_exportState, clock=p_clock)
      
      CALL ESMF_VMGetCurrent(vm, rc=rc)
      
      CALL ESMF_VMGet(vm, mpiCommunicator=mpicomtmp, rc=rc)
      
      CALL wrf_set_dm_communicator( mpicomtmp )
      
      CALL wrf_init( no_init1=.TRUE. )
      
      call ESMF_TimeIntervalSet(couplingInterval, h=1, rc=rc)
      
      CALL AttachTimesToState( exportState, esmStartTime, esmStopTime, &
                               couplingInterval)

      CALL wrf_getDecompInfo( ids, ide, jds, jde, kds, kde, &
                              ims, ime, jms, jme, kms, kme, &
                              ips, ipe, jps, jpe, kps, kpe, &
                              domdesc, bdy_mask )
      
      CALL AttachDecompToState( exportState,                  &
                                ids, ide, jds, jde, kds, kde, &
                                ims, ime, jms, jme, kms, kme, &
                                ips, ipe, jps, jpe, kps, kpe, &
                                domdesc, bdy_mask )

      CALL AttachDecompToState( importState,                  &
                                ids, ide, jds, jde, kds, kde, &
                                ims, ime, jms, jme, kms, kme, &
                                ips, ipe, jps, jpe, kps, kpe, &
                                domdesc, bdy_mask )


      ! register the WRF entries in ESMF
      do iEntry = 1, nList
        entryNameNUOPC = trim(nuopc_entryNameList(iEntry));
        entryNameWRF = trim(wrf_nameList(iEntry));
        exportEntry = ATMtoOCN(iEntry);
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
      subroutine ATM_Init2(gcomp, importState, exportState, clock, rc)

      TYPE(ESMF_GridComp) :: gcomp
      TYPE(ESMF_State) :: importState, exportState
      TYPE(ESMF_Clock) :: clock
      integer, intent(out) :: rc

      TYPE(ESMF_GridComp), TARGET :: t_gcomp
      TYPE(ESMF_State),    TARGET :: t_importState, t_exportState
      TYPE(ESMF_Clock),    TARGET :: t_clock

      TYPE(ESMF_GridComp), POINTER :: p_gcomp
      TYPE(ESMF_State),    POINTER :: p_importState, p_exportState
      TYPE(ESMF_Clock),    POINTER :: p_clock

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
      CHARACTER(LEN=256) :: ofile
      INTEGER :: itemCount, i
      TYPE(ESMF_StateIntent_Flag) :: stateintent
      CHARACTER (ESMF_MAXSTR) :: statename
      CHARACTER (ESMF_MAXSTR), ALLOCATABLE :: itemNames(:)
      TYPE(ESMF_StateItem_Flag), ALLOCATABLE :: itemTypes(:)
      TYPE(ESMF_Time) :: currentTime, nextTime
      CHARACTER(LEN=256) :: timeStr
      INTEGER :: nI, nJ

!
!-----------------------------------------------------------------------
!     Local variable declaration
!-----------------------------------------------------------------------
!
      type(ESMF_Field) :: field
      type(ESMF_DistGrid) :: distGrid
      type(ESMF_Grid) :: atmGridIn
      type(ESMF_Grid) :: atmGridOut
      type(ESMF_Grid) , pointer :: esmfgrid


      integer :: myThid = 1
      integer :: comm, localPet, petCount
      type(ESMF_Field) :: esmffield
!
      type(ESMF_VM) :: vm
      real(ESMF_KIND_R4), pointer :: ptr_esmffield(:,:)

        CALL get_ijk_from_grid( head_grid ,                   &
                                ids, ide, jds, jde, kds, kde, &
                                ims, ime, jms, jme, kms, kme, &
                                ips, ipe, jps, jpe, kps, kpe  )

      
      rc = ESMF_SUCCESS

      CALL ESMF_StateGet( exportState, itemCount=itemCount, &
                          stateintent=stateintent, rc=rc )

      CALL ESMF_StateGet( importState, itemCount=itemCount, &
                          stateintent=stateintent, rc=rc )

      t_gcomp = gcomp
      t_importState = importState
      t_exportState = exportState
      t_clock = clock

      p_gcomp => t_gcomp
      p_importState => t_importState
      p_exportState => t_exportState
      p_clock => t_clock

      CALL ESMF_SetCurrent( gcomp=p_gcomp, importState=p_importState, &
                            exportState=p_exportState, clock=p_clock)

      CALL wrf_state_populate( rc )

      !! Examine importState
      !! CALL ESMF_StateGet( importState, itemCount=itemCount, &
      !!                     stateintent=stateintent, name=statename, &
      !!                     rc=rc )

      !! ALLOCATE ( itemNames(itemCount), itemTypes(itemCount) )

      !! CALL ESMF_StateGet( importState, itemNameList=itemNames, &
      !!                     itemtypeList=itemTypes, rc=rc )
      !! DO i=1, itemCount
      !!   PRINT *, 'nuopc wrf_component_init2: ', &
      !!            'importState contains field <',TRIM(itemNames(i)),'>'
      !! ENDDO
      !! DEALLOCATE ( itemNames, itemTypes )

      !! Examine exportState
      !! CALL ESMF_StateGet( exportState, itemCount=itemCount, &
      !!                     stateintent=stateintent, name=statename, &
      !!                     rc=rc )

      !! ALLOCATE ( itemNames(itemCount), itemTypes(itemCount) )

      !! CALL ESMF_StateGet( exportState, itemNameList=itemNames, &
      !!                     itemtypeList=itemTypes, rc=rc )
      !! DO i=1, itemCount
      !!   PRINT *, 'nuopc wrf_component_init2: ', &
      !!            'exportState contains field <',TRIM(itemNames(i)),'>'
      !! ENDDO
      !! DEALLOCATE ( itemNames, itemTypes )

      !! run an empty step to fill the data
      CALL ESMF_ClockGet( clock, currTime=currentTime, rc=rc )
      nextTime = currentTime
      head_grid%start_subtime = currentTime
      head_grid%stop_subtime = nextTime
  
      !! CALL wrf_timetoa ( head_grid%start_subtime, timeStr )
      !! PRINT *, 'wrf_component_run: head_grid%start_subtime ', &
      !!          TRIM(timeStr)
      !! CALL wrf_timetoa ( head_grid%stop_subtime, timeStr )
      !! PRINT *, 'wrf_component_run: head_grid%stop_subtime ', &
      !!          TRIM(timeStr)

      ! run an empty step (for initialization)
      call wrf_run();

      end subroutine
!
!-----------------------------------------------------------------------
!     Atmosphere Check Import Fields
!-----------------------------------------------------------------------
!
      subroutine ATM_CheckImport(gcomp, rc)

      type(ESMF_GridComp)  :: gcomp
      integer, intent(out) :: rc
      
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
      
      rc = ESMF_SUCCESS

      call NUOPC_ModelGet(gcomp, modelClock=clock, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
          line=__LINE__, file=FILENAME)) return

      call NUOPC_CompSetClock(gcomp, clock, atmTimeStep, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
          line=__LINE__, file=FILENAME)) return

      end subroutine
!
!-----------------------------------------------------------------------
!     Run
!-----------------------------------------------------------------------
!
      SUBROUTINE ATM_RUN(gcomp, rc)
      type(ESMF_GridComp)  :: gcomp
      integer, intent(out) :: rc
      
      ! local variables
      type(ESMF_Clock) :: clock
      type(ESMF_State) :: importState, exportState
      TYPE(ESMF_Time) :: currentTime, nextTime
      TYPE(ESMF_Time) :: startTime, stopTime
      TYPE(ESMF_TimeInterval) :: deltaTime
      TYPE(ESMF_TimeInterval) :: runLength     ! how long to run in this call
      CHARACTER(LEN=256) :: timeStr
      CHARACTER(LEN=256) :: ofile
      type(ESMF_Grid) , pointer :: esmfgrid
      type(ESMF_Grid) :: getgrid
      integer :: iLoop_atm = 1
      integer :: n,m

      TYPE(ESMF_GridComp), TARGET :: t_gcomp
      TYPE(ESMF_State),    TARGET :: t_importState, t_exportState
      TYPE(ESMF_Clock),    TARGET :: t_clock

      TYPE(ESMF_GridComp), POINTER :: p_gcomp
      TYPE(ESMF_State),    POINTER :: p_importState, p_exportState
      TYPE(ESMF_Clock),    POINTER :: p_clock

      CHARACTER (ESMF_MAXSTR) :: statename
      CHARACTER (ESMF_MAXSTR), ALLOCATABLE :: itemNames(:)
      TYPE(ESMF_StateItem_Flag), ALLOCATABLE :: itemTypes(:)
      INTEGER :: itemCount, i
      TYPE(ESMF_StateIntent_Flag) :: stateintent
      TYPE(ESMF_Time) :: currTime
      TYPE(ESMF_TimeInterval) :: timeStep
      type(ESMF_Field) :: field_ocnmask, field_sst, field_sstin
      real(ESMF_KIND_R4), pointer :: ptrX(:,:), ptrY(:,:)
      real(ESMF_KIND_R4), pointer :: ptr_sst(:,:)
      real(ESMF_KIND_R4), pointer :: ptr_sstin(:,:)
      real(ESMF_KIND_R4), pointer :: ptr_ocnmask(:,:)
      INTEGER :: ids, ide, jds, jde, kds, kde
      INTEGER :: ims, ime, jms, jme, kms, kme
      INTEGER :: ips, ipe, jps, jpe, kps, kpe
      INTEGER :: nI, nJ
      real(ESMF_KIND_R8) :: wTimeStart, wTimeEnd

      call ESMF_VMWtime(wTimeStart)
  
      rc = ESMF_SUCCESS

      CALL get_ijk_from_grid( head_grid ,                   &
                                ids, ide, jds, jde, kds, kde, &
                                ims, ime, jms, jme, kms, kme, &
                                ips, ipe, jps, jpe, kps, kpe  )
      
      ! query the Component for its clock, importState and exportState
      call ESMF_GridCompGet(gcomp, clock=clock, &
        importState=importState, exportState=exportState, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out

      t_gcomp = gcomp
      t_importState = importState
      t_exportState = exportState
      t_clock = clock

      p_gcomp => t_gcomp
      p_importState => t_importState
      p_exportState => t_exportState
      p_clock => t_clock

      CALL ESMF_SetCurrent( gcomp=p_gcomp, importState=p_importState, &
                            exportState=p_exportState, clock=p_clock)
  
      CALL ESMF_ClockGet( clock, currTime=currentTime, &
                          timeStep=runLength, rc=rc )
      nextTime = currentTime + runLength
      head_grid%start_subtime = currentTime
      head_grid%stop_subtime = nextTime
  
      !! CALL wrf_timetoa ( head_grid%start_subtime, timeStr )
      !! PRINT *, 'wrf_component_run: head_grid%start_subtime ', &
      !!          TRIM(timeStr)
      !! CALL wrf_timetoa ( head_grid%stop_subtime, timeStr )
      !! PRINT *, 'wrf_component_run: head_grid%stop_subtime ', &
      !!          TRIM(timeStr)
      call ESMF_StateGet(importState, itemName="SST", field=field_sst, rc=rc)
      call ESMF_StateGet(importState, itemName="OCNMASK", field=field_ocnmask, rc=rc)
      call ESMF_StateGet(exportState, itemName="SST_INPUT", field=field_sstin, rc=rc)
      call ESMF_FieldGet(field_sst, localDE=0, farrayPtr=ptr_sst, rc=rc)
      call ESMF_FieldGet(field_sstin, localDE=0, farrayPtr=ptr_sstin, rc=rc)
      call ESMF_FieldGet(field_ocnmask, localDE=0, farrayPtr=ptr_ocnmask, rc=rc)
        
      do nJ = jps,MIN(jde-1,jpe)
        do nI = ips,MIN(ide-1,ipe)
          if (ptr_ocnmask(nI,nJ) .lt. 0.5) then
            ptr_sst(nI,nJ) = ptr_sstin(nI,nJ)
          endif
        end do
      end do

      if (associated(ptr_sst)) then
        nullify(ptr_sst)
      end if
      if (associated(ptr_sstin)) then
        nullify(ptr_sstin)
      end if
      if (associated(ptr_ocnmask)) then
        nullify(ptr_ocnmask)
      end if

      call wrf_run();

      iLoop_atm = iLoop_atm + 1

      call ESMF_VMWtime(wTimeEnd)
      atm_wall_time = atm_wall_time + wTimeEnd - wTimeStart

      end subroutine

      SUBROUTINE wrf_findCouplingInterval(startTime, stopTime, &
                                          couplingInterval )
        TYPE(ESMF_Time),         INTENT(IN   ) :: startTime
        TYPE(ESMF_Time),         INTENT(IN   ) :: stopTime
        TYPE(ESMF_TimeInterval), INTENT(  OUT) :: couplingInterval
        ! locals
        LOGICAL :: foundcoupling
        INTEGER :: rc
        INTEGER :: io_form
        ! external function prototype
        INTEGER, EXTERNAL :: use_package
   
        ! deduce coupling time-step
        foundcoupling = .FALSE.
   
        include "med_find_esmf_coupling.inc"
   
        ! look for erroneous use of io_form...  
        CALL nl_get_io_form_restart( 1, io_form )
        IF ( use_package( io_form ) == IO_ESMF ) THEN
          ! CALL wrf_error_fatal ( 'wrf_findCouplingInterval:  ERROR:  ESMF cannot be used for WRF restart I/O' )
        ENDIF
        CALL nl_get_io_form_input( 1, io_form )
        IF ( use_package( io_form ) == IO_ESMF ) THEN
          ! CALL wrf_error_fatal ( 'wrf_findCouplingInterval:  ERROR:  ESMF cannot be used for WRF input' )
        ENDIF
        CALL nl_get_io_form_history( 1, io_form )
        IF ( use_package( io_form ) == IO_ESMF ) THEN
          ! CALL wrf_error_fatal ( 'wrf_findCouplingInterval:  ERROR:  ESMF cannot be used for WRF history output' )
        ENDIF
        CALL nl_get_io_form_boundary( 1, io_form )
        IF ( use_package( io_form ) == IO_ESMF ) THEN
          ! CALL wrf_error_fatal ( 'wrf_findCouplingInterval:  ERROR:  ESMF cannot be used for WRF boundary I/O' )
        ENDIF
   
        ! If nobody uses IO_ESMF, then default is to run WRF all the way to 
        ! the end.  
        IF ( .NOT. foundcoupling ) THEN
          couplingInterval = stopTime - startTime
          ! call wrf_debug ( 1, 'WARNING:  ESMF coupling not used in this WRF run' )
        ENDIF
   
      END SUBROUTINE wrf_findCouplingInterval
   
      SUBROUTINE wrf_getDecompInfo( ids, ide, jds, jde, kds, kde, &
                                    ims, ime, jms, jme, kms, kme, &
                                    ips, ipe, jps, jpe, kps, kpe, &
                                    domdesc, bdy_mask )
        INTEGER, INTENT(OUT) :: ids, ide, jds, jde, kds, kde
        INTEGER, INTENT(OUT) :: ims, ime, jms, jme, kms, kme
        INTEGER, INTENT(OUT) :: ips, ipe, jps, jpe, kps, kpe
        INTEGER, INTENT(OUT) :: domdesc
        LOGICAL, INTENT(OUT) :: bdy_mask(4)

        ! extract decomposition information from head_grid
        CALL get_ijk_from_grid( head_grid ,                   &
                                ids, ide, jds, jde, kds, kde, &
                                ims, ime, jms, jme, kms, kme, &
                                ips, ipe, jps, jpe, kps, kpe  )
        !! #if 0
        !! ! JM
        !! ! with version 3 of ESMF's staggering concepts, WRF's non-staggered grid is equivalent to 
        !! ! esmf's 'exclusive' region -- that is the set of points that are owned by the 'DE' (eyeroll)
        !! ! WRF, on the other hand, is returning the 'staggered' dimensions here.  So convert to the
        !! ! unstaggered dims before returning.
        !! ! Don't bother with vertical dimension for the time being, since we're only doing 2D coupling.
        !! !
        !!      ide = ide-1 ; ipe = MIN(ide,ipe)
        !!      jde = jde-1 ; jpe = MIN(jde,jpe)
        !! #else
        !! ! JM
        !! ! with version 4 I have no damned clue at this writing... just random shots for now
        !! ! see if this works.
        !!      ipe = MIN(ide-1,ipe)
        !!      jpe = MIN(jde-1,jpe)
        !! #endif
        ide = ide-1 ; ipe = MIN(ide,ipe)
        jde = jde-1 ; jpe = MIN(jde,jpe)
   
        domdesc = head_grid%domdesc
        bdy_mask = head_grid%bdy_mask

        END SUBROUTINE wrf_getDecompInfo
   
   
        SUBROUTINE wrf_state_populate( ierr )

        ! Driver layer
        USE module_domain, ONLY : domain
        USE module_io_domain
        ! Model layer
        USE module_configure, ONLY : grid_config_rec_type
        USE module_configure, ONLY : model_to_grid_config_rec
        USE module_bc_time_utilities
   
        IMPLICIT NONE
   
        ! Arguments
        INTEGER, INTENT(OUT)       :: ierr
        ! Local
        TYPE(domain), POINTER      :: grid
        TYPE(grid_config_rec_type) :: config_flags
        INTEGER                    :: stream, idum1, idum2, io_form
        CHARACTER*80               :: fname, n2
        ! external function prototype
        INTEGER, EXTERNAL          :: use_package
   
        ! for now support coupling to head_grid only
        grid => head_grid
   
        CALL model_to_grid_config_rec ( grid%id ,&
          model_config_rec , config_flags )
        CALL set_scalar_indices_from_config ( grid%id , idum1 , idum2 )
   
        stream = 0 
        ierr = 0
        
        include "med_open_esmf_calls.inc"
   
      END SUBROUTINE wrf_state_populate


      end module mod_esmf_atm
