
!TODO:  remove duplication between ext_esmf_read_field and 
!TODO:  ext_esmf_write_field

!TODO:  how to deal with time?  (via current ESMF_Clock)
!TODO:  to begin, use it as an error check...


!--- read_field
SUBROUTINE ext_esmf_read_field ( DataHandle , DateStr , VarName , Field , FieldType , Comm , IOComm, &
                                 DomainDesc , MemoryOrder , Stagger , DimNames ,              &
                                 DomainStart , DomainEnd ,                                    &
                                 MemoryStart , MemoryEnd ,                                    &
                                 PatchStart , PatchEnd ,                                      &
                                 Status )
  USE module_ext_esmf
  USE ESMF
  USE NUOPC
  IMPLICIT NONE
  INTEGER       ,INTENT(IN)    :: DataHandle 
  CHARACTER*(*) ,intent(inout) :: DateStr
  CHARACTER*(*) ,intent(inout) :: VarName
  integer       ,intent(inout) :: FieldType
  integer       ,intent(inout) :: Comm
  integer       ,intent(inout) :: IOComm
  integer       ,intent(inout) :: DomainDesc
  character*(*) ,intent(inout) :: MemoryOrder
  character*(*) ,intent(inout) :: Stagger
  character*(*) ,intent(inout) :: DimNames(*)
  integer       ,intent(inout) :: DomainStart(*), DomainEnd(*)
  integer       ,intent(inout) :: MemoryStart(*), MemoryEnd(*)
  integer       ,intent(inout) :: PatchStart(*),  PatchEnd(*)
  REAL          ,INTENT(INOUT) :: Field(*)
  integer       ,intent(out)   :: Status
  ! Local declarations
  INTEGER :: ids,ide,jds,jde,kds,kde
  INTEGER :: ims,ime,jms,jme,kms,kme
  INTEGER :: ips,ipe,jps,jpe,kps,kpe
  TYPE(ESMF_State), POINTER :: importstate
  TYPE(ESMF_Field) :: tmpField
  TYPE(ESMF_Array) :: tmpArray
  TYPE(ESMF_ArraySpec) :: arrayspec
  INTEGER :: esmf_kind
  REAL(ESMF_KIND_R4), POINTER :: tmp_esmf_r4_ptr(:,:)
  REAL(ESMF_KIND_R4), POINTER :: tmp_esmf_r4_ptr_global(:,:)
  REAL(ESMF_KIND_R4), DIMENSION(:,:), ALLOCATABLE :: data_esmf_real_ptr
  REAL(ESMF_KIND_R4), POINTER :: data_esmf_real_ptr_global(:,:)
  INTEGER(ESMF_KIND_I4), POINTER :: data_esmf_int_ptr(:,:)
  INTEGER(ESMF_KIND_I4), POINTER :: data_esmf_int_ptr_global(:,:)
  INTEGER, PARAMETER :: esmf_rank = 2
  INTEGER :: DomainEndFull(esmf_rank), idefull, jdefull, ict, i, j
  INTEGER :: PatchEndFull(esmf_rank), ipefull, jpefull
  ! esmf_counts is redundant.  remove it as soon as ESMF_ArrayCreate no 
  ! longer requires it
  INTEGER :: esmf_counts(esmf_rank)
  INTEGER :: rc, debug_level
  LOGICAL, EXTERNAL :: has_char
  character*256 mess
  character*256 ofile
  INTEGER :: nI, nJ, iG, jG
  real(ESMF_KIND_R8), parameter :: MISSING_R8 = 1.0d20

  CALL get_wrf_debug_level( debug_level )

  IF ( .NOT. int_valid_handle( DataHandle ) ) THEN
    CALL wrf_error_fatal("ext_esmf_read_field: invalid data handle" )
  ENDIF
  IF ( .NOT. int_handle_in_use( DataHandle ) ) THEN
    CALL wrf_error_fatal("ext_esmf_read_field: DataHandle not opened" )
  ENDIF
  IF ( .NOT. opened_for_read( DataHandle ) ) THEN
    CALL wrf_error_fatal("ext_esmf_read_field: DataHandle not opened for read" )
  ENDIF

  !! write(mess,*)'ext_esmf_read_field ',DataHandle, TRIM(DateStr), TRIM(VarName)
  !! call wrf_debug( 300, TRIM(mess) )

! BY RUI, only do it when it is SST field
if (TRIM(VarNAME) == 'SST' .or. TRIM(VarNAME) == 'UOCE' .or. TRIM(VarNAME) == 'VOCE') THEN

  IF      ( FieldType .EQ. WRF_REAL ) THEN
    esmf_kind = ESMF_KIND_R4
  ELSE IF ( FieldType .EQ. WRF_DOUBLE ) THEN
  !    esmf_kind = ESMF_KIND_R8
    CALL wrf_error_fatal( 'ext_esmf_read_field, WRF_DOUBLE not yet supported')
  ELSE IF ( FieldType .EQ. WRF_INTEGER ) THEN
    esmf_kind = ESMF_KIND_I4
  !TODO:  implement this (below)
    CALL wrf_error_fatal( 'ext_esmf_read_field, WRF_INTEGER not yet implemented')
  ELSE IF ( FieldType .EQ. WRF_LOGICAL ) THEN
    CALL wrf_error_fatal( 'ext_esmf_read_field, WRF_LOGICAL not yet supported')
  ENDIF

  ims = MemoryStart(1) ; ime = MemoryEnd(1)
  jms = MemoryStart(2) ; jme = MemoryEnd(2)
  kms = MemoryStart(3) ; kme = MemoryEnd(3)

  ips = PatchStart(1) ; ipe = PatchEnd(1)
  jps = PatchStart(2) ; jpe = PatchEnd(2)
  kps = PatchStart(3) ; kpe = PatchEnd(3)

  ids = DomainStart(1) ; ide = DomainEnd(1)
  jds = DomainStart(2) ; jde = DomainEnd(2)
  kds = DomainStart(3) ; kde = DomainEnd(3)

  ! For now, treat all arrays as 2D...  
  IF ( kms /= kme ) THEN
    CALL wrf_error_fatal( 'ext_esmf_read_field:  rank > 2 not yet supported')
  ENDIF

  CALL ioesmf_endfullhack( esmf_rank, DomainEnd, PatchEnd, Stagger, &
                           DomainEndFull, PatchEndFull )
  idefull = DomainEndFull(1)
  jdefull = DomainEndFull(2)
  ipefull = PatchEndFull(1)
  jpefull = PatchEndFull(2)

  ! case 1: the file is opened for read but not committed ("training")
  IF ( .NOT. okay_to_read( DataHandle ) )  THEN

    ! Training:  build the ESMF import state
    !! write(mess,*) ' ext_esmf_read_field: TRAINING READ:  DataHandle = ', DataHandle
    !! call wrf_debug( 300, TRIM(mess) )

    ! First, build the ESMF_Grid for this DataHandle, if it does not 
    CALL ioesmf_create_grid( DataHandle, esmf_rank, MemoryOrder, Stagger,      &
                             DomainStart(1:esmf_rank), DomainEnd(1:esmf_rank), &
                             MemoryStart(1:esmf_rank), MemoryEnd(1:esmf_rank), &
                             PatchStart(1:esmf_rank), PatchEnd(1:esmf_rank) )
    ! Grab the current importState and add to it...
    CALL ESMF_ImportStateGetCurrent( importstate, rc )
    IF ( rc /= ESMF_SUCCESS ) THEN
      CALL wrf_error_fatal("ext_esmf_read_field, training:  ESMF_ImportStateGetCurrent failed" )
    ENDIF

    !! ALLOCATE( tmp_esmf_r4_ptr(ips:ipe,jps:jpe) )
    !! ALLOCATE( tmp_esmf_r4_ptr_global(ids:ide,jds:jde) )
    !! write(mess,*)'ext_esmf_read_field: calling ESMF_FieldCreate field=',trim(varname)
    !! PRINT *, "TESTBUG: ext_esmf_read_field, creating tmpfield", trim(VarName)
    !! CALL wrf_debug ( 100, mess )
    call ESMF_ArraySpecSet(arraySpec, typekind=ESMF_TYPEKIND_R4, &
                           rank=2, rc=rc)

    tmpField = ESMF_FieldCreate(         &
                 grid( DataHandle )%ptr, &
                 arraySpec,              &
                 staggerloc=ESMF_STAGGERLOC_CENTER,    &
                 indexflag=ESMF_INDEX_GLOBAL, &
                 name=TRIM(VarName),     &
                 rc=rc )

    call ESMF_FieldGet(tmpField, localDe=0, farrayPtr=tmp_esmf_r4_ptr_global, rc=rc)
    tmp_esmf_r4_ptr_global = MISSING_R8

    if (associated(tmp_esmf_r4_ptr_global)) then
      nullify(tmp_esmf_r4_ptr_global)
    end if
    IF ( rc /= ESMF_SUCCESS ) THEN
      WRITE(mess,*) ' ext_esmf_read_field: ESMF_FieldCreate failed, rc = ', rc
      CALL wrf_error_fatal( TRIM(mess) )
    ENDIF
    CALL wrf_debug ( 100, 'ext_esmf_read_field: back from ESMF_FieldCreate' )
    CALL wrf_debug ( 100 , TRIM(mess) )

    !! PRINT *, "TESTBUG: reading tmpField1... in case 1, ", TRIM(VarName)
    !! write (ofile, "(A13,A10,A3)") "tmpReadField1", TRIM(VarName), ".nc"
    CALL NUOPC_Realize( importstate, field=tmpField, rc=rc ) 
    IF ( rc /= ESMF_SUCCESS ) THEN
      CALL wrf_error_fatal("ext_esmf_read_field:  ESMF_StateAddField failed" )
    ENDIF

  ! case 2: opened for read and committed
  ELSE IF ( okay_to_read( DataHandle ) )  THEN

    ! read:  extract data from the ESMF import state
    ! Grab the current importState
    CALL ESMF_ImportStateGetCurrent( importstate, rc )
    IF ( rc /= ESMF_SUCCESS ) THEN
      CALL wrf_error_fatal("ext_esmf_read_field:  ESMF_ImportStateGetCurrent failed" )
    ENDIF
    ! grab the Field
    CALL ESMF_StateGet( importstate, itemName=TRIM(VarName), &
                             field=tmpfield, rc=rc )
    IF ( rc /= ESMF_SUCCESS ) THEN
      CALL wrf_error_fatal("ext_esmf_read_field:  ESMF_StateGet failed" )
    ENDIF

    ! grab a pointer to the import state data and copy data into Field
    IF      ( FieldType .EQ. WRF_REAL ) THEN
      if (.not.allocated(data_esmf_real_ptr)) then
        ALLOCATE( data_esmf_real_ptr(ips:ipe,jps:jpe) )
      end if
      CALL ESMF_FieldGet( tmpField, localDe=0, farrayPtr=data_esmf_real_ptr_global, rc=rc )
      IF ( rc /= ESMF_SUCCESS ) THEN
        CALL wrf_error_fatal("ext_esmf_read_field:  ESMF_FieldGetDataPointer(r4) failed" )
      ENDIF

      do nJ = jps, jpe
        do nI = ips, ipe
          data_esmf_real_ptr(nI,nJ) = data_esmf_real_ptr_global(nI,nJ)

          !! PRINT *, "TESTBUG: data_esmf_real_ptr_global is : ", data_esmf_real_ptr_global(nI,nJ)
          !! PRINT *, "TESTBUG: data_esmf_real_ptr is : ", data_esmf_real_ptr(nI,nJ)
        end do
      end do

      CALL ioesmf_extract_data_real( data_esmf_real_ptr, Field,            &
                                     ips, ipe, jps, jpe, kps, kpe, &
                                     ims, ime, jms, jme, kms, kme )
      DEALLOCATE(data_esmf_real_ptr)
    ELSE IF ( FieldType .EQ. WRF_INTEGER ) THEN
    ENDIF

  ENDIF
end if

  Status = 0

  RETURN

END SUBROUTINE ext_esmf_read_field

