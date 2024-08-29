! $Id$
!-------------------------------------------------------------------------
! Earth System Modeling Framework
! Copyright 2002-2017, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!-------------------------------------------------------------------------

!BOPI
!
! !DESCRIPTION:
!  Global storage of arrays and scalars, using the following 
!    ESMF objects: ESMF\_Field, ESMF\_Grid, ESMF\_Array.
!
!EOPI

      module InjectArraysMod
!
! ESMF modules
!
      use ESMF
    
      implicit none
      
      private
      
!
! Fields
!
      type(ESMF_Field), public, save :: field_sie, field_u, field_v, field_rho, &
                                        field_rhoi, field_rhou, field_rhov, &
                                        field_p, field_q, field_flag
!
! scalars
!
      integer, public, save :: imin, imax, jmin, jmax
      integer, save :: imin_t, imax_t, jmin_t, jmax_t
      
!
! subroutines
!
      public InjectArraysAlloc, InjectArraysDealloc

      contains

!-------------------------------------------------------------------------
 
      subroutine InjectArraysAlloc(grid, rc)

      type(ESMF_Grid) :: grid
      integer, intent(out), optional :: rc

! Local variables
!
      integer :: status
      integer :: haloLWidth(2), haloUWidth(2)
      type(ESMF_ArraySpec) :: arrayspec
      integer, dimension(2) :: lb, ub, tlb, tub
!
! Set initial values
!
      status = ESMF_FAILURE
!
! Initialize return code
!
      if(present(rc)) then
        rc = ESMF_FAILURE
      endif
!
! create fields and get pointers to data
!
      haloLWidth = 1
      haloUWidth = 1
      call ESMF_ArraySpecSet(arrayspec, rank=2, &
                             typekind=ESMF_TYPEKIND_R4, rc=status)
      if(status /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=status)

      field_sie  = ESMF_FieldCreate(grid, arrayspec, &
                   indexflag=ESMF_INDEX_GLOBAL, &
                   totalLWidth=haloLWidth, totalUWidth=haloUWidth, name="SIE", rc=status)
      if(status /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=status)

      field_u    = ESMF_FieldCreate(grid, arrayspec, staggerloc=ESMF_STAGGERLOC_EDGE1, &
                   indexflag=ESMF_INDEX_GLOBAL, &
                   totalLWidth=haloLWidth, totalUWidth=haloUWidth, name="U", rc=status)
      if(status /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=status)

      field_v    = ESMF_FieldCreate(grid, arrayspec, staggerloc=ESMF_STAGGERLOC_EDGE2, &
                   indexflag=ESMF_INDEX_GLOBAL, &
                   totalLWidth=haloLWidth, totalUWidth=haloUWidth, name="V", rc=status)
      if(status /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=status)

      field_rho  = ESMF_FieldCreate(grid, arrayspec, &
                   indexflag=ESMF_INDEX_GLOBAL, &
                   totalLWidth=haloLWidth, totalUWidth=haloUWidth, name="RHO", rc=status)
      if(status /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=status)

      field_rhoi = ESMF_FieldCreate(grid, arrayspec, &
                   indexflag=ESMF_INDEX_GLOBAL, &
                   totalLWidth=haloLWidth, totalUWidth=haloUWidth, name="RHOI", rc=status)
      if(status /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=status)

      field_rhou = ESMF_FieldCreate(grid, arrayspec, staggerloc=ESMF_STAGGERLOC_EDGE1, &
                   indexflag=ESMF_INDEX_GLOBAL, &
                   totalLWidth=haloLWidth, totalUWidth=haloUWidth, name="RHOU", rc=status)
      if(status /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=status)

      field_rhov = ESMF_FieldCreate(grid, arrayspec, staggerloc=ESMF_STAGGERLOC_EDGE2, &
                   indexflag=ESMF_INDEX_GLOBAL, &
                   totalLWidth=haloLWidth, totalUWidth=haloUWidth, name="RHOV", rc=status)
      if(status /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=status)

      field_p    = ESMF_FieldCreate(grid, arrayspec, &
                   indexflag=ESMF_INDEX_GLOBAL, &
                   totalLWidth=haloLWidth, totalUWidth=haloUWidth, name="P", rc=status)
      if(status /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=status)

      field_q    = ESMF_FieldCreate(grid, arrayspec, &
                   indexflag=ESMF_INDEX_GLOBAL, &
                   totalLWidth=haloLWidth, totalUWidth=haloUWidth, name="Q", rc=status)
      if(status /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=status)

      field_flag = ESMF_FieldCreate(grid, arrayspec, &
                   indexflag=ESMF_INDEX_GLOBAL, &
                   totalLWidth=haloLWidth, totalUWidth=haloUWidth, name="FLAG", rc=status)
      if(status /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=status)

      if(status .NE. ESMF_SUCCESS) then
        print *, "ERROR in InjectArraysAlloc"
        return
      endif
!
! get bounds information from Field flag
!      
      call ESMF_FieldGetBounds(field_flag, &
                        totalLBound=tlb, totalUBound=tub, &
                        exclusiveLBound=lb, exclusiveUBound=ub, rc=status)
      if(status /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=status)

      ! Computational region: data unique to this DE
      imin = lb(1)
      imax = ub(1)
      jmin = lb(2)
      jmax = ub(2)
      ! Total region: data plus the halo widths
      imin_t = tlb(1) 
      imax_t = tub(1)
      jmin_t = tlb(2)
      jmax_t = tub(2)

      if(present(rc)) rc = ESMF_SUCCESS

      end subroutine InjectArraysAlloc

!-------------------------------------------------------------------------
 
      subroutine InjectArraysDealloc(rc)

      integer, intent(out), optional :: rc

! Local variables
!
      integer :: status
!
! Set initial values
!
      status = ESMF_FAILURE
!
! Initialize return code
!
      if(present(rc)) then
        rc = ESMF_FAILURE
      endif
!
! deallocate global arrays - destroy the Fields
!
      call ESMF_FieldDestroy(field_sie , rc=status)
      if(status /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=status)
      call ESMF_FieldDestroy(field_u   , rc=status)
      if(status /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=status)
      call ESMF_FieldDestroy(field_v   , rc=status)
      if(status /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=status)
      call ESMF_FieldDestroy(field_rho , rc=status)
      if(status /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=status)
      call ESMF_FieldDestroy(field_rhoi, rc=status)
      if(status /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=status)
      call ESMF_FieldDestroy(field_rhou, rc=status)
      if(status /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=status)
      call ESMF_FieldDestroy(field_rhov, rc=status)
      if(status /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=status)
      call ESMF_FieldDestroy(field_p   , rc=status)
      if(status /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=status)
      call ESMF_FieldDestroy(field_q   , rc=status)
      if(status /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=status)
      call ESMF_FieldDestroy(field_flag, rc=status)
      if(status /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=status)

      if(present(rc)) rc = ESMF_SUCCESS

      end subroutine InjectArraysDealloc

!------------------------------------------------------------------------------
    end module InjectArraysMod
    
