! $Id$
!
! Earth System Modeling Framework
! Copyright 2002-2017, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!-------------------------------------------------------------------------
!BOP
!
! \subsection{Source file for Data for Flow Solver FlowArraysMod.F90}
!
! !DESCRIPTION:
!  Allocate and deallocate ESMF objects which handle data arrays
!  including ESMF\_Fields, ESMF\_Grids,  and ESMF\_Arrays.
!
!EOP
!
      module FlowArraysMod
!
! ESMF modules
!
      use ESMF
    
      implicit none
      
      private
      
!
! Fortran array pointers
!
      real(kind=ESMF_KIND_R4), dimension(:,:), public, pointer, save :: sie, u, v, omega
      real(kind=ESMF_KIND_R4), dimension(:,:), public, pointer, save :: rho, rhoi, rhou, rhov
      real(kind=ESMF_KIND_R4), dimension(:,:), public, pointer, save :: p, q, flag, de

!
! Fields
!
      type(ESMF_Field), public, save :: field_sie, field_u, field_v, field_rho, field_rhoi, &
                                        field_rhou, field_rhov, field_p, field_q, field_flag, &
                                        field_de, field_omega
!
! scalars
!
      integer, public, save :: imin, imax, jmin, jmax
      integer, public, save :: imin_t, imax_t, jmin_t, jmax_t

!
! subroutines
!
      public FlowArraysAlloc, FlowArraysDealloc
      
      contains

!-------------------------------------------------------------------------
 
      subroutine FlowArraysAlloc(grid, rc)

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
!BOE
!
! !DESCRIPTION:
! \subsubsection{Example of Field Creation and Array Usage}
!
!     The following piece of code provides an example of Field creation used in
!     the demo.  In this example we create a Field from an ArraySpec, which
!     designates the rank, type, and kind of the data.  First initialize the
!     ArraySpec with rank 2 for a two-dimensional array
!     and kind ESMF\_KIND\_R4:
!BOC
      call ESMF_ArraySpecSet(arrayspec, rank=2, typekind=ESMF_TYPEKIND_R4, rc=status)
      if(status /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=status)
!EOC
!     Next, create a Field named "SIE" using the ArraySpec with a relative
!     stagger at the cell centers (by default):
!BOC
      field_sie  = ESMF_FieldCreate(grid, arrayspec, indexflag=ESMF_INDEX_GLOBAL, &
                   totalLWidth=haloLWidth, totalUWidth=haloUWidth, name="SIE", rc=status)
      if(status /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=status)
!EOC
!     Once the Field has been created, we get a pointer to
!     the Fortran 90 array data, and call it "sie".
!     Inside the Component "sie" can be used like an array made by an
!     F90 allocation but will reference the data inside "field_sie."
!BOC
      call ESMF_FieldGet(field_sie, farrayPtr=sie, rc=status)
      if(status /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=status)
!EOC
!EOE

      field_u    = ESMF_FieldCreate(grid, arrayspec, staggerloc=ESMF_STAGGERLOC_EDGE1, &
                   indexflag=ESMF_INDEX_GLOBAL, &
                   totalLWidth=haloLWidth, totalUWidth=haloUWidth, name="U", rc=status)
      if(status /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=status)
      call ESMF_FieldGet(field_u, farrayPtr=u, rc=status)
      if(status /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=status)

      field_v    = ESMF_FieldCreate(grid, arrayspec, staggerloc=ESMF_STAGGERLOC_EDGE2, &
                   indexflag=ESMF_INDEX_GLOBAL, &
                   totalLWidth=haloLWidth, totalUWidth=haloUWidth, name="V", rc=status)
      if(status /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=status)
      call ESMF_FieldGet(field_v, farrayPtr=v, rc=status)
      if(status /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=status)

      field_rho  = ESMF_FieldCreate(grid, arrayspec, &
                   indexflag=ESMF_INDEX_GLOBAL, &
                   totalLWidth=haloLWidth, totalUWidth=haloUWidth, name="RHO", rc=status)
      if(status /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=status)
      call ESMF_FieldGet(field_rho, farrayPtr=rho, rc=status)
      if(status /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=status)

      field_rhoi = ESMF_FieldCreate(grid, arrayspec, &
                   indexflag=ESMF_INDEX_GLOBAL, &
                   totalLWidth=haloLWidth, totalUWidth=haloUWidth, name="RHOI", rc=status)
      if(status /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=status)
      call ESMF_FieldGet(field_rhoi, farrayPtr=rhoi, rc=status)
      if(status /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=status)

      field_rhou = ESMF_FieldCreate(grid, arrayspec, staggerloc=ESMF_STAGGERLOC_EDGE1, &
                   indexflag=ESMF_INDEX_GLOBAL, &
                   totalLWidth=haloLWidth, totalUWidth=haloUWidth, name="RHOU", rc=status)
      if(status /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=status)
      call ESMF_FieldGet(field_rhou, farrayPtr=rhou, rc=status)
      if(status /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=status)

      field_rhov = ESMF_FieldCreate(grid, arrayspec, staggerloc=ESMF_STAGGERLOC_EDGE2, &
                   indexflag=ESMF_INDEX_GLOBAL, &
                   totalLWidth=haloLWidth, totalUWidth=haloUWidth, name="RHOV", rc=status)
      if(status /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=status)
      call ESMF_FieldGet(field_rhov, farrayPtr=rhov, rc=status)
      if(status /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=status)

      field_p    = ESMF_FieldCreate(grid, arrayspec, &
                   indexflag=ESMF_INDEX_GLOBAL, &
                   totalLWidth=haloLWidth, totalUWidth=haloUWidth, name="P", rc=status)
      if(status /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=status)
      call ESMF_FieldGet(field_p, farrayPtr=p, rc=status)
      if(status /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=status)

      field_omega= ESMF_FieldCreate(grid, arrayspec, &
                   indexflag=ESMF_INDEX_GLOBAL, &
                   totalLWidth=haloLWidth, totalUWidth=haloUWidth, name="OMEGA", rc=status)
      if(status /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=status)
      call ESMF_FieldGet(field_omega, farrayPtr=omega, rc=status)
      if(status /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=status)

      field_q    = ESMF_FieldCreate(grid, arrayspec, &
                   indexflag=ESMF_INDEX_GLOBAL, &
                   totalLWidth=haloLWidth, totalUWidth=haloUWidth, name="Q", rc=status)
      if(status /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=status)
      call ESMF_FieldGet(field_q, farrayPtr=q, rc=status)
      if(status /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=status)

      field_flag = ESMF_FieldCreate(grid, arrayspec, &
                   indexflag=ESMF_INDEX_GLOBAL, &
                   totalLWidth=haloLWidth, totalUWidth=haloUWidth, name="FLAG", rc=status)
      if(status /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=status)
      call ESMF_FieldGet(field_flag, farrayPtr=flag, rc=status)
      if(status /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=status)

      field_de   = ESMF_FieldCreate(grid, arrayspec, &
                   indexflag=ESMF_INDEX_GLOBAL, &
                   totalLWidth=haloLWidth, totalUWidth=haloUWidth, name="DE", rc=status)
      if(status /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=status)
      call ESMF_FieldGet(field_de, farrayPtr=de, rc=status)
      if(status /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=status)

      if(status .NE. ESMF_SUCCESS) then
        print *, "ERROR in FlowArraysAlloc"
        return
      endif
!
! get bounds information from Field DE for center stagger
!
      call ESMF_FieldGetBounds(field_de, &
                        totalLBound=tlb, totalUBound=tub, &
                        exclusiveLBound=lb, exclusiveUBound=ub, rc=status)
      if(status /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=status)

      ! Exclusive region: data unique to this DE, can be used as source for halo operation
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

      end subroutine FlowArraysAlloc

!-------------------------------------------------------------------------
 
      subroutine FlowArraysDealloc(rc)

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
      call ESMF_FieldDestroy(field_de  , rc=status)
      if(status /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT, rc=status)

      if(status .NE. ESMF_SUCCESS) then
        print *, "ERROR in FlowArraysDealloc"
        return
      endif

      if(present(rc)) rc = ESMF_SUCCESS

      end subroutine FlowArraysDealloc

!------------------------------------------------------------------------------
    end module FlowArraysMod
    
