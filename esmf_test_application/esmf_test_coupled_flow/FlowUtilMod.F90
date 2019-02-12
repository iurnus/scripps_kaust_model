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
! \subsection{Utility module for Coupled Flow Demo}
!
! !DESCRIPTION:
!  ESMF utility module for Coupled Flow Demo.  This file contains the
!  utility subroutines to query and set Field needed attributes in import
!  and export states. The coupler component sets up needed Fields during
!  initialization and redistributes data from import state to export state
!  depending on if a Field redistribution is needed or not.
!
!
!EOE
module FlowUtilMod

use ESMF

implicit none
private

public setFieldNeeded
public isFieldNeeded

contains

!-------------------------------------------------------------------------
!BOPI
! !IROUTINE: setFieldNeeded - Set the needed attribute of a Field with fieldname in a state

! !INTERFACE:
  subroutine setFieldNeeded(state, fieldName, needed, rc)

!
! !ARGUMENTS:
    type(ESMF_State), intent(inout)         :: state
    character(len=*), intent(in)            :: fieldName
    logical, intent(in)                     :: needed
    integer, intent(out), optional          :: rc
!
! !DESCRIPTION:
!     Set the needed attribute of a Field with fieldname in a state.
!     \begin{description}
!     \item [state]
!           State object.
!     \item [fieldName]
!           Field name.
!     \item [needed]
!           Set if data in Field with fieldName needs to be redistributed during coupler step.
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!
!     \end{description}
!
!EOPI

    ! local variables
    type(ESMF_Field)                        :: field
    integer                                 :: localrc

    call ESMF_StateGet(state, itemName=fieldName, field=field, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__, rcToReturn=rc)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)

    call ESMF_AttributeSet(field, "needed", needed, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__, rcToReturn=rc)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)

    if(present(rc)) rc = ESMF_SUCCESS

  end subroutine setFieldNeeded

!-------------------------------------------------------------------------
!BOPI
! !IROUTINE: isFieldNeeded - Query if Field with fieldname in state is needed for coupling

! !INTERFACE:
  function isFieldNeeded(state, fieldName, rc)

!
! !RETURN VALUE:
    logical                                 :: isFieldNeeded

!
! !ARGUMENTS:
    type(ESMF_State), intent(inout)         :: state
    character(len=*), intent(in)            :: fieldName
    integer, intent(out), optional          :: rc
!
! !DESCRIPTION:
!     Query if Field with fieldname in state is needed for coupling.
!     \begin{description}
!     \item [state]
!           State object.
!     \item [fieldName]
!           Field name.
!     \item [rc]
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!
!     \end{description}
!
!EOPI

    ! local variables
    type(ESMF_Field)                        :: field
    integer                                 :: localrc

    call ESMF_StateGet(state, itemName=fieldName, field=field, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__, rcToReturn=rc)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)

    call ESMF_AttributeGet(field, "needed", isFieldNeeded, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__, rcToReturn=rc)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)

    if(present(rc)) rc = ESMF_SUCCESS

  end function isFieldNeeded

end module
