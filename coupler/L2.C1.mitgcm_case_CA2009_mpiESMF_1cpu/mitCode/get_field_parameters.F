C $Header: /u/gcmpack/MITgcm/pkg/exf/exf_diagnostics_fill.F,v 1.20 2017/05/11 20:57:50 jmc Exp $
C $Name: checkpoint66h $

#include "EXF_OPTIONS.h"

      SUBROUTINE GET_FIELD_PARAMETERS(
     I  lwdown_ESMF, swdown_ESMF, myThid )

      IMPLICIT NONE

C     == global variables ==
#include "EEPARAMS.h"
#include "SIZE.h"
#include "PARAMS.h"
#include "GRID.h"
#include "EXF_PARAM.h"
#include "EXF_FIELDS.h"
#include "EXF_CONSTANTS.h"

      INTEGER myThid
      INTEGER i,j,bi,bj
      REAL*8 lwdown_ESMF(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      REAL*8 swdown_ESMF(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)

      print *, "converting parameters"
      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
         DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
            lwdown(i,j,bi,bj) = lwdown_ESMF(i,j,bi,bj)
            swdown(i,j,bi,bj) = swdown_ESMF(i,j,bi,bj)
          ENDDO
         ENDDO
       ENDDO
      ENDDO

      RETURN
      END
