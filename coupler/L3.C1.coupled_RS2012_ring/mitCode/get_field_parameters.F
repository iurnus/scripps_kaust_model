C $Header: /u/gcmpack/MITgcm/pkg/exf/exf_diagnostics_fill.F,v 1.20 2017/05/11 20:57:50 jmc Exp $
C $Name: checkpoint66h $

#include "EXF_OPTIONS.h"

      SUBROUTINE GET_FIELD_PARAMETERS(
     I  lwdown_ESMF, swdown_ESMF, hl_ESMF, hs_ESMF, uwind_ESMF, 
     I  vwind_ESMF, atemp_ESMF, aqh_ESMF, evap_ESMF,
     I  precip_ESMF, myThid )

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
      REAL*8 hl_ESMF(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      REAL*8 hs_ESMF(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      REAL*8 uwind_ESMF(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      REAL*8 vwind_ESMF(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      REAL*8 atemp_ESMF(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      REAL*8 aqh_ESMF(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      REAL*8 evap_ESMF(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      REAL*8 precip_ESMF(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)

      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
         DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
            lwflux(i,j,bi,bj) = lwdown_ESMF(i,j,bi,bj)
            swflux(i,j,bi,bj) = swdown_ESMF(i,j,bi,bj)
            hl(i,j,bi,bj) = hl_ESMF(i,j,bi,bj)
            hs(i,j,bi,bj) = hs_ESMF(i,j,bi,bj)
            uwind(i,j,bi,bj) = uwind_ESMF(i,j,bi,bj)
            vwind(i,j,bi,bj) = vwind_ESMF(i,j,bi,bj)
            atemp(i,j,bi,bj) = atemp_ESMF(i,j,bi,bj)
            aqh(i,j,bi,bj) = aqh_ESMF(i,j,bi,bj)
            evap(i,j,bi,bj) = evap_ESMF(i,j,bi,bj)
            precip(i,j,bi,bj) = precip_ESMF(i,j,bi,bj)
          ENDDO
         ENDDO
       ENDDO
      ENDDO

      RETURN
      END
