C $Header: /u/gcmpack/MITgcm/pkg/cal/cal_set.F,v 1.10 2014/06/05 19:38:45 jmc Exp $
C $Name: checkpoint66h $

#include "CAL_OPTIONS.h"

      SUBROUTINE GET_SEAICE(area_ESMF, tice_ESMF, myThid)

#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "FFIELDS.h"
#include "SEAICE_SIZE.h"
#include "SEAICE_PARAMS.h"
#include "SEAICE.h"


      INTEGER myThid
      INTEGER i,j,k,bi,bj
      Real*8  area_ESMF(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      ! Real*8  heff_ESMF(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      Real*8  tice_ESMF(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)

      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
         DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
            area_ESMF(i,j,bi,bj) = AREA(i,j,bi,bj)
            ! heff_ESMF(i,j,bi,bj) = HEFF(i,j,bi,bj)
            tice_ESMF(i,j,bi,bj) = TICES(i,j,1,bi,bj)
          ENDDO
         ENDDO
       ENDDO
      ENDDO

      RETURN
      END
