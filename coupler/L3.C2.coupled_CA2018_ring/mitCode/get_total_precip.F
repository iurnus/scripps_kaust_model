C $Header: /u/gcmpack/MITgcm/pkg/cal/cal_set.F,v 1.10 2014/06/05 19:38:45 jmc Exp $
C $Name: checkpoint66h $

#include "CAL_OPTIONS.h"

      SUBROUTINE GET_TOTAL_PRECIP(
     I  total_precip_ESMF, total_evap_ESMF, myThid)

#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "TOTAL_PRECIP.h"

      INTEGER myThid
      INTEGER i,j,k,bi,bj
      Real*8  total_evap_ESMF(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      Real*8  total_precip_ESMF(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)

      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
           total_evap_ESMF(i,j,bi,bj) = total_evap(i,j,bi,bj)
           total_precip_ESMF(i,j,bi,bj) = total_precip(i,j,bi,bj)
         ENDDO
        ENDDO
       ENDDO
      ENDDO

      RETURN
      END
