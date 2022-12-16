C $Header: /u/gcmpack/MITgcm/pkg/cal/cal_set.F,v 1.10 2014/06/05 19:38:45 jmc Exp $
C $Name: checkpoint66h $

#include "CAL_OPTIONS.h"

      SUBROUTINE GET_UVOCE(uoce_ESMF, voce_ESMF, myThid)

#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "DYNVARS.h"

      INTEGER myThid
      INTEGER i,j,k,bi,bj
      Real*8  uoce_ESMF(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      Real*8  voce_ESMF(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)

      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
        DO k=1,Nr
         DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
            uoce_ESMF(i,j,k,bi,bj) = uVel(i,j,k,bi,bj)
            voce_ESMF(i,j,k,bi,bj) = vVel(i,j,k,bi,bj)
          ENDDO
         ENDDO
        ENDDO
       ENDDO
      ENDDO

      RETURN
      END
