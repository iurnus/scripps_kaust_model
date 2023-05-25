#include "MOM_VECINV_OPTIONS.h"

      SUBROUTINE MOM_VI_V_GRAD_VS(
     I        bi,bj,k,
     I        uFld,vFld,uStokes,vStokes,
     O        sStokes,
     I        myThid)
      IMPLICIT NONE
C     /==========================================================\
C     | S/R MOM_V_GRAD_VS                                        |
C     |==========================================================|
C     \==========================================================/

C     == Global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "GRID.h"

C     == Routine arguments ==
      INTEGER bi,bj,k
      _RL uFld(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL vFld(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL uStokes(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL vStokes(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL sStokes(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      INTEGER myThid

C     == Local variables ==
      INTEGER i,j

      DO j=2-OLy,sNy+OLy
        DO i=1-OLx,sNx+OLx
          sStokes(i,j)=-_recip_dyC(i,j,bi,bj)
     &               *(uStokes(i,j)*(uFld(i,j)-uFld(i,j-1))
     &                +vStokes(i,j)*(vFld(i,j)-vFld(i,j-1)))
     &               *_maskS(i,j,k,bi,bj)*recip_deepFacC(k)
        ENDDO
      ENDDO

      RETURN
      END
