#include "MOM_VECINV_OPTIONS.h"

      SUBROUTINE MOM_VI_U_GRAD_US(
     I        bi,bj,k,
     I        uFld,vFld,uStokes,vStokes,
     O        sStokes,
     I        myThid)
      IMPLICIT NONE
C     /==========================================================\
C     | S/R MOM_U_GRAD_US                                        |
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

      DO j=1-OLy,sNy+OLy
        DO i=2-OLx,sNx+OLx
          sStokes(i,j)=-_recip_dxC(i,j,bi,bj)
     &               *(uStokes(i,j)*(uFld(i,j)-uFld(i-1,j))
     &                +vStokes(i,j)*(vFld(i,j)-vFld(i-1,j)))
     &               *_maskW(i,j,k,bi,bj)*recip_deepFacC(k)
        ENDDO
      ENDDO

      RETURN
      END
