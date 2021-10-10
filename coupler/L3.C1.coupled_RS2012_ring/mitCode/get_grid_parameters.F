C $Header: /u/gcmpack/MITgcm/model/src/write_grid.F,v 1.40 2016/08/22 22:06:01 jmc Exp $
C $Name: checkpoint66h $

#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"
#include "AD_CONFIG.h"
#ifdef ALLOW_AUTODIFF
# include "AUTODIFF_OPTIONS.h"
#endif

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: GET_PARAMETERS
C     !INTERFACE:
      SUBROUTINE GET_GRID_PARAMETERS(
     O  rA_ESMF, xC_ESMF, xG_ESMF, yC_ESMF, yG_ESMF,
     O  maskC_ESMF, maskS_ESMF, maskW_ESMF,
     I  myThid)

C     !DESCRIPTION:
C     Writes the model geometry and grid arrays to file(s) using which
C     ever I/O package is available/active.  This includes horizontal
C     and vertical grid and finite volume modulating factors (hFacs).
C     Essentailly, it dumps the contents of GRID.h to disk.

C     !CALLING SEQUENCE:
C     INITIALIZE_FIXED
C       |
C       |-- WRITE_GRID

C     !USES:
      IMPLICIT NONE

#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#ifdef ALLOW_MNC
#include "MNC_PARAMS.h"
#endif
#include "GRID.h"

      INTEGER myThid
      INTEGER i,j,k,bi,bj
      REAL*8 rA_ESMF(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      REAL*8 xC_ESMF(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      REAL*8 xG_ESMF(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      REAL*8 yC_ESMF(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      REAL*8 yG_ESMF(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      REAL*8 maskC_ESMF(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      REAL*8 maskS_ESMF(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      REAL*8 maskW_ESMF(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
CEOP

      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
         DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
            rA_ESMF(i,j,bi,bj) = rA(i,j,bi,bj)
            xC_ESMF(i,j,bi,bj) = xC(i,j,bi,bj)
            xG_ESMF(i,j,bi,bj) = xG(i,j,bi,bj)
            yC_ESMF(i,j,bi,bj) = yC(i,j,bi,bj)
            yG_ESMF(i,j,bi,bj) = yG(i,j,bi,bj)
          ENDDO
         ENDDO
       ENDDO
      ENDDO

      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
        DO k=1,Nr
         DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
            maskC_ESMF(i,j,k,bi,bj) = maskC(i,j,k,bi,bj)
            maskS_ESMF(i,j,k,bi,bj) = maskS(i,j,k,bi,bj)
            maskW_ESMF(i,j,k,bi,bj) = maskW(i,j,k,bi,bj)
          ENDDO
         ENDDO
        ENDDO
       ENDDO
      ENDDO


      RETURN
      END
