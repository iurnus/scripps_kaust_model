CBOP
C     !ROUTINE: TOTAL_PRECIP.h
C     !INTERFACE:
C     include "TOTAL_PRECIP.h"
C     !DESCRIPTION:
C     \bv
C     *==========================================================*
C     | TOTAL_PRECIP.h
C     | o Dynamical model variables (common block TOTAL_PRECIP)
C     *==========================================================*
C     | The value and two levels of time tendency are held for
C     | each prognostic variable.
C     *==========================================================*
C     \ev
CEOP
      COMMON /TOTAL_PRECIP_R/
     &                   total_precip, total_evap, sst_ini
      _RL  total_precip(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  total_evap(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  sst_ini(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
