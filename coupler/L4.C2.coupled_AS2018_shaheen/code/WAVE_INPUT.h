CBOP
C     !ROUTINE: WAVE_INPUT.h
C     !INTERFACE:
C     include "WAVE_INPUT.h"
C     !DESCRIPTION:
C     \bv
C     *==========================================================*
C     | WAVE_.h
C     | o Dynamical model variables (common block WAVE_INPUT)
C     *==========================================================*
C     | The value and two levels of time tendency are held for
C     | each prognostic variable.
C     *==========================================================*
C     \ev
CEOP
C     wave_stokesx: surface Stokes velocity, horizontal
C     wave_stokesy: surface Stokes velocity, meridional
C     wave_stokesx: mixed-layer averaged Stokes velocity, horizontal
C     wave_stokesy: mixed-layer averaged Stokes velocity, meridional
C     wave_stokes3dx: 3-D Stokes velocity, horizontal
C     wave_stokes3dy: 3-D Stokes velocity, meridional
C     wave_langmuir: Langmuir enhancement coefficient, from Li et al. 2016
C     wave_lasl: La_sl in Li et al. 2016
C     wave_tauix: stress tau_ix
C     wave_tauiy: stress tau_iy
C     wave_tauox: stress tau_ox
C     wave_tauoy: stress tau_oy
C     wave_number: mean? wave number
      COMMON /WAVE_INPUT_R/
     &          wave_stokesx, wave_stokesy,
     &          wave_stokesxh, wave_stokesyh,
     &          wave_stokes3dx, wave_stokes3dy,
     &          wave_langmuir, wave_lasl,
     &          wave_tauix, wave_tauiy,
     &          wave_tauox, wave_tauoy, wave_number
      _RL  wave_stokesx(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  wave_stokesy(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  wave_stokesxh(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  wave_stokesyh(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  wave_stokes3dx(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL  wave_stokes3dy(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL  wave_langmuir(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  wave_lasl(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  wave_tauix(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  wave_tauiy(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  wave_tauox(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  wave_tauoy(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  wave_number(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
