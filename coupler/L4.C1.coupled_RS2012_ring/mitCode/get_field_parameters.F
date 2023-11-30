C $Header: /u/gcmpack/MITgcm/pkg/exf/exf_diagnostics_fill.F,v 1.20 2017/05/11 20:57:50 jmc Exp $
C $Name: checkpoint66h $

#include "EXF_OPTIONS.h"

      SUBROUTINE GET_FIELD_PARAMETERS(
     I  lwdown_ESMF, swdown_ESMF, hl_ESMF, hs_ESMF, uwind_ESMF, 
     I  vwind_ESMF, atemp_ESMF, aqh_ESMF, evap_ESMF, precip_ESMF, 
     I  stokesx_ESMF, stokesy_ESMF, stokesxh_ESMF, stokesyh_ESMF,
     I  langmuir_ESMF, lasl_ESMF, 
     I  tauix_ESMF, tauiy_ESMF, tauox_ESMF, tauoy_ESMF, wn_ESMF, myThid)

      IMPLICIT NONE

C     == global variables ==
#include "EEPARAMS.h"
#include "SIZE.h"
#include "PARAMS.h"
#include "GRID.h"
#include "EXF_PARAM.h"
#include "EXF_FIELDS.h"
#include "EXF_CONSTANTS.h"
#include "WAVE_INPUT.h"

      INTEGER myThid
      INTEGER i,j,k,bi,bj
      REAL*8 wsm, tmpbulk, tmpspeed, tmpspeedsq
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
      REAL*8 stokesx_ESMF(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      REAL*8 stokesy_ESMF(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      REAL*8 stokesxh_ESMF(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      REAL*8 stokesyh_ESMF(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      REAL*8 langmuir_ESMF(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      REAL*8 lasl_ESMF(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      REAL*8 tauix_ESMF(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      REAL*8 tauiy_ESMF(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      REAL*8 tauox_ESMF(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      REAL*8 tauoy_ESMF(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      REAL*8 wn_ESMF(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      REAL*8 stokes_coeff

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
            wave_stokesx(i,j,bi,bj) = stokesx_ESMF(i,j,bi,bj)
            wave_stokesy(i,j,bi,bj) = stokesy_ESMF(i,j,bi,bj)
            wave_stokesxh(i,j,bi,bj) = stokesxh_ESMF(i,j,bi,bj)
            wave_stokesyh(i,j,bi,bj) = stokesyh_ESMF(i,j,bi,bj)
            wave_langmuir(i,j,bi,bj) = langmuir_ESMF(i,j,bi,bj)
            wave_lasl(i,j,bi,bj) = lasl_ESMF(i,j,bi,bj)
            wave_tauix(i,j,bi,bj) = tauix_ESMF(i,j,bi,bj)
            wave_tauiy(i,j,bi,bj) = tauiy_ESMF(i,j,bi,bj)
            wave_tauox(i,j,bi,bj) = tauox_ESMF(i,j,bi,bj)
            wave_tauoy(i,j,bi,bj) = tauoy_ESMF(i,j,bi,bj)
            wave_number(i,j,bi,bj) = wn_ESMF(i,j,bi,bj)
            DO k=1,Nr
             IF ( stokesProfile .eq. 1) THEN 
              stokes_coeff = exp(-2*wn_ESMF(i,j,bi,bj)*abs(rC(k)))
              wave_stokes3dx(i,j,k,bi,bj) = stokes_coeff
     &                                     *stokesx_ESMF(i,j,bi,bj)
              wave_stokes3dy(i,j,k,bi,bj) = stokes_coeff
     &                                     *stokesy_ESMF(i,j,bi,bj)
             ELSEIF ( stokesProfile .eq. 2) THEN 
              stokes_coeff = exp(2*wn_ESMF(i,j,bi,bj)/3.*abs(rC(k)))
     &                      /(1-8*wn_ESMF(i,j,bi,bj)/3.*abs(rC(k)))
              wave_stokes3dx(i,j,k,bi,bj) = stokes_coeff
     &                                     *stokesx_ESMF(i,j,bi,bj)
              wave_stokes3dy(i,j,k,bi,bj) = stokes_coeff
     &                                     *stokesy_ESMF(i,j,bi,bj)
             ENDIF
            ENDDO

            ! print *, 'i,j,: ', i,j, stokesx_ESMF(i,j,bi,bj), maskC(i,j,1,bi,bj)
            ! if (maskC(i,j,1,bi,bj) .gt. 0.95) then
            !   print *, 'i,j,: ', i,j
            !   print *, 'stokes: ', stokesx_ESMF(i,j,bi,bj)
            !   print *, 'maskC', maskC(i,j,1,bi,bj)
            ! endif
            ! if (maskC(i,j,1,bi,bj) .lt. 0.05) then
            !   print *, 'i,j,: ', i,j
            !   print *, 'stokes-: ', stokesx_ESMF(i,j,bi,bj)
            !   print *, 'maskC-', maskC(i,j,1,bi,bj)
            ! endif

          ENDDO
         ENDDO

       ENDDO
      ENDDO

      RETURN
      END
