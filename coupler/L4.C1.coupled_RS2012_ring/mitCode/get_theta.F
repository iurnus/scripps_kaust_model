C $Header: /u/gcmpack/MITgcm/pkg/cal/cal_set.F,v 1.10 2014/06/05 19:38:45 jmc Exp $
C $Name: checkpoint66h $

#include "CAL_OPTIONS.h"

      SUBROUTINE GET_THETA(theta_ESMF, mld_ESMF, myThid)

#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "FFIELDS.h"
#include "GRID.h"
#include "DYNVARS.h"
#include "KPP.h"

      INTEGER myThid
      INTEGER i,j,k,bi,bj
      Real*8  theta_ESMF(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      Real*8  mld_ESMF(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL sumMLDepth(1-OLx:sNx+OLx,1-OLy:sNy+OLy)

      _RL dens_surf (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL dens_z    (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL delta_dens(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL blg_stra   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL rhoKm1     (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL rhoKp1     (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL blg_minstra(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL blg_str2   (Nr)
      _RL blg_str2max
      integer blgI,blgJ


      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)

        ! returning theta
        DO k=1,Nr
         DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
            theta_ESMF(i,j,k,bi,bj) = theta(i,j,k,bi,bj)
          ENDDO
         ENDDO
        ENDDO

        ! initializing mld
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
            SumMLDepth(i,j) = drf(1)
          ENDDO
        ENDDO

        CALL FIND_RHO_2D(
     I     1-OLx, sNx+OLx, 1-OLy, sNy+OLy, 1,
     I     theta(1-OLx,1-OLy,1,bi,bj), salt(1-OLx,1-OLy,1,bi,bj),
     O     dens_surf,
     I     1, bi, bj, myThid )

        DO k=1,Nr
          DO j=1-OLy,sNy+OLy
           DO i=1-OLx,sNx+OLx
               if (k.eq.1) then
                 delta_dens(i,j,1) = 0. _d 0
               else
                 delta_dens(i,j,k) = 9999. _d 0
               endif
            ENDDO
          ENDDO
        ENDDO

        DO k = 2,Nr
        
          !  Potential density
          CALL FIND_RHO_2D(
     I        1-OLx, sNx+OLx, 1-OLy, sNy+OLy, 1,
     I        theta(1-OLx,1-OLy,k,bi,bj), salt(1-OLx,1-OLy,k,bi,bj),
     O        dens_z,
     I        k, bi, bj, myThid )
          
          DO j=1-OLy,sNy+OLy
            DO i=1-OLx,sNx+OLx
              IF (hFacC(i,j,k,bi,bj) .gt. 0. _d 0) THEN
                delta_dens(i,j,k) = dens_z(i,j)-dens_surf(i,j)
                IF (delta_dens(i,j,k) .LT. 0.03 _d 0) THEN
                  SumMLDepth(i,j) = SumMLDepth(i,j)+drF(k)
                ENDIF
              ENDIF
        
            ENDDO
          ENDDO
        ENDDO

        ! returning mld
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
           ! use old mld definition
           ! mld_ESMF(i,j,bi,bj) = SumMLDepth(i,j)
           ! use new mld definition from KPP
           mld_ESMF(i,j,bi,bj) = max(KPPhbl(i,j,bi,bj),10. _d 0)
         ENDDO
        ENDDO

       ENDDO
      ENDDO

      RETURN
      END
