#include "MOM_VECINV_OPTIONS.h"
#ifdef ALLOW_AUTODIFF
# include "AUTODIFF_OPTIONS.h"
#endif
#ifdef ALLOW_MOM_COMMON
# include "MOM_COMMON_OPTIONS.h"
#endif

      SUBROUTINE MOM_VECINV(
     I        bi,bj,k,iMin,iMax,jMin,jMax,
     I        kappaRU, kappaRV,
     I        fVerUkm, fVerVkm,
     O        fVerUkp, fVerVkp,
     O        guDiss, gvDiss,
     I        myTime, myIter, myThid )
C     *==========================================================*
C     | S/R MOM_VECINV                                           |
C     | o Form the right hand-side of the momentum equation.     |
C     *==========================================================*
C     | Terms are evaluated one layer at a time working from     |
C     | the bottom to the top. The vertically integrated         |
C     | barotropic flow tendency term is evluated by summing the |
C     | tendencies.                                              |
C     | Notes:                                                   |
C     | We have not sorted out an entirely satisfactory formula  |
C     | for the diffusion equation bc with lopping. The present  |
C     | form produces a diffusive flux that does not scale with  |
C     | open-area. Need to do something to solidfy this and to   |
C     | deal "properly" with thin walls.                         |
C     *==========================================================*
      IMPLICIT NONE

C     == Global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "SURFACE.h"
#include "DYNVARS.h"
#include "FFIELDS.h"
#ifdef ALLOW_MOM_COMMON
# include "MOM_VISC.h"
#endif
#ifdef ALLOW_TIMEAVE
# include "TIMEAVE_STATV.h"
#endif
#ifdef ALLOW_MNC
# include "MNC_PARAMS.h"
#endif
#ifdef ALLOW_AUTODIFF_TAMC
# include "tamc.h"
# include "tamc_keys.h"
#endif
#include "WAVE_INPUT.h"

C     == Routine arguments ==
C     bi,bj   :: current tile indices
C     k       :: current vertical level
C     iMin,iMax,jMin,jMax :: loop ranges
C     fVerU   :: Flux of momentum in the vertical direction, out of the upper
C     fVerV   :: face of a cell k ( flux into the cell above ).
C     fVerUkm :: vertical viscous flux of U, interface above (k-1/2)
C     fVerVkm :: vertical viscous flux of V, interface above (k-1/2)
C     fVerUkp :: vertical viscous flux of U, interface below (k+1/2)
C     fVerVkp :: vertical viscous flux of V, interface below (k+1/2)

C     guDiss  :: dissipation tendency (all explicit terms), u component
C     gvDiss  :: dissipation tendency (all explicit terms), v component
C     myTime  :: current time
C     myIter  :: current time-step number
C     myThid  :: my Thread Id number
      INTEGER bi,bj,k
      INTEGER iMin,iMax,jMin,jMax
      _RL kappaRU(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr+1)
      _RL kappaRV(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr+1)
      _RL fVerUkm(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL fVerVkm(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL fVerUkp(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL fVerVkp(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL guDiss(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL gvDiss(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL     myTime
      INTEGER myIter
      INTEGER myThid

#ifdef ALLOW_MOM_VECINV

C     == Functions ==
      LOGICAL  DIFFERENT_MULTIPLE
      EXTERNAL DIFFERENT_MULTIPLE

C     == Local variables ==
C     strainBC :: same as strain but account for no-slip BC
C     vort3BC  :: same as vort3  but account for no-slip BC
      _RL      vF (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL      vrF(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL      uCf(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL      vCf(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RS hFacZ   (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RS h0FacZ  (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RS r_hFacZ (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL uFld    (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL vFld    (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL uStokes (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL vStokes (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL del2u   (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL del2v   (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL dStar   (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL zStar   (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL tension (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL strain  (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL strainBC(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL stretching(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL Nsquare (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL cDrag   (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL KE      (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL omega3  (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL vort3   (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL vort3BC (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL hDiv    (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL viscAh_Z(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL viscAh_D(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL viscA4_Z(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL viscA4_D(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
C     i,j    :: Loop counters
      INTEGER i,j
C     xxxFac :: On-off tracer parameters used for switching terms off.
      _RL  ArDudrFac
      _RL  ArDvdrFac
      _RL  sideMaskFac
      LOGICAL bottomDragTerms
      LOGICAL writeDiag
#ifdef ALLOW_AUTODIFF_TAMC
      INTEGER imomkey
#endif

#ifdef ALLOW_MNC
      INTEGER offsets(9)
      CHARACTER*(1) pf
#endif

#ifdef ALLOW_AUTODIFF
C--   only the kDown part of fverU/V is set in this subroutine
C--   the kUp is still required
C--   In the case of mom_fluxform kUp is set as well
C--   (at least in part)
      fVerUkm(1,1) = fVerUkm(1,1)
      fVerVkm(1,1) = fVerVkm(1,1)
#endif

#ifdef ALLOW_AUTODIFF_TAMC
          act0 = k - 1
          max0 = Nr
          act1 = bi - myBxLo(myThid)
          max1 = myBxHi(myThid) - myBxLo(myThid) + 1
          act2 = bj - myByLo(myThid)
          max2 = myByHi(myThid) - myByLo(myThid) + 1
          act3 = myThid - 1
          max3 = nTx*nTy
          act4 = ikey_dynamics - 1
          imomkey = (act0 + 1)
     &                    + act1*max0
     &                    + act2*max0*max1
     &                    + act3*max0*max1*max2
     &                    + act4*max0*max1*max2*max3
#endif /* ALLOW_AUTODIFF_TAMC */

      writeDiag = DIFFERENT_MULTIPLE(diagFreq, myTime, deltaTClock)

#ifdef ALLOW_MNC
      IF (useMNC .AND. snapshot_mnc .AND. writeDiag) THEN
        IF ( writeBinaryPrec .EQ. precFloat64 ) THEN
          pf(1:1) = 'D'
        ELSE
          pf(1:1) = 'R'
        ENDIF
        IF ((bi .EQ. 1).AND.(bj .EQ. 1).AND.(k .EQ. 1)) THEN
          CALL MNC_CW_SET_UDIM('mom_vi', -1, myThid)
          CALL MNC_CW_RL_W_S('D','mom_vi',0,0,'T',myTime,myThid)
          CALL MNC_CW_SET_UDIM('mom_vi', 0, myThid)
          CALL MNC_CW_I_W_S('I','mom_vi',0,0,'iter',myIter,myThid)
        ENDIF
        DO i = 1,9
          offsets(i) = 0
        ENDDO
        offsets(3) = k
c       write(*,*) 'offsets = ',(offsets(i),i=1,9)
      ENDIF
#endif /*  ALLOW_MNC  */

C--   Initialise intermediate terms
      DO j=1-OLy,sNy+OLy
       DO i=1-OLx,sNx+OLx
        vF(i,j)    = 0.
        vrF(i,j)   = 0.
        uCf(i,j)   = 0.
        vCf(i,j)   = 0.
        del2u(i,j) = 0.
        del2v(i,j) = 0.
        dStar(i,j) = 0.
        zStar(i,j) = 0.
        guDiss(i,j)= 0.
        gvDiss(i,j)= 0.
        vort3(i,j) = 0.
        omega3(i,j)= 0.
        KE(i,j)    = 0.
C-    need to initialise hDiv for MOM_VI_DEL2UV(call FILL_CS_CORNER_TR_RL)
        hDiv(i,j)  = 0.
c       viscAh_Z(i,j) = 0.
c       viscAh_D(i,j) = 0.
c       viscA4_Z(i,j) = 0.
c       viscA4_D(i,j) = 0.
        strain(i,j)  = 0. _d 0
        strainBC(i,j)= 0. _d 0
        stretching(i,j) = 0. _d 0
        Nsquare(i,j) = 0. _d 0
        tension(i,j) = 0. _d 0
#ifdef ALLOW_AUTODIFF
        hFacZ(i,j)   = 0. _d 0
#endif
       ENDDO
      ENDDO

C--   Term by term tracer parmeters
C     o U momentum equation
      ArDudrFac    = vfFacMom*1.
C     o V momentum equation
      ArDvdrFac    = vfFacMom*1.

C note: using standard stencil (no mask) results in under-estimating
C       vorticity at a no-slip boundary by a factor of 2 = sideDragFactor
      IF ( no_slip_sides ) THEN
        sideMaskFac = sideDragFactor
      ELSE
        sideMaskFac = 0. _d 0
      ENDIF

      IF ( selectImplicitDrag.EQ.0 .AND.
     &      (  no_slip_bottom
     &    .OR. selectBotDragQuadr.GE.0
     &    .OR. bottomDragLinear.NE.0. ) ) THEN
       bottomDragTerms=.TRUE.
      ELSE
       bottomDragTerms=.FALSE.
      ENDIF

C--   Calculate open water fraction at vorticity points
      CALL MOM_CALC_HFACZ(bi,bj,k,hFacZ,r_hFacZ,myThid)

C     Make local copies of horizontal flow field
      DO j=1-OLy,sNy+OLy
       DO i=1-OLx,sNx+OLx
        uFld(i,j) = uVel(i,j,k,bi,bj)
        vFld(i,j) = vVel(i,j,k,bi,bj)
        IF ( stokesCoriolis .eq. 1) THEN 
         uStokes(i,j) = wave_stokes3dx(i,j,k,bi,bj)
         vStokes(i,j) = wave_stokes3dy(i,j,k,bi,bj)
        ELSE
         uStokes(i,j) = 0.0d0
         vStokes(i,j) = 0.0d0
        ENDIF
       ENDDO
      ENDDO

#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE ufld(:,:) =
CADJ &     comlev1_bibj_k, key = imomkey, byte = isbyte
CADJ STORE vfld(:,:) =
CADJ &     comlev1_bibj_k, key = imomkey, byte = isbyte
CADJ STORE hFacZ(:,:) =
CADJ &     comlev1_bibj_k, key = imomkey, byte = isbyte
CADJ STORE r_hFacZ(:,:) =
CADJ &     comlev1_bibj_k, key = imomkey, byte = isbyte
CADJ STORE fverukm(:,:) =
CADJ &     comlev1_bibj_k, key = imomkey, byte = isbyte
CADJ STORE fvervkm(:,:) =
CADJ &     comlev1_bibj_k, key = imomkey, byte = isbyte
#endif

C note (jmc) : Dissipation and Vort3 advection do not necesary
C              use the same maskZ (and hFacZ)  => needs 2 call(s)
c     CALL MOM_VI_HFACZ_DISS(bi,bj,k,hFacZ,r_hFacZ,myThid)

      CALL MOM_CALC_KE(bi,bj,k,selectKEscheme,uFld,vFld,KE,myThid)

      CALL MOM_CALC_RELVORT3(bi,bj,k,uFld,vFld,hFacZ,vort3,myThid)

#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE KE(:,:) =
CADJ &     comlev1_bibj_k, key = imomkey, byte = isbyte
CADJ STORE vort3(:,:) =
CADJ &     comlev1_bibj_k, key = imomkey, byte = isbyte
CADJ STORE vort3bc(:,:) =
CADJ &     comlev1_bibj_k, key = imomkey, byte = isbyte
#endif

C-    mask vort3 and account for no-slip / free-slip BC in vort3BC:
      DO j=1-OLy,sNy+OLy
       DO i=1-OLx,sNx+OLx
         vort3BC(i,j) = vort3(i,j)
         IF ( hFacZ(i,j).EQ.zeroRS ) THEN
           vort3BC(i,j) = sideMaskFac*vort3BC(i,j)
           vort3(i,j)   = 0.
         ENDIF
       ENDDO
      ENDDO

#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE vort3(:,:) =
CADJ &     comlev1_bibj_k, key = imomkey, byte = isbyte
CADJ STORE vort3bc(:,:) =
CADJ &     comlev1_bibj_k, key = imomkey, byte = isbyte
#endif

      IF (momViscosity) THEN
C--    For viscous term, compute horizontal divergence, tension & strain
C      and mask relative vorticity (free-slip case):

       DO j=1-OLy,sNy+OLy
        DO i=1-OLx,sNx+OLx
          h0FacZ(i,j) = hFacZ(i,j)
        ENDDO
       ENDDO
#ifdef NONLIN_FRSURF
       IF ( no_slip_sides .AND. nonlinFreeSurf.GT.0 ) THEN
        DO j=2-OLy,sNy+OLy
         DO i=2-OLx,sNx+OLx
          h0FacZ(i,j) = MIN(
     &       MIN( h0FacW(i,j,k,bi,bj), h0FacW(i,j-1,k,bi,bj) ),
     &       MIN( h0FacS(i,j,k,bi,bj), h0FacS(i-1,j,k,bi,bj) ) )
         ENDDO
        ENDDO
       ENDIF
#endif /* NONLIN_FRSURF */

#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE h0FacZ(:,:) =
CADJ &     comlev1_bibj_k, key = imomkey, byte = isbyte
CADJ STORE hFacZ(:,:) =
CADJ &     comlev1_bibj_k, key = imomkey, byte = isbyte
#endif

       CALL MOM_CALC_HDIV(bi,bj,k,2,uFld,vFld,hDiv,myThid)

       IF ( useVariableVisc .OR. useStrainTensionVisc ) THEN
        CALL MOM_CALC_TENSION( bi,bj,k,uFld,vFld,tension,myThid )
        CALL MOM_CALC_STRAIN( bi,bj,k,uFld,vFld,hFacZ,strain,myThid )
C-    mask strain and account for no-slip / free-slip BC in strainBC:
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
           strainBC(i,j) = strain(i,j)
           IF ( hFacZ(i,j).EQ.zeroRS ) THEN
             strainBC(i,j) = sideMaskFac*strainBC(i,j)
             strain(i,j)   = 0.
           ENDIF
         ENDDO
        ENDDO
#ifdef ALLOW_LEITH_QG
        IF ( viscC2LeithQG.NE.zeroRL ) THEN
          CALL MOM_VISC_QGL_STRETCH(bi,bj,k,
     O                            stretching, Nsquare,
     I                            myTime, myIter, myThid )
          CALL MOM_VISC_QGL_LIMIT(bi,bj,k,
     O                          stretching,
     I                          Nsquare, uFld,vFld,vort3,
     I                          myTime, myIter, myThid )
        ENDIF
#endif /* ALLOW_LEITH_QG */
       ENDIF

#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE hdiv(:,:) =
CADJ &     comlev1_bibj_k, key = imomkey, byte = isbyte
CADJ STORE tension(:,:) =
CADJ &     comlev1_bibj_k, key = imomkey, byte = isbyte
CADJ STORE strain(:,:) =
CADJ &     comlev1_bibj_k, key = imomkey, byte = isbyte
CADJ STORE strainbc(:,:) =
CADJ &     comlev1_bibj_k, key = imomkey, byte = isbyte
#endif

C--    Calculate Lateral Viscosities
       DO j=1-OLy,sNy+OLy
        DO i=1-OLx,sNx+OLx
         viscAh_D(i,j) = viscAhD
         viscAh_Z(i,j) = viscAhZ
         viscA4_D(i,j) = viscA4D
         viscA4_Z(i,j) = viscA4Z
        ENDDO
       ENDDO
       IF ( useVariableVisc ) THEN
C-     uses vort3BC & strainBC which account for no-slip / free-slip BC
#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE KE(:,:) =
CADJ &     comlev1_bibj_k, key = imomkey, byte = isbyte
#endif
         CALL MOM_CALC_VISC( bi, bj, k,
     O            viscAh_Z, viscAh_D, viscA4_Z, viscA4_D,
     I            hDiv, vort3BC, tension, strainBC, stretching,
     I            KE, hfacZ, myThid )
       ENDIF

#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE viscAh_Z(:,:) =
CADJ &     comlev1_bibj_k, key = imomkey, byte = isbyte
CADJ STORE viscAh_D(:,:) =
CADJ &     comlev1_bibj_k, key = imomkey, byte = isbyte
CADJ STORE viscA4_Z(:,:) =
CADJ &     comlev1_bibj_k, key = imomkey, byte = isbyte
CADJ STORE viscA4_D(:,:) =
CADJ &     comlev1_bibj_k, key = imomkey, byte = isbyte
#endif

#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE hDiv(:,:) =
CADJ &     comlev1_bibj_k, key = imomkey, byte = isbyte
CADJ STORE vort3(:,:) =
CADJ &     comlev1_bibj_k, key = imomkey, byte = isbyte
CADJ STORE hFacZ(:,:) =
CADJ &     comlev1_bibj_k, key = imomkey, byte = isbyte
#endif

C      Calculate del^2 u and del^2 v for bi-harmonic term
       IF (useBiharmonicVisc) THEN
         CALL MOM_VI_DEL2UV(bi,bj,k,hDiv,vort3,hFacZ,
     O                      del2u,del2v,
     I                      myThid)
#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE del2u(:,:) =
CADJ &     comlev1_bibj_k, key = imomkey, byte = isbyte
CADJ STORE del2v(:,:) =
CADJ &     comlev1_bibj_k, key = imomkey, byte = isbyte
#endif
         CALL MOM_CALC_HDIV(bi,bj,k,2,del2u,del2v,dStar,myThid)
         CALL MOM_CALC_RELVORT3(bi,bj,k,
     &                          del2u,del2v,hFacZ,zStar,myThid)
       ENDIF

#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE del2u(:,:) =
CADJ &     comlev1_bibj_k, key = imomkey, byte = isbyte
CADJ STORE del2v(:,:) =
CADJ &     comlev1_bibj_k, key = imomkey, byte = isbyte
CADJ STORE dStar(:,:) =
CADJ &     comlev1_bibj_k, key = imomkey, byte = isbyte
CADJ STORE zStar(:,:) =
CADJ &     comlev1_bibj_k, key = imomkey, byte = isbyte
#endif

C---   Calculate dissipation terms for U and V equations

C-     in terms of tension and strain
       IF (useStrainTensionVisc) THEN
C      use masked strain as if free-slip since side-drag is computed separately
         CALL MOM_HDISSIP( bi, bj, k,
     I            tension, strain, hFacZ,
     I            viscAh_Z, viscAh_D, viscA4_Z, viscA4_D,
     I            useHarmonicVisc, useBiharmonicVisc, useVariableVisc,
     O            guDiss, gvDiss,
     I            myThid )
       ELSE
C-     in terms of vorticity and divergence
         CALL MOM_VI_HDISSIP( bi, bj, k,
     I            hDiv, vort3, dStar, zStar, hFacZ,
     I            viscAh_Z, viscAh_D, viscA4_Z, viscA4_D,
     I            useHarmonicVisc, useBiharmonicVisc, useVariableVisc,
     O            guDiss, gvDiss,
     I            myThid )
       ENDIF

C---  Other dissipation terms in Zonal momentum equation

C--   Vertical flux (fVer is at upper face of "u" cell)
C     Eddy component of vertical flux (interior component only) -> vrF
       IF ( .NOT.implicitViscosity ) THEN
        CALL MOM_U_RVISCFLUX(bi,bj,k+1,uVel,kappaRU,vrF,myThid)
C     Combine fluxes
        DO j=jMin,jMax
         DO i=iMin,iMax
          fVerUkp(i,j) = ArDudrFac*vrF(i,j)
         ENDDO
        ENDDO

#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE fVerUkp(:,:) =
CADJ &     comlev1_bibj_k, key = imomkey, byte = isbyte
#endif

C--   Tendency is minus divergence of the fluxes
C     vert.visc.flx is scaled by deepFac2F (deep-atmos) and rhoFac (anelastic)
        DO j=jMin,jMax
         DO i=iMin,iMax
          guDiss(i,j) = guDiss(i,j)
     &    -_recip_hFacW(i,j,k,bi,bj)*recip_drF(k)
     &    *recip_rAw(i,j,bi,bj)
     &    *( fVerUkp(i,j) - fVerUkm(i,j) )*rkSign
     &    *recip_deepFac2C(k)*recip_rhoFacC(k)
         ENDDO
        ENDDO
       ENDIF

C-- No-slip and drag BCs appear as body forces in cell abutting topography
       IF ( no_slip_sides ) THEN
C-     No-slip BCs impose a drag at walls...
        CALL MOM_U_SIDEDRAG( bi, bj, k,
     I           uFld, del2u, h0FacZ,
     I           viscAh_Z, viscA4_Z,
     I           useHarmonicVisc, useBiharmonicVisc, useVariableVisc,
     O           vF,
     I           myThid )
        DO j=jMin,jMax
         DO i=iMin,iMax
          guDiss(i,j) = guDiss(i,j)+vF(i,j)
         ENDDO
        ENDDO
       ENDIF

C-    No-slip BCs impose a drag at bottom
       IF ( bottomDragTerms ) THEN
#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE KE(:,:) =
CADJ &     comlev1_bibj_k, key = imomkey, byte = isbyte
#endif
        CALL MOM_U_BOTDRAG_COEFF( bi, bj, k, .TRUE.,
     I                  uFld, vFld, kappaRU, KE,
     O                  cDrag,
     I                  myIter, myThid )
        DO j=jMin,jMax
         DO i=iMin,iMax
           gUdiss(i,j) = gUdiss(i,j)
     &                 - cDrag(i,j)*uFld(i,j)
     &                 *_recip_hFacW(i,j,k,bi,bj)*recip_drF(k)
         ENDDO
        ENDDO
        IF ( useDiagnostics ) THEN
         DO j=jMin,jMax
          DO i=iMin,iMax
           botDragU(i,j,bi,bj) = botDragU(i,j,bi,bj)
     &                         - cDrag(i,j)*uFld(i,j)*rUnit2mass
          ENDDO
         ENDDO
        ENDIF
       ENDIF
#ifdef ALLOW_SHELFICE
       IF ( useShelfIce ) THEN
#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE KE(:,:) =
CADJ &     comlev1_bibj_k, key = imomkey, byte = isbyte
#endif
        CALL SHELFICE_U_DRAG( bi, bj, k,
     I                uFld, vFld, KE, kappaRU,
     O                vF,
     I                myThid )
        DO j=jMin,jMax
         DO i=iMin,iMax
          guDiss(i,j) = guDiss(i,j) + vF(i,j)
         ENDDO
        ENDDO
       ENDIF
#endif /* ALLOW_SHELFICE */

C---  Other dissipation terms in Meridional momentum equation

C--   Vertical flux (fVer is at upper face of "v" cell)
C     Eddy component of vertical flux (interior component only) -> vrF
       IF ( .NOT.implicitViscosity ) THEN
        CALL MOM_V_RVISCFLUX(bi,bj,k+1,vVel,kappaRV,vrF,myThid)
C     Combine fluxes -> fVerV
        DO j=jMin,jMax
         DO i=iMin,iMax
          fVerVkp(i,j) = ArDvdrFac*vrF(i,j)
         ENDDO
        ENDDO
#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE fVerVkp(:,:) =
CADJ &     comlev1_bibj_k, key = imomkey, byte = isbyte
#endif
C--   Tendency is minus divergence of the fluxes
C     vert.visc.flx is scaled by deepFac2F (deep-atmos) and rhoFac (anelastic)
        DO j=jMin,jMax
         DO i=iMin,iMax
          gvDiss(i,j) = gvDiss(i,j)
     &    -_recip_hFacS(i,j,k,bi,bj)*recip_drF(k)
     &    *recip_rAs(i,j,bi,bj)
     &    *( fVerVkp(i,j) - fVerVkm(i,j) )*rkSign
     &    *recip_deepFac2C(k)*recip_rhoFacC(k)
         ENDDO
        ENDDO
       ENDIF

C-- No-slip and drag BCs appear as body forces in cell abutting topography
       IF ( no_slip_sides ) THEN
C-     No-slip BCs impose a drag at walls...
        CALL MOM_V_SIDEDRAG( bi, bj, k,
     I           vFld, del2v, h0FacZ,
     I           viscAh_Z, viscA4_Z,
     I           useHarmonicVisc, useBiharmonicVisc, useVariableVisc,
     O           vF,
     I           myThid )
        DO j=jMin,jMax
         DO i=iMin,iMax
          gvDiss(i,j) = gvDiss(i,j)+vF(i,j)
         ENDDO
        ENDDO
       ENDIF

C-    No-slip BCs impose a drag at bottom
       IF ( bottomDragTerms ) THEN
#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE KE(:,:) =
CADJ &     comlev1_bibj_k, key = imomkey, byte = isbyte
#endif
        CALL MOM_V_BOTDRAG_COEFF( bi, bj, k, .TRUE.,
     I                  uFld, vFld, kappaRV, KE,
     O                  cDrag,
     I                  myIter, myThid )
        DO j=jMin,jMax
         DO i=iMin,iMax
           gvDiss(i,j) = gvDiss(i,j)
     &                 - cDrag(i,j)*vFld(i,j)
     &                 *_recip_hFacS(i,j,k,bi,bj)*recip_drF(k)
         ENDDO
        ENDDO
        IF ( useDiagnostics ) THEN
         DO j=jMin,jMax
          DO i=iMin,iMax
           botDragV(i,j,bi,bj) = botDragV(i,j,bi,bj)
     &                         - cDrag(i,j)*vFld(i,j)*rUnit2mass
          ENDDO
         ENDDO
        ENDIF
       ENDIF
#ifdef ALLOW_SHELFICE
       IF ( useShelfIce ) THEN
#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE KE(:,:) =
CADJ &     comlev1_bibj_k, key = imomkey, byte = isbyte
#endif
        CALL SHELFICE_V_DRAG( bi, bj, k,
     I                uFld, vFld, KE, kappaRV,
     O                vF,
     I                myThid )
        DO j=jMin,jMax
         DO i=iMin,iMax
          gvDiss(i,j) = gvDiss(i,j) + vF(i,j)
         ENDDO
        ENDDO
       ENDIF
#endif /* ALLOW_SHELFICE */

C--   if (momViscosity) end of block.
      ENDIF

C-    Return to standard hfacZ (min-4) and mask vort3 accordingly:
c     CALL MOM_VI_MASK_VORT3(bi,bj,k,hFacZ,r_hFacZ,vort3,myThid)

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

C---  Prepare for Advection & Coriolis terms:
C-    calculate absolute vorticity
      IF (useAbsVorticity)
     &  CALL MOM_CALC_ABSVORT3(bi,bj,k,vort3,omega3,myThid)

#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE omega3(:,:) =
CADJ &     comlev1_bibj_k, key = imomkey, byte = isbyte
#endif

C--   Horizontal Coriolis terms
c     IF (useCoriolis .AND. .NOT.useCDscheme
c    &    .AND. .NOT. useAbsVorticity) THEN
C- jmc: change it to keep the Coriolis terms when useAbsVorticity=T & momAdvection=F
      IF ( useCoriolis .AND.
     &     .NOT.( useCDscheme .OR. useAbsVorticity.AND.momAdvection )
     &   ) THEN
       IF (useAbsVorticity) THEN
        CALL MOM_VI_U_CORIOLIS(bi,bj,k,selectVortScheme,useJamartMomAdv,
     &                         vFld+vStokes,omega3,hFacZ,r_hFacZ,
     &                         uCf,myThid)
        CALL MOM_VI_V_CORIOLIS(bi,bj,k,selectVortScheme,useJamartMomAdv,
     &                         uFld+uStokes,omega3,hFacZ,r_hFacZ,
     &                         vCf,myThid)
       ELSE
        CALL MOM_VI_CORIOLIS(bi,bj,k,uFld+uStokes,vFld+uStokes,
     &                       hFacZ,r_hFacZ,uCf,vCf,myThid)
       ENDIF
       DO j=jMin,jMax
        DO i=iMin,iMax
         gU(i,j,k,bi,bj) = uCf(i,j)
         gV(i,j,k,bi,bj) = vCf(i,j)
        ENDDO
       ENDDO
       IF ( writeDiag ) THEN
         IF (snapshot_mdsio) THEN
           CALL WRITE_LOCAL_RL('fV','I10',1,uCf,bi,bj,k,myIter,myThid)
           CALL WRITE_LOCAL_RL('fU','I10',1,vCf,bi,bj,k,myIter,myThid)
         ENDIF
#ifdef ALLOW_MNC
         IF (useMNC .AND. snapshot_mnc) THEN
           CALL MNC_CW_RL_W_OFFSET(pf,'mom_vi',bi,bj, 'fV', uCf,
     &          offsets, myThid)
           CALL MNC_CW_RL_W_OFFSET(pf,'mom_vi',bi,bj, 'fU', vCf,
     &          offsets, myThid)
         ENDIF
#endif /*  ALLOW_MNC  */
       ENDIF
#ifdef ALLOW_DIAGNOSTICS
       IF ( useDiagnostics ) THEN
         CALL DIAGNOSTICS_FILL(uCf,'Um_Cori ',k,1,2,bi,bj,myThid)
         CALL DIAGNOSTICS_FILL(vCf,'Vm_Cori ',k,1,2,bi,bj,myThid)
       ENDIF
#endif /* ALLOW_DIAGNOSTICS */
      ELSE
       DO j=jMin,jMax
        DO i=iMin,iMax
         gU(i,j,k,bi,bj) = 0. _d 0
         gV(i,j,k,bi,bj) = 0. _d 0
        ENDDO
       ENDDO
      ENDIF

#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE ucf(:,:) =
CADJ &     comlev1_bibj_k, key = imomkey, byte = isbyte
CADJ STORE vcf(:,:) =
CADJ &     comlev1_bibj_k, key = imomkey, byte = isbyte
#endif

      IF (momAdvection) THEN
C--   Horizontal advection of relative (or absolute) vorticity
       IF ( (highOrderVorticity.OR.upwindVorticity)
     &     .AND.useAbsVorticity ) THEN
        CALL MOM_VI_U_CORIOLIS_C4(bi,bj,k,selectVortScheme,
     &                         highOrderVorticity,upwindVorticity,
     &                         vFld+vStokes,omega3,r_hFacZ,
     &                         uCf,myThid)
       ELSEIF ( (highOrderVorticity.OR.upwindVorticity) ) THEN
        CALL MOM_VI_U_CORIOLIS_C4(bi,bj,k,selectVortScheme,
     &                         highOrderVorticity, upwindVorticity,
     &                         vFld+vStokes,vort3, r_hFacZ,
     &                         uCf,myThid)
       ELSEIF ( useAbsVorticity ) THEN
        CALL MOM_VI_U_CORIOLIS(bi,bj,k,selectVortScheme,useJamartMomAdv,
     &                         vFld+vStokes,omega3,hFacZ,r_hFacZ,
     &                         uCf,myThid)
       ELSE
        CALL MOM_VI_U_CORIOLIS(bi,bj,k,selectVortScheme,useJamartMomAdv,
     &                         vFld+vStokes,vort3, hFacZ,r_hFacZ,
     &                         uCf,myThid)
       ENDIF
       DO j=jMin,jMax
        DO i=iMin,iMax
         gU(i,j,k,bi,bj) = gU(i,j,k,bi,bj)+uCf(i,j)
        ENDDO
       ENDDO
       IF ( (highOrderVorticity.OR.upwindVorticity)
     &     .AND.useAbsVorticity ) THEN
        CALL MOM_VI_V_CORIOLIS_C4(bi,bj,k,selectVortScheme,
     &                         highOrderVorticity, upwindVorticity,
     &                         uFld+uStokes,omega3,r_hFacZ,
     &                         vCf,myThid)
       ELSEIF ( (highOrderVorticity.OR.upwindVorticity) ) THEN
        CALL MOM_VI_V_CORIOLIS_C4(bi,bj,k,selectVortScheme,
     &                         highOrderVorticity, upwindVorticity,
     &                         uFld+uStokes,vort3, r_hFacZ,
     &                         vCf,myThid)
       ELSEIF ( useAbsVorticity ) THEN
        CALL MOM_VI_V_CORIOLIS(bi,bj,k,selectVortScheme,useJamartMomAdv,
     &                         uFld+uStokes,omega3,hFacZ,r_hFacZ,
     &                         vCf,myThid)
       ELSE
        CALL MOM_VI_V_CORIOLIS(bi,bj,k,selectVortScheme,useJamartMomAdv,
     &                         uFld+uStokes,vort3, hFacZ,r_hFacZ,
     &                         vCf,myThid)
       ENDIF
       DO j=jMin,jMax
        DO i=iMin,iMax
         gV(i,j,k,bi,bj) = gV(i,j,k,bi,bj)+vCf(i,j)
        ENDDO
       ENDDO

#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE ucf(:,:) =
CADJ &     comlev1_bibj_k, key = imomkey, byte = isbyte
CADJ STORE vcf(:,:) =
CADJ &     comlev1_bibj_k, key = imomkey, byte = isbyte
#endif

       IF ( writeDiag ) THEN
         IF (snapshot_mdsio) THEN
           CALL WRITE_LOCAL_RL('zV','I10',1,uCf,bi,bj,k,myIter,myThid)
           CALL WRITE_LOCAL_RL('zU','I10',1,vCf,bi,bj,k,myIter,myThid)
         ENDIF
#ifdef ALLOW_MNC
         IF (useMNC .AND. snapshot_mnc) THEN
           CALL MNC_CW_RL_W_OFFSET(pf,'mom_vi',bi,bj, 'zV', uCf,
     &          offsets, myThid)
           CALL MNC_CW_RL_W_OFFSET(pf,'mom_vi',bi,bj, 'zU', vCf,
     &          offsets, myThid)
         ENDIF
#endif /*  ALLOW_MNC  */
       ENDIF

#ifdef ALLOW_TIMEAVE
       IF (taveFreq.GT.0.) THEN
         CALL TIMEAVE_CUMUL_1K1T(uZetatave,vCf,deltaTClock,
     &                           Nr, k, bi, bj, myThid)
         CALL TIMEAVE_CUMUL_1K1T(vZetatave,uCf,deltaTClock,
     &                           Nr, k, bi, bj, myThid)
       ENDIF
#endif /* ALLOW_TIMEAVE */
#ifdef ALLOW_DIAGNOSTICS
       IF ( useDiagnostics ) THEN
         CALL DIAGNOSTICS_FILL(uCf,'Um_AdvZ3',k,1,2,bi,bj,myThid)
         CALL DIAGNOSTICS_FILL(vCf,'Vm_AdvZ3',k,1,2,bi,bj,myThid)
       ENDIF
#endif /* ALLOW_DIAGNOSTICS */

C--   Vertical shear terms (-w*du/dr & -w*dv/dr)
       IF ( .NOT. momImplVertAdv ) THEN
        CALL MOM_VI_U_VERTSHEAR(bi,bj,k,uVel,wVel,uCf,myThid)
        DO j=jMin,jMax
         DO i=iMin,iMax
          gU(i,j,k,bi,bj) = gU(i,j,k,bi,bj)+uCf(i,j)
         ENDDO
        ENDDO
        CALL MOM_VI_V_VERTSHEAR(bi,bj,k,vVel,wVel,vCf,myThid)
        DO j=jMin,jMax
         DO i=iMin,iMax
          gV(i,j,k,bi,bj) = gV(i,j,k,bi,bj)+vCf(i,j)
         ENDDO
        ENDDO
#ifdef ALLOW_DIAGNOSTICS
        IF ( useDiagnostics ) THEN
         CALL DIAGNOSTICS_FILL(uCf,'Um_AdvRe',k,1,2,bi,bj,myThid)
         CALL DIAGNOSTICS_FILL(vCf,'Vm_AdvRe',k,1,2,bi,bj,myThid)
        ENDIF
#endif /* ALLOW_DIAGNOSTICS */
       ENDIF

C--   Bernoulli term
       CALL MOM_VI_U_GRAD_KE(bi,bj,k,KE,uCf,myThid)
       DO j=jMin,jMax
        DO i=iMin,iMax
         gU(i,j,k,bi,bj) = gU(i,j,k,bi,bj)+uCf(i,j)
        ENDDO
       ENDDO
       CALL MOM_VI_V_GRAD_KE(bi,bj,k,KE,vCf,myThid)
       DO j=jMin,jMax
        DO i=iMin,iMax
         gV(i,j,k,bi,bj) = gV(i,j,k,bi,bj)+vCf(i,j)
        ENDDO
       ENDDO

       CALL MOM_VI_U_GRAD_US(bi,bj,k,uFld,vFld,
     &                       uStokes,vStokes,uCf,myThid)
       CALL MOM_VI_V_GRAD_VS(bi,bj,k,uFld,vFld,
     &                       uStokes,vStokes,uCf,myThid)
       DO j=jMin,jMax
        DO i=iMin,iMax
         gU(i,j,k,bi,bj) = gU(i,j,k,bi,bj)+uCf(i,j)
         gV(i,j,k,bi,bj) = gV(i,j,k,bi,bj)+vCf(i,j)
        ENDDO
       ENDDO

       IF ( writeDiag ) THEN
         IF (snapshot_mdsio) THEN
           CALL WRITE_LOCAL_RL('KEx','I10',1,uCf,bi,bj,k,myIter,myThid)
           CALL WRITE_LOCAL_RL('KEy','I10',1,vCf,bi,bj,k,myIter,myThid)
         ENDIF
#ifdef ALLOW_MNC
         IF (useMNC .AND. snapshot_mnc) THEN
           CALL MNC_CW_RL_W_OFFSET(pf,'mom_vi',bi,bj, 'KEx', uCf,
     &          offsets, myThid)
           CALL MNC_CW_RL_W_OFFSET(pf,'mom_vi',bi,bj, 'KEy', vCf,
     &          offsets, myThid)
         ENDIF
#endif /*  ALLOW_MNC  */
       ENDIF

C--   end if momAdvection
      ENDIF

C--   3.D Coriolis term (horizontal momentum, Eastward component: -fprime*w)
      IF ( use3dCoriolis ) THEN
        CALL MOM_U_CORIOLIS_NH(bi,bj,k,wVel,uCf,myThid)
        DO j=jMin,jMax
         DO i=iMin,iMax
          gU(i,j,k,bi,bj) = gU(i,j,k,bi,bj)+uCf(i,j)
         ENDDO
        ENDDO
       IF ( usingCurvilinearGrid ) THEN
C-     presently, non zero angleSinC array only supported with Curvilinear-Grid
        CALL MOM_V_CORIOLIS_NH(bi,bj,k,wVel,vCf,myThid)
        DO j=jMin,jMax
         DO i=iMin,iMax
          gV(i,j,k,bi,bj) = gV(i,j,k,bi,bj)+vCf(i,j)
         ENDDO
        ENDDO
       ENDIF
      ENDIF

C--   Non-Hydrostatic (spherical) metric terms
      IF ( useNHMTerms ) THEN
       CALL MOM_U_METRIC_NH(bi,bj,k,uFld,wVel,uCf,myThid)
       DO j=jMin,jMax
        DO i=iMin,iMax
         gU(i,j,k,bi,bj) = gU(i,j,k,bi,bj)+uCf(i,j)
        ENDDO
       ENDDO
       CALL MOM_V_METRIC_NH(bi,bj,k,vFld,wVel,vCf,myThid)
       DO j=jMin,jMax
        DO i=iMin,iMax
         gV(i,j,k,bi,bj) = gV(i,j,k,bi,bj)+vCf(i,j)
        ENDDO
       ENDDO
      ENDIF

C--   Set du/dt & dv/dt on boundaries to zero
      DO j=jMin,jMax
       DO i=iMin,iMax
        gU(i,j,k,bi,bj) = gU(i,j,k,bi,bj)*_maskW(i,j,k,bi,bj)
        gV(i,j,k,bi,bj) = gV(i,j,k,bi,bj)*_maskS(i,j,k,bi,bj)
       ENDDO
      ENDDO

#ifdef ALLOW_DEBUG
      IF ( debugLevel .GE. debLevC
     &   .AND. k.EQ.4 .AND. myIter.EQ.nIter0
     &   .AND. nPx.EQ.1 .AND. nPy.EQ.1
     &   .AND. useCubedSphereExchange ) THEN
        CALL DEBUG_CS_CORNER_UV( ' uDiss,vDiss from MOM_VECINV',
     &             guDiss,gvDiss, k, standardMessageUnit,bi,bj,myThid )
      ENDIF
#endif /* ALLOW_DEBUG */

      IF ( writeDiag ) THEN
        IF (useBiharmonicVisc) THEN
         CALL WRITE_LOCAL_RL( 'del2u', 'I10', 1, del2u,
     &                        bi,bj,k, myIter, myThid )
         CALL WRITE_LOCAL_RL( 'del2v', 'I10', 1, del2v,
     &                        bi,bj,k, myIter, myThid )
         CALL WRITE_LOCAL_RL( 'dStar', 'I10', 1, dStar,
     &                        bi,bj,k, myIter, myThid )
         CALL WRITE_LOCAL_RL( 'zStar', 'I10', 1, zStar,
     &                        bi,bj,k, myIter, myThid )
        ENDIF
        IF (snapshot_mdsio) THEN
         CALL WRITE_LOCAL_RL('W3','I10',1,omega3, bi,bj,k,myIter,myThid)
         CALL WRITE_LOCAL_RL('Z3','I10',1,vort3BC,bi,bj,k,myIter,myThid)
         CALL WRITE_LOCAL_RL('KE','I10',1,KE,     bi,bj,k,myIter,myThid)
         CALL WRITE_LOCAL_RL('D', 'I10',1,hDiv,   bi,bj,k,myIter,myThid)
         CALL WRITE_LOCAL_RL('Dt','I10',1,tension,bi,bj,k,myIter,myThid)
         CALL WRITE_LOCAL_RL( 'Ds', 'I10', 1, strainBC,
     &                        bi,bj,k, myIter, myThid )
         CALL WRITE_LOCAL_RL('Du','I10',1,guDiss, bi,bj,k,myIter,myThid)
         CALL WRITE_LOCAL_RL('Dv','I10',1,gvDiss, bi,bj,k,myIter,myThid)
        ENDIF
#ifdef ALLOW_MNC
        IF (useMNC .AND. snapshot_mnc) THEN
          CALL MNC_CW_RL_W_OFFSET(pf,'mom_vi',bi,bj,'W3',omega3,
     &          offsets, myThid)
          CALL MNC_CW_RL_W_OFFSET(pf,'mom_vi',bi,bj,'Z3',vort3BC,
     &          offsets, myThid)
          CALL MNC_CW_RL_W_OFFSET(pf,'mom_vi',bi,bj,'KE',KE,
     &          offsets, myThid)
          CALL MNC_CW_RL_W_OFFSET(pf,'mom_vi',bi,bj,'D', hDiv,
     &          offsets, myThid)
          CALL MNC_CW_RL_W_OFFSET(pf,'mom_vi',bi,bj,'Dt',tension,
     &          offsets, myThid)
          CALL MNC_CW_RL_W_OFFSET(pf,'mom_vi',bi,bj,'Ds',strainBC,
     &          offsets, myThid)
          CALL MNC_CW_RL_W_OFFSET(pf,'mom_vi',bi,bj,'Du',guDiss,
     &          offsets, myThid)
          CALL MNC_CW_RL_W_OFFSET(pf,'mom_vi',bi,bj,'Dv',gvDiss,
     &          offsets, myThid)
        ENDIF
#endif /*  ALLOW_MNC  */
      ENDIF

#ifdef ALLOW_DIAGNOSTICS
      IF ( useDiagnostics ) THEN
        CALL DIAGNOSTICS_FILL(vort3BC,'momVort3',k,1,2,bi,bj,myThid)
        CALL DIAGNOSTICS_FILL(KE,     'momKE   ',k,1,2,bi,bj,myThid)
       IF (momViscosity) THEN
        CALL DIAGNOSTICS_FILL(hDiv,   'momHDiv ',k,1,2,bi,bj,myThid)
       ENDIF
       IF ( useVariableVisc .OR. useStrainTensionVisc ) THEN
        CALL DIAGNOSTICS_FILL(tension, 'Tension ',k,1,2,bi,bj,myThid)
        CALL DIAGNOSTICS_FILL(strainBC,'Strain  ',k,1,2,bi,bj,myThid)
C         stretching will be zero, unless using QG Leith
        IF ( viscC2LeithQG.NE.zeroRL ) THEN
          CALL DIAGNOSTICS_FILL(stretching,
     I                          'Stretch ',k,1,2,bi,bj,myThid)
        ENDIF
       ENDIF
        CALL DIAGNOSTICS_FILL(gU(1-OLx,1-OLy,k,bi,bj),
     &                                'Um_Advec',k,1,2,bi,bj,myThid)
        CALL DIAGNOSTICS_FILL(gV(1-OLx,1-OLy,k,bi,bj),
     &                                'Vm_Advec',k,1,2,bi,bj,myThid)
      ENDIF
#endif /* ALLOW_DIAGNOSTICS */

#endif /* ALLOW_MOM_VECINV */

      RETURN
      END
