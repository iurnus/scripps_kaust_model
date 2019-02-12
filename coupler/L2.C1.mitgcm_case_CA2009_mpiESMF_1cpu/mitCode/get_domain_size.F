C $Header: /u/gcmpack/MITgcm_contrib/ESMF/global_ocean.128x60x15/code/get_domain_size.F,v 1.1 2004/03/30 03:58:56 cnh Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

      SUBROUTINE GET_DOMAIN_SIZE(
     O  sNx_ESMF, sNy_ESMF, OLx_ESMF, OLy_ESMF, nSx_ESMF, nSy_ESMF,
     O  nPx_ESMF, nPy_ESMF, Nx_ESMF, Ny_ESMF, Nr_ESMF,
     O  myXGlobalLo_ESMF, myYGlobalLo_ESMF)

      IMPLICIT NONE

C     == Global data ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "EESUPPORT.h"

C     == Routine arguments ==
      INTEGER sNx_ESMF, sNy_ESMF, OLx_ESMF, OLy_ESMF
      INTEGER nSx_ESMF, nSy_ESMF, nPx_ESMF, nPy_ESMF
      INTEGER Nx_ESMF, Ny_ESMF, Nr_ESMF
      INTEGER myXGlobalLo_ESMF, myYGlobalLo_ESMF

      sNx_ESMF = sNx
      sNy_ESMF = sNy
      OLx_ESMF = OLx
      OLy_ESMF = OLy
      nSx_ESMF = nSx
      nSy_ESMF = nSy
      nPx_ESMF = nPx
      nPy_ESMF = nPy
      Nx_ESMF = Nx
      Ny_ESMF = Ny
      Nr_ESMF = Nr
      myXGlobalLo_ESMF = myXGlobalLo
      myYGlobalLo_ESMF = myYGlobalLo

      print *, "snx_ESMF = ", sNx_ESMF

      RETURN
      END
