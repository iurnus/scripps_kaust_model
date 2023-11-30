C $Header: /u/gcmpack/MITgcm/pkg/obcs/OBCS_OPTIONS.h,v 1.4 2004/09/20 23:22:57 heimbach Exp $
C $Name:  $
 
C CPP options file for OBCS package
C
C Use this file for selecting options within the OBCS package
C
C OBCS is enabled with ALLOW_OBCS in CPP_OPTIONS.h

#ifndef OBCS_OPTIONS_H
#define OBCS_OPTIONS_H
#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"

#ifdef ALLOW_OBCS

C Enable individual open boundaries
#undef ALLOW_OBCS_NORTH
#undef ALLOW_OBCS_SOUTH
#define ALLOW_OBCS_EAST
#undef ALLOW_OBCS_WEST

C This include hooks to the Orlanski Open Boundary Radiation code
#undef ALLOW_ORLANSKI

C This include hooks to the NON-linear free surface
#undef NONLIN_FRSURF

C Enable OB values to be prescribed via EXF package
#define ALLOW_OBCS_PRESCRIBE

C This includes hooks to sponge layer treatment of uvel, vvel
#define ALLOW_OBCS_SPONGE

C balance barotropic velocity
#define ALLOW_OBCS_BALANCE

#endif /* ALLOW_OBCS */
#endif /* OBCS_OPTIONS_H */

