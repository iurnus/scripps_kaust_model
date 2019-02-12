C $Header: /u/gcmpack/MITgcm/eesupp/src/main.F,v 1.28 2017/01/28 18:39:06 jmc Exp $
C $Name: checkpoint66h $

CBOI
C
C !TITLE: WRAPPER CODE SYNOPSIS
C !AUTHORS: mitgcm developers ( support@mitgcm.org )
C !AFFILIATION: Massachussetts Institute of Technology
C !DATE:
C !INTRODUCTION:
C     Wrapper synopsis and code Routines in the subdirectories under
C     eesupp/ ( src/ and inc/ ) provide the core framework within which
C     numerical and ancilliary software of MITgcm operates.  The eesupp/
C     directories provide a collection of software we call {\bf WRAPPER}
C     ( ({\bf W}rappable {\bf A}pplication {\bf P}aralell {\bf
C     P}rogramming {\bf E}nvironment {\bf R}esource).  The {bf WRAPPER}
C     provides a generic bootstrapping capability to start applications
C     in a manner that allows them to exploit single and
C     multi-processing environments on all present day hardware
C     platforms (spanning vector SMP systems to distributed memory and
C     processing cluster systems). Numerical applications must be coded
C     to fit within the {\bf WRAPPER}. This entails applications
C     adopting a particular style for declaring data structures
C     representing grids and values on grids. The {\bf WRAPPER}
C     currently provides support for grid point models using a single
C     global indexing system. This is sufficient for latitude-logitude,
C     cylindrical, and cartesian coordinate configurations. There is
C     also limited support for composing grids in which no single,
C     sructured global index can be defined. At present, this support is
C     limited to specific configurations of projections of a cube onto
C     the sphere.
C
C     The main functions supported by the current {\bf WRAPPER} code are
C     \begin{itemize}
C     \item program startup and termination including
C       creation/management of multiple threads and/or processes
C     \item communication and synchronisatioin operations between
C       multiple processes and/or threads
C     \item multi-process input and output operations to disk and to
C       other applications
C     \end{itemize}
C
C     Multi-process execution assumes the existence of MPI for process
C     startup and termination. However, MPI does not have to be used for
C     performance critical operations. Instead, {\bf WRAPPER}
C     performance critical parallel primitives are implemented to allow
C     them to bind to different low-level system software
C     layers. Bindings exist for using {\bf WRAPPER} with portable
C     systems such as MPI and UNIX System V IPC memory mapping, as well
C     bindings for high-performance propreitary systems such as Myrinet
C     GM software and Compaq IMC memory channel technology.
C
CEOI

C--   Get C preprocessor options
#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C !ROUTINE: MAIN

C !INTERFACE:
      PROGRAM MAIN

C !DESCRIPTION:
C     *==========================================================*
C     | PROGRAM MAIN
C     | o MAIN wrapper for MITgcm UV implementation.
C     *==========================================================*
C     | MAIN controls the "execution environment".
C     | Its main functions are
C     | 1. call procedure EEBOOT to perform execution environment
C     |    initialisation.
C     | 2. call procedure THE\_MODEL\_MAIN once for each concurrent
C     |    thread. THE\_MODEL\_MAIN is the user supplied top-level
C     |    routine.
C     | 3. call procedure EEDIE to perform execution environment
C     |    shutdown.
C     *==========================================================*

C      !CALLING SEQUENCE:
C
C      main()
C      |
C      |--eeboot()         :: WRAPPER initilization
C      |
C      |--check_threads()  :: Validate multiple thread start up.
C      |
C      |--the_model_main() :: Numerical code top-level driver routine
C      |
C      |--eedie()          :: WRAPPER termination

C     !USES:
      IMPLICIT NONE

      INTEGER myThid
      INTEGER iLoopIn
      Real*8 myTimeIn
      INTEGER myIterIn
      INTEGER nTimeStepsIn
      INTEGER I
      INTEGER dummyComm

      !! myIterIn = 72000
      !! myTimeIn = 6220800000d0
      myIterIn = 0
      myTimeIn = 0d0
      nTimeStepsIn = 1
      iLoopIn = 1
      dummyComm = -1

      CALL MIT_INIT(myThid, dummyComm)

      print *, "myIterIn before the loop is: ", myIterIn
      print *, "myTimeIn before the loop is: ", myTimeIn
      print *, "nTimeStepsIn before the loop is: ", nTimeStepsIn
      print *, "myThid before the loop is: ", myThid

      DO I = 1,21600
        print *, "iLoopIn before the loop is: ", iLoopIn
        print *, "myIterIn before the loop is: ", myIterIn
        print *, "myTimeIn before the loop is: ", myTimeIn
        CALL MIT_RUN(iLoopIn, myTimeIn, myIterIn, nTimeStepsIn, myThid)
        iLoopIn = iLoopIn + 1
        print *, "iLoopIn after the loop is: ", iLoopIn
        print *, "myIterIn after the loop is: ", myIterIn
        print *, "myTimeIn after the loop is: ", myTimeIn
        print *, "myThid before the loop is: ", myThid
      ENDDO

      CALL EEDIE

      !! RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
