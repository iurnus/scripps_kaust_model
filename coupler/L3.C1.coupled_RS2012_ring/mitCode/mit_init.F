C $Header: /u/gcmpack/MITgcm/eesupp/src/mit_run.F,v 1.28 2017/01/28 18:39:06 jmc Exp $
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
      SUBROUTINE MIT_INIT(myThid, myComm)

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

C     == Global variables ==
C     Include all the "shared" data here. That means all common
C     blocks used in the model. On many implementations this is not
C     necessary but doing this is the safest method.
#include "SIZE.h"
#include "EEPARAMS.h"
#include "EESUPPORT.h"
#include "PARAMS.h"
#include "GRID.h"
#include "DYNVARS.h"
#include "FFIELDS.h"
#include "SURFACE.h"
#ifdef HAVE_SIGREG
#include "SIGREG.h"
#endif

C     !LOCAL VARIABLES:
C     msgBuf       :: I/O message buffer
C     I            :: loop counter
C     myThid       :: thread Id number
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      INTEGER myThid
      INTEGER I
      INTEGER myComm

#ifdef USE_OMP_THREADING
      INTEGER OMP_GET_THREAD_NUM
      EXTERNAL OMP_GET_THREAD_NUM
#endif

CEOP

      print *, "calling FGSL_IEEE_ENV_SETUP"
#ifdef USE_GSL_IEEE
      CALL FGSL_IEEE_ENV_SETUP ()
#endif

C--   Set up the execution environment
C     EEBOOT loads a execution environment parameter file
C     ( called "eedata" by default ) and sets variables accordingly.
      myThid = 1
      print *, "calling EEBOOT"
      CALL EEBOOT( myComm )

C--   Trap errors
      IF ( eeBootError ) THEN
       fatalError = .TRUE.
       GOTO 999
      ENDIF

      IF (useSETRLSTK) THEN
        CALL setrlstk
      ENDIF
      
      IF (useSIGREG) THEN
        i_got_signal = 0
        CALL sigreg( i_got_signal )
      ENDIF

      DO I=1,nThreads
        myThid = I

        print *, "calling INITIALISE_FIXED"
        CALL INITIALISE_FIXED( myThid )

        print *, "calling INITIALISE_VARIA"
        CALL INITIALISE_VARIA( myThid )

        print *, "calling INI_TOTAL_PRECIP"
        CALL INI_TOTAL_PRECIP( myThid )

      ENDDO

#ifdef HAVE_PTHREADS
c      IF (usePTHREADS) THEN
        CALL PTINIT(nThreads)
c      ELSE
#endif

  999 CONTINUE

C--   Write closedown status
      IF ( fatalError ) THEN
       WRITE( msgBuf,'(A)') 'MIT_INIT(): ends with fatal Error'
       CALL PRINT_ERROR( msgBuf, 1 )
       WRITE(standardMessageUnit,'(A)')
     &      'MIT_INIT(): ends with fatal Error'
       STOP 'ABNORMAL END: PROGRAM MAIN'
      ENDIF

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
