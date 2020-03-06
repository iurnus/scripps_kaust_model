C $Header: /u/gcmpack/MITgcm/pkg/cal/cal_set.F,v 1.10 2014/06/05 19:38:45 jmc Exp $
C $Name: checkpoint66h $

#include "CAL_OPTIONS.h"

      SUBROUTINE MIT_GETCLOCK(myTime0, myIter0)

#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "DYNVARS.h"

      REAL*8 myTime0
      INTEGER myIter0

      myIter0 = NINT( (startTime-baseTime)/deltaTClock)
      myTime0 = startTime

      RETURN
      END
