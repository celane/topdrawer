C      MORTRAN 2.79 (BRACKETED KEYWORD MACROS OF 09/28/81)

      REAL FUNCTION CPULIM(T)
      IMPLICIT NONE
      REAL T
      CPULIM=1.E10
      END

      REAL FUNCTION CPUTIM(T)
      IMPLICIT NONE
      REAL T,CLOCK
      CPUTIM=CLOCK()-T
      END

C     SUBROUTINE HELP(STR)
C     CHARACTER *(*) STR
C     WRITE(0,*) 'HELP: ',STR
C     END

      LOGICAL FUNCTION MATCHC(string,match)
      IMPLICIT NONE
      character*(*) string,match
      INTEGER I,J,IL,JL
      MATCHC=.true.
      I=1
      J=1
      IL=len(string)
      JL=len(match)
      if (match(J:J) .eq. '*') then
      J=J+1
      if (J.gt.JL) return
      if (match(JL:JL) .eq. '*') THEN
      JL=JL-1
      if (J.gt.JL) return
      MATCHC=index(string,match(J:JL)).gt.0
      else
      MATCHC=index(string,match(J:JL)).gt.0.or.len(match(J:JL)).gt.IL
      endif
      elseif (match(JL:JL) .eq. '*') then
      JL=JL-1
      IL=MIN(IL,JL)
      MATCHC=string(1:IL) .eq. match(1:JL)
      else
      MATCHC=string .eq. match
      endif
      return
      end
C     SUBROUTINE READPR(Prompt,line,isize)
C     IMPLICIT NONE
C     CHARACTER*(*) prompt,line
C     INTEGER I, ISIZE
C     IF ( PROMPT .NE. ' ' ) WRITE (*,*) PROMPT 
C     READ ( *, '(A)', END=10010, ERR=10020 ) LINE 
C     ISIZE = 0 
C     DO I = 1 , LEN(LINE)-1 
C     IF ( LINE(I:I) .NE. ' ' ) ISIZE = MAX ( ISIZE , I ) 
C     ENDDO 
C     RETURN
C0010 CONTINUE
C     WRITE (*,*) ' *READPR* endfile ' 
C     STOP
C0020 CONTINUE
C     WRITE (*,*) ' *READPR* error   ' 
C     STOP
C     END
      SUBROUTINE DEFKEY ( OPT )
      CHARACTER*(*) OPT
      RETURN
      END
      REAL FUNCTION RELTIM(OLDTIM)
      IMPLICIT NONE
      INTEGER*4 TIME, ORIGIN
      REAL OLDTIM
      DATA ORIGIN/-1/
      IF (ORIGIN.LT.0) ORIGIN=TIME()
      RELTIM=FLOAT(TIME()-ORIGIN)-OLDTIM
      END
      SUBROUTINE SPAWN(STR)
      INTEGER   T2BTRIM
      CHARACTER *(*) STR,SHELL*80
      IF (STR.NE.' ') THEN
      CALL SYSTEM(STR)
      ELSE
      CALL GETENV('SHELL',SHELL)
      CALL SYSTEM(SHELL(:T2BTRIM(SHELL))//' -l')
      ENDIF
      RETURN
      END
      SUBROUTINE T2SKEY ( I ,  J , R )
      RETURN
      END
      SUBROUTINE T2_TTSTAT(IWIDE,IHIGH)
      IMPLICIT NONE
      INTEGER IWIDE,IHIGH
      IWIDE=80
      IHIGH=24
      END

      SUBROUTINE T2_WAIT(SECS)
      IMPLICIT NONE
      REAL T , SECS , RELTIM
      T=RELTIM(0.0)
 10   CONTINUE
      IF (RELTIM(T).lt.SECS) GOTO 10
      END

      SUBROUTINE TDTIME(STR)
      CHARACTER*(*) STR
      CHARACTER*24  FSTR, FDATE
      FSTR = FDATE ( )
      STR =         FSTR (12:19)
      END

      SUBROUTINE DATE(STR)
      CHARACTER *(*)  STR
      CHARACTER*24   FSTR, FDATE
*  Format of FSTR is  "Day Mon dd hh:mm:ss yyyy"
      FSTR = FDATE ( )
      STR = FSTR(9:10) //'-'// FSTR(5:7) //'-'// FSTR(23:24)
      RETURN
      END
