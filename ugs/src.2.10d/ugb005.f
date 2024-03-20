      SUBROUTINE UGB005
C
C *******************  THE UNIFIED GRAPHICS SYSTEM  *******************
C *             PROCESS TWO-DIMENSIONAL TRANSFORMATION DATA           *
C *                                                                   *
C *  THIS SUBROUTINE IS USED TO PROCESS THE TWO-DIMENSIONAL           *
C *  TRANSFORMATION DATA.  IT IS CALLED WHENEVER A NEW WINDOW AND     *
C *  VIEW PORT ARE AVAILABLE.                                         *
C *                                                                   *
C *  THE CALLING SEQUENCE IS:                                         *
C *    CALL UGB005                                                    *
C *                                                                   *
C *  THERE ARE NO PARAMETERS IN THE CALLING SEQUENCE.                 *
C *                                                                   *
C *                          ROBERT C. BEACH                          *
C *                    COMPUTATION RESEARCH GROUP                     *
C *                STANFORD LINEAR ACCELERATOR CENTER                 *
C *                                                                   *
C *********************************************************************
C
      INCLUDE       'UGSYSTEM:UGDDACBK.FOR'
C
      REAL          FLT1,FLT2,FLT3
      INTEGER       INT1
C
C  INITIALIZE THE TRANSFORMATION DATA.
      DDATR(1)=(REAL(DDAWD(1,2)-DDAWD(1,1)))/(DDAWS(1,2)-DDAWS(1,1))
      DDATR(2)=(REAL(DDAWD(2,2)-DDAWD(2,1)))/(DDAWS(2,2)-DDAWS(2,1))
      DDATR(3)=REAL(DDAWD(1,1))
      DDATR(4)=REAL(DDAWD(2,1))
      FLT3=REAL(DDABD(1,2)-DDABD(1,1))
      FLT1=DDABX*FLT3
      FLT2=DDABY*REAL(DDABD(2,2)-DDABD(2,1))
      IF (FLT2.LE.FLT1) FLT3=FLT3-(FLT1-FLT2)/DDABX
      DDATR(5)=FLT3/DDATR(1)
      DDATR(6)=(DDABX*DDATR(1))/(DDABY*DDATR(2))
C
C  INITIALIZE THE SHIELDS.
      DDASA=0
      DO 101 INT1=1,DDAZ5
        DDASF(INT1)=0
  101 CONTINUE
C
C  RETURN TO CALLER.
      RETURN
C
      END
