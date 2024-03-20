      SUBROUTINE UGWDOW(OPTN,VPRT,WDOW)
C
C *******************  THE UNIFIED GRAPHICS SYSTEM  *******************
C *                  DEFINE THE WINDOW AND VIEW PORT                  *
C *                                                                   *
C *  THIS SUBROUTINE WILL TAKE A RECTANGULAR SUBSET OF THE WORLD      *
C *  COORDINATE SYSTEM (THE WINDOW) AND MAP IT INTO A RECTANGULAR     *
C *  SUBSET OF THE DRAWING SPACE (THE VIEW PORT).  THE CURRENT        *
C *  WINDOW AND VIEW PORT MAY ALSO BE RETRIEVED.                      *
C *                                                                   *
C *  THE CALLING SEQUENCE IS:                                         *
C *    CALL UGWDOW(OPTN,VPRT,WDOW)                                    *
C *                                                                   *
C *  THE PARAMETERS IN THE CALLING SEQUENCE ARE:                      *
C *    OPTN  THE OPTIONS LIST.                                        *
C *    VPRT  THE LIMITS OF THE VIEW PORT.                             *
C *    WDOW  THE LIMITS OF THE WINDOW.                                *
C *                                                                   *
C *                          ROBERT C. BEACH                          *
C *                    COMPUTATION RESEARCH GROUP                     *
C *                STANFORD LINEAR ACCELERATOR CENTER                 *
C *                                                                   *
C *********************************************************************
C
      CHARACTER*(*) OPTN
      REAL          VPRT(2,2),WDOW(2,2)
C
      INCLUDE       'UGSYSTEM:UGDDACBK.FOR'
C
      INCLUDE       'UGSYSTEM:UGERRCBK.FOR'
C
      INTEGER*4     EXST(2),EXGP,EXWD
      EQUIVALENCE   (EXGP,EXST(1)),       (EXWD,EXST(2))
C
      REAL          AVPT(2,2)
C
      INTEGER       INT1,INT2
      INTEGER       DESC_NUM
      PARAMETER    (DESC_NUM = 3)
      INTEGER*4     INST(4,DESC_NUM)
      CHARACTER*16  IFLAG(DESC_NUM)
C
      DATA    (INST(I,1),I=1,4) /1,3,1,1 /, IFLAG(1) / 'GET' /
      DATA    (INST(I,2),I=1,4) /1,3,1,2 /, IFLAG(2) / 'PUT' /
      DATA    (INST(I,3),I=1,4) /1,6,2,1 /, IFLAG(3) / 'WINDOW' /
C
C  IS THERE AN ACTIVE DEVICE?
      IF (DDAAI.EQ.0) GO TO 304
C
C  SCAN THE OPTIONS LIST.
      EXGP=0
      EXWD=0
      CALL UGOPTION(OPTN,DESC_NUM,INST,IFLAG,EXST,EXSTR)
C
C  PERFORM THE REQUESTED OPERATION.
      IF (EXGP.EQ.1) THEN
        VPRT(1,1)=DDAWA(1,1)
        VPRT(2,1)=DDAWA(2,1)
        VPRT(1,2)=DDAWA(1,2)
        VPRT(2,2)=DDAWA(2,2)
        WDOW(1,1)=DDAWS(1,1)
        WDOW(2,1)=DDAWS(2,1)
        WDOW(1,2)=DDAWS(1,2)
        WDOW(2,2)=DDAWS(2,2)
      ELSE IF (EXGP.EQ.2) THEN
C  OBTAIN THE ACTUAL VIEW PORT.
        DO 102 INT1=1,2
          DO 101 INT2=1,2
            IF (EXWD.EQ.0) THEN
              AVPT(INT1,INT2)=VPRT(INT1,INT2)
            ELSE
              AVPT(INT1,INT2)=
     X          ((DDAWA(INT1,   2)-DDAWA(INT1,1))*
     X           ( VPRT(INT1,INT2)-DDAWS(INT1,1))/
     X           (DDAWS(INT1,   2)-DDAWS(INT1,1)))+DDAWA(INT1,1)
            END IF
  101     CONTINUE
  102   CONTINUE
C  CHECK INPUT DATA FOR CONSISTENCY.
        IF (AVPT(1,1).GE.AVPT(1,2)) GO TO 301
        IF (AVPT(2,1).GE.AVPT(2,2)) GO TO 301
        IF (AVPT(1,1).LT.DDADS(1,1)) GO TO 302
        IF (AVPT(1,2).GT.DDADS(1,2)) GO TO 302
        IF (AVPT(2,1).LT.DDADS(2,1)) GO TO 302
        IF (AVPT(2,2).GT.DDADS(2,2)) GO TO 302
        IF (WDOW(1,1).GE.WDOW(1,2)) GO TO 303
        IF (WDOW(2,1).GE.WDOW(2,2)) GO TO 303
C  TRANSFER THE DATA TO THE DDA.
        DO 104 INT1=1,2
          DO 103 INT2=1,2
            DDAWD(INT1,INT2)=NINT(REAL(DDADD(INT1,1))+
     X          (REAL(DDADD(INT1,   2)-DDADD(INT1,1))*
     X               ( AVPT(INT1,INT2)-DDADS(INT1,1))/
     X               (DDADS(INT1,   2)-DDADS(INT1,1))))
            DDAWA(INT1,INT2)=AVPT(INT1,INT2)
            DDAWS(INT1,INT2)=WDOW(INT1,INT2)
  103     CONTINUE
  104   CONTINUE
        DDAWX=DDADX*(AVPT(1,2)-AVPT(1,1))/
     X              (WDOW(1,2)-WDOW(1,1))
        DDAWY=DDADY*(AVPT(2,2)-AVPT(2,1))/
     X              (WDOW(2,2)-WDOW(2,1))
        CALL UGB005
      END IF
C
C  RESET ERROR INDICATORS AND RETURN TO CALLER.
      UGELV=0
      UGENM='        '
      UGEIX=0
  201 RETURN
C
C  REPORT ERRORS TO THE UNIFIED GRAPHICS SYSTEM ERROR PROCESSOR.
  301 CALL UGRERR(3,'UGWDOW  ', 1)
      GO TO 201
  302 CALL UGRERR(3,'UGWDOW  ', 2)
      GO TO 201
  303 CALL UGRERR(3,'UGWDOW  ', 3)
      GO TO 201
  304 CALL UGRERR(3,'UGWDOW  ',12)
      GO TO 201
C
      END
