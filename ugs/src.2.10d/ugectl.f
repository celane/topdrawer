      SUBROUTINE UGECTL(OPTN,STRG,IARY,XARY,YARY)
C
C *******************  THE UNIFIED GRAPHICS SYSTEM  *******************
C *                EVENT CONTROL FOR A GRAPHIC DEVICE                 *
C *                                                                   *
C *  THIS SUBROUTINE MAY BE USED TO PERFORM A NUMBER OF OPERATIONS    *
C *  RELATIVE TO EVENTS.  DEFAULT VALUES MAY BE MODIFIED AND          *
C *  SAMPLED CONTROLS MAY BE READ.                                    *
C *                                                                   *
C *  THE CALLING SEQUENCE IS:                                         *
C *    CALL UGECTL(OPTN,STRG,IARY,XARY,YARY)                          *
C *                                                                   *
C *  THE PARAMETERS IN THE CALLING SEQUENCE ARE:                      *
C *    OPTN  THE OPTIONS LIST.                                        *
C *    STRG  A CHARACTER STRING WHICH MAY BE USED FOR BOTH INPUT AND  *
C *          OUTPUT.                                                  *
C *    IARY  A FIXED POINT ARRAY WHICH MAY BE USED FOR BOTH INPUT     *
C *          AND OUTPUT.                                              *
C *    XARY  A FLOATING POINT ARRAY WHICH MAY BE USED FOR BOTH INPUT  *
C *          AND OUTPUT.                                              *
C *    YARY  A FLOATING POINT ARRAY WHICH MAY BE USED FOR BOTH INPUT  *
C *          AND OUTPUT.                                              *
C *                                                                   *
C *                          ROBERT C. BEACH                          *
C *                    COMPUTATION RESEARCH GROUP                     *
C *                STANFORD LINEAR ACCELERATOR CENTER                 *
C *                                                                   *
C *********************************************************************
C
      CHARACTER*(*) OPTN,STRG
      INTEGER       IARY(*)
      REAL          XARY(*),YARY(*)
C
      INCLUDE       'UGSYSTEM:UGDDACBK.FOR'
C
      INCLUDE       'UGSYSTEM:UGERRCBK.FOR'
C
      INTEGER*4     EXST(DDAZ3+6),EXKB,EXKF,EXSR,EXLC,EXVL,EXBU(DDAZ3+1)
      EQUIVALENCE   (EXKB,EXST(1)),       (EXKF,EXST(2)),
     X              (EXSR,EXST(3)),       (EXLC,EXST(4)),
     X              (EXVL,EXST(5)),       (EXBU,EXST(6))
C
      INTEGER       EFLG
      INTEGER       DDIN(3),DDEX(4)
C
      INTEGER       INT1
      INTEGER       DESC_NUM
      PARAMETER    (DESC_NUM = 6)
      INTEGER*4     INST(4,DESC_NUM)
      CHARACTER*16  IFLAG(DESC_NUM)
C
      DATA   (INST(I,1), I=1,4) / 1,6,1,1 /, IFLAG(1) / 'KEYBOARD'/
      DATA   (INST(I,2), I=1,4) / 1,6,2,1 /, IFLAG(2) / 'LOCASE'/
      DATA   (INST(I,3), I=1,4) / 1,6,3,1 /, IFLAG(3) / 'STROKE'/
      DATA   (INST(I,4), I=1,4) / 1,7,4,1 /, IFLAG(4) / 'LOCATOR'/
      DATA   (INST(I,5), I=1,4) / 1,8,5,0 /, IFLAG(5) / 'VALUATOR'/
      DATA   (INST(I,6), I=1,4) / 1,6,6,12 /, IFLAG(6) / 'BUTTON'/
C
C  IS THERE AN INTERACTIVE ACTIVE DEVICE?
      EFLG=0
      IF (DDAAI.EQ.0) GO TO 301
      IF (DDAIL.NE.3) GO TO 306
C
C  SCAN THE OPTIONS LIST.
      EXKB=0
      EXKF=0
      EXSR=0
      EXLC=0
      EXVL=0
      EXBU(DDAZ3+1)=-1
      CALL UGOPTION(OPTN,DESC_NUM,INST,IFLAG,EXST,EXSTR)
C
C  PERFORM THE REQUESTED OPERATIONS.
C    PROCESS "KEYBOARD" IF REQUESTED.
      IF (EXKB.NE.0) THEN
        IF (DDAIC(1).EQ.0) GO TO 302
        CALL UGB003(XARY(1),YARY(1),DDAKX,DDAKY)
        DDAKN=MIN(LEN(STRG),DDAZ2)
        DDAKS(1:DDAKN)=STRG(1:DDAKN)
        DDAKF=EXKF
        DDIN(1)=11
        DDIN(2)=1
        CALL UGZ006(DDAAC,0,0,DDIN,' ',DDEX)
      END IF
C    PROCESS "BUTTON" IF REQUESTED.
  101 IF (EXBU(DDAZ3+1).NE.-1) THEN
        IF (DDAIC(3).EQ.0) GO TO 303
        DO 102 INT1=1,DDAZ3
          DDABF(INT1)=EXBU(INT1)
  102   CONTINUE
        DDIN(1)=11
        DDIN(2)=3
        CALL UGZ006(DDAAC,0,0,DDIN,' ',DDEX)
      END IF
C    PROCESS "STROKE" IF REQUESTED.
  103 IF (EXSR.NE.0) THEN
        IF (DDAIC(4).EQ.0) GO TO 304
        DDASL=NINT((2.0**30)*XARY(1))
        DDAST=NINT(100.0*YARY(1))
        DDASN=MIN(MAX(IARY(1),0),DDAZ4)
        DDIN(1)=11
        DDIN(2)=4
        CALL UGZ006(DDAAC,0,0,DDIN,' ',DDEX)
      END IF
C    PROCESS "LOCATOR" IF REQUESTED.
  104 IF (EXLC.NE.0) THEN
        XARY(1)=0.0
        YARY(1)=0.0
        IF (DDAIC(5).EQ.0) GO TO 305
        IF (DDABC(5).EQ.0) GO TO 307
        DDIN(1)=14
        DDIN(2)=5
        CALL UGZ006(DDAAC,0,0,DDIN,' ',DDEX)
        CALL UGB004(DDEX(2),DDEX(3),XARY(1),YARY(1))
        STRG(1:1)=CHAR(DDEX(4))  ! for use of Topdrawer
      END IF
C    PROCESS "VALUATOR" IF REQUESTED.
  105 IF (EXVL.NE.0) THEN
        XARY(1)=0.0
        IF (EXVL.LT.1) GO TO 306
        IF (EXVL.GT.DDAIC(6)) GO TO 306
        IF (DDABC(6).EQ.0) GO TO 308
        DDIN(1)=14
        DDIN(2)=6
        DDIN(3)=EXVL
        CALL UGZ006(DDAAC,0,0,DDIN,' ',DDEX)
        XARY(1)=REAL(DDEX(2))/(2.0**30)
      END IF
C
C  RESET ERROR INDICATORS AND RETURN TO CALLER.
      IF (EFLG.EQ.0) THEN
        UGELV=0
        UGENM='        '
        UGEIX=0
      END IF
  201 RETURN
C
C  REPORT ERRORS TO THE UNIFIED GRAPHICS SYSTEM ERROR PROCESSOR.
  301 CALL UGRERR(3,'UGECTL  ',12)
      GO TO 201
  302 CALL UGRERR(2,'UGECTL  ',13)
      EFLG=1
      GO TO 101
  303 CALL UGRERR(2,'UGECTL  ',13)
      EFLG=1
      GO TO 103
  304 CALL UGRERR(2,'UGECTL  ',13)
      EFLG=1
      GO TO 104
  305 CALL UGRERR(2,'UGECTL  ',13)
      EFLG=1
      GO TO 105
  306 CALL UGRERR(2,'UGECTL  ',13)
      GO TO 201
  307 CALL UGRERR(2,'UGECTL  ',15)
      EFLG=1
      GO TO 105
  308 CALL UGRERR(2,'UGECTL  ',15)
      GO TO 201
C
      END
