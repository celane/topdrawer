      SUBROUTINE UGEVNT(OPTN,TIME,STRG,IARY,XARY,YARY)
C
C *******************  THE UNIFIED GRAPHICS SYSTEM  *******************
C *                      OBTAIN AN EVENT RECORD                       *
C *                                                                   *
C *  THIS SUBROUTINE MAY BE USED TO RETRIEVE AN EVENT RECORD FOR THE  *
C *  ACTIVE GRAPHIC DEVICE.  THE SUBROUTINE MAY RETURN IMMEDIATELY,   *
C *  WAIT A SPECIFIED TIME BEFORE RETURNING, OR WAIT UNTIL AN EVENT   *
C *  OCCURS.                                                          *
C *                                                                   *
C *  THE CALLING SEQUENCE IS:                                         *
C *    CALL UGEVNT(OPTN,TIME,STRG,IARY,XARY,YARY)                     *
C *                                                                   *
C *  THE PARAMETERS IN THE CALLING SEQUENCE ARE:                      *
C *    OPTN  THE OPTIONS LIST.                                        *
C *    TIME  THE NUMBER OF SECONDS THE SUBROUTINE IS TO WAIT.         *
C *    STRG  A CHARACTER STRING TO HOLD THE OUTPUT VALUES.            *
C *    IARY  A FIXED POINT ARRAY TO HOLD THE OUTPUT VALUES.           *
C *    XARY  A FLOATING POINT ARRAY TO HOLD THE OUTPUT VALUES.        *
C *    YARY  A FLOATING POINT ARRAY TO HOLD THE OUTPUT VALUES.        *
C *                                                                   *
C *                          ROBERT C. BEACH                          *
C *                    COMPUTATION RESEARCH GROUP                     *
C *                STANFORD LINEAR ACCELERATOR CENTER                 *
C *                                                                   *
C *********************************************************************
C
      CHARACTER*(*) OPTN
      REAL          TIME
      CHARACTER*(*) STRG
      INTEGER       IARY(*)
      REAL          XARY(*),YARY(*)
C
      INCLUDE       'UGSYSTEM:UGDDACBK.FOR'
C
      INCLUDE       'UGSYSTEM:UGERRCBK.FOR'
C
      INTEGER       DDIN(2),DDEX(2*DDAZ4+2)
      CHARACTER*(DDAZ2) DDST
C
      INTEGER       INT1,INT2
C
C  IS THERE AN INTERACTIVE ACTIVE DEVICE?
      IF (DDAAI.EQ.0) GO TO 301
      IF (DDAIL.NE.3) GO TO 302
C
C  OBTAIN THE NEXT EVENT RECORD.
      DDIN(1)=13
      IF (TIME.LT.0.0) THEN
        DDIN(2)=-1
      ELSE
        DDIN(2)=NINT(100.0*TIME)
      END IF
      CALL UGZ006(DDAAC,0,0,DDIN,DDST,DDEX)
C
C  PROCESS THE EVENT DATA.
      IARY(1)=DDEX(1)
C    PROCESS "KEYBOARD" IF REQUESTED.
      IF (DDEX(1).EQ.1) THEN
        INT1=MIN(DDEX(2),DDAKN)
        IARY(2)=INT1
        INT1=MIN(INT1,LEN(STRG))
        IF (INT1.GT.0) STRG(1:INT1)=DDST(1:INT1)
C    PROCESS "PICK" IF REQUESTED.
      ELSE IF (DDEX(1).EQ.2) THEN
        IARY(2)=DDEX(2)
        IARY(3)=DDEX(3)
C    PROCESS "BUTTON" IF REQUESTED.
      ELSE IF (DDEX(1).EQ.3) THEN
        IARY(2)=DDEX(2)
C    PROCESS "STROKE" IF REQUESTED.
      ELSE IF (DDEX(1).EQ.4) THEN
        INT1=MIN(DDEX(2),DDASN)
        IARY(2)=INT1
        DO 101 INT2=1,INT1
          CALL UGB004(DDEX(2*INT2+1),DDEX(2*INT2+2),
     X                XARY(INT2),YARY(INT2))
  101   CONTINUE
      END IF
C
C  RESET ERROR INDICATORS AND RETURN TO CALLER.
      UGELV=0
      UGENM='        '
      UGEIX=0
  201 RETURN
C
C  REPORT ERRORS TO THE UNIFIED GRAPHICS SYSTEM ERROR PROCESSOR.
  301 CALL UGRERR(3,'UGEVNT  ',12)
      GO TO 201
  302 CALL UGRERR(2,'UGEVNT  ',13)
      GO TO 201
C
      END
