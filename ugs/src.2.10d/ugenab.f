      SUBROUTINE UGENAB(OPTN)
C
C *******************  THE UNIFIED GRAPHICS SYSTEM  *******************
C *                ENABLE AN INTERACTIVE CONTROL UNIT                 *
C *                                                                   *
C *  THIS SUBROUTINE MAY BE USED TO ENABLE AN INTERACTIVE CONTROL     *
C *  UNIT ON A GRAPHIC DEVICE.                                        *
C *                                                                   *
C *  THE CALLING SEQUENCE IS:                                         *
C *    CALL UGENAB(OPTN)                                              *
C *                                                                   *
C *  THE PARAMETER IN THE CALLING SEQUENCE IS:                        *
C *    OPTN  THE OPTIONS LIST.                                        *
C *                                                                   *
C *                          ROBERT C. BEACH                          *
C *                    COMPUTATION RESEARCH GROUP                     *
C *                STANFORD LINEAR ACCELERATOR CENTER                 *
C *                                                                   *
C *********************************************************************
C
      CHARACTER*(*) OPTN
C
      INCLUDE       'UGSYSTEM:UGDDACBK.FOR'
C
      INCLUDE       'UGSYSTEM:UGERRCBK.FOR'
C
      INTEGER*4     EXST(DDAZ1),EXIC(DDAZ1)
      EQUIVALENCE   (EXIC(1),EXST(1))
C
      INTEGER       EFLG
      INTEGER       DDIN(3),DDEX(1)
C
      INTEGER       INT1
      INTEGER       DESC_NUM
      PARAMETER    (DESC_NUM  = 6)
      INTEGER*4     INST(4,DESC_NUM)
      CHARACTER*16  IFLAG(DESC_NUM)
C
      DATA    (INST(I,1),I=1,4) /1,8,1,1 /, IFLAG(1) / 'KEYBOARD'/
      DATA    (INST(I,2),I=1,4) /1,4,2,1 /, IFLAG(2) / 'PICK'/
      DATA    (INST(I,3),I=1,4) /1,6,3,1 /, IFLAG(3) / 'BUTTON'/
      DATA    (INST(I,4),I=1,4) /1,6,4,1 /, IFLAG(4) / 'STROKE'/
      DATA    (INST(I,5),I=1,4) /1,7,5,1 /, IFLAG(5) / 'LOCATOR'/
      DATA    (INST(I,6),I=1,4) /1,8,6,1 /, IFLAG(6) / 'VALUATOR'/
C
C  IS THERE AN INTERACTIVE ACTIVE DEVICE?
      EFLG=0
      IF (DDAAI.EQ.0) GO TO 401
      IF (DDAIL.NE.3) GO TO 402
C
C  SCAN THE OPTIONS LIST.
      DO 101 INT1=1,DDAZ1
        EXIC(INT1)=0
  101 CONTINUE
      CALL UGOPTION(OPTN,DESC_NUM,INST,IFLAG,EXST,EXSTR)
C
C  CHECK FOR ENABLE REQUESTS.
      DO 202 INT1=1,DDAZ1
        IF (EXIC(INT1).NE.0) THEN
          IF (DDAIC(INT1).EQ.0) GO TO 403
          IF (DDABC(INT1).EQ.0) THEN
            DDABC(INT1)=DDAIC(INT1)
            DDIN(1)=12
            DDIN(2)=1
            DDIN(3)=INT1
            CALL UGZ006(DDAAC,0,0,DDIN,' ',DDEX)
          END IF
        END IF
  201   CONTINUE
  202 CONTINUE
C
C  RESET ERROR INDICATORS AND RETURN TO CALLER.
      IF (EFLG.EQ.0) THEN
        UGELV=0
        UGENM='        '
        UGEIX=0
      END IF
  301 RETURN
C
C  REPORT ERRORS TO THE UNIFIED GRAPHICS SYSTEM ERROR PROCESSOR.
  401 CALL UGRERR(3,'UGENAB  ',12)
      GO TO 301
  402 CALL UGRERR(2,'UGENAB  ',13)
      GO TO 301
  403 CALL UGRERR(2,'UGENAB  ',13)
      EFLG=1
      GO TO 201
C
      END
