      SUBROUTINE UGMCTL(OPTN,STRG,IARY,XARY)
C
C *******************  THE UNIFIED GRAPHICS SYSTEM  *******************
C *            MISCELLANEOUS CONTROL FOR A GRAPHIC DEVICE             *
C *                                                                   *
C *  THIS SUBROUTINE MAY BE USED TO PERFORM MISCELLANEOUS CONTROL     *
C *  OPERATIONS.  SOME CONTROL FUNCTIONS APPLY TO THE UNIFIED         *
C *  GRAPHICS SYSTEM ITSELF WHILE OTHERS APPLY TO THE ACTIVE GRAPHIC  *
C *  DEVICE.                                                          *
C *                                                                   *
C *  THE CALLING SEQUENCE IS:                                         *
C *    CALL UGMCTL(OPTN,STRG,IARY,XARY)                               *
C *                                                                   *
C *  THE PARAMETERS IN THE CALLING SEQUENCE ARE:                      *
C *    OPTN  THE OPTIONS LIST.                                        *
C *    STRG  A CHARACTER STRING WHICH MAY BE USED FOR BOTH INPUT AND  *
C *          OUTPUT.                                                  *
C *    IARY  A FIXED POINT ARRAY WHICH MAY BE USED FOR BOTH INPUT     *
C *          AND OUTPUT.                                              *
C *    XARY  A FLOATING POINT ARRAY WHICH MAY BE USED FOR BOTH INPUT  *
C *          AND OUTPUT.                                              *
C *                                                                   *
C *                          ROBERT C. BEACH                          *
C *                    COMPUTATION RESEARCH GROUP                     *
C *                STANFORD LINEAR ACCELERATOR CENTER                 *
C *                                                                   *
C *********************************************************************
C
      CHARACTER*(*) OPTN
      CHARACTER*(*) STRG
      INTEGER       IARY(*)
      REAL          XARY(*)
C
      INCLUDE       'UGSYSTEM:UGDDACBK.FOR'
C
      INCLUDE       'UGSYSTEM:UGEMSCBK.FOR'
C
      INCLUDE       'UGSYSTEM:UGERRCBK.FOR'
C
      INTEGER*4     EXST(3),EXEU,EXEM,EXBP
      EQUIVALENCE   (EXEU,EXST(1)),       (EXEM,EXST(2)),
     X              (EXBP,EXST(3))
C
      INTEGER       DDIN(2),DDEX(1)
      INTEGER       DESC_NUM
      PARAMETER    (DESC_NUM = 3)
      INTEGER*4     INST(4,DESC_NUM)
      CHARACTER*16  IFLAG(DESC_NUM)
C
      DATA   (INST(I,1),I=1,4) / 2,7,1,0 /,IFLAG(1) / 'ERRUNIT' /
      DATA   (INST(I,2),I=1,4) / 2,6,2,0 /,IFLAG(2) / 'ERRMAX' /
      DATA   (INST(I,3),I=1,4) / 1,4,3,1 /,IFLAG(3) / 'BEEP' /
C
C  SCAN THE OPTIONS LIST.
      EXEU=EMMEU
      EXEM=EMMPM
      EXBP=0
      CALL UGOPTION(OPTN,DESC_NUM,INST,IFLAG,EXST,EXSTR)
C
C  PROCESS REQUESTS THAT DO NOT REQUIRE AN ACTIVE DEVICE.
      EMMEU=EXEU
      EMMPM=EXEM
C
C  PROCESS REQUESTS THAT REQUIRE AN ACTIVE DEVICE.
C  PROCESS "BEEP" IF NECESSARY.
      IF (EXBP.NE.0) THEN
        IF (DDAAI.EQ.0) GO TO 201
        IF (DDAIL.EQ.1) GO TO 202
        DDIN(1)=10
        DDIN(2)=1
        CALL UGZ006(DDAAC,0,0,DDIN,' ',DDEX)
      END IF
C
C  RESET ERROR INDICATORS AND RETURN TO CALLER.
      UGELV=0
      UGENM='        '
      UGEIX=0
  101 RETURN
C
C  REPORT ERRORS TO THE UNIFIED GRAPHICS SYSTEM ERROR PROCESSOR.
  201 CALL UGRERR(3,'UGMCTL  ',12)
      GO TO 101
  202 CALL UGRERR(2,'UGMCTL  ',13)
      GO TO 101
C
      END
