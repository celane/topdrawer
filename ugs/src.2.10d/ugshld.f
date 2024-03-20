      SUBROUTINE UGSHLD(OPTN,SHLD)
C
C *******************  THE UNIFIED GRAPHICS SYSTEM  *******************
C *                          DEFINE A SHIELD                          *
C *                                                                   *
C *  THIS SUBROUTINE WILL TAKE A RECTANGULAR SUBSET OF THE CURRENT    *
C *  WINDOW AND SHIELD IT FROM THE DRAWING OF GRAPHIC PRIMITIVES.     *
C *  IT CAN ALSO RETRIEVE THE PARAMETERS OF A SHIELD OR DELETE A      *
C *  SHIELD.                                                          *
C *                                                                   *
C *  THE CALLING SEQUENCE IS:                                         *
C *    CALL UGSHLD(OPTN,SHLD)                                         *
C *                                                                   *
C *  THE PARAMETERS IN THE CALLING SEQUENCE ARE:                      *
C *    OPTN  THE OPTIONS LIST.                                        *
C *    SHLD  THE LIMITS OF THE SHIELD IN THE WINDOW COORDINATE        *
C *          SYSTEM.                                                  *
C *                                                                   *
C *                          ROBERT C. BEACH                          *
C *                    COMPUTATION RESEARCH GROUP                     *
C *                STANFORD LINEAR ACCELERATOR CENTER                 *
C *                                                                   *
C *********************************************************************
C
      CHARACTER*(*) OPTN
      REAL          SHLD(2,2)
C
      INCLUDE       'UGSYSTEM:UGDDACBK.FOR'
C
      INCLUDE       'UGSYSTEM:UGERRCBK.FOR'
C
      INTEGER*4     EXST(2),EXGP,EXSI
      EQUIVALENCE   (EXGP,EXST(1)),       (EXSI,EXST(2))
C
      INTEGER       INT1
      INTEGER       DESC_NUM
      PARAMETER    (DESC_NUM = 4)
      INTEGER*4     INST(4,DESC_NUM)
      CHARACTER*16  IFLAG(DESC_NUM)
C
      DATA   (INST(I,1),I=1,4) / 1,3,1,1 /, IFLAG(1) / 'GET' /
      DATA   (INST(I,2),I=1,4) / 1,3,1,2 /, IFLAG(2) / 'PUT' /
      DATA   (INST(I,3),I=1,4) / 1,6,1,3 /, IFLAG(3) / 'DELETE' /
      DATA   (INST(I,4),I=1,4) / 2,6,2,0 /, IFLAG(4) / 'SHIELD' /
C
C  IS THERE AN ACTIVE DEVICE?
      IF (DDAAI.EQ.0) GO TO 303
C
C  SCAN THE OPTIONS LIST.
      EXGP=0
      EXSI=1
      CALL UGOPTION(OPTN,DESC_NUM,INST,IFLAG,EXST,EXSTR)
C
C  CHECK FOR A VALID SHIELD INDEX.
      IF (EXSI.LT.1) GO TO 302
      IF (EXSI.GT.DDAZ5) GO TO 302
C
C  PERFORM THE REQUESTED OPERATION.
      IF (EXGP.EQ.1) THEN
        IF (DDASF(EXSI).NE.0) THEN
          SHLD(1,1)=DDASH(1,1,EXSI)
          SHLD(2,1)=DDASH(2,1,EXSI)
          SHLD(1,2)=DDASH(1,2,EXSI)
          SHLD(2,2)=DDASH(2,2,EXSI)
        ELSE
          SHLD(1,1)=0.0
          SHLD(2,1)=0.0
          SHLD(1,2)=0.0
          SHLD(2,2)=0.0
        END IF
      ELSE IF (EXGP.EQ.2) THEN
        IF (SHLD(1,1).GE.SHLD(1,2)) GO TO 301
        IF (SHLD(2,1).GE.SHLD(2,2)) GO TO 301
        DDASF(EXSI)=1
        DDASH(1,1,EXSI)=SHLD(1,1)
        DDASH(2,1,EXSI)=SHLD(2,1)
        DDASH(1,2,EXSI)=SHLD(1,2)
        DDASH(2,2,EXSI)=SHLD(2,2)
      ELSE IF (EXGP.EQ.3) THEN
        DDASF(EXSI)=0
      END IF
      IF ((EXGP.EQ.2).OR.(EXGP.EQ.3)) THEN
        DDASA=0
        DO 101 INT1=1,DDAZ5
          IF (DDASF(INT1).NE.0) DDASA=DDASA+1
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
  301 CALL UGRERR(3,'UGSHLD  ', 1)
      GO TO 201
  302 CALL UGRERR(3,'UGSHLD  ', 2)
      GO TO 201
  303 CALL UGRERR(3,'UGSHLD  ',12)
      GO TO 201
C
      END
