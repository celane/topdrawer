      SUBROUTINE UGPICT(OPTN,IDNT)
C
C *******************  THE UNIFIED GRAPHICS SYSTEM  *******************
C *              CONTROL THE PICTURE ON A GRAPHIC DEVICE              *
C *                                                                   *
C *  THIS SUBROUTINE MAY BE USED TO CONTROL THE PICTURE ON A GRAPHIC  *
C *  DEVICE.  THE ENTIRE PICTURE MAY BE CLEARED AND INDIVIDUAL        *
C *  GRAPHIC SEGMENTS MAY BE DELETED OR HAVE CERTAIN OF THEIR         *
C *  PROPERTIES CHANGED.                                              *
C *                                                                   *
C *  THE CALLING SEQUENCE IS:                                         *
C *    CALL UGPICT(OPTN,IDNT)                                         *
C *                                                                   *
C *  THE PARAMETERS IN THE CALLING SEQUENCE ARE:                      *
C *    OPTN  THE OPTIONS LIST.                                        *
C *    IDNT  A VALUE WHICH MAY GIVE THE IDENTIFICATION OF A SPECIFIC  *
C *          GRAPHIC SEGMENT.                                         *
C *                                                                   *
C *                          ROBERT C. BEACH                          *
C *                    COMPUTATION RESEARCH GROUP                     *
C *                STANFORD LINEAR ACCELERATOR CENTER                 *
C *                                                                   *
C *********************************************************************
C
      CHARACTER*(*) OPTN
      INTEGER       IDNT
C
      INCLUDE       'UGSYSTEM:UGDDACBK.FOR'
C
      INCLUDE       'UGSYSTEM:UGERRCBK.FOR'

      INTEGER*4     EXST(5),EXCL,EXWD,EXPK,EXOM,EXNF
      EQUIVALENCE   (EXCL,EXST(1)),       (EXWD,EXST(2)),
     X              (EXPK,EXST(3)),       (EXOM,EXST(4)),
     X              (EXNF,EXST(5))

      CHARACTER*8   EXSTR(1)
      CHARACTER*8   EXAL
      EQUIVALENCE   (EXAL,EXSTR(1))

      INTEGER       DDIN(5),DDEX(1)

      INTEGER       INT1,INT2
      INTEGER       DESC_NUM
      PARAMETER    (DESC_NUM = 9)
      INTEGER*4     INST(4,DESC_NUM)
      CHARACTER*16  IFLAG(DESC_NUM)
C
      DATA   (INST(I,1),I=1,4) / 1,5,1,1 /, IFLAG(1) / 'CLEAR' /
      DATA   (INST(I,2),I=1,4) / 1,6,2,1 /, IFLAG(2) / 'WINDOW' /
      DATA   (INST(I,3),I=1,4) / 1,6,3,1 /, IFLAG(3) / 'NOPICK' /
      DATA   (INST(I,4),I=1,4) / 1,4,3,2 /, IFLAG(4) / 'PICK' /
      DATA   (INST(I,5),I=1,4) / 1,7,4,1 /, IFLAG(5) / 'INCLUDE' /
      DATA   (INST(I,6),I=1,4) / 1,4,4,2 /, IFLAG(6) / 'OMIT' /
      DATA   (INST(I,7),I=1,4) / 1,2,5,1 /, IFLAG(7) / 'ON' /
      DATA   (INST(I,8),I=1,4) / 1,3,5,2 /, IFLAG(8) / 'OFF' /

      DATA   (INST(I,9),I=1,4) / 4,5,1,8 /, IFLAG(9) / 'ALIAS' /
C
C  IS THERE AN ACTIVE DEVICE?
      IF (DDAAI.EQ.0) GO TO 401
C
C  SCAN THE OPTIONS LIST.
      EXCL=0
      EXWD=0
      EXPK=0
      EXOM=0
      EXNF=0
      EXAL='        '
      CALL UGOPTION(OPTN,DESC_NUM,INST,IFLAG,EXST,EXSTR)
C
C  TURN THE SCREEN OFF IF REQUESTED.
      IF (EXNF.EQ.2) THEN
        DDIN(1)=4
        DDIN(2)=2
        CALL UGZ006(DDAAC,0,0,DDIN,' ',DDEX)
      END IF
C
C  CHECK THE IDENTIFICATION VALUE.
      IF (IDNT.EQ.0) THEN
C  IDENTIFICATION IS ZERO: PROCESS PICTURE.
        IF ((EXPK+EXOM).NE.0) GO TO 402
C  CLEAR THE SCREEN OR WINDOW IF REQUESTED.
        IF (EXCL.EQ.0) GO TO 201
        DDIN(1)=3
        IF (EXWD.EQ.0) THEN
          DDIN(2)=0
          CALL UGZ006(DDAAC,0,0,DDIN,EXAL,DDEX)
          DO 102 INT1=1,2
            DO 101 INT2=1,2
              DDAWA(INT1,INT2)=DDADS(INT1,INT2)
              DDAWS(INT1,INT2)=DDADS(INT1,INT2)
              DDAWD(INT1,INT2)=DDADD(INT1,INT2)
  101       CONTINUE
  102     CONTINUE
          DDAWX=DDADX
          DDAWY=DDADY
          CALL UGB005
          CALL UGB010
          CALL UGB011(INT1)
          IF (DDADF.EQ.3) THEN
            DDIN(1)=15
            DDIN(2)=1
            CALL UGZ006(DDAAC,0,0,DDIN,'        ',DDEX)
          END IF
          DDAFW=0
        ELSE
          IF (DDADM.NE.2) GO TO 402
          DDIN(2)=1
          CALL UGZ006(DDAAC,0,0,DDIN,' ',DDEX)
          IF (DDEX(1).NE.0) GO TO 402
        END IF
      ELSE
C  IDENTIFICATION IS NOT ZERO: PROCESS SEGMENT.
        IF (EXAL.NE.'        ') GO TO 402
        IF ((EXOM.NE.0).AND.(DDADM.NE.3)) GO TO 402
        IF ((EXPK.NE.0).AND.(DDAIC(2).EQ.0)) GO TO 402
C  MANIPULATE SEGMENT IF REQUESTED.
        IF ((EXCL+EXPK+EXOM).NE.0) THEN
          DDIN(1)=7
          DDIN(2)=IDNT
          DDIN(3)=EXCL
          DDIN(4)=EXPK
          DDIN(5)=EXOM
          CALL UGZ006(DDAAC,0,0,DDIN,OPTN,DDEX)
        END IF
      END IF
C
C  TURN THE SCREEN ON IF REQUESTED.
  201 IF (EXNF.EQ.1) THEN
        DDIN(1)=4
        DDIN(2)=1
        CALL UGZ006(DDAAC,0,0,DDIN,' ',DDEX)
      END IF
C
C  RESET ERROR INDICATORS AND RETURN TO CALLER.
      UGELV=0
      UGENM='        '
      UGEIX=0
  301 RETURN
C
C  REPORT ERRORS TO THE UNIFIED GRAPHICS SYSTEM ERROR PROCESSOR.
  401 CALL UGRERR(3,'UGPICT  ',12)
      GO TO 301
  402 CALL UGRERR(2,'UGPICT  ',13)
      GO TO 201
C
      END
