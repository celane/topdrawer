      SUBROUTINE UG3WRD(OPTN,VPRT,WVOL)
C
C *******************  THE UNIFIED GRAPHICS SYSTEM  *******************
C *    INITIALIZE THE THREE-DIMENSIONAL VIEW PORT AND WORLD VOLUME    *
C *                                                                   *
C *  THIS SUBROUTINE MAY BE USED TO INITIALIZE THE THREE-DIMENSIONAL  *
C *  VIEW PORT AND THE WORLD VOLUME.  IT MAY ALSO BE USED TO          *
C *  RETRIEVE THE CURRENT VALUES OF THESE PARAMETERS.                 *
C *                                                                   *
C *  THE CALLING SEQUENCE IS:                                         *
C *    CALL UG3WRD(OPTN,VPRT,WVOL)                                    *
C *                                                                   *
C *  THE PARAMETERS IN THE CALLING SEQUENCE ARE:                      *
C *    OPTN  THE OPTIONS LIST.                                        *
C *    VPRT  THE LIMITS OF THE THREE-DIMENSIONAL VIEW PORT.           *
C *    WVOL  THE LIMITS OF THE WORLD VOLUME.                          *
C *                                                                   *
C *                          ROBERT C. BEACH                          *
C *                    COMPUTATION RESEARCH GROUP                     *
C *                STANFORD LINEAR ACCELERATOR CENTER                 *
C *                                                                   *
C *********************************************************************
C
      CHARACTER*(*) OPTN
      REAL          VPRT(2,2),WVOL(3,2)
C
      INCLUDE       'UGSYSTEM:UGDDACBK.FOR'
C
      INCLUDE       'UGSYSTEM:UGERRCBK.FOR'
C
      INTEGER*4     EXST(1),EXGP
      EQUIVALENCE   (EXGP,EXST(1))
C
      INTEGER       DDIN(2),DDEX(1)
C
      INTEGER       INT1
      INTEGER       DESC_NUM
      PARAMETER    (DESC_NUM = 2)
      INTEGER*4     INST(4,DESC_NUM)
      CHARACTER*16  IFLAG(DESC_NUM)
C
      DATA    (INST(I,1),I=1,4) / 1,3,1,1 /, IFLAG(1) / 'GET'/
      DATA    (INST(I,2),I=1,4) / 1,3,1,2 /, IFLAG(2) / 'PUT'/
C
C  IS THERE AN ACTIVE DEVICE?
      IF (DDAAI.EQ.0) GO TO 305
C
C  SCAN THE OPTIONS LIST.
      EXGP=0
      CALL UGOPTION(OPTN,DESC_NUM,INST,IFLAG,EXST,EXSTR)
C
C  PERFORM THE REQUESTED OPERATION.
      IF (EXGP.EQ.1) THEN
        VPRT(1,1)=DDA3V(1,1)
        VPRT(2,1)=DDA3V(2,1)
        VPRT(1,2)=DDA3V(1,2)
        VPRT(2,2)=DDA3V(2,2)
        WVOL(1,1)=DDA3W(1,1)
        WVOL(2,1)=DDA3W(2,1)
        WVOL(3,1)=DDA3W(3,1)
        WVOL(1,2)=DDA3W(1,2)
        WVOL(2,2)=DDA3W(2,2)
        WVOL(3,2)=DDA3W(3,2)
      ELSE IF (EXGP.EQ.2) THEN
C  IS THIS AN APPROPRIATE TIME TO CHANGE THE PARAMETERS?
        IF (DDAFW.NE.0) GO TO 301
C  CHECK INPUT DATA FOR CONSISTENCY.
        IF (VPRT(1,1).GE.VPRT(1,2)) GO TO 302
        IF (VPRT(2,1).GE.VPRT(2,2)) GO TO 302
        IF (VPRT(1,1).LT.DDADS(1,1)) GO TO 303
        IF (VPRT(1,2).GT.DDADS(1,2)) GO TO 303
        IF (VPRT(2,1).LT.DDADS(2,1)) GO TO 303
        IF (VPRT(2,2).GT.DDADS(2,2)) GO TO 303
        IF (WVOL(1,1).GE.WVOL(1,2)) GO TO 304
        IF (WVOL(2,1).GE.WVOL(2,2)) GO TO 304
        IF (WVOL(3,1).GE.WVOL(3,2)) GO TO 304
C  TRANSFER THE DATA TO THE DDA.
        DDA3V(1,1)=VPRT(1,1)
        DDA3V(2,1)=VPRT(2,1)
        DDA3V(1,2)=VPRT(1,2)
        DDA3V(2,2)=VPRT(2,2)
        DDA3W(1,1)=WVOL(1,1)
        DDA3W(2,1)=WVOL(2,1)
        DDA3W(3,1)=WVOL(3,1)
        DDA3W(1,2)=WVOL(1,2)
        DDA3W(2,2)=WVOL(2,2)
        DDA3W(3,2)=WVOL(3,2)
        CALL UGB010
        CALL UGB011(INT1)
C  SEND THE PROJECTION PARAMETERS TO A THREE-DIMENSIONAL DEVICE.
        IF (DDADF.EQ.3) THEN
          DDIN(1)=15
          DDIN(2)=1
          CALL UGZ006(DDAAC,0,0,DDIN,'        ',DDEX)
        END IF
      END IF
C
C  RESET ERROR INDICATORS AND RETURN TO CALLER.
      UGELV=0
      UGENM='        '
      UGEIX=0
  201 RETURN
C
C  REPORT ERRORS TO THE UNIFIED GRAPHICS SYSTEM ERROR PROCESSOR.
  301 CALL UGRERR(3,'UG3WRD  ', 1)
      GO TO 201
  302 CALL UGRERR(3,'UG3WRD  ', 2)
      GO TO 201
  303 CALL UGRERR(3,'UG3WRD  ', 3)
      GO TO 201
  304 CALL UGRERR(3,'UG3WRD  ', 4)
      GO TO 201
  305 CALL UGRERR(3,'UG3WRD  ',12)
      GO TO 201
C
      END
