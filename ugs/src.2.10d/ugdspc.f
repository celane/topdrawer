      SUBROUTINE UGDSPC(OPTN,XSIZ,YSIZ,AFFY)
C
C *******************  THE UNIFIED GRAPHICS SYSTEM  *******************
C *                   INITIALIZE THE DRAWING SPACE                    *
C *                                                                   *
C *  THIS SUBROUTINE MAY BE USED TO INITIALIZE THE DRAWING SPACE AND  *
C *  RE-DEFINE THE ASPECT RATIO OF A PICTURE.  IT MAY ALSO BE USED    *
C *  TO RETRIEVE THE PARAMETERS OF THE CURRENT DRAWING SPACE.         *
C *                                                                   *
C *  THE CALLING SEQUENCE IS:                                         *
C *    CALL UGDSPC(OPTN,XSIZ,YSIZ,AFFY)                               *
C *                                                                   *
C *  THE PARAMETERS IN THE CALLING SEQUENCE ARE:                      *
C *    OPTN  THE OPTIONS LIST.                                        *
C *    XSIZ  THE SIZE OF THE PICTURE IN THE X DIRECTION IN            *
C *          ARBITRARY UNITS.                                         *
C *    YSIZ  THE SIZE OF THE PICTURE IN THE Y DIRECTION IN            *
C *          ARBITRARY UNITS.                                         *
C *    AFFY  THE AFFINITY VALUE FOR XSIZ AND YSIZ.                    *
C *                                                                   *
C *                          ROBERT C. BEACH                          *
C *                    COMPUTATION RESEARCH GROUP                     *
C *                STANFORD LINEAR ACCELERATOR CENTER                 *
C *                                                                   *
C *********************************************************************
C
      CHARACTER*(*) OPTN
      REAL          XSIZ,YSIZ,AFFY
C
      INCLUDE       'UGSYSTEM:UGDDACBK.FOR'
C
      INCLUDE       'UGSYSTEM:UGERRCBK.FOR'
C
      INTEGER*4     EXST(1),EXGP
      EQUIVALENCE   (EXGP,EXST(1))
C
      INTEGER       DRU1(2,2)
      INTEGER       DDIN(2),DDEX(1)
C
      REAL          FLT1,FLT2
      INTEGER       INT1,INT2
      INTEGER       DESC_NUM
      PARAMETER     (DESC_NUM = 2)
      INTEGER*4     INST(4,DESC_NUM)
      CHARACTER*16  IFLAG(DESC_NUM)
C
      DATA     (INST(I,1),I=1,4) / 1,3,1,1/, IFLAG(1) /'GET'/
      DATA     (INST(I,2),I=1,4) / 1,3,1,2/, IFLAG(2) /'PUT'/
C
C  IS THERE AN ACTIVE DEVICE?
      IF (DDAAI.EQ.0) GO TO 304
C
C  SCAN THE OPTIONS LIST.
      EXGP=0
      CALL UGOPTION(OPTN,DESC_NUM,INST,IFLAG,EXST,EXSTR)
C
C  PERFORM THE REQUESTED OPERATION.
      IF (EXGP.EQ.1) THEN
        XSIZ=DDADS(1,2)
        YSIZ=DDADS(2,2)
        AFFY=DDADA
* * *
*      WRITE (*,*) ' UGDSPC - ',OPTN,XSIZ,YSIZ,AFFY
* * *
      ELSE IF (EXGP.EQ.2) THEN
C  IS THIS AN APPROPRIATE TIME TO CHANGE THE DRAWING SPACE?
        IF (DDAFW.NE.0) GO TO 301
C  CHECK INPUT DATA FOR CONSISTENCY.
        IF (XSIZ.LE.0.0) GO TO 302
        IF (YSIZ.LE.0.0) GO TO 302
        IF (AFFY.LT.0.0) GO TO 303
        IF (AFFY.GT.1.0) GO TO 303
C  TRANSFER THE DATA TO THE DDA.
        DO 102 INT1=1,2
          DO 101 INT2=1,2
            DDADD(INT1,INT2)=DDABD(INT1,INT2)
            DRU1(INT1,INT2)=DDABD(INT1,INT2)
  101     CONTINUE
  102   CONTINUE
        IF (DDABE(1,1).NE.0) THEN
          DRU1(1,1)=DRU1(1,2)-NINT(XSIZ*
     X      REAL(DRU1(1,2)-DRU1(1,1))/YSIZ)
          IF (XSIZ.GE.YSIZ) DDADD(1,1)=DRU1(1,1)
        ELSE IF (DDABE(1,2).NE.0) THEN
          DRU1(1,2)=DRU1(1,1)+NINT(XSIZ*
     X      REAL(DRU1(1,2)-DRU1(1,1))/YSIZ)
          IF (XSIZ.GE.YSIZ) DDADD(1,2)=DRU1(1,2)
        ELSE IF (DDABE(2,1).NE.0) THEN
          DRU1(2,1)=DRU1(2,2)-NINT(YSIZ*
     X      REAL(DRU1(2,2)-DRU1(2,1))/XSIZ)
          IF (YSIZ.GE.XSIZ) DDADD(2,1)=DRU1(2,1)
        ELSE IF (DDABE(2,2).NE.0) THEN
          DRU1(2,2)=DRU1(2,1)+NINT(YSIZ*
     X      REAL(DRU1(2,2)-DRU1(2,1))/XSIZ)
          IF (YSIZ.GE.XSIZ) DDADD(2,2)=DRU1(2,2)
        ELSE
          FLT1=YSIZ*DDABX*REAL(DDABD(1,2)-DDABD(1,1))
          FLT2=XSIZ*DDABY*REAL(DDABD(2,2)-DDABD(2,1))
          IF (FLT2.LE.FLT1) THEN
            INT1=NINT(0.5*(FLT1-FLT2)*
     X        REAL(DRU1(1,2)-DRU1(1,1))/FLT1)
            DRU1(1,1)=DRU1(1,1)+INT1
            DRU1(1,2)=DRU1(1,2)-INT1
          ELSE
            INT1=NINT(0.5*(FLT2-FLT1)*
     X        REAL(DRU1(2,2)-DRU1(2,1))/FLT2)
            DRU1(2,1)=DRU1(2,1)+INT1
            DRU1(2,2)=DRU1(2,2)-INT1
          END IF
        END IF
        DO 104 INT1=1,2
          DO 103 INT2=1,2
            DDADD(INT1,INT2)=
     X        NINT((1.0-AFFY)*REAL(DDADD(INT1,INT2))+
     X                  AFFY *REAL( DRU1(INT1,INT2)))
  103     CONTINUE
  104   CONTINUE
        DDADS(1,1)=0.0
        DDADS(1,2)=XSIZ
        DDADS(2,1)=0.0
        DDADS(2,2)=YSIZ
        DDADA=AFFY
        DDADX=DDABX*REAL(DDADD(1,2)-DDADD(1,1))/
     X                  (DDADS(1,2)-DDADS(1,1))
        DDADY=DDABY*REAL(DDADD(2,2)-DDADD(2,1))/
     X                  (DDADS(2,2)-DDADS(2,1))
        DO 106 INT1=1,2
          DO 105 INT2=1,2
            DDAWA(INT1,INT2)=DDADS(INT1,INT2)
            DDAWS(INT1,INT2)=DDADS(INT1,INT2)
            DDAWD(INT1,INT2)=DDADD(INT1,INT2)
            DDA3V(INT1,INT2)=DDADS(INT1,INT2)
  105     CONTINUE
  106   CONTINUE
        DDAWX=DDADX
        DDAWY=DDADY
        CALL UGB005
        DDA3W(1,1)=-4.5
        DDA3W(2,1)=-4.5
        DDA3W(3,1)=-4.5
        DDA3W(1,2)=5.5
        DDA3W(2,2)=5.5
        DDA3W(3,2)=5.5
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
  301 CALL UGRERR(3,'UGDSPC  ', 1)
      GO TO 201
  302 CALL UGRERR(3,'UGDSPC  ', 2)
      GO TO 201
  303 CALL UGRERR(3,'UGDSPC  ', 3)
      GO TO 201
  304 CALL UGRERR(3,'UGDSPC  ',12)
      GO TO 201
C
      END
