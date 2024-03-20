      SUBROUTINE UGMES3(PNT1,PNT2,ULVL,TOLR,WKAR,HFCU,PNT3,PNT4,VFLG)
C
C *******************  THE UNIFIED GRAPHICS SYSTEM  *******************
C *             CHECK A LINE AGAINST THE HEIGHT FUNCTION              *
C *                                                                   *
C *  THIS SUBROUTINE IS USED BY UGMESH TO CHECK THE CURRENT LINE      *
C *  SEGMENT AGAINST THE HEIGHT FUNCTION.  IF PART OF THE LINE IS     *
C *  VISIBLE, A FLAG IS SET AND THE VISIBLE PART IS SAVED IN TWO      *
C *  OUTPUT POINTS.                                                   *
C *                                                                   *
C *  THE CALLING SEQUENCE IS:                                         *
C *    CALL UGMES3(PNT1,PNT2,ULVL,TOLR,WKAR,HFCU,PNT3,PNT4,VFLG)      *
C *                                                                   *
C *  THE PARAMETERS IN THE CALLING SEQUENCE ARE:                      *
C *    PNT1  AN END POINT OF THE CURRENT LINE SEGMENT.                *
C *    PNT2  AN END POINT OF THE CURRENT LINE SEGMENT.                *
C *    ULVL  A VALUE OF 1.0 FOR AN UPPER SURFACE AND -1.0 FOR A       *
C *          LOWER SURFACE.                                           *
C *    TOLR  A TOLERANCE.                                             *
C *    WKAR  A WORK AREA.                                             *
C *    HFCU  THE INDEX OF THE CURRENT HEIGHT FUNCTION ELEMENT.        *
C *    PNT3  AN END POINT OF THE VISIBLE LINE SEGMENT.                *
C *    PNT4  AN END POINT OF THE VISIBLE LINE SEGMENT.                *
C *    VFLG  A FLAG INDICATING IF A VISIBLE SEGMENT IS AVAILABLE.     *
C *                                                                   *
C *                          ROBERT C. BEACH                          *
C *                    COMPUTATION RESEARCH GROUP                     *
C *                STANFORD LINEAR ACCELERATOR CENTER                 *
C *                                                                   *
C *********************************************************************
C
      REAL          PNT1(2),PNT2(2)
      REAL          ULVL
      REAL*4        TOLR
      REAL          WKAR(*)
      INTEGER*4     HFCU
      REAL          PNT3(2),PNT4(2)
      LOGICAL       VFLG
C
      REAL*4        HFB1
      INTEGER*2     HFP1(2)
      EQUIVALENCE   (HFP1(1),HFB1)
C
      REAL          XINP(4),YINP(4)
      INTEGER       VIND(2)
C
      REAL          FLT1,FLT2,FLT3,FLT4,FLT5,FLT6,FLT7,FLT8,FLT9
      INTEGER       INT1,INT2,INT3
C
C  COMPARE LINE SEGMENT WITH HEIGHT FUNCTION.
      VFLG=.TRUE.
      PNT3(1)=PNT1(1)
      PNT3(2)=PNT1(2)
      PNT4(1)=PNT2(1)
      PNT4(2)=PNT2(2)
      FLT1=TOLR*(PNT2(1)-PNT1(1))
      FLT2=TOLR*(PNT2(2)-PNT1(2))
      XINP(1)=PNT1(1)+FLT1
      YINP(1)=PNT1(2)+FLT2
      XINP(2)=PNT2(1)-FLT1
      YINP(2)=PNT2(2)-FLT2
      DO 108 INT1=1,2
  101   IF (WKAR(HFCU+1).GE.XINP(INT1)) THEN
          HFB1=WKAR(HFCU)
          HFCU=HFP1(2)
          GO TO 101
        END IF
        INT2=0
  102   HFB1=WKAR(HFCU)
        INT3=HFP1(1)
        FLT1=XINP(INT1)-WKAR(INT3+1)
        IF (FLT1.LE.0.0) GO TO 104
  103   HFCU=INT3
        GO TO 102
  104   IF (FLT1.EQ.0.0) THEN
          INT2=1
          IF (ULVL*(YINP(INT1)-WKAR(INT3+2)).GE.0.0) GO TO 103
          GO TO 106
        END IF
        IF (INT2.NE.0) GO TO 105
        IF (ULVL*(((WKAR(HFCU+2)-WKAR(INT3+2))/(WKAR(HFCU+1)-
     X    WKAR(INT3+1)))*FLT1+WKAR(INT3+2)-YINP(INT1)).GT.0.0)
     X    GO TO 106
  105   VIND(INT1)=1
        GO TO 107
  106   VIND(INT1)=0
  107   CONTINUE
  108 CONTINUE
      IF ((VIND(1).EQ.0).AND.(VIND(2).EQ.0)) GO TO 302
      IF ((VIND(1).EQ.1).AND.(VIND(2).EQ.1)) GO TO 301
C
C  INTERSECT THE HEIGHT FUNCTION AND THE LINE SEGMENT.
  201 IF (WKAR(HFCU+1).GE.MIN(PNT3(1),PNT4(1))) THEN
        HFB1=WKAR(HFCU)
        HFCU=HFP1(2)
        GO TO 201
      END IF
  202 HFB1=WKAR(HFCU)
      INT3=HFP1(1)
      IF (INT3.EQ.0) GO TO 302
      FLT1=WKAR(HFCU+1)-WKAR(INT3+1)
      FLT2=WKAR(HFCU+2)-WKAR(INT3+2)
      FLT3=PNT3(1)-PNT4(1)
      FLT4=PNT3(2)-PNT4(2)
      FLT5=PNT3(1)-WKAR(INT3+1)
      FLT6=PNT3(2)-WKAR(INT3+2)
      FLT7=FLT1*FLT4-FLT2*FLT3
      IF (FLT7.NE.0.0) GO TO 204
  203 HFCU=INT3
      GO TO 202
  204 FLT8=(FLT5*FLT4-FLT6*FLT3)/FLT7
      IF ((FLT8.LT.0.0).OR.(FLT8.GT.1.0)) GO TO 203
      FLT9=(FLT1*FLT6-FLT2*FLT5)/FLT7
      IF ((FLT9.LT.0.0).OR.(FLT9.GT.1.0)) GO TO 203
      IF (((FLT8.LT.TOLR).OR.(FLT8.GT.1.0-TOLR)).AND.
     X  ((FLT9.LT.TOLR).OR.(FLT9.GT.1.0-TOLR))) GO TO 203
      FLT1=PNT3(1)-FLT3*FLT9
      FLT2=PNT3(2)-FLT4*FLT9
      IF (VIND(1).EQ.0) THEN
        PNT3(1)=FLT1
        PNT3(2)=FLT2
      ELSE
        PNT4(1)=FLT1
        PNT4(2)=FLT2
      END IF
C
C  RETURN TO CALLING SUBROUTINE.
  301 RETURN
  302 VFLG=.FALSE.
      GO TO 301
C
      END
