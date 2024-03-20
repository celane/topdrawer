      SUBROUTINE UGMES4(PNT1,PNT2,TOLR,WKAR,HFCU,HFFL,EFLG)
C
C *******************  THE UNIFIED GRAPHICS SYSTEM  *******************
C *                    UPDATE THE HEIGHT FUNCTION                     *
C *                                                                   *
C *  THIS SUBROUTINE IS USED BY UGMESH TO UPDATE THE HEIGHT           *
C *  FUNCTION USING THE GIVEN LINE SEGMENT.                           *
C *                                                                   *
C *  THE CALLING SEQUENCE IS:                                         *
C *    CALL UGMES4(PNT1,PNT2,TOLR,WKAR,HFCU,HFFL,EFLG)                *
C *                                                                   *
C *  THE PARAMETERS IN THE CALLING SEQUENCE ARE:                      *
C *    PNT1  AN END POINT OF THE GIVEN SEGMENT.                       *
C *    PNT2  AN END POINT OF THE GIVEN SEGMENT.                       *
C *    TOLR  A TOLERANCE.                                             *
C *    WKAR  A WORK ARRAY.                                            *
C *    HFCU  THE INDEX OF THE CURRENT HEIGHT FUNCTION ELEMENT.        *
C *    HFFL  THE INDEX OF THE LAST HEIGHT FUNCTION ELEMENT.           *
C *    EFLG  AN ERROR FLAG.                                           *
C *                                                                   *
C *                          ROBERT C. BEACH                          *
C *                    COMPUTATION RESEARCH GROUP                     *
C *                STANFORD LINEAR ACCELERATOR CENTER                 *
C *                                                                   *
C *********************************************************************
C
      REAL          PNT1(2),PNT2(2)
      REAL*4        TOLR
      REAL          WKAR(*)
      INTEGER*4     HFCU,HFFL
      INTEGER       EFLG
C
      REAL*4        HFB1
      INTEGER*2     HFP1(2)
      EQUIVALENCE   (HFP1(1),HFB1)
C
      REAL          XINP(4),YINP(4)
      INTEGER       ILRP,IFRP,NINP
C
      REAL          FLT1
      INTEGER       INT1,INT2,INT3
C
C  UPDATE THE HEIGHT FUNCTION.
      EFLG=0
      FLT1=PNT2(1)-PNT1(1)
      IF (FLT1.EQ.0.0) GO TO 201
      IF (FLT1.LT.0.0) THEN
        FLT1=PNT1(1)
        PNT1(1)=PNT2(1)
        PNT2(1)=FLT1
        FLT1=PNT1(2)
        PNT1(2)=PNT2(2)
        PNT2(2)=FLT1
      END IF
      NINP=0
      ILRP=HFCU
  101 IF (WKAR(ILRP+1).GE.PNT1(1)) THEN
        HFB1=WKAR(ILRP)
        ILRP=HFP1(2)
        GO TO 101
      END IF
  102 HFB1=WKAR(ILRP)
      INT1=HFP1(1)
      FLT1=PNT1(1)-WKAR(INT1+1)
      IF (FLT1.GE.0.0) THEN
        ILRP=INT1
        IF (FLT1.NE.0.0) GO TO 102
        IF (WKAR(ILRP+2).EQ.PNT1(2)) GO TO 104
        GO TO 103
      END IF
      FLT1=((WKAR(ILRP+2)-WKAR(INT1+2))*FLT1/
     X  (WKAR(ILRP+1)-WKAR(INT1+1)))+WKAR(INT1+2)
      IF (ABS(FLT1-PNT1(2)).GT.TOLR) THEN
        NINP=NINP+1
        XINP(NINP)=PNT1(1)
        YINP(NINP)=FLT1
      END IF
  103 NINP=NINP+1
      XINP(NINP)=PNT1(1)
      YINP(NINP)=PNT1(2)
  104 IFRP=ILRP
  105 IF (WKAR(IFRP+1).LE.PNT2(1)) THEN
        HFB1=WKAR(IFRP)
        IFRP=HFP1(1)
        GO TO 105
      END IF
  106 HFB1=WKAR(IFRP)
      INT1=HFP1(2)
      FLT1=PNT2(1)-WKAR(INT1+1)
      IF (FLT1.LE.0.0) THEN
        IFRP=INT1
        IF (FLT1.NE.0.0) GO TO 106
        IF (WKAR(IFRP+2).EQ.PNT2(2)) GO TO 108
        FLT1=PNT2(2)
        GO TO 107
      END IF
      FLT1=((WKAR(IFRP+2)-WKAR(INT1+2))*FLT1/
     X  (WKAR(IFRP+1)-WKAR(INT1+1)))+WKAR(INT1+2)
  107 NINP=NINP+1
      XINP(NINP)=PNT2(1)
      YINP(NINP)=PNT2(2)
      IF (ABS(FLT1-PNT2(2)).GT.TOLR) THEN
        NINP=NINP+1
        XINP(NINP)=PNT2(1)
        YINP(NINP)=FLT1
      END IF
  108 INT1=0
  109 HFB1=WKAR(ILRP)
      ILRP=HFP1(1)
  110 IF (ILRP.NE.IFRP) THEN
        IF (INT1.NE.NINP) THEN
          INT1=INT1+1
          WKAR(ILRP+1)=XINP(INT1)
          WKAR(ILRP+2)=YINP(INT1)
          GO TO 109
        END IF
        HFB1=WKAR(ILRP)
        INT2=HFP1(2)
        INT3=HFP1(1)
        HFB1=WKAR(INT2)
        HFP1(1)=INT3
        WKAR(INT2)=HFB1
        HFB1=WKAR(INT3)
        HFP1(2)=INT2
        WKAR(INT3)=HFB1
        HFP1(1)=HFFL
        WKAR(ILRP)=HFB1
        HFFL=ILRP
        ILRP=INT3
        GO TO 110
      END IF
  111 IF (INT1.NE.NINP) THEN
        IF (HFFL.EQ.0) GO TO 202
        INT2=HFFL
        HFB1=WKAR(INT2)
        HFFL=HFP1(1)
        HFB1=WKAR(ILRP)
        INT3=HFP1(2)
        HFP1(2)=INT2
        WKAR(ILRP)=HFB1
        HFB1=WKAR(INT3)
        HFP1(1)=INT2
        WKAR(INT3)=HFB1
        HFP1(2)=INT3
        HFP1(1)=ILRP
        WKAR(INT2)=HFB1
        INT1=INT1+1
        WKAR(INT2+1)=XINP(INT1)
        WKAR(INT2+2)=YINP(INT1)
        GO TO 111
      END IF
      HFCU=IFRP
C
C  RETURN TO CALLING SUBROUTINE.
  201 RETURN
  202 EFLG=1
      GO TO 201
C
      END
