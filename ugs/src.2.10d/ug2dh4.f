      SUBROUTINE UG2DH4(PNT1,PNT2,TOLR,WKAR,HFCU,HFFL,EFLG)
C
C *******************  THE UNIFIED GRAPHICS SYSTEM  *******************
C *                    UPDATE THE HEIGHT FUNCTION                     *
C *                                                                   *
C *  THIS SUBROUTINE IS USED BY UG2DHG TO UPDATE THE HEIGHT           *
C *  FUNCTION USING THE GIVEN LINE SEGMENT.                           *
C *                                                                   *
C *  THE CALLING SEQUENCE IS:                                         *
C *    CALL UG2DH4(PNT1,PNT2,TOLR,WKAR,HFCU,HFFL,EFLG)                *
C *                                                                   *
C *  THE PARAMETERS IN THE CALLING SEQUENCE ARE:                      *
C *    PNT1  AN END POINT OF THE GIVEN SEGMENT.                       *
C *    PNT2  AN END POINT OF THE GIVEN SEGMENT.                       *
C *    TOLR  A TOLERANCE.                                             *
C *    WKAR  A WORK ARRAY.                                            *
C *    HFCU  THE INDEX OF THE CURRENT HEIGHT FUNCTION ELEMENT.        *
C *    HFFL  THE INDEX OF THE HEIGHT FUNCTION FREE ELEMENT LIST.      *
C *    EFLG  AN ERROR FLAG.                                           *
C *                                                                   *
C *                          ROBERT C. BEACH                          *
C *                    COMPUTATION RESEARCH GROUP                     *
C *                STANFORD LINEAR ACCELERATOR CENTER                 *
C *                                                                   *
C *********************************************************************
C
      REAL          PNT1(2),PNT2(2)
      REAL          TOLR
      REAL*4        WKAR(*)
      INTEGER       HFCU,HFFL
      LOGICAL       EFLG
C
      REAL*4        HFB1
      INTEGER*2     HFP1(2)
      EQUIVALENCE   (HFP1(1),HFB1)
C
      REAL          PNTA(2),PNTB(2),PNTC(2),PNTD(2)
      INTEGER       HFNX,HFPA,HFPD
C
      INTEGER       INT1,INT2
C
C  SAVE AND ORDER THE GIVEN LINE SEGMENT.
      IF (ABS(PNT1(1)-PNT2(1)).LE.TOLR) THEN
        GO TO 401
      ELSE IF (PNT1(1).LT.PNT2(1)) THEN
        PNTB(1)=PNT1(1)
        PNTB(2)=PNT1(2)
        PNTC(1)=PNT2(1)
        PNTC(2)=PNT2(2)
      ELSE
        PNTB(1)=PNT2(1)
        PNTB(2)=PNT2(2)
        PNTC(1)=PNT1(1)
        PNTC(2)=PNT1(2)
      END IF
C
C  BRACKET THE SECTION OF HEIGHT FUNCTION TO BE DELETED.
  101 IF (WKAR(HFCU+1).GE.(PNTB(1)-TOLR)) THEN
        HFB1=WKAR(HFCU)
        HFCU=HFP1(2)
        GO TO 101
      END IF
  102 HFB1=WKAR(HFCU)
      HFNX=HFP1(1)
      IF ((PNTB(1)-TOLR).GT.WKAR(HFNX+1)) THEN
        HFCU=HFNX
        GO TO 102
      END IF
      HFPA=HFCU
      CALL UG2DH5(PNTB,WKAR(HFCU+1),WKAR(HFNX+1),PNTA)
  103 IF ((PNTC(1)+TOLR).GE.WKAR(HFNX+1)) THEN
        HFCU=HFNX
        HFB1=WKAR(HFCU)
        HFNX=HFP1(1)
        GO TO 103
      END IF
      HFPD=HFNX
      CALL UG2DH5(PNTC,WKAR(HFCU+1),WKAR(HFNX+1),PNTD)
      HFCU=HFPA
C
C  FREE ALL HEIGHT FUNCTION BLOCKS THAT HAVE BEEN BRACKETED.
      INT1=HFPA
      HFB1=WKAR(INT1)
      INT2=HFP1(1)
  201 IF (INT2.NE.HFPD) THEN
        INT1=INT2
        HFB1=WKAR(INT1)
        INT2=HFP1(1)
        HFP1(1)=HFFL
        WKAR(INT1)=HFB1
        HFFL=INT1
        GO TO 201
      END IF
C
C  INSERT THE NEW POINTS INTO THE HEIGHT FUNCTION.
C    IF NECESSARY, INSERT PNTA AND ADVANCE HFPA.
      IF ((PNTA(1).NE.WKAR(HFPA+1)).OR.
     X    (ABS(PNTA(2)-WKAR(HFPA+2)).GE.TOLR)) THEN
        IF (HFFL.EQ.0) GO TO 403
        HFB1=WKAR(HFPA)
        HFP1(1)=HFFL
        WKAR(HFPA)=HFB1
        HFB1=WKAR(HFFL)
        INT1=HFP1(1)
        HFP1(2)=HFPA
        WKAR(HFFL)=HFB1
        WKAR(HFFL+1)=PNTA(1)
        WKAR(HFFL+2)=PNTA(2)
        HFPA=HFFL
        HFFL=INT1
      END IF
C    IF NECESSARY, INSERT PNTB AND ADVANCE HFPA.
      IF ((PNTB(1).NE.WKAR(HFPA+1)).OR.
     X    (ABS(PNTB(2)-WKAR(HFPA+2)).GE.TOLR)) THEN
        IF (HFFL.EQ.0) GO TO 403
        HFB1=WKAR(HFPA)
        HFP1(1)=HFFL
        WKAR(HFPA)=HFB1
        HFB1=WKAR(HFFL)
        INT1=HFP1(1)
        HFP1(2)=HFPA
        WKAR(HFFL)=HFB1
        WKAR(HFFL+1)=PNTB(1)
        WKAR(HFFL+2)=PNTB(2)
        HFPA=HFFL
        HFFL=INT1
      END IF
C    IF NECESSARY, INSERT PNTD AND DECREMENT HFPD.
      IF ((PNTD(1).NE.WKAR(HFPD+1)).OR.
     X    (ABS(PNTD(2)-WKAR(HFPD+2)).GE.TOLR)) THEN
        IF (HFFL.EQ.0) GO TO 403
        HFB1=WKAR(HFPD)
        HFP1(2)=HFFL
        WKAR(HFPD)=HFB1
        HFB1=WKAR(HFFL)
        INT1=HFP1(1)
        HFP1(1)=HFPD
        WKAR(HFFL)=HFB1
        WKAR(HFFL+1)=PNTD(1)
        WKAR(HFFL+2)=PNTD(2)
        HFPD=HFFL
        HFFL=INT1
      END IF
C    IF NECESSARY, INSERT PNTC AND DECREMENT HFPD.
      IF ((PNTC(1).NE.WKAR(HFPD+1)).OR.
     X    (ABS(PNTC(2)-WKAR(HFPD+2)).GE.TOLR)) THEN
        IF (HFFL.EQ.0) GO TO 403
        HFB1=WKAR(HFPD)
        HFP1(2)=HFFL
        WKAR(HFPD)=HFB1
        HFB1=WKAR(HFFL)
        INT1=HFP1(1)
        HFP1(1)=HFPD
        WKAR(HFFL)=HFB1
        WKAR(HFFL+1)=PNTC(1)
        WKAR(HFFL+2)=PNTC(2)
        HFPD=HFFL
        HFFL=INT1
      END IF
C    LINK HFPA AND HFPD TOGETHER.
      HFB1=WKAR(HFPA)
      HFP1(1)=HFPD
      WKAR(HFPA)=HFB1
      HFB1=WKAR(HFPD)
      HFP1(2)=HFPA
      WKAR(HFPD)=HFB1
C
C  RETURN TO CALLING SUBROUTINE WITH ERROR FLAG SET.
  401 EFLG=.FALSE.
  402 RETURN
  403 EFLG=.TRUE.
      GO TO 402
C
      END
