      SUBROUTINE UG2DHG(OPTN,LSUB,ARAY,MDIM,NDIM,TRNS,WKAR,LDIM)
C
C *******************  THE UNIFIED GRAPHICS SYSTEM  *******************
C *                2-D HISTOGRAM GENERATION SUBROUTINE                *
C *                                                                   *
C *  THIS SUBROUTINE MAY BE USED TO GENERATE A DESCRIPTION OF A 2-D   *
C *  HISTOGRAM.  A USER SUPPLIED SUBROUTINE IS CALLED TO PROCESS THE  *
C *  LINE SEGMENT END POINTS.                                         *
C *                                                                   *
C *  THE CALLING SEQUENCE IS:                                         *
C *    CALL UG2DHG(OPTN,LSUB,ARAY,MDIM,NDIM,TRNS,WKAR,LDIM)           *
C *                                                                   *
C *  THE PARAMETERS IN THE CALLING SEQUENCE ARE:                      *
C *    OPTN  THE OPTIONS LIST.                                        *
C *    LSUB  THE LINE SEGMENT END POINT SUBROUTINE.                   *
C *    ARAY  THE ARRAY DEFINING THE 2-D HISTOGRAM.                    *
C *    MDIM  THE EXTENT OF THE 2-D HISTOGRAM IN THE X DIRECTION.      *
C *    NDIM  THE EXTENT OF THE 2-D HISTOGRAM IN THE Y DIRECTION.      *
C *    TRNS  THE PROJECTION TRANSFORMATION.                           *
C *    WKAR  A WORK AREA.                                             *
C *    LDIM  THE NUMBER OF WORDS IN WKAR.                             *
C *                                                                   *
C *                          ROBERT C. BEACH                          *
C *                    COMPUTATION RESEARCH GROUP                     *
C *                STANFORD LINEAR ACCELERATOR CENTER                 *
C *                                                                   *
C *********************************************************************
C
      CHARACTER*(*) OPTN
      EXTERNAL      LSUB
      REAL          ARAY(MDIM,NDIM)
      INTEGER       MDIM,NDIM
      REAL          TRNS(31)
      REAL*4        WKAR(*)
      INTEGER       LDIM
C
      INCLUDE       'UGSYSTEM:UGERRCBK.FOR'
C
      INTEGER*4     EXST(1)
      REAL*4        EXTL
      EQUIVALENCE   (EXTL,EXST(1))
C
      REAL*4        HFB1
      INTEGER*2     HFP1(2)
      EQUIVALENCE   (HFP1(1),HFB1)
      INTEGER       HFCU,HFFL
C
      REAL          TOLR
      REAL          PNTX(2)
      LOGICAL       XLFG,XHFG,YLFG,YHFG
      INTEGER       V1FG
      INTEGER       V1LO,V1HI,V1DL,V2LO,V2HI,V2DL
      INTEGER       V1IX,V2IX
      REAL          PNT3(3)
      REAL          PT2A(2),PT2B(2),PT2C(2),PT2D(2),
     X              PT2E(2),PT2F(2),PT2G(2),PT2H(2),
     X              PT2I(2),PT2J(2),PT2K(2),PT2L(2)
      REAL          V1CL,V1FR,V2MN,V2MX,V3HI
      LOGICAL       VHFG,VLFG,LSFG
      LOGICAL       L1FG,L2FG,L3FG,L4FG
C
      LOGICAL       LGL1
      INTEGER       INT1,INT2
C
      INTEGER       DESC_NUM
      PARAMETER    (DESC_NUM = 1)
      INTEGER*4     INST(4,DESC_NUM)
      CHARACTER*16  IFLAG(DESC_NUM)
C
      DATA    (INST(I,1),I=1,4) / 3,5,1,0 /, IFLAG(1) / 'TOLER' /
C
C  SCAN THE OPTIONS LIST, INITIALIZE, AND CHECK THE INPUT..
      EXTL=0.00005
      CALL UGOPTION(OPTN,DESC_NUM,INST,IFLAG,EXST,EXSTR)
      TOLR=EXTL
      PNTX(1)=1E10
      PNTX(2)=1E10
      IF ((MDIM.LT.3).OR.(NDIM.LT.3)) GO TO 401
      IF ((TRNS(9).NE.0.0).OR.(TRNS(10).NE.0.0).OR.
     X    (TRNS(11).NE.0.0).OR.(TRNS(12).EQ.0.0)) GO TO 403
C
C  INITIALIZE THE HEIGHT FUNCTION.
      INT1=3*(MIN(LDIM,32767)/3)-2
      IF (INT1.LT.7) GO TO 402
      HFP1(1)=4
      HFP1(2)=0
      WKAR(1)=HFB1
      WKAR(2)=-1E10
      WKAR(3)=-1E10
      HFP1(1)=0
      HFP1(2)=1
      WKAR(4)=HFB1
      WKAR(5)=1E10
      WKAR(6)=-1E10
      DO 101 INT2=7,INT1,3
        IF (INT2.EQ.INT1) THEN
          HFP1(1)=0
        ELSE
          HFP1(1)=INT2+3
        END IF
        WKAR(INT2)=HFB1
  101 CONTINUE
      HFCU=1
      HFFL=7
C
C  COMPUTE THE NORMALIZED VIEW OF THE 2-D HISTOGRAM.
      XLFG=.FALSE.
      XHFG=.FALSE.
      YLFG=.FALSE.
      YHFG=.FALSE.
      IF (ABS(TRNS(22)).GT.ABS(TRNS(23))) THEN
        V1FG=1
        IF (TRNS(22).GT.0.0) THEN
          XLFG=.TRUE.
          V1LO=2
          V1HI=NDIM-1
          V1DL=1
        ELSE
          XHFG=.TRUE.
          V1LO=NDIM
          V1HI=3
          V1DL=-1
        END IF
        IF (TRNS(23).GT.0.0) THEN
          YLFG=.TRUE.
          V2LO=2
          V2HI=MDIM-1
          V2DL=1
        ELSE
          IF (TRNS(23).LT.0.0) YHFG=.TRUE.
          V2LO=MDIM
          V2HI=3
          V2DL=-1
        END IF
      ELSE
        V1FG=2
        IF (TRNS(23).GT.0.0) THEN
          YLFG=.TRUE.
          V1LO=2
          V1HI=MDIM-1
          V1DL=1
        ELSE
          YHFG=.TRUE.
          V1LO=MDIM
          V1HI=3
          V1DL=-1
        END IF
        IF (TRNS(22).GT.0.0) THEN
          XLFG=.TRUE.
          V2LO=2
          V2HI=NDIM-1
          V2DL=1
        ELSE
          IF (TRNS(22).LT.0.0) XHFG=.TRUE.
          V2LO=NDIM
          V2HI=3
          V2DL=-1
        END IF
      END IF
C
C  DRAW THE VISIBLE BASE OF THE COLUMNS.
      PNT3(3)=ARAY(1,1)
      IF (XLFG.OR.YLFG) THEN
        PNT3(1)=ARAY(1,2)
        PNT3(2)=ARAY(2,1)
        CALL UGPROJ(TRNS,PNT3,PT2A)
      END IF
      IF (YLFG.OR.XHFG) THEN
        PNT3(1)=ARAY(1,NDIM)
        PNT3(2)=ARAY(2,1)
        CALL UGPROJ(TRNS,PNT3,PT2B)
      END IF
      IF (XHFG.OR.YHFG) THEN
        PNT3(1)=ARAY(1,NDIM)
        PNT3(2)=ARAY(MDIM,1)
        CALL UGPROJ(TRNS,PNT3,PT2C)
      END IF
      IF (YHFG.OR.XLFG) THEN
        PNT3(1)=ARAY(1,2)
        PNT3(2)=ARAY(MDIM,1)
        CALL UGPROJ(TRNS,PNT3,PT2D)
      END IF
      IF (YLFG) THEN
        CALL UG2DH2(LSUB,PT2A,PT2B,PNTX)
        CALL UG2DH4(PT2A,PT2B,TOLR,WKAR,HFCU,HFFL,LGL1)
        IF (LGL1) GO TO 402
      END IF
      IF (XHFG) THEN
        CALL UG2DH2(LSUB,PT2B,PT2C,PNTX)
        CALL UG2DH4(PT2B,PT2C,TOLR,WKAR,HFCU,HFFL,LGL1)
        IF (LGL1) GO TO 402
      END IF
      IF (YHFG) THEN
        CALL UG2DH2(LSUB,PT2C,PT2D,PNTX)
        CALL UG2DH4(PT2C,PT2D,TOLR,WKAR,HFCU,HFFL,LGL1)
        IF (LGL1) GO TO 402
      END IF
      IF (XLFG) THEN
        CALL UG2DH2(LSUB,PT2D,PT2A,PNTX)
        CALL UG2DH4(PT2D,PT2A,TOLR,WKAR,HFCU,HFFL,LGL1)
        IF (LGL1) GO TO 402
      END IF
C
C  DRAW THE INDIVIDUAL COLUMNS.
      DO 202 V1IX=V1LO,V1HI,V1DL
        DO 201 V2IX=V2LO,V2HI,V2DL
C    OBTAIN THE V1-V2-V3 COORDINATES OF THE COLUMN.
          IF (V1FG.EQ.1) THEN
            V1CL=ARAY(1,V1IX)
            V1FR=ARAY(1,V1IX+V1DL)
            V2MN=MIN(ARAY(V2IX,1),ARAY(V2IX+V2DL,1))
            V2MX=MAX(ARAY(V2IX,1),ARAY(V2IX+V2DL,1))
            V3HI=ARAY(V2IX+MIN(0,V2DL),V1IX+MIN(0,V1DL))
          ELSE
            V1CL=ARAY(V1IX,1)
            V1FR=ARAY(V1IX+V1DL,1)
            V2MN=MIN(ARAY(1,V2IX),ARAY(1,V2IX+V2DL))
            V2MX=MAX(ARAY(1,V2IX),ARAY(1,V2IX+V2DL))
            V3HI=ARAY(V1IX+MIN(0,V1DL),V2IX+MIN(0,V2DL))
          END IF
C    DRAW THE VERTICAL LINES OF THE COLUMN.
          IF (TRNS(24-V1FG).GT.0.0) THEN
            VHFG=.FALSE.
          ELSE
            VHFG=.TRUE.
            PNT3(1)=V1FR
            PNT3(2)=V2MX
            PNT3(3)=ARAY(1,1)
            CALL UG2DH1(V1FG,PNT3,TRNS,PT2A)
            PNT3(3)=V3HI
            CALL UG2DH1(V1FG,PNT3,TRNS,PT2B)
            PT2B(1)=PT2A(1)
            CALL UG2DH3(PT2A,PT2B,TOLR,WKAR,HFCU,PT2C,PT2D,LSFG)
            IF (LSFG) CALL UG2DH2(LSUB,PT2C,PT2D,PNTX)
          END IF
          PNT3(1)=V1CL
          PNT3(2)=V2MX
          PNT3(3)=ARAY(1,1)
          CALL UG2DH1(V1FG,PNT3,TRNS,PT2A)
          PNT3(3)=V3HI
          CALL UG2DH1(V1FG,PNT3,TRNS,PT2B)
          PT2B(1)=PT2A(1)
          CALL UG2DH3(PT2A,PT2B,TOLR,WKAR,HFCU,PT2C,PT2D,LSFG)
          IF (LSFG) CALL UG2DH2(LSUB,PT2C,PT2D,PNTX)
          PNT3(1)=V1CL
          PNT3(2)=V2MN
          PNT3(3)=ARAY(1,1)
          CALL UG2DH1(V1FG,PNT3,TRNS,PT2A)
          PNT3(3)=V3HI
          CALL UG2DH1(V1FG,PNT3,TRNS,PT2B)
          PT2B(1)=PT2A(1)
          CALL UG2DH3(PT2A,PT2B,TOLR,WKAR,HFCU,PT2C,PT2D,LSFG)
          IF (LSFG) CALL UG2DH2(LSUB,PT2C,PT2D,PNTX)
          IF (TRNS(24-V1FG).LT.0.0) THEN
            VLFG=.FALSE.
          ELSE
            VLFG=.TRUE.
            PNT3(1)=V1FR
            PNT3(2)=V2MN
            PNT3(3)=ARAY(1,1)
            CALL UG2DH1(V1FG,PNT3,TRNS,PT2A)
            PNT3(3)=V3HI
            CALL UG2DH1(V1FG,PNT3,TRNS,PT2B)
            PT2B(1)=PT2A(1)
            CALL UG2DH3(PT2A,PT2B,TOLR,WKAR,HFCU,PT2C,PT2D,LSFG)
            IF (LSFG) CALL UG2DH2(LSUB,PT2C,PT2D,PNTX)
          END IF
C    DRAW THE TOP OF THE COLUMN.
          PNT3(1)=V1FR
          PNT3(2)=V2MN
          PNT3(3)=V3HI
          CALL UG2DH1(V1FG,PNT3,TRNS,PT2A)
          PNT3(1)=V1CL
          CALL UG2DH1(V1FG,PNT3,TRNS,PT2B)
          PNT3(2)=V2MX
          CALL UG2DH1(V1FG,PNT3,TRNS,PT2C)
          PNT3(1)=V1FR
          CALL UG2DH1(V1FG,PNT3,TRNS,PT2D)
          IF (VLFG.OR.(TRNS(24).LT.0.0)) THEN
            CALL UG2DH3(PT2A,PT2B,TOLR,WKAR,HFCU,PT2E,PT2F,L1FG)
            IF (L1FG) CALL UG2DH2(LSUB,PT2E,PT2F,PNTX)
          END IF
          CALL UG2DH3(PT2B,PT2C,TOLR,WKAR,HFCU,PT2G,PT2H,L2FG)
          IF (L2FG) CALL UG2DH2(LSUB,PT2G,PT2H,PNTX)
          IF (VHFG.OR.(TRNS(24).LT.0.0)) THEN
            CALL UG2DH3(PT2C,PT2D,TOLR,WKAR,HFCU,PT2I,PT2J,L3FG)
            IF (L3FG) CALL UG2DH2(LSUB,PT2I,PT2J,PNTX)
          END IF
          IF (TRNS(24).LT.0.0) THEN
            CALL UG2DH3(PT2D,PT2A,TOLR,WKAR,HFCU,PT2K,PT2L,L4FG)
            IF (L4FG) CALL UG2DH2(LSUB,PT2K,PT2L,PNTX)
          END IF
C    ADJUST THE HEIGHT FUNCTION.
          IF ((VLFG.OR.(TRNS(24).LT.0.0)).AND.(L1FG)) THEN
            IF ((      VLFG .AND.(TRNS(24).GE.0.0)).OR.
     X          ((.NOT.VLFG).AND.(TRNS(24).LT.0.0))) THEN
              CALL UG2DH4(PT2E,PT2F,TOLR,WKAR,HFCU,HFFL,LGL1)
              IF (LGL1) GO TO 402
            END IF
          END IF
          IF (L2FG) THEN
            IF (TRNS(24).GE.0.0) THEN
              CALL UG2DH4(PT2G,PT2H,TOLR,WKAR,HFCU,HFFL,LGL1)
              IF (LGL1) GO TO 402
            END IF
          END IF
          IF ((VHFG.OR.(TRNS(24).LT.0.0)).AND.(L3FG)) THEN
            IF ((      VHFG .AND.(TRNS(24).GE.0.0)).OR.
     X          ((.NOT.VHFG).AND.(TRNS(24).LT.0.0))) THEN
              CALL UG2DH4(PT2I,PT2J,TOLR,WKAR,HFCU,HFFL,LGL1)
              IF (LGL1) GO TO 402
            END IF
          END IF
          IF (L4FG) THEN
            CALL UG2DH4(PT2K,PT2L,TOLR,WKAR,HFCU,HFFL,LGL1)
            IF (LGL1) GO TO 402
          END IF
  201   CONTINUE
  202 CONTINUE
C
C  RESET ERROR INDICATORS AND RETURN TO CALLER.
      UGELV=0
      UGENM='        '
      UGEIX=0
  301 RETURN
C
C  REPORT ERRORS TO THE UNIFIED GRAPHICS SYSTEM ERROR PROCESSOR.
  401 CALL UGRERR(3,'UG2DHG  ',1)
      GO TO 301
  402 CALL UGRERR(3,'UG2DHG  ',2)
      GO TO 301
  403 CALL UGRERR(3,'UG2DHG  ',3)
      GO TO 301
C
      END
