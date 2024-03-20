      SUBROUTINE UG2DH3(PNT1,PNT2,TOLR,WKAR,HFCU,PNT3,PNT4,VFLG)
C
C *******************  THE UNIFIED GRAPHICS SYSTEM  *******************
C *             CHECK A LINE AGAINST THE HEIGHT FUNCTION              *
C *                                                                   *
C *  THIS SUBROUTINE IS USED BY UG2DHG TO CHECK THE CURRENT LINE      *
C *  SEGMENT AGAINST THE HEIGHT FUNCTION.  IF PART OF THE LINE IS     *
C *  VISIBLE, A FLAG IS SET AND THE VISIBLE PART IS SAVED IN TWO      *
C *  OUTPUT POINTS.                                                   *
C *                                                                   *
C *  THE CALLING SEQUENCE IS:                                         *
C *    CALL UG2DH3(PNT1,PNT2,TOLR,WKAR,HFCU,PNT3,PNT4,VFLG)           *
C *                                                                   *
C *  THE PARAMETERS IN THE CALLING SEQUENCE ARE:                      *
C *    PNT1  AN END POINT OF THE CURRENT LINE SEGMENT.                *
C *    PNT2  AN END POINT OF THE CURRENT LINE SEGMENT.                *
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
      REAL          TOLR
      REAL*4        WKAR(*)
      INTEGER       HFCU
      REAL          PNT3(2),PNT4(2)
      LOGICAL       VFLG
C
      REAL*4        HFB1
      INTEGER*2     HFP1(2)
      EQUIVALENCE   (HFP1(1),HFB1)
C
      LOGICAL       FGSW,FGVI,FGPA
      INTEGER       HFNX
      REAL          TPT3(2),PTP3(2),PTP4(2),PNTI(2)
C
C  SAVE AND ORDER GIVEN LINE SEGMENT, AND SET HEIGHT FUNCTION INDEX.
      IF (PNT1(1).LE.PNT2(1)) THEN
        PNT3(1)=PNT1(1)
        PNT3(2)=PNT1(2)
        PNT4(1)=PNT2(1)
        PNT4(2)=PNT2(2)
        FGSW=.FALSE.
      ELSE
        PNT3(1)=PNT2(1)
        PNT3(2)=PNT2(2)
        PNT4(1)=PNT1(1)
        PNT4(2)=PNT1(2)
        FGSW=.TRUE.
      END IF
  101 IF (WKAR(HFCU+1).GT.PNT3(1)) THEN
        HFB1=WKAR(HFCU)
        HFCU=HFP1(2)
        GO TO 101
      END IF
  102 HFB1=WKAR(HFCU)
      HFNX=HFP1(1)
      IF (PNT3(1).GE.WKAR(HFNX+1)) THEN
        HFCU=HFNX
        GO TO 102
      END IF
      TPT3(1)=PNT3(1)
      TPT3(2)=PNT3(2)
      FGVI=.FALSE.
C
C  DETERMINE WHICH PART (IF ANY) OF THE GIVEN LINE IS ABOVE THE
C  HEIGHT FUNCTION.
  201 CALL UG2DH5(TPT3,WKAR(HFCU+1),WKAR(HFNX+1),PTP3)
      IF (PNT4(1).LE.WKAR(HFNX+1)) THEN
C    GIVEN LINE TERMINATES IN CURRENT HEIGHT FUNCTION SEGMENT.
        CALL UG2DH5(PNT4,WKAR(HFCU+1),WKAR(HFNX+1),PTP4)
        IF (TPT3(2).GE.PTP3(2)) THEN
          IF (PNT4(2).GE.PTP4(2)) THEN
C      GIVEN LINE IS ABOVE CURRENT HEIGHT FUNCTION SEGMENT.
            CONTINUE
          ELSE
C      FIRST POINT IS ABOVE CURRENT HEIGHT FUNCTION, SECOND BELOW.
            CALL UG2DH6(WKAR(HFCU+1),WKAR(HFNX+1),PNT1,PNT2,
     X        PNTI,TOLR,FGPA)
            IF (FGPA) THEN
              PNT4(1)=PNTI(1)
              PNT4(2)=PNTI(2)
            ELSE
              PNT4(1)=TPT3(1)
              PNT4(2)=TPT3(2)
            END IF
          END IF
        ELSE
          IF (PNT4(2).GE.PTP4(2)) THEN
C      FIRST POINT IS BELOW CURRENT HEIGHT FUNCTION, SECOND ABOVE.
            CALL UG2DH6(WKAR(HFCU+1),WKAR(HFNX+1),PNT1,PNT2,
     X        PNTI,TOLR,FGPA)
            IF (FGPA) THEN
              PNT3(1)=PNTI(1)
              PNT3(2)=PNTI(2)
            ELSE
              PNT4(1)=TPT3(1)
              PNT4(2)=TPT3(2)
            END IF
          ELSE
C      GIVEN LINE IS BELOW CURRENT HEIGHT FUNCTION SEGMENT.
            PNT4(1)=TPT3(1)
            PNT4(2)=TPT3(2)
          END IF
        END IF
        GO TO 301
      ELSE
C    GIVEN LINE DOES NOT TERMINATE IN CURRENT HEIGHT FUNCTION SEGMENT.
        CALL UG2DH5(WKAR(HFNX+1),PNT1,PNT2,PTP4)
        IF (TPT3(2).GE.PTP3(2)) THEN
          IF (PTP4(2).GE.WKAR(HFNX+2)) THEN
C      GIVEN LINE IS ABOVE CURRENT HEIGHT FUNCTION SEGMENT.
            TPT3(1)=PTP4(1)
            TPT3(2)=PTP4(2)
            FGVI=.TRUE.
          ELSE
C      FIRST POINT IS ABOVE CURRENT HEIGHT FUNCTION, SECOND BELOW.
            CALL UG2DH6(WKAR(HFCU+1),WKAR(HFNX+1),PNT1,PNT2,
     X        PNTI,TOLR,FGPA)
            IF (FGPA) THEN
              PNT4(1)=PNTI(1)
              PNT4(2)=PNTI(2)
              GO TO 301
            ELSE
              IF (FGVI) THEN
                TPT3(1)=PTP4(1)
                TPT3(2)=PTP4(2)
              ELSE
                TPT3(1)=PTP4(1)
                TPT3(2)=PTP4(2)
                PNT3(1)=PTP4(1)
                PNT3(2)=PTP4(2)
              END IF
            END IF
          END IF
        ELSE
          IF (PTP4(2).GE.WKAR(HFNX+2)) THEN
C      FIRST POINT IS BELOW CURRENT HEIGHT FUNCTION, SECOND ABOVE.
            CALL UG2DH6(WKAR(HFCU+1),WKAR(HFNX+1),PNT1,PNT2,
     X        PNTI,TOLR,FGPA)
            IF (FGPA) THEN
              PNT3(1)=PNTI(1)
              PNT3(2)=PNTI(2)
              TPT3(1)=PNTI(1)
              TPT3(2)=PNTI(2)
              FGVI=.TRUE.
            ELSE
              IF (FGVI) THEN
                TPT3(1)=PTP4(1)
                TPT3(2)=PTP4(2)
              ELSE
                TPT3(1)=PTP4(1)
                TPT3(2)=PTP4(2)
                PNT3(1)=PTP4(1)
                PNT3(2)=PTP4(2)
              END IF
            END IF
          ELSE
C      GIVEN LINE IS BELOW CURRENT HEIGHT FUNCTION SEGMENT.
            IF (FGVI) THEN
              PNT4(1)=TPT3(1)
              PNT4(2)=TPT3(2)
              GO TO 301
            ELSE
              TPT3(1)=PTP4(1)
              TPT3(2)=PTP4(2)
              PNT3(1)=PTP4(1)
              PNT3(2)=PTP4(2)
            END IF
          END IF
        END IF
      END IF
C    ADVANCE HEIGHT FUNCTION POINTER TO NEXT NON-VERTICAL SEGMENT.
  202 HFCU=HFNX
      HFB1=WKAR(HFCU)
      HFNX=HFP1(1)
      IF (WKAR(HFCU+1).EQ.WKAR(HFNX+1)) GO TO 202
      GO TO 201
C
C  PROCESS THE OUTPUT SEGMENT.
  301 IF ((PNT3(1).EQ.PNT4(1)).AND.
     X    (PNT3(2).EQ.PNT4(2))) THEN
        VFLG=.FALSE.
      ELSE
        IF (FGSW) THEN
          TPT3(1)=PNT3(1)
          TPT3(2)=PNT3(2)
          PNT3(1)=PNT4(1)
          PNT3(2)=PNT4(2)
          PNT4(1)=TPT3(1)
          PNT4(2)=TPT3(2)
        END IF
        VFLG=.TRUE.
      END IF
C
C  RETURN TO CALLING SUBROUTINE.
      RETURN
C
      END
