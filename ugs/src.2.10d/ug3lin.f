      SUBROUTINE UG3LIN(OPTN,XCRD,YCRD,ZCRD,BBIT,SEGM)
C
C *******************  THE UNIFIED GRAPHICS SYSTEM  *******************
C *         ADD A THREE-DIMENSIONAL LINE TO A GRAPHIC SEGMENT         *
C *                                                                   *
C *  THIS SUBROUTINE MAY BE USED TO ADD A SINGLE THREE-DIMENSIONAL    *
C *  LINE SEGMENT END POINT TO A GRAPHIC SEGMENT.  THE END POINT MAY  *
C *  BE EITHER BLANKED OR INTENSIFIED.  THE PROGRAMMER SHOULD BLANK   *
C *  TO THE FIRST END POINT AND THE EITHER INTENSIFY OR BLANK TO THE  *
C *  FOLLOWING END POINTS TO CREATE THE PICTURE.                      *
C *                                                                   *
C *  THE CALLING SEQUENCE IS:                                         *
C *    CALL UG3LIN(OPTN,XCRD,YCRD,ZCRD,BBIT,SEGM)                     *
C *                                                                   *
C *  THE PARAMETERS IN THE CALLING SEQUENCE ARE:                      *
C *    OPTN  THE OPTIONS LIST.                                        *
C *    XCRD  THE X COORDINATE OF THE VECTOR END POINT.                *
C *    YCRD  THE Y COORDINATE OF THE VECTOR END POINT.                *
C *    ZCRD  THE Z COORDINATE OF THE VECTOR END POINT.                *
C *    BBIT  THE BLANKING BIT.                                        *
C *    SEGM  THE GRAPHIC SEGMENT WHICH WILL HAVE THE LINE END POINT   *
C *          ADDED TO IT.                                             *
C *                                                                   *
C *                          ROBERT C. BEACH                          *
C *                    COMPUTATION RESEARCH GROUP                     *
C *                STANFORD LINEAR ACCELERATOR CENTER                 *
C *                                                                   *
C *********************************************************************
C
      CHARACTER*(*) OPTN
      REAL          XCRD,YCRD,ZCRD
      INTEGER       BBIT
      INTEGER*4     SEGM(*)
C
      INCLUDE       'UGSYSTEM:UGERRCBK.FOR'
      INCLUDE       'UGSYSTEM:UGPOTCBK.FOR'
      INCLUDE       'UGSYSTEM:UGPOTDCL.FOR'
C
C  LENGTH OF MODE BLOCK FOR THREE-DIMENSIONAL LINE DATA.
      INTEGER       NMOD
      PARAMETER     (NMOD=7)
C
      INTEGER*4     MODE(NMOD),XPNT(3)
      INTEGER       RTRY,NDTA
      LOGICAL       BLANK

      LOGICAL       XPFG
C
      INTEGER*4     OVI4
      REAL*4        OVR4
      EQUIVALENCE   (OVI4,OVR4)
      INTEGER       INT1
C
C  SCAN THE OPTIONS LIST.
      EXIL=POTIL
      EXCR=POTCR
      EXBL=POTBL
      EXPI=POTPI
      CALL UGOPTION(OPTN,DESC_NUM2,POTST,IFLAG2,EXST,EXSTR)
C
C  CONSTRUCT THE MODE SPECIFICATION.
      MODE(1)=8
      MODE(2)=NMOD+3
      MODE(3)=EXIL
      MODE(4)=EXCR
      MODE(5)=EXBL
      MODE(6)=EXPI
      MODE(7)=PODLS
C
C  INSERT THE MODE SPECIFICATION AND THE DATA INTO THE SEGMENT.
      NDTA=3
      XPFG=.FALSE.
      BLANK = (BBIT.NE.0)
      IF (BLANK) THEN
        IF (SEGM(SEGM(3)).NE.8) THEN
          BLANK = .FALSE.
        ELSE
          IF ((SEGM(SEGM(3)+2).NE.MODE(3)).OR.
     X        (SEGM(SEGM(3)+3).NE.MODE(4)).OR.
     X        (SEGM(SEGM(3)+4).NE.MODE(5)).OR.
     X        (SEGM(SEGM(3)+5).NE.MODE(6))) THEN
            INT1=SEGM(3)+SEGM(SEGM(3)+1)
            XPNT(1)=SEGM(INT1-3)
            XPNT(2)=SEGM(INT1-2)
            XPNT(3)=SEGM(INT1-1)
            XPFG=.TRUE.
            NDTA=6
            MODE(2)=NMOD+6
          END IF
        END IF
      END IF
  101 RTRY=SEGM(1)
      CALL UGB002(MODE,NMOD,NDTA,SEGM,INT1)
      IF (INT1.EQ.0) GO TO 301
      IF (XPFG) THEN
        SEGM(INT1)=XPNT(1)
        SEGM(INT1+1)=IBCLR(XPNT(2),LSBFLO)
        SEGM(INT1+2)=XPNT(3)
        INT1=INT1+3
      END IF
      OVR4=XCRD
      SEGM(INT1)=OVI4
      OVR4=YCRD
      IF (BLANK) THEN
         SEGM(INT1+1) = IBSET(OVI4,LSBFLO)
      ELSE
         SEGM(INT1+1) = IBCLR(OVI4,LSBFLO)
      ENDIF
      OVR4=ZCRD
      SEGM(INT1+2)=OVI4
C
C  RESET ERROR INDICATORS AND RETURN TO CALLER.
      UGELV=0
      UGENM='        '
      UGEIX=0
  201 RETURN
C
C  REPORT ERRORS TO THE UNIFIED GRAPHICS SYSTEM ERROR PROCESSOR.
  301 CALL UGRERR(2,'UG3LIN  ',11)
      IF (SEGM(1).NE.RTRY) GO TO 101
      GO TO 201
C
      END
