      SUBROUTINE UGLINE(OPTN,XCRD,YCRD,BBIT,SEGM)
C
C *******************  THE UNIFIED GRAPHICS SYSTEM  *******************
C *                  ADD A LINE TO A GRAPHIC SEGMENT                  *
C *                                                                   *
C *  THIS SUBROUTINE MAY BE USED TO ADD A SINGLE LINE SEGMENT END     *
C *  POINT TO A GRAPHIC SEGMENT.  THE END POINT MAY BE EITHER         *
C *  BLANKED OR INTENSIFIED.  THE PROGRAMMER SHOULD BLANK TO THE      *
C *  FIRST END POINT AND THE EITHER INTENSIFY OR BLANK TO THE         *
C *  FOLLOWING END POINTS TO CREATE THE PICTURE.                      *
C *                                                                   *
C *  THE CALLING SEQUENCE IS:                                         *
C *    CALL UGLINE(OPTN,XCRD,YCRD,BBIT,SEGM)                          *
C *                                                                   *
C *  THE PARAMETERS IN THE CALLING SEQUENCE ARE:                      *
C *    OPTN  THE OPTIONS LIST.                                        *
C *    XCRD  THE X COORDINATE OF THE VECTOR END POINT.                *
C *    YCRD  THE Y COORDINATE OF THE VECTOR END POINT.                *
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
      REAL          XCRD,YCRD
      INTEGER       BBIT
      INTEGER*4     SEGM(*)
C
      INCLUDE       'UGSYSTEM:UGERRCBK.FOR'
C
      INCLUDE       'UGSYSTEM:UGPOTCBK.FOR'
C
      INCLUDE       'UGSYSTEM:UGPOTDCL.FOR'
C
C  LENGTH OF MODE BLOCK FOR LINE DATA.
      INTEGER       NMOD
      PARAMETER     (NMOD=7)
C
      INTEGER*4     MODE(NMOD),XPNT(2)
      INTEGER       RTRY,NDTA
      LOGICAL       XPFG,BLANK
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
      EXLS=POTLS
      CALL UGOPTION(OPTN,DESC_NUM2,POTST,IFLAG2,EXST,EXSTR)
C
C  CONSTRUCT THE MODE SPECIFICATION.
      MODE(1)=2
      MODE(2)=NMOD+2
      MODE(3)=EXIL
      MODE(4)=EXCR
      MODE(5)=EXBL
      MODE(6)=EXPI
      MODE(7)=EXLS
C
C  INSERT THE MODE SPECIFICATION AND THE DATA INTO THE SEGMENT.
      NDTA=2
      XPFG=.FALSE.
      BLANK = (BBIT.NE.0)
      IF (BLANK) THEN
        IF (SEGM(SEGM(3)).NE.2) THEN
          BLANK = .FALSE.
        ELSE
          IF ((SEGM(SEGM(3)+2).NE.MODE(3)).OR.
     X        (SEGM(SEGM(3)+3).NE.MODE(4)).OR.
     X        (SEGM(SEGM(3)+4).NE.MODE(5)).OR.
     X        (SEGM(SEGM(3)+5).NE.MODE(6)).OR.
     X        (SEGM(SEGM(3)+6).NE.MODE(7))) THEN
            INT1=SEGM(3)+SEGM(SEGM(3)+1)
            XPNT(1)=SEGM(INT1-2)
            XPNT(2)=SEGM(INT1-1)
            XPFG=.TRUE.
            NDTA=4
            MODE(2)=NMOD+4
          END IF
        END IF
      END IF
  101 RTRY=SEGM(1)
      CALL UGB002(MODE,NMOD,NDTA,SEGM,INT1)
      IF (INT1.EQ.0) GO TO 301
      IF (XPFG) THEN
        SEGM(INT1)=XPNT(1)
        SEGM(INT1+1)=IBCLR(XPNT(2),LSBFLO)
        INT1=INT1+2
      END IF
      OVR4=XCRD
      SEGM(INT1)=OVI4
      OVR4=YCRD
      IF (BLANK) THEN
         SEGM(INT1+1) = IBSET(OVI4,LSBFLO)
      ELSE
         SEGM(INT1+1) = IBCLR(OVI4,LSBFLO)
      ENDIF
C
C  RESET ERROR INDICATORS AND RETURN TO CALLER.
      UGELV=0
      UGENM='        '
      UGEIX=0
  201 RETURN
C
C  REPORT ERRORS TO THE UNIFIED GRAPHICS SYSTEM ERROR PROCESSOR.
  301 CALL UGRERR(2,'UGLINE  ',11)
      IF (SEGM(1).NE.RTRY) GO TO 101
      GO TO 201
C
      END
