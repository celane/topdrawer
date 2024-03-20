      SUBROUTINE UGPLIN(OPTN,XARY,YARY,NPTS,BBTS,NBTS,SEGM)
C
C *******************  THE UNIFIED GRAPHICS SYSTEM  *******************
C *            ADD AN ARRAY OF LINES TO A GRAPHIC SEGMENT             *
C *                                                                   *
C *  THIS SUBROUTINE MAY BE USED TO ADD AN ARRAY OF LINES TO A        *
C *  GRAPHIC SEGMENT.  THE LINES MAY BE EITHER BLANKED OR DRAWN.      *
C *                                                                   *
C *  THE CALLING SEQUENCE IS:                                         *
C *    CALL UGPLIN(OPTN,XARY,YARY,NPTS,BBTS,NBTS,SEGM)                *
C *                                                                   *
C *  THE PARAMETERS IN THE CALLING SEQUENCE ARE:                      *
C *    OPTN  THE OPTIONS LIST.                                        *
C *    XARY  THE X COORDINATES OF THE LINE END POINTS.                *
C *    YARY  THE Y COORDINATES OF THE LINE END POINTS.                *
C *    NPTS  THE NUMBER OF END POINTS IN THE ARRAYS.                  *
C *    BBTS  THE BLANKING BIT ARRAY.                                  *
C *    NBTS  THE NUMBER OF BLANKING BITS IN THE ARRAY.                *
C *    SEGM  THE GRAPHIC SEGMENT WHICH WILL HAVE THE LINE END POINTS  *
C *          ADDED TO IT.                                             *
C *                                                                   *
C *                          ROBERT C. BEACH                          *
C *                    COMPUTATION RESEARCH GROUP                     *
C *                STANFORD LINEAR ACCELERATOR CENTER                 *
C *                                                                   *
C *********************************************************************
C
      CHARACTER*(*) OPTN
      REAL          XARY(*),YARY(*)
      INTEGER       NPTS
      INTEGER*4     BBTS(*)
      INTEGER       NBTS
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
      INTEGER*4     MODE(NMOD)
      INTEGER       RTRY,IBTS,KBTS
      LOGICAL       BTFG,BLANK
C
      INTEGER*4     OVI4
      REAL*4        OVR4
      EQUIVALENCE   (OVI4,OVR4)
      INTEGER       INT1,INT2,INT3,INT4

      LOGICAL       BTEST

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
      IF (NPTS.LT.1) GO TO 201
      MODE(1)=2
      MODE(2)=NMOD+2*NPTS
      MODE(3)=EXIL
      MODE(4)=EXCR
      MODE(5)=EXBL
      MODE(6)=EXPI
      MODE(7)=EXLS
C
C  INSERT THE MODE SPECIFICATION AND THE DATA INTO THE SEGMENT.
  101 RTRY=SEGM(1)
      CALL UGB002(MODE,NMOD,2*NPTS,SEGM,INT1)
      IF (INT1.EQ.0) GO TO 301
      IBTS=NBTS
      BTFG=.TRUE.
      IF (IBTS.LE.0) THEN
        IBTS=-IBTS
        BTFG=.FALSE.
      END IF
      KBTS=0
      DO 102 INT2=1,NPTS
        OVR4=XARY(INT2)
        SEGM(INT1)=OVI4
        OVR4=YARY(INT2)
        SEGM(INT1+1)=OVI4
        BLANK = .FALSE.
        IF (KBTS.NE.0) THEN
          IF (BTFG) THEN
            BLANK = (BBTS(KBTS).NE.0)
          ELSE
            INT3 = 1 + (KBTS-1)/32
            INT4 = MOD (KBTS-1 ,32)
            BLANK = BTEST ( BBTS(INT3) , INT4 )
          END IF
        END IF
        IF (BLANK) THEN
           SEGM(INT1+1) = IBSET( SEGM(INT1+1) , LSBFLO )
        ELSE
           SEGM(INT1+1) = IBCLR( SEGM(INT1+1) , LSBFLO )
        ENDIF
        IF (KBTS.GE.IBTS) THEN
          KBTS=1
        ELSE
          KBTS=KBTS+1
        END IF
        INT1=INT1+2
  102 CONTINUE
C
C  RESET ERROR INDICATORS AND RETURN TO CALLER.
  201 UGELV=0
      UGENM='        '
      UGEIX=0
  202 RETURN
C
C  REPORT ERRORS TO THE UNIFIED GRAPHICS SYSTEM ERROR PROCESSOR.
  301 CALL UGRERR(2,'UGPLIN  ',11)
      IF (SEGM(1).NE.RTRY) GO TO 101
      GO TO 202
C
      END
