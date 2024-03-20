      SUBROUTINE UG3PMK(OPTN,XARY,YARY,ZARY,NPTS,SEGM)
C
C *******************  THE UNIFIED GRAPHICS SYSTEM  *******************
C *  ADD AN ARRAY OF THREE-DIMENSIONAL MARKERS TO A GRAPHIC SEGMENT   *
C *                                                                   *
C *  THIS SUBROUTINE MAY BE USED TO ADD AN ARRAY OF THREE-            *
C *  DIMENSIONAL MARKERS TO A GRAPHIC SEGMENT.                        *
C *                                                                   *
C *  THE CALLING SEQUENCE IS:                                         *
C *    CALL UG3PMK(OPTN,XARY,YARY,ZARY,NPTS,SEGM)                     *
C *                                                                   *
C *  THE PARAMETERS IN THE CALLING SEQUENCE ARE:                      *
C *    OPTN  THE OPTIONS LIST.                                        *
C *    XARY  THE X COORDINATES OF THE MARKERS.                        *
C *    YARY  THE Y COORDINATES OF THE MARKERS.                        *
C *    ZARY  THE Z COORDINATES OF THE MARKERS.                        *
C *    NPTS  THE NUMBER OF MARKERS IN THE ARRAYS.                     *
C *    SEGM  THE GRAPHIC SEGMENT WHICH WILL HAVE THE MARKERS ADDED    *
C *          TO IT.                                                   *
C *                                                                   *
C *                          ROBERT C. BEACH                          *
C *                    COMPUTATION RESEARCH GROUP                     *
C *                STANFORD LINEAR ACCELERATOR CENTER                 *
C *                                                                   *
C *********************************************************************
C
      CHARACTER*(*) OPTN
      REAL          XARY(*),YARY(*),ZARY(*)
      INTEGER       NPTS
      INTEGER*4     SEGM(*)
C
      INCLUDE       'UGSYSTEM:UGERRCBK.FOR'
C
      INCLUDE       'UGSYSTEM:UGPOTCBK.FOR'
C
      INCLUDE       'UGSYSTEM:UGPOTDCL.FOR'
C
C  LENGTH OF MODE BLOCK FOR THREE-DIMENSIONAL MARKER DATA.
      INTEGER       NMOD
      PARAMETER     (NMOD=8)
C
      INTEGER*4     MODE(NMOD)
      INTEGER       RTRY
C
      INTEGER*4     OVI4
      REAL*4        OVR4
      EQUIVALENCE   (OVI4,OVR4)
      INTEGER       INT1,INT2
C
C  SCAN THE OPTIONS LIST.
      EXIL=POTIL
      EXCR=POTCR
      EXBL=POTBL
      EXPI=POTPI
      EXCC=POTCC
      CALL UGOPTION(OPTN,DESC_NUM2,POTST,IFLAG2,EXST,EXSTR)
C
C  CONSTRUCT THE MODE SPECIFICATION.
      IF (NPTS.LT.1) GO TO 201
      MODE(1)=7
      MODE(2)=NMOD+3*NPTS
      MODE(3)=EXIL
      MODE(4)=EXCR
      MODE(5)=EXBL
      MODE(6)=EXPI
      OVR4=-PODDZ
      MODE(7)=OVI4
      MODE(8)=PODMK
C
C  INSERT THE MODE SPECIFICATION AND THE DATA INTO THE SEGMENT.
  101 RTRY=SEGM(1)
      CALL UGB002(MODE,NMOD,3*NPTS,SEGM,INT1)
      IF (INT1.EQ.0) GO TO 301
      DO 102 INT2=1,NPTS
        IF (EXCC.EQ.1) THEN
          OVR4=XARY(1)
        ELSE
          OVR4=XARY(INT2)
        END IF
        SEGM(INT1)=OVI4
        IF (EXCC.EQ.2) THEN
          OVR4=YARY(1)
        ELSE
          OVR4=YARY(INT2)
        END IF
        SEGM(INT1+1)=OVI4
        IF (EXCC.EQ.3) THEN
          OVR4=ZARY(1)
        ELSE
          OVR4=ZARY(INT2)
        END IF
        SEGM(INT1+2)=OVI4
        INT1=INT1+3
  102 CONTINUE
C
C  RESET ERROR INDICATORS AND RETURN TO CALLER.
  201 UGELV=0
      UGENM='        '
      UGEIX=0
  202 RETURN
C
C  REPORT ERRORS TO THE UNIFIED GRAPHICS SYSTEM ERROR PROCESSOR.
  301 CALL UGRERR(2,'UG3PMK  ',11)
      IF (SEGM(1).NE.RTRY) GO TO 101
      GO TO 202
C
      END
