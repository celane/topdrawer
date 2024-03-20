      SUBROUTINE UGPMRK(OPTN,XARY,YARY,NPTS,SEGM)
C
C *******************  THE UNIFIED GRAPHICS SYSTEM  *******************
C *           ADD AN ARRAY OF MARKERS TO A GRAPHIC SEGMENT            *
C *                                                                   *
C *  THIS SUBROUTINE MAY BE USED TO ADD AN ARRAY OF MARKERS TO A      *
C *  GRAPHIC SEGMENT.                                                 *
C *                                                                   *
C *  THE CALLING SEQUENCE IS:                                         *
C *    CALL UGPMRK(OPTN,XARY,YARY,NPTS,SEGM)                          *
C *                                                                   *
C *  THE PARAMETERS IN THE CALLING SEQUENCE ARE:                      *
C *    OPTN  THE OPTIONS LIST.                                        *
C *    XARY  THE X COORDINATES OF THE MARKERS.                        *
C *    YARY  THE Y COORDINATES OF THE MARKERS.                        *
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
      REAL          XARY(*),YARY(*)
      INTEGER       NPTS
      INTEGER*4     SEGM(*)
C
      INCLUDE       'UGSYSTEM:UGERRCBK.FOR'
C
      INCLUDE       'UGSYSTEM:UGPOTCBK.FOR'
C
      INCLUDE       'UGSYSTEM:UGPOTDCL.FOR'
C
C  LENGTH OF MODE BLOCK FOR MARKER DATA.
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
      EXMK=POTMK
      EXSZ=-POTSZ
      EXDZ=-POTDZ
      CALL UGOPTION(OPTN,DESC_NUM2,POTST,IFLAG2,EXST,EXSTR)
C
C  CONSTRUCT THE MODE SPECIFICATION.
      IF (NPTS.LT.1) GO TO 201
      MODE(1)=1
      MODE(2)=NMOD+2*NPTS
      MODE(3)=EXIL
      MODE(4)=EXCR
      MODE(5)=EXBL
      MODE(6)=EXPI
      IF (EXSZ.GT.0.0) THEN
        OVR4=EXSZ
      ELSE IF (EXDZ.GT.0.0) THEN
        OVR4=-EXDZ
      ELSE IF (EXSZ.LT.0.0) THEN
        OVR4=-EXSZ
      ELSE IF (EXDZ.LT.0.0) THEN
        OVR4=EXDZ
      ELSE
        OVR4=0.0
      END IF
      MODE(7)=OVI4
      IF ((EXMK.LT.0).OR.(EXMK.GT.9)) THEN
        MODE(8)=PODMK
      ELSE
        MODE(8)=EXMK
      END IF
C
C  INSERT THE MODE SPECIFICATION AND THE DATA INTO THE SEGMENT.
  101 RTRY=SEGM(1)
      CALL UGB002(MODE,NMOD,2*NPTS,SEGM,INT1)
      IF (INT1.EQ.0) GO TO 301
      DO 102 INT2=1,NPTS
        OVR4=XARY(INT2)
        SEGM(INT1)=OVI4
        OVR4=YARY(INT2)
        SEGM(INT1+1)=OVI4
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
  301 CALL UGRERR(2,'UGPMRK  ',11)
      IF (SEGM(1).NE.RTRY) GO TO 101
      GO TO 202
C
      END
