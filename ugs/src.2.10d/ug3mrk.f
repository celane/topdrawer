      SUBROUTINE UG3MRK(OPTN,XCRD,YCRD,ZCRD,SEGM)
C
C *******************  THE UNIFIED GRAPHICS SYSTEM  *******************
C *        ADD A THREE-DIMENSIONAL MARKER TO A GRAPHIC SEGMENT        *
C *                                                                   *
C *  THIS SUBROUTINE MAY BE USED TO ADD A SINGLE THREE-DIMENSIONAL    *
C *  MARKER TO A GRAPHIC SEGMENT.                                     *
C *                                                                   *
C *  THE CALLING SEQUENCE IS:                                         *
C *    CALL UG3MRK(OPTN,XCRD,YCRD,ZCRD,SEGM)                          *
C *                                                                   *
C *  THE PARAMETERS IN THE CALLING SEQUENCE ARE:                      *
C *    OPTN  THE OPTIONS LIST.                                        *
C *    XCRD  THE X COORDINATE OF THE MARKER.                          *
C *    YCRD  THE Y COORDINATE OF THE MARKER.                          *
C *    ZCRD  THE Z COORDINATE OF THE MARKER.                          *
C *    SEGM  THE GRAPHIC SEGMENT WHICH WILL HAVE THE MARKER ADDED TO  *
C *          IT.                                                      *
C *                                                                   *
C *                          ROBERT C. BEACH                          *
C *                    COMPUTATION RESEARCH GROUP                     *
C *                STANFORD LINEAR ACCELERATOR CENTER                 *
C *                                                                   *
C *********************************************************************
C
      CHARACTER*(*) OPTN
      REAL          XCRD,YCRD,ZCRD
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
      MODE(1)=7
      MODE(2)=NMOD+3
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
      CALL UGB002(MODE,NMOD,3,SEGM,INT1)
      IF (INT1.EQ.0) GO TO 301
      OVR4=XCRD
      SEGM(INT1)=OVI4
      OVR4=YCRD
      SEGM(INT1+1)=OVI4
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
  301 CALL UGRERR(2,'UG3MRK  ',11)
      IF (SEGM(1).NE.RTRY) GO TO 101
      GO TO 201
C
      END
