      SUBROUTINE UGPFIL(OPTN,XPOL,YPOL,NPOL,SEGM)
C
C *******************  THE UNIFIED GRAPHICS SYSTEM  *******************
C *          ADD A POLYGON TO BE FILLED TO A GRAPHIC SEGMENT          *
C *                                                                   *
C *  THIS SUBROUTINE MAY BE USED TO ADD POLYGON THAT WILL BE FILLED   *
C *  WITH COLOR TO A GRAPHIC SEGMENT.                                 *
C *                                                                   *
C *  THE CALLING SEQUENCE IS:                                         *
C *    CALL UGPFIL(OPTN,XPOL,YPOL,NPOL,SEGM)                          *
C *                                                                   *
C *  THE PARAMETERS IN THE CALLING SEQUENCE ARE:                      *
C *    OPTN  THE OPTIONS LIST.                                        *
C *    XPOL  THE X COORDINATES OF THE VERTICES OF THE POLYGON.        *
C *    YPOL  THE Y COORDINATES OF THE VERTICES OF THE POLYGON.        *
C *    NPOL  THE NUMBER OF VERTICES IN THE POLYGON.                   *
C *    SEGM  THE GRAPHIC SEGMENT WHICH WILL HAVE THE POLYGON ADDED    *
C *          TO IT.                                                   *
C *                                                                   *
C *                          ROBERT C. BEACH                          *
C *                    COMPUTATION RESEARCH GROUP                     *
C *                STANFORD LINEAR ACCELERATOR CENTER                 *
C *                                                                   *
C *********************************************************************
C
      CHARACTER*(*) OPTN
      REAL          XPOL(*),YPOL(*)
      INTEGER       NPOL
      INTEGER*4     SEGM(*)
C
      INCLUDE       'UGSYSTEM:UGERRCBK.FOR'
C
      INCLUDE       'UGSYSTEM:UGPOTCBK.FOR'
C
      INCLUDE       'UGSYSTEM:UGPOTDCL.FOR'
C
C  LENGTH OF MODE BLOCK FOR POLYGON FILL DATA.
      INTEGER       NMOD
      PARAMETER     (NMOD=7)
C
      INTEGER*4     MODE(NMOD)
      INTEGER       RTRY,KPTS
C
      INTEGER*4     OVI4
      REAL*4        OVR4
      EQUIVALENCE   (OVI4,OVR4)
      INTEGER       INT1,INT2,INT3
C
C  SCAN THE OPTIONS LIST.
      EXIL=POTIL
      EXCR=POTCR
      EXBL=POTBL
      EXPI=POTPI
      CALL UGOPTION(OPTN,DESC_NUM2,POTST,IFLAG2,EXST,EXSTR)
C
C  DETERMINE THE NUMBER OF VERTICES IN THE POLYGON.
      IF ((XPOL(1).EQ.XPOL(NPOL)).AND.
     X    (YPOL(1).EQ.YPOL(NPOL))) THEN
        KPTS=NPOL
      ELSE
        KPTS=NPOL+1
      END IF
      IF (KPTS.LT.4) GO TO 301
      IF (KPTS.GT.32) GO TO 301
C
C  CONSTRUCT THE MODE SPECIFICATION.
      MODE(1)=5
      MODE(2)=NMOD+2*KPTS
      MODE(3)=EXIL
      MODE(4)=EXCR
      MODE(5)=EXBL
      MODE(6)=EXPI
      MODE(7)=KPTS
C
C  INSERT THE MODE SPECIFICATION AND THE DATA INTO THE SEGMENT.
  101 RTRY=SEGM(1)
      CALL UGB002(MODE,-NMOD,2*KPTS,SEGM,INT1)
      IF (INT1.EQ.0) GO TO 302
      DO 102 INT2=1,KPTS
        IF ((INT2.EQ.KPTS).AND.(KPTS.NE.NPOL)) THEN
          INT3=1
        ELSE
          INT3=INT2
        END IF
        OVR4=XPOL(INT3)
        SEGM(INT1)=OVI4
        OVR4=YPOL(INT3)
        SEGM(INT1+1)=OVI4
        INT1=INT1+2
  102 CONTINUE
C
C  RESET ERROR INDICATORS AND RETURN TO CALLER.
      UGELV=0
      UGENM='        '
      UGEIX=0
  201 RETURN
C
C  REPORT ERRORS TO THE UNIFIED GRAPHICS SYSTEM ERROR PROCESSOR.
  301 CALL UGRERR(2,'UGPFIL  ', 1)
      GO TO 201
  302 CALL UGRERR(2,'UGPFIL  ',11)
      IF (SEGM(1).NE.RTRY) GO TO 101
      GO TO 201
C
      END
