      SUBROUTINE UGTEXT(OPTN,XCRD,YCRD,TEXT,SEGM)
C
C *******************  THE UNIFIED GRAPHICS SYSTEM  *******************
C *                ADD CHARACTERS TO A GRAPHIC SEGMENT                *
C *                                                                   *
C *  THIS SUBROUTINE MAY BE USED TO ADD TEXT MATERIAL TO A GRAPHIC    *
C *  SEGMENT.  THE TEXT WILL NORMALLY OCCUPY A SINGLE LINE.  THE      *
C *  ACTUAL CHARACTERS WILL BE PRODUCED EITHER BY A HARDWARE          *
C *  CHARACTER GENERATOR ON THE DEVICE, OR BY A PROGRAMMED STROKE     *
C *  GENERATOR WHICH WILL SIMULATE A HARDWARE CHARACTER GENERATOR.    *
C *                                                                   *
C *  THE CALLING SEQUENCE IS:                                         *
C *    CALL UGTEXT(OPTN,XCRD,YCRD,TEXT,SEGM)                          *
C *                                                                   *
C *  THE PARAMETERS IN THE CALLING SEQUENCE ARE:                      *
C *    OPTN  THE OPTIONS LIST.                                        *
C *    XCRD  THE X COORDINATE OF THE FIRST CHARACTER.                 *
C *    YCRD  THE Y COORDINATE OF THE FIRST CHARACTER.                 *
C *    TEXT  THE CHARACTERS TO BE DISPLAYED.                          *
C *    SEGM  THE GRAPHIC SEGMENT WHICH WILL HAVE THE CHARACTERS       *
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
      CHARACTER*(*) TEXT
      INTEGER*4     SEGM(*)
C
      INCLUDE       'UGSYSTEM:UGERRCBK.FOR'
C
      INCLUDE       'UGSYSTEM:UGPOTCBK.FOR'
C
      INCLUDE       'UGSYSTEM:UGPOTDCL.FOR'
C
C  LENGTH OF MODE BLOCK FOR TEXT DATA.
      INTEGER       NMOD
      PARAMETER     (NMOD=10)
C  LENGTH OF SECONDARY MODE BLOCK FOR TEXT DATA.
      INTEGER       NSMD
      PARAMETER     (NSMD=4)
C
      INTEGER*4     MODE(NMOD)
      INTEGER       RTRY,NTXT,NCWD
C
      INTEGER*4     OVI4
      REAL*4        OVR4
      CHARACTER*4   OVC4
      EQUIVALENCE   (OVI4,OVR4,OVC4)
      INTEGER       INT1,INT2,INT3,INT4
C
C  SCAN THE OPTIONS LIST.
      EXIL=POTIL
      EXCR=POTCR
      EXBL=POTBL
      EXPI=POTPI
      EXSZ=-POTSZ
      EXDZ=-POTDZ
      EXAG=POTAG
      EXJF=POTJF
      EXCP=POTCP
      CALL UGOPTION(OPTN,DESC_NUM2,POTST,IFLAG2,EXST,EXSTR)
C
C  CONSTRUCT THE MODE SPECIFICATION.
      NTXT=LEN(TEXT)
      NCWD=(NTXT+3)/4
      IF (NTXT.LT.1) GO TO 201
      IF (NTXT.GT.1024) GO TO 301
      MODE(1)=3
      MODE(2)=NMOD+NSMD+NCWD
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
        OVR4=-PODDZ
      END IF
      MODE(7)=OVI4
      OVR4=EXAG
      MODE(8)=OVI4
      MODE(9)=EXJF
      MODE(10)=EXCP
C
C  INSERT THE MODE SPECIFICATION AND THE DATA INTO THE SEGMENT.
  101 RTRY=SEGM(1)
      CALL UGB002(MODE,NMOD,NSMD+NCWD,SEGM,INT1)
      IF (INT1.EQ.0) GO TO 302
      SEGM(INT1)=NSMD+NCWD
      OVR4=XCRD
      SEGM(INT1+1)=OVI4
      OVR4=YCRD
      SEGM(INT1+2)=OVI4
      SEGM(INT1+3)=NTXT
      INT1=INT1+NSMD
      INT2=1
      DO 102 INT3=1,NCWD
        OVC4='    '
        INT4=MIN(4,NTXT-INT2+1)
        OVC4(1:INT4)=TEXT(INT2:INT2+INT4-1)
        INT2=INT2+4
        SEGM(INT1)=OVI4
        INT1=INT1+1
  102 CONTINUE
C
C  RESET ERROR INDICATORS AND RETURN TO CALLER.
  201 UGELV=0
      UGENM='        '
      UGEIX=0
  202 RETURN
C
C  REPORT ERRORS TO THE UNIFIED GRAPHICS SYSTEM ERROR PROCESSOR.
  301 CALL UGRERR(2,'UGTEXT  ', 1)
      GO TO 202
  302 CALL UGRERR(2,'UGTEXT  ',11)
      IF (SEGM(1).NE.RTRY) GO TO 101
      GO TO 202
C
      END
