      SUBROUTINE UGDDAT(OPTN,XCRD,YCRD,DATA,SEGM)
C
C *******************  THE UNIFIED GRAPHICS SYSTEM  *******************
C *          ADD DEVICE-DEPENDENT DATA TO A GRAPHIC SEGMENT           *
C *                                                                   *
C *  THIS SUBROUTINE MAY BE USED TO ADD DEVICE-DEPENDENT DATA TO A    *
C *  GRAPHIC SEGMENT.  THIS DATA WILL BE IGNORED ON MOST DEVICES.     *
C *  ON DEVICES WHERE IT IS RECOGNIZED AND PROCESSED, THE ACTION IS   *
C *  COMPLETELY DEVICE-DEPENDENT.                                     *
C *                                                                   *
C *  THE CALLING SEQUENCE IS:                                         *
C *    CALL UGDDAT(OPTN,XCRD,YCRD,DATA,SEGM)                          *
C *                                                                   *
C *  THE PARAMETERS IN THE CALLING SEQUENCE ARE:                      *
C *    OPTN  THE OPTIONS LIST.                                        *
C *    XCRD  THE X COORDINATE ASSOCIATED WITH THE DATA.               *
C *    YCRD  THE Y COORDINATE ASSOCIATED WITH THE DATA.               *
C *    DATA  THE DEVICE-DEPENDENT DATA.                               *
C *    SEGM  THE GRAPHIC SEGMENT WHICH WILL HAVE THE DEVICE-          *
C *          DEPENDENT DATA ADDED TO IT.                              *
C *                                                                   *
C *                          ROBERT C. BEACH                          *
C *                    COMPUTATION RESEARCH GROUP                     *
C *                STANFORD LINEAR ACCELERATOR CENTER                 *
C *                                                                   *
C *********************************************************************
C
      CHARACTER*(*) OPTN
      REAL          XCRD,YCRD
      CHARACTER*(*) DATA
      INTEGER*4     SEGM(*)
C
      INCLUDE       'UGSYSTEM:UGERRCBK.FOR'
C
      INCLUDE       'UGSYSTEM:UGPOTCBK.FOR'
C
      INCLUDE       'UGSYSTEM:UGPOTDCL.FOR'
C
C  LENGTH OF MODE BLOCK FOR DEVICE-DEPENDENT DATA.
      INTEGER       NMOD
      PARAMETER     (NMOD=6)
C  LENGTH OF SECONDARY MODE BLOCK FOR DEVICE-DEPENDENT DATA.
      INTEGER       NSMD
      PARAMETER     (NSMD=4)
C
      INTEGER*4     MODE(NMOD)
      INTEGER       RTRY,NDTA,NCWD
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
      CALL UGOPTION(OPTN,DESC_NUM2,POTST,IFLAG2,EXST,EXSTR)
C
C  CONSTRUCT THE MODE SPECIFICATION.
      NDTA=LEN(DATA)
      NCWD=(NDTA+3)/4
      IF (NDTA.LT.1) GO TO 201
      IF (NDTA.GT.1024) GO TO 301
      MODE(1)=6
      MODE(2)=NMOD+NSMD+NCWD
      MODE(3)=EXIL
      MODE(4)=EXCR
      MODE(5)=EXBL
      MODE(6)=EXPI
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
      SEGM(INT1+3)=NDTA
      INT1=INT1+NSMD
      INT2=1
      DO 102 INT3=1,NCWD
        OVI4=0
        INT4=MIN(4,NDTA-INT2+1)
        OVC4(1:INT4)=DATA(INT2:INT2+INT4-1)
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
  301 CALL UGRERR(2,'UGDDAT  ', 1)
      GO TO 202
  302 CALL UGRERR(2,'UGDDAT  ',11)
      IF (SEGM(1).NE.RTRY) GO TO 101
      GO TO 202
C
      END
