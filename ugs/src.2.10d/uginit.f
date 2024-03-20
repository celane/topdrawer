
      SUBROUTINE UGINIT(OPTN,SEGM,NSEG)
C
C *******************  THE UNIFIED GRAPHICS SYSTEM  *******************
C *                   INITIALIZE A GRAPHIC SEGMENT                    *
C *                                                                   *
C *  THIS SUBROUTINE MAY BE USED TO CLEAR AND INITIALIZE A GRAPHIC    *
C *  SEGMENT.  AFTER THIS SUBROUTINE HAS BEEN USED TO INITIALIZE THE  *
C *  GRAPHIC SEGMENT, OTHER SUBROUTINES MAY BE CALLED TO ADD PICTURE  *
C *  DESCRIPTION DATA TO THE GRAPHIC SEGMENT.                         *
C *                                                                   *
C *  THE CALLING SEQUENCE IS:                                         *
C *    CALL UGINIT(OPTN,SEGM,NSEG)                                    *
C *                                                                   *
C *  THE PARAMETERS IN THE CALLING SEQUENCE ARE:                      *
C *    OPTN  THE OPTIONS LIST.                                        *
C *    SEGM  THE GRAPHIC SEGMENT WHICH IS TO BE INITIALIZED.          *
C *    NSEG  THE DIMENSION OF THE GRAPHIC SEGMENT.                    *
C *                                                                   *
C *                          ROBERT C. BEACH                          *
C *                    COMPUTATION RESEARCH GROUP                     *
C *                STANFORD LINEAR ACCELERATOR CENTER                 *
C *                                                                   *
C *********************************************************************
C
      CHARACTER*(*) OPTN
      INTEGER*4     SEGM(*)
      INTEGER       NSEG
C
      INCLUDE       'UGSYSTEM:UGERRCBK.FOR'
C
C  LENGTH OF MODE BLOCK FOR LINE DATA.
      INTEGER       NMOD
      PARAMETER     (NMOD=7)
C
      INTEGER*4     EXST(1),EXTY
      EQUIVALENCE   (EXTY,EXST(1))
C
      INTEGER       INT1,INT2
      INTEGER       DESC_NUM
      PARAMETER    (DESC_NUM = 3)
      INTEGER*4     INST(4,DESC_NUM)
      CHARACTER*16  IFLAG(DESC_NUM)
C
      DATA   (INST(I,1),I=1,4) / 1,5,1,1 /, IFLAG(1) / 'CLEAR'/
      DATA   (INST(I,2),I=1,4) / 1,5,1,2 /, IFLAG(2) / 'RESET'/
      DATA   (INST(I,3),I=1,4) / 1,8,1,3 /, IFLAG(3) / 'CONTINUE'/
C
C  SCAN THE OPTIONS LIST AND BRANCH TO PROPER SECTION.
      EXTY=1
      CALL UGOPTION(OPTN,DESC_NUM,INST,IFLAG,EXST,EXSTR)
      GO TO (101,201,301),EXTY
C
C  PROCESS 'CLEAR' OPTION.
  101 IF (NSEG.LT.16) GO TO 501
      SEGM(1)=3
      SEGM(2)=4
      SEGM(3)=0
      SEGM(4)=NSEG-1
      GO TO 401
C
C  PROCESS 'RESET' OPTION.
  201 SEGM(SEGM(1)+1)=NSEG-1
      GO TO 401
C
C  PROCESS 'CONTINUE' OPTION.
  301 INT1=SEGM(3)
      INT2=SEGM(INT1+1)
      IF (INT1.LE.0) GO TO 101
      IF (SEGM(INT1).EQ.2) THEN
        SEGM(1)=3+NMOD+2
        SEGM(2)=4
        SEGM(3)=4
        SEGM(4)=2
        SEGM(5)=NMOD+2
        SEGM(6)=SEGM(INT1+2)
        SEGM(7)=SEGM(INT1+3)
        SEGM(8)=SEGM(INT1+4)
        SEGM(9)=SEGM(INT1+5)
        SEGM(10)=SEGM(INT1+6)
        SEGM(11)=SEGM(INT1+INT2-2)
        SEGM(12)=IBCLR(SEGM(INT1+INT2-1),LSBFLO)
        SEGM(13)=NSEG-1
      ELSE IF (SEGM(INT1).EQ.8) THEN
        SEGM(1)=3+NMOD+3
        SEGM(2)=4
        SEGM(3)=4
        SEGM(4)=8
        SEGM(5)=NMOD+3
        SEGM(6)=SEGM(INT1+2)
        SEGM(7)=SEGM(INT1+3)
        SEGM(8)=SEGM(INT1+4)
        SEGM(9)=SEGM(INT1+5)
        SEGM(10)=SEGM(INT1+6)
        SEGM(11)=SEGM(INT1+INT2-3)
        SEGM(12)=IBCLR(SEGM(INT1+INT2-2),LSBFLO)
        SEGM(13)=SEGM(INT1+INT2-1)
        SEGM(14)=NSEG-1
      ELSE
        GO TO 101
      END IF
C
C  RESET ERROR INDICATORS AND RETURN TO CALLER.
  401 UGELV=0
      UGENM='        '
      UGEIX=0
  402 RETURN
C
C  REPORT ERRORS TO THE UNIFIED GRAPHICS SYSTEM ERROR PROCESSOR.
  501 CALL UGRERR(3,'UGINIT  ', 1)
      GO TO 402
C
      END
