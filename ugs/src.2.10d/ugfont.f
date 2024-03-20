      SUBROUTINE UGFONT(OPTN)
C
C *******************  THE UNIFIED GRAPHICS SYSTEM  *******************
C *              SELECT THE EXTENDED CHARACTER SET FONT               *
C *                                                                   *
C *  THIS SUBROUTINE MAY BE USED TO SELECT THE FONT FOR THE EXTENDED  *
C *  CHARACTER SET.                                                   *
C *                                                                   *
C *  THE CALLING SEQUENCE IS:                                         *
C *    CALL UGFONT(OPTN)                                              *
C *                                                                   *
C *  THE PARAMETER IN THE CALLING SEQUENCE IS:                        *
C *    OPTN  THE OPTIONS LIST.                                        *
C *                                                                   *
C *                          ROBERT C. BEACH                          *
C *                    COMPUTATION RESEARCH GROUP                     *
C *                STANFORD LINEAR ACCELERATOR CENTER                 *
C *                                                                   *
C *********************************************************************
C
      CHARACTER*(*) OPTN
C
      INCLUDE       'UGSYSTEM:UGMCACBK.FOR'
      INCLUDE       'UGSYSTEM:UGERRCBK.FOR'

      INTEGER*4     EXST(1),EXCG
      EQUIVALENCE   (EXCG,EXST(1))

      CHARACTER*8   NAMS(2)
      INTEGER       DESC_NUM
      PARAMETER    (DESC_NUM = 2)
      INTEGER*4     INST(4,DESC_NUM)
      CHARACTER*16  IFLAG(DESC_NUM)

      DATA    (INST(I,1), I=1,4) / 1,7,1,1 /, IFLAG(1) / 'SIMPLEX'/
      DATA    (INST(I,2), I=1,4) / 1,6,1,2 /, IFLAG(2) / 'DUPLEX '/

      DATA          NAMS/'SIMPLEX ',
     +                   'DUPLEX  '/

C  SCAN THE OPTIONS LIST.
      EXCG=0
      CALL UGOPTION(OPTN,DESC_NUM,INST,IFLAG,EXST,EXSTR)
C
C  HAS A NEW FONT BEEN REQUESTED? SAVE THE NAME OF THE NEW FONT.
      IF (EXCG.NE.0) MCACN = NAMS(EXCG)
C
C  RESET ERROR INDICATORS AND RETURN TO CALLER.
  101 UGELV=0
      UGENM='        '
      UGEIX=0
      RETURN
C
      END
