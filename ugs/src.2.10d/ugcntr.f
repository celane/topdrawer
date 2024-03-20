      SUBROUTINE UGCNTR(OPTN,LSUB,TSUB,ARAY,MDIM,NDIM,
     X                  LOCR,HICR,NCTR,WKAR,LDIM)
C
C *******************  THE UNIFIED GRAPHICS SYSTEM  *******************
C *                      CONTOUR PLOT SUBROUTINE                      *
C *                                                                   *
C *  THIS SUBROUTINE MAY BE USED TO GENERATE A DESCRIPTION OF A       *
C *  CONTOUR PLOT.  A PAIR OF USER SUPPLIED SUBROUTINES ARE CALLED    *
C *  TO PROCESS THE LINE SEGMENT END POINTS AND THE TEXT.  THE        *
C *  ALGORITHM USED HERE PRODUCES THE CONTOURS AS CONCATENATED LINE   *
C *  SEGMENTS.                                                        *
C *                                                                   *
C *  THE CALLING SEQUENCE IS:                                         *
C *    CALL UGCNTR(OPTN,LSUB,TSUB,ARAY,MDIM,NDIM,                     *
C *                LOCR,HICR,NCTR,WKAR,LDIM)                          *
C *                                                                   *
C *  THE PARAMETERS IN THE CALLING SEQUENCE ARE:                      *
C *    OPTN  THE OPTIONS LIST.                                        *
C *    LSUB  THE LINE SEGMENT END POINT SUBROUTINE.                   *
C *    TSUB  THE LABEL SUBROUTINE.                                    *
C *    ARAY  THE ARRAY DEFINING THE CONTOUR DATA.                     *
C *    MDIM  THE EXTENT OF THE CONTOUR DATA IN THE X DIRECTION.       *
C *    NDIM  THE EXTENT OF THE CONTOUR DATA IN THE Y DIRECTION.       *
C *    LOCR  THE SMALLEST PRIMARY CONTOUR VALUE.                      *
C *    HICR  THE HIGHEST PRIMARY CONTOUR VALUE.                       *
C *    NCTR  THE NUMBER OF CONTOUR VALUES.                            *
C *    WKAR  A WORK ARRAY.                                            *
C *    LDIM  THE NUMBER OF WORDS IN WKAR.                             *
C *                                                                   *
C *                          ROBERT C. BEACH                          *
C *                    COMPUTATION RESEARCH GROUP                     *
C *                STANFORD LINEAR ACCELERATOR CENTER                 *
C *                                                                   *
C *********************************************************************
C
      CHARACTER*(*) OPTN
      EXTERNAL      LSUB,TSUB
      REAL          ARAY(MDIM,NDIM)
      INTEGER       MDIM,NDIM
      REAL          LOCR,HICR
      INTEGER       NCTR
      INTEGER*4     WKAR(*)
      INTEGER       LDIM
C
      INCLUDE       'UGSYSTEM:UGERRCBK.FOR'
C
      INTEGER*4     EXST(2),EXSC
      REAL*4        EXTL
      EQUIVALENCE   (EXSC,EXST(1)),      (EXTL,EXST(2))
C
      INTEGER       KDIM
      INTEGER       NCNT,ICNT,MCNT,IROW,ICOL
      REAL          ZCNT
C
      INTEGER       INT1
      INTEGER       DESC_NUM
      PARAMETER    (DESC_NUM = 2)
      INTEGER*4     INST(4,DESC_NUM)
      CHARACTER*16  IFLAG(DESC_NUM)
C
      DATA     (INST(I,1),I=1,4) / 2,4,1,0 /, IFLAG(1) /'NSCL'/
      DATA     (INST(I,2),I=1,4) / 3,5,2,0 /, IFLAG(2) /'TOLER'/
C
C  SCAN THE OPTIONS LIST.
      EXSC=0
      EXTL=0.0001
      CALL UGOPTION(OPTN,DESC_NUM,INST,IFLAG,EXST,EXSTR)
C
C  INITIALIZE CONTOUR GENERATION.
      IF ((MDIM.LT.3).OR.(NDIM.LT.3)) GO TO 301
      KDIM=((MDIM-2)*(NDIM-1)+(NDIM-2)+15)/15
      IF (LDIM.LT.KDIM) GO TO 302
      INT1=MAX(NCTR,2)
      NCNT=INT1+(INT1-1)*EXSC
C
C  LOOP FOR EACH CONTOUR.
      DO 106 ICNT=1,NCNT
        ZCNT=LOCR+REAL(ICNT-1)*(HICR-LOCR)/REAL(NCNT-1)
        MCNT=MOD(ICNT-1,EXSC+1)
C  CLEAR SEGMENT BIT MAP.
        DO 101 INT1=1,KDIM
          WKAR(INT1)=0
  101   CONTINUE
C  PROCESS LOWER AND UPPER BOUNDARY.
        DO 102 ICOL=3,NDIM
          CALL UGCNT1(LSUB,TSUB,1,3,ICOL,ARAY,MDIM,NDIM,
     X                ZCNT,WKAR,1,MCNT,EXTL,INT1)
          IF (INT1.NE.0) GO TO 303
          CALL UGCNT1(LSUB,TSUB,3,MDIM,ICOL,ARAY,MDIM,NDIM,
     X                ZCNT,WKAR,1,MCNT,EXTL,INT1)
          IF (INT1.NE.0) GO TO 303
  102   CONTINUE
C  PROCESS LEFT AND RIGHT BOUNDARY.
        DO 103 IROW=3,MDIM
          CALL UGCNT1(LSUB,TSUB,0,IROW,3,ARAY,MDIM,NDIM,
     X                ZCNT,WKAR,1,MCNT,EXTL,INT1)
          IF (INT1.NE.0) GO TO 303
          CALL UGCNT1(LSUB,TSUB,2,IROW,NDIM,ARAY,MDIM,NDIM,
     X                ZCNT,WKAR,1,MCNT,EXTL,INT1)
          IF (INT1.NE.0) GO TO 303
  103   CONTINUE
C  PROCESS INTERIOR SIDES OF SURFACE PATCHES.
        IF (NDIM.GE.4) THEN
          DO 105 ICOL=4,NDIM
            DO 104 IROW=3,MDIM
              CALL UGCNT1(LSUB,TSUB,0,IROW,ICOL,ARAY,MDIM,NDIM,
     X                    ZCNT,WKAR,0,MCNT,EXTL,INT1)
              IF (INT1.NE.0) GO TO 303
  104       CONTINUE
  105     CONTINUE
        END IF
  106 CONTINUE
C
C  RESET ERROR INDICATORS AND RETURN TO CALLER.
      UGELV=0
      UGENM='        '
      UGEIX=0
  201 RETURN
C
C  REPORT ERRORS TO THE UNIFIED GRAPHICS SYSTEM ERROR PROCESSOR.
  301 CALL UGRERR(3,'UGCNTR  ',1)
      GO TO 201
  302 CALL UGRERR(3,'UGCNTR  ',2)
      GO TO 201
  303 CALL UGRERR(4,'UGCNTR  ',3)
      GO TO 201
C
      END
