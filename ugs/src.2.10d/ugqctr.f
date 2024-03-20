      SUBROUTINE UGQCTR(OPTN,LSUB,TSUB,ARAY,MDIM,NDIM,LOCR,HICR,NCTR)
C
C *******************  THE UNIFIED GRAPHICS SYSTEM  *******************
C *                   QUICK CONTOUR PLOT SUBROUTINE                   *
C *                                                                   *
C *  THIS SUBROUTINE MAY BE USED TO GENERATE A DESCRIPTION OF A       *
C *  CONTOUR PLOT.  A PAIR OF USER SUPPLIED SUBROUTINES ARE CALLED    *
C *  TO PROCESS THE LINE SEGMENT END POINTS AND THE TEXT.  THE        *
C *  ALGORITHM USED HERE IS QUICK, SIMPLE, AND DOES NOT REQUIRE ANY   *
C *  WORK SPACE.  HOWEVER, THE CONTOURS ARE NOT PRODUCED AS           *
C *  CONCATENATED LINE SEGMENTS.                                      *
C *                                                                   *
C *  THE CALLING SEQUENCE IS:                                         *
C *    CALL UGQCTR(OPTN,LSUB,TSUB,ARAY,MDIM,NDIM,LOCR,HICR,NCTR)      *
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
C
      INCLUDE       'UGSYSTEM:UGERRCBK.FOR'
C
      INTEGER*4     EXST(1),EXSC
      EQUIVALENCE   (EXSC,EXST(1))
C
      INTEGER       NCNT,ICNT,MCNT,IROW,ICOL
      REAL          ZCNT
      REAL          XLOV,XHIV,YLOV,YHIV
      REAL          Z00V,Z01V,Z10V,Z11V
      REAL          PNTS(4,2)
      INTEGER       NPTS

      INTEGER       INT1
      INTEGER       DESC_NUM
      PARAMETER    (DESC_NUM = 1)
      INTEGER*4     INST(4,DESC_NUM)
      CHARACTER*4   IFLAG(DESC_NUM)

      DATA   (INST(I,1),I=1,4) / 2,4,1,0 /, IFLAG(1) / 'NSCL' /
C
C  SCAN THE OPTIONS LIST.
      EXSC=0
      CALL UGOPTION(OPTN,DESC_NUM,INST,IFLAG,EXST,EXSTR)
C
C  INITIALIZE CONTOUR GENERATION.
      IF ((MDIM.LT.3).OR.(NDIM.LT.3)) GO TO 301
      INT1=MAX(NCTR,2)
      NCNT=INT1+(INT1-1)*EXSC
C
C  LOOP FOR EACH ROW.
      DO 104 IROW=3,MDIM
        YLOV=ARAY(IROW-1,1)
        YHIV=ARAY(IROW  ,1)
C  LOOP FOR EACH COLUMN.
        DO 103 ICOL=3,NDIM
          XLOV=ARAY(1,ICOL-1)
          XHIV=ARAY(1,ICOL  )
          Z00V=ARAY(IROW-1,ICOL-1)
          Z01V=ARAY(IROW  ,ICOL-1)
          Z10V=ARAY(IROW-1,ICOL  )
          Z11V=ARAY(IROW  ,ICOL  )
C  LOOP FOR EACH CONTOUR.
          DO 102 ICNT=1,NCNT
            ZCNT=LOCR+REAL(ICNT-1)*(HICR-LOCR)/REAL(NCNT-1)
            IF (MOD(ICNT-1,EXSC+1).EQ.0) THEN
              MCNT=0
            ELSE
              MCNT=2
            END IF
C  COMPUTE THE CROSSING POINTS FOR THE CURRENT RECTANGLE.
            NPTS=0
            CALL UGQCT1(XLOV,Z00V,XHIV,Z10V,ZCNT,PNTS(NPTS+1,1),INT1)
            IF (INT1.NE.0) THEN
              NPTS=NPTS+1
              PNTS(NPTS,2)=YLOV
              IF ((MCNT.EQ.0).AND.(IROW.EQ.3)) THEN
                CALL TSUB(PNTS(NPTS,1),PNTS(NPTS,2),ZCNT,1)
              END IF
            END IF
            CALL UGQCT1(XLOV,Z01V,XHIV,Z11V,ZCNT,PNTS(NPTS+1,1),INT1)
            IF (INT1.NE.0) THEN
              NPTS=NPTS+1
              PNTS(NPTS,2)=YHIV
              IF ((MCNT.EQ.0).AND.(IROW.EQ.MDIM)) THEN
                CALL TSUB(PNTS(NPTS,1),PNTS(NPTS,2),ZCNT,3)
              END IF
            END IF
            CALL UGQCT1(YLOV,Z00V,YHIV,Z01V,ZCNT,PNTS(NPTS+1,2),INT1)
            IF (INT1.NE.0) THEN
              NPTS=NPTS+1
              PNTS(NPTS,1)=XLOV
              IF ((MCNT.EQ.0).AND.(ICOL.EQ.3)) THEN
                CALL TSUB(PNTS(NPTS,1),PNTS(NPTS,2),ZCNT,0)
              END IF
            END IF
            CALL UGQCT1(YLOV,Z10V,YHIV,Z11V,ZCNT,PNTS(NPTS+1,2),INT1)
            IF (INT1.NE.0) THEN
              NPTS=NPTS+1
              PNTS(NPTS,1)=XHIV
              IF ((MCNT.EQ.0).AND.(ICOL.EQ.NDIM)) THEN
                CALL TSUB(PNTS(NPTS,1),PNTS(NPTS,2),ZCNT,2)
              END IF
            END IF
C  OUTPUT ANY ACCUMULATED LINE SEGMENTS.
            DO 101 INT1=1,NPTS
              CALL LSUB(PNTS(INT1,1),PNTS(INT1,2),MOD(INT1-1,2)+MCNT)
  101       CONTINUE
  102     CONTINUE
  103   CONTINUE
  104 CONTINUE
C
C  RESET ERROR INDICATORS AND RETURN TO CALLER.
      UGELV=0
      UGENM='        '
      UGEIX=0
  201 RETURN
C
C  REPORT ERRORS TO THE UNIFIED GRAPHICS SYSTEM ERROR PROCESSOR.
  301 CALL UGRERR(3,'UGQCTR  ',1)
      GO TO 201
C
      END
