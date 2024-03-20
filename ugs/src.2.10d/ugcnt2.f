      SUBROUTINE UGCNT2(ISID,IROW,ICOL,ZCNT,XCRD,YCRD,
     X                  ARAY,MDIM,NDIM,OFLG)
C
C *******************  THE UNIFIED GRAPHICS SYSTEM  *******************
C *                DETERMINE CONTOUR-SIDE INTERSECTION                *
C *                                                                   *
C *  THIS SUBROUTINE IS USED BY UGCNTR TO DETERMINE IF A SIDE IS      *
C *  INTERSECTED BY A CONTOUR LINE.                                   *
C *                                                                   *
C *  THE CALLING SEQUENCE IS:                                         *
C *    CALL UGCNT2(ISID,IROW,ICOL,ZCNT,XCRD,YCRD,                     *
C *                ARAY,MDIM,NDIM,OFLG)                               *
C *                                                                   *
C *  THE PARAMETERS IN THE CALLING SEQUENCE ARE:                      *
C *    ISID  INDEX OF THE SIDE BEING PROCESSED.                       *
C *    IROW  INDEX OF THE ROW BEING PROCESSED.                        *
C *    ICOL  INDEX OF THE COLUMN BEING PROCESSED.                     *
C *    ZCNT  VALUE OF THE CONTOUR LINE.                               *
C *    XCRD  X COORDINATE OF THE INTERSECTION.                        *
C *    YCRD  Y COORDINATE OF THE INTERSECTION.                        *
C *    ARAY  THE ARRAY DEFINING THE CONTOUR DATA.                     *
C *    MDIM  THE EXTENT OF THE CONTOUR DATA IN THE X DIRECTION.       *
C *    NDIM  THE EXTENT OF THE CONTOUR DATA IN THE Y DIRECTION.       *
C *    OFLG  AN OUTPUT FLAG INDICATING IF AN INTERSECTION WAS FOUND.  *
C *                                                                   *
C *                          ROBERT C. BEACH                          *
C *                    COMPUTATION RESEARCH GROUP                     *
C *                STANFORD LINEAR ACCELERATOR CENTER                 *
C *                                                                   *
C *********************************************************************
C
      INTEGER       ISID,IROW,ICOL
      REAL          ZCNT
      REAL          XCRD,YCRD
      REAL          ARAY(MDIM,NDIM)
      INTEGER       MDIM,NDIM
      INTEGER       OFLG
C
      REAL          IVR1,IVR2,DVR1,DVR2
      LOGICAL       VFLG
C
      REAL          FLT1
C
C  OBTAIN THE INDEPENDENT AND DEPENDENT VARIABLES.
      IF (ISID.EQ.0) THEN
        DVR1=ARAY(IROW-1,ICOL-1)
        DVR2=ARAY(IROW,ICOL-1)
        XCRD=ARAY(1,ICOL-1)
        VFLG=.TRUE.
      ELSE IF (ISID.EQ.1) THEN
        DVR1=ARAY(IROW-1,ICOL-1)
        DVR2=ARAY(IROW-1,ICOL)
        YCRD=ARAY(IROW-1,1)
        VFLG=.FALSE.
      ELSE IF (ISID.EQ.2) THEN
        DVR1=ARAY(IROW-1,ICOL)
        DVR2=ARAY(IROW,ICOL)
        XCRD=ARAY(1,ICOL)
        VFLG=.TRUE.
      ELSE
        DVR1=ARAY(IROW,ICOL-1)
        DVR2=ARAY(IROW,ICOL)
        YCRD=ARAY(IROW,1)
        VFLG=.FALSE.
      END IF
      IF (VFLG) THEN
        IVR1=ARAY(IROW-1,1)
        IVR2=ARAY(IROW,1)
      ELSE
        IVR1=ARAY(1,ICOL-1)
        IVR2=ARAY(1,ICOL)
      END IF
C
C  CHECK FOR AN INTERSECTION.
      IF (((DVR1.LT.ZCNT).AND.(DVR2.LT.ZCNT)).OR.
     X  ((DVR1.GE.ZCNT).AND.(DVR2.GE.ZCNT))) GO TO 101
C  COMPUTE THE OTHER COORDINATE.
      FLT1=IVR1+(ZCNT-DVR1)*(IVR2-IVR1)/(DVR2-DVR1)
      IF (VFLG) THEN
        YCRD=FLT1
      ELSE
        XCRD=FLT1
      END IF
C
C  RETURN WITH INTERSECTION.
      OFLG=1
      GO TO 102
C  RETURN WITHOUT INTERSECTION.
  101 OFLG=0
  102 RETURN
C
      END
