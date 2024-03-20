      SUBROUTINE UGCNT1(LSUB,TSUB,ISID,IROW,ICOL,ARAY,MDIM,NDIM,
     X                  ZCNT,WKAR,BDFG,MCNT,EXTL,EFLG)
C
C *******************  THE UNIFIED GRAPHICS SYSTEM  *******************
C *                  PROCESS A SIDE OF A SURFACE PATCH                *
C *                                                                   *
C *  THIS SUBROUTINE IS USED BY UGCNTR TO PROCESS A SIDE OF A         *
C *  SURFACE PATCH.  IF THE SIDE HAS NOT BEEN CHECKED BEFORE, THEN    *
C *  THE CONTOUR IS EXAMINED TO SEE IF IT CROSSES THE SIDE.  IF IT    *
C *  DOES NOT, THE SIDE IS MARKED AS HAVING BEEN CHECKED.  IF THE     *
C *  CONTOUR CROSSES THE SIDE, THE CONTOUR IS FOLLOWED UNTIL IT IS    *
C *  COMPLETE AND ALL AFFECTED SIDES ARE MARKED AS HAVING BEEN        *
C *  CHECKED.                                                         *
C *                                                                   *
C *  THE CALLING SEQUENCE IS:                                         *
C *    CALL UGCNT1(LSUB,TSUB,ISID,IROW,ICOL,ARAY,MDIM,NDIM,           *
C *                ZCNT,WKAR,BDFG,MCNT,EXTL,EFLG)                     *
C *                                                                   *
C *  THE PARAMETERS IN THE CALLING SEQUENCE ARE:                      *
C *    LSUB  THE LINE SEGMENT END POINT SUBROUTINE.                   *
C *    TSUB  THE LABEL SUBROUTINE.                                    *
C *    ISID  INDEX OF THE SIDE BEING PROCESSED.                       *
C *    IROW  INDEX OF THE ROW BEING PROCESSED.                        *
C *    ICOL  INDEX OF THE COLUMN BEING PROCESSED.                     *
C *    ARRY  THE ARRAY DEFINING THE CONTOUR DATA.                     *
C *    MDIM  THE EXTENT OF THE CONTOUR DATA IN THE X DIRECTION.       *
C *    NDIM  THE EXTENT OF THE CONTOUR DATA IN THE Y DIRECTION.       *
C *    ZCNT  VALUE OF THE CONTOUR LINE.                               *
C *    WKAR  A WORK ARRAY.                                            *
C *    BDFG  A FLAG INDICATING IF A BOUNDARY IS BEING PROCESSED.      *
C *    MCNT  A FLAG INDICATING IF A PRIMARY CONTOUR IS BEING          *
C *          PROCESSED.                                               *
C *    EXTL  A TOLERANCE.                                             *
C *    EFLG  AN ERROR FLAG.                                           *
C *                                                                   *
C *                          ROBERT C. BEACH                          *
C *                    COMPUTATION RESEARCH GROUP                     *
C *                STANFORD LINEAR ACCELERATOR CENTER                 *
C *                                                                   *
C *********************************************************************
C
      EXTERNAL      LSUB,TSUB
      INTEGER       ISID,IROW,ICOL
      REAL          ARAY(MDIM,NDIM)
      INTEGER       MDIM,NDIM
      REAL          ZCNT
      INTEGER*4     WKAR(*)
      INTEGER       BDFG,MCNT
      REAL*4        EXTL
      INTEGER       EFLG
C
      INTEGER       JROW,JCOL,JSID
      REAL          XPNT,YPNT
      INTEGER       INFG,MKFG
C
C  INITIALIZE THE INTERNAL COUNTER.
      EFLG=0
      JROW=IROW
      JCOL=ICOL
      JSID=ISID
C  DO ANY CONTOURS BEGIN AT THIS SIDE?
      CALL UGCNT3(1,JSID,JROW,JCOL,NDIM,WKAR,MKFG)
      IF (MKFG.EQ.1) GO TO 201
      CALL UGCNT2(JSID,JROW,JCOL,ZCNT,XPNT,YPNT,ARAY,MDIM,NDIM,INFG)
      IF (INFG.NE.0) THEN
C  START DRAWING THE CONTOUR CURVE.
        IF ((BDFG.NE.0).AND.(MCNT.EQ.0)) CALL TSUB(XPNT,YPNT,ZCNT,JSID)
        CALL UGCNT4(LSUB,XPNT,YPNT,0,MCNT,EXTL)
C  FIND THE OTHER SIDE OF THE PATCH.
  101   JSID=MOD(JSID+2,4)
        CALL UGCNT3(1,JSID,JROW,JCOL,NDIM,WKAR,MKFG)
        IF (MKFG.NE.1) THEN
          CALL UGCNT2(JSID,JROW,JCOL,ZCNT,XPNT,YPNT,ARAY,MDIM,NDIM,INFG)
          IF (INFG.NE.0) GO TO 102
          CALL UGCNT3(0,JSID,JROW,JCOL,NDIM,WKAR,MKFG)
        END IF
        JSID=MOD(JSID+1,4)
        CALL UGCNT3(1,JSID,JROW,JCOL,NDIM,WKAR,MKFG)
        IF (MKFG.NE.1) THEN
          CALL UGCNT2(JSID,JROW,JCOL,ZCNT,XPNT,YPNT,ARAY,MDIM,NDIM,INFG)
          IF (INFG.NE.0) GO TO 102
          CALL UGCNT3(0,JSID,JROW,JCOL,NDIM,WKAR,MKFG)
        END IF
        JSID=MOD(JSID+2,4)
        CALL UGCNT3(1,JSID,JROW,JCOL,NDIM,WKAR,MKFG)
        IF (MKFG.EQ.1) GO TO 202
        CALL UGCNT2(JSID,JROW,JCOL,ZCNT,XPNT,YPNT,ARAY,MDIM,NDIM,INFG)
        IF (INFG.EQ.0) GO TO 202
C  DRAW CURRENT PART OF THE CONTOUR.
  102   CALL UGCNT4(LSUB,XPNT,YPNT,1,MCNT,EXTL)
C  MARK THE LINE PROCESSED.
        CALL UGCNT3(0,JSID,JROW,JCOL,NDIM,WKAR,MKFG)
C  FIND THE ADJACENT SURFACE PATCH.
        IF (JSID.EQ.3) THEN
          IF (JROW.GE.MDIM) GO TO 103
          JROW=JROW+1
          JSID=1
        ELSE IF (JSID.EQ.2) THEN
          IF (JCOL.GE.NDIM) GO TO 103
          JCOL=JCOL+1
          JSID=0
        ELSE IF (JSID.EQ.1) THEN
          IF (JROW.LE.3) GO TO 103
          JROW=JROW-1
          JSID=3
        ELSE
          IF (JCOL.LE.3) GO TO 103
          JCOL=JCOL-1
          JSID=2
        END IF
C  CHECK FOR CLOSURE OF THE CONTOUR LINE.
        IF ((JROW.EQ.IROW).AND.(JCOL.EQ.ICOL).AND.(JSID.EQ.ISID))
     X    GO TO 201
        GO TO 101
C  FINISH AN OPEN CURVE.
  103   IF ((BDFG.NE.0).AND.(MCNT.EQ.0)) CALL TSUB(XPNT,YPNT,ZCNT,JSID)
        JROW=IROW
        JCOL=ICOL
        JSID=ISID
      END IF
      CALL UGCNT3(0,JSID,JROW,JCOL,NDIM,WKAR,MKFG)
C
C  RETURN TO CALLING SUBROUTINE.
  201 RETURN
C  SET ERROR FLAG.
  202 EFLG=1
      GO TO 201
C
      END
