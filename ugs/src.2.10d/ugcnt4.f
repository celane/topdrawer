      SUBROUTINE UGCNT4(LSUB,XCRD,YCRD,BBIT,CFLG,TOLR)
C
C *******************  THE UNIFIED GRAPHICS SYSTEM  *******************
C *                 DRAW A LINE TO THE CURRENT POINT                  *
C *                                                                   *
C *  THIS SUBROUTINE IS USED BY UGCNTR TO DRAW A LINE TO THE CURRENT  *
C *  POSITION.                                                        *
C *                                                                   *
C *  THE CALLING SEQUENCE IS:                                         *
C *    CALL UGCNT4(LSUB,XCRD,YCRD,BBIT,CFLG,TOLR)                     *
C *                                                                   *
C *  THE PARAMETERS IN THE CALLING SEQUENCE ARE:                      *
C *    LSUB  THE LINE SEGMENT END POINT SUBROUTINE.                   *
C *    XCRD  X COORDINATE OF THE CURRENT POINT.                       *
C *    YCRD  Y COORDINATE OF THE CURRENT POINT.                       *
C *    BBIT  THE BLANKING BIT.                                        *
C *    CFLG  A FLAG INDICATING A PRIMARY OR SECONDARY CONTOUR.        *
C *    TOLR  A TOLERANCE TO DETECT CONCURRENT POINTS.                 *
C *                                                                   *
C *                          ROBERT C. BEACH                          *
C *                    COMPUTATION RESEARCH GROUP                     *
C *                STANFORD LINEAR ACCELERATOR CENTER                 *
C *                                                                   *
C *********************************************************************
C
      EXTERNAL      LSUB
      REAL          XCRD,YCRD
      INTEGER       BBIT,CFLG
      REAL          TOLR
C
      SAVE          XSAV,YSAV
      REAL          XSAV,YSAV
C
C  DRAW TO THE GIVEN POINT.
      IF (BBIT.NE.0) THEN
        IF ((ABS(XCRD-XSAV)+ABS(YCRD-YSAV)).LT.TOLR) GO TO 101
      END IF
      IF (CFLG.EQ.0) THEN
        CALL LSUB(XCRD,YCRD,BBIT)
      ELSE
        CALL LSUB(XCRD,YCRD,BBIT+2)
      END IF
      XSAV=XCRD
      YSAV=YCRD
C
C  RETURN TO CALLING SUBROUTINE.
  101 RETURN
C
      END
