      SUBROUTINE    UGG004(BBIT,XCRD,YCRD,ZCRD)
C
C *******************  THE UNIFIED GRAPHICS SYSTEM  *******************
C *             THREE-DIMENSIONAL LINE SCISSORING MODULE              *
C *                                                                   *
C *  THIS SUBROUTINE MAY BE USED TO RETRIEVE A POINT FROM THE         *
C *  THREE-DIMENSIONAL LINE SCISSORING MODULE.                        *
C *                                                                   *
C *  THE CALLING SEQUENCE IS:                                         *
C *    CALL UGG004(BBIT,XCRD,YCRD,ZCRD)                               *
C *                                                                   *
C *  THE PARAMETERS IN THE CALLING SEQUENCE ARE:                      *
C *    BBIT  THE BLANKING BIT OR TERMINATION FLAG: 0 MEANS MOVE       *
C *          WITHOUT DRAWING, 1 MEANS DRAW, AND -1 MEANS NO MORE      *
C *          DATA IS AVAILABLE.                                       *
C *    XCRD  THE X COORDINATE OF A LINE END POINT.                    *
C *    YCRD  THE Y COORDINATE OF A LINE END POINT.                    *
C *    ZCRD  THE Z COORDINATE OF A LINE END POINT.                    *
C *                                                                   *
C *                          ROBERT C. BEACH                          *
C *                    COMPUTATION RESEARCH GROUP                     *
C *                STANFORD LINEAR ACCELERATOR CENTER                 *
C *                                                                   *
C *********************************************************************
C
      INTEGER       BBIT
      REAL          XCRD,YCRD,ZCRD
C
      INCLUDE       'UGSYSTEM:UGG00CBK.FOR'
C
C  PASS AN END POINT BACK TO THE CALLING MODULE.
  101 IF (KAVL.GE.NAVL) THEN
        BBIT=-1
      ELSE
        BBIT=MOD(KAVL,2)
        KAVL=KAVL+1
        XCRD=XSEG(KAVL)
        YCRD=YSEG(KAVL)
        ZCRD=ZSEG(KAVL)
        IF (IFLG.NE.0) THEN
          IF (BBIT.EQ.0) THEN
            IF (XCRD.EQ.PXCD) THEN
              IF (YCRD.EQ.PYCD) THEN
                IF (ZCRD.EQ.PZCD) GO TO 101
              END IF
            END IF
          END IF
        END IF
        IFLG=1
        PXCD=XCRD
        PYCD=YCRD
        PZCD=ZCRD
      END IF
C
C  RETURN TO CALLING SUBROUTINE.
      RETURN
C
      END
