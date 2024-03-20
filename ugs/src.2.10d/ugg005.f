      SUBROUTINE    UGG005(LIMS,XCRD,YCRD,ZCRD,FLAG)
C
C *******************  THE UNIFIED GRAPHICS SYSTEM  *******************
C *             THREE-DIMENSIONAL LINE SCISSORING MODULE              *
C *                                                                   *
C *  THIS SUBROUTINE IS USED TO ENCODE THE RELATION OF THE GIVEN      *
C *  POINT WITH THE RECTANGULAR PARALLELOPIPED SCISSORING LIMITS.     *
C *                                                                   *
C *  THE CALLING SEQUENCE IS:                                         *
C *    CALL UGG005(LIMS,XCRD,YCRD,ZCRD,FLAG)                          *
C *                                                                   *
C *  THE PARAMETERS IN THE CALLING SEQUENCE ARE:                      *
C *    LIMS  AN ARRAY OF CONTAINING THE LIMITS.                       *
C *    XCRD  THE GIVEN X COORDINATE.                                  *
C *    YCRD  THE GIVEN Y COORDINATE.                                  *
C *    ZCRD  THE GIVEN Z COORDINATE.                                  *
C *    FLAG  THE ENCODED FLAG.                                        *
C *                                                                   *
C *                          ROBERT C. BEACH                          *
C *                    COMPUTATION RESEARCH GROUP                     *
C *                STANFORD LINEAR ACCELERATOR CENTER                 *
C *                                                                   *
C *********************************************************************
C
      REAL          LIMS(3,2)
      REAL          XCRD,YCRD,ZCRD
      INTEGER       FLAG
C
C  GENERATE THE FLAG VALUE.
      FLAG=0
      IF (XCRD.LT.LIMS(1,1)) THEN
        FLAG=1
      ELSE IF (XCRD.GT.LIMS(1,2)) THEN
        FLAG=2
      END IF
      IF (YCRD.LT.LIMS(2,1)) THEN
        FLAG=FLAG+4
      ELSE IF (YCRD.GT.LIMS(2,2)) THEN
        FLAG=FLAG+8
      END IF
      IF (ZCRD.LT.LIMS(3,1)) THEN
        FLAG=FLAG+16
      ELSE IF (ZCRD.GT.LIMS(3,2)) THEN
        FLAG=FLAG+32
      END IF
C
C  RETURN TO CALLING SUBROUTINE.
      RETURN
C
      END
