      SUBROUTINE UGPROJ(TRAN,PT3D,PT2D)
C
C *********************************************************************
C *         PROJECT A POINT INTO THE SCREEN COORDINATE SYSTEM         *
C *                                                                   *
C *  THIS SUBROUTINE USES THE TRANSFORMATION DEFINED BY SUBROUTINES   *
C *  UGTRAN OR UGORTH TO PROJECT A THREE DIMENSIONAL POINT INTO TWO   *
C *  DIMENSIONS.                                                      *
C *                                                                   *
C *  THE CALLING SEQUENCE IS:                                         *
C *    CALL UGPROJ(TRAN,PT3D,PT2D)                                    *
C *                                                                   *
C *  THE PARAMETERS IN THE CALLING SEQUENCE ARE:                      *
C *    TRAN  THE GIVEN TRANSFORMATION.                                *
C *    PT3D  THE GIVEN POINT IN 3-SPACE.                              *
C *    PT2D  THE COMPUTED POINT IN 2-SPACE.                           *
C *                                                                   *
C *                          ROBERT C. BEACH                          *
C *                    COMPUTATION RESEARCH GROUP                     *
C *                STANFORD LINEAR ACCELERATOR CENTER                 *
C *                                                                   *
C *********************************************************************
C
      REAL          TRAN(31)
      REAL          PT3D(3),PT2D(2)
C
      REAL          VECT(3)
      REAL          TEMP
      INTEGER       INT1,INT2,INT3
C
      DO 102 INT1=1,3
        INT2=4*INT1
        TEMP=TRAN(INT2)
        DO 101 INT3=1,3
          TEMP=TEMP+PT3D(INT3)*TRAN(INT2+INT3-4)
  101   CONTINUE
        VECT(INT1)=TEMP
  102 CONTINUE
      PT2D(1)=VECT(1)/VECT(3)
      PT2D(2)=VECT(2)/VECT(3)
      RETURN
C
      END
