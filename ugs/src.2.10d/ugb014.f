      SUBROUTINE UGB014(VECT,FLAG)
C
C *******************  THE UNIFIED GRAPHICS SYSTEM  *******************
C *                        NORMALIZE A VECTOR                         *
C *                                                                   *
C *  THIS SUBROUTINE IS USED NORMALIZE A VECTOR.  THE VECTOR IS ALSO  *
C *  TESTED TO MAKE SURE THE INPUT VECTOR IS NOT TOO SMALL TO BEGIN   *
C *  WITH.                                                            *
C *                                                                   *
C *  THE CALLING SEQUENCE IS:                                         *
C *    CALL UGB014(VECT,FLAG)                                         *
C *                                                                   *
C *  THE PARAMETERS IN THE CALLING SEQUENCE ARE:                      *
C *    VECT  THE VECTOR TO BE NORMALIZED.                             *
C *    FLAG  AN ERROR FLAG (0 MEANS THE VECTOR COULD NOT BE           *
C *          NORMALIZED, 1 MEANS IT WAS NORMALIZED).                  *
C *                                                                   *
C *                          ROBERT C. BEACH                          *
C *                    COMPUTATION RESEARCH GROUP                     *
C *                STANFORD LINEAR ACCELERATOR CENTER                 *
C *                                                                   *
C *********************************************************************
C
      REAL          VECT(3)
      INTEGER       FLAG
C
      REAL          LENG
C
C  NORMALIZE THE VECTOR.
      FLAG=1
      LENG=SQRT(VECT(1)*VECT(1)+VECT(2)*VECT(2)+VECT(3)*VECT(3))
      IF (LENG.NE.0.0) THEN
        VECT(1)=VECT(1)/LENG
        VECT(2)=VECT(2)/LENG
        VECT(3)=VECT(3)/LENG
        IF (LENG.LT.0.001) FLAG=0
      ELSE
        FLAG=0
      END IF
C
C  RETURN TO CALLER.
      RETURN
C
      END
