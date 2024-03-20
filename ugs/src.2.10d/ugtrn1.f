      SUBROUTINE UGTRN1(VEC1,VEC2,VEC3)
C
C *******************  THE UNIFIED GRAPHICS SYSTEM  *******************
C *               FORM THE CROSS PRODUCT OF TWO VECTORS               *
C *                                                                   *
C *  THIS SUBROUTINE MAY BE USED TO FORM THE CROSS PRODUCT OF TWO     *
C *  VECTORS.                                                         *
C *                                                                   *
C *  THE CALLING SEQUENCE IS:                                         *
C *    CALL UGTRN1(VEC1,VEC2,VEC3)                                    *
C *                                                                   *
C *  THE PARAMETERS IN THE CALLING SEQUENCE ARE:                      *
C *    VEC1  THE FIRST GIVEN VECTOR.                                  *
C *    VEC2  THE SECOND GIVEN VECTOR.                                 *
C *    VEC3  THE CROSS PRODUCT OF VEC1 AND VEC2.                      *
C *                                                                   *
C *                          ROBERT C. BEACH                          *
C *                    COMPUTATION RESEARCH GROUP                     *
C *                STANFORD LINEAR ACCELERATOR CENTER                 *
C *                                                                   *
C *********************************************************************
C
      REAL          VEC1(3),VEC2(3),VEC3(3)
C
C  FORM THE CROSS PRODUCT.
      VEC3(1)=VEC1(2)*VEC2(3)-VEC1(3)*VEC2(2)
      VEC3(2)=VEC1(3)*VEC2(1)-VEC1(1)*VEC2(3)
      VEC3(3)=VEC1(1)*VEC2(2)-VEC1(2)*VEC2(1)
C
C  RETURN TO CALLING PROGRAM.
      RETURN
C
      END
