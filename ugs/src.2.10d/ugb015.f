      SUBROUTINE UGB015(VCT1,VCT2,VCT3)
C
C *******************  THE UNIFIED GRAPHICS SYSTEM  *******************
C *             COMPUTE THE CROSS PRODUCT OF TWO VECTORS              *
C *                                                                   *
C *  THIS SUBROUTINE IS USED TO COMPUTE THE CROSS PRODUCT OF TWO      *
C *  VECTORS.                                                         *
C *                                                                   *
C *  THE CALLING SEQUENCE IS:                                         *
C *    CALL UGB015(VCT1,VCT2,VCT3)                                    *
C *                                                                   *
C *  THE PARAMETERS IN THE CALLING SEQUENCE ARE:                      *
C *    VCT1  THE FIRST INPUT VECTOR.                                  *
C *    VCT2  THE SECOND INPUT VECTOR.                                 *
C *    VCT3  THE COMPUTED CROSS PRODUCT.                              *
C *                                                                   *
C *                          ROBERT C. BEACH                          *
C *                    COMPUTATION RESEARCH GROUP                     *
C *                STANFORD LINEAR ACCELERATOR CENTER                 *
C *                                                                   *
C *********************************************************************
C
      REAL          VCT1(3),VCT2(3),VCT3(3)
C
C  COMPUTE THE CROSS PRODUCT VECTOR.
      VCT3(1)=VCT1(2)*VCT2(3)-VCT1(3)*VCT2(2)
      VCT3(2)=VCT1(3)*VCT2(1)-VCT1(1)*VCT2(3)
      VCT3(3)=VCT1(1)*VCT2(2)-VCT1(2)*VCT2(1)
C
C  RETURN TO CALLER.
      RETURN
C
      END
