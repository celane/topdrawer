      SUBROUTINE UGTRN2(MAT1,MAT2,TOLR,FLAG)
C
C *******************  THE UNIFIED GRAPHICS SYSTEM  *******************
C *                      INVERT A 3 BY 3 MATRIX                       *
C *                                                                   *
C *  THIS SUBROUTINE MAY BE USED TO FORM THE INVERSE OF A 3 BY 3      *
C *  MATRIX.  THE MATRIX MUST BE STORED BY COLUMNS.                   *
C *                                                                   *
C *  THE CALLING SEQUENCE IS:                                         *
C *    CALL UGTRN2(MAT1,MAT2,TOLR,FLAG)                               *
C *                                                                   *
C *  THE PARAMETERS IN THE CALLING SEQUENCE ARE:                      *
C *    MAT1  THE GIVEN MATRIX.                                        *
C *    MAT2  THE INVERSE OF MAT1.                                     *
C *    TOLR  A TOLERANCE TO BE USED IN THE CALCULATION.               *
C *    FLAG  A ZERO IF THE MATRIX WAS SINGULAR OR A ONE OTHERWISE.    *
C *                                                                   *
C *                          ROBERT C. BEACH                          *
C *                    COMPUTATION RESEARCH GROUP                     *
C *                STANFORD LINEAR ACCELERATOR CENTER                 *
C *                                                                   *
C *********************************************************************
C
      REAL          MAT1(9),MAT2(9)
      REAL          TOLR
      INTEGER       FLAG
C
      REAL          DETR
C
C  PRODUCE THE MATRIX INVERSE.
      DETR=MAT1(1)*MAT1(5)*MAT1(9)+
     X     MAT1(3)*MAT1(4)*MAT1(8)+
     X     MAT1(2)*MAT1(6)*MAT1(7)-
     X     MAT1(3)*MAT1(5)*MAT1(7)-
     X     MAT1(1)*MAT1(6)*MAT1(8)-
     X     MAT1(2)*MAT1(4)*MAT1(9)
      IF (ABS(DETR).LT.TOLR) GO TO 102
      MAT2(1)=(MAT1(5)*MAT1(9)-MAT1(6)*MAT1(8))/DETR
      MAT2(2)=(MAT1(3)*MAT1(8)-MAT1(2)*MAT1(9))/DETR
      MAT2(3)=(MAT1(2)*MAT1(6)-MAT1(3)*MAT1(5))/DETR
      MAT2(4)=(MAT1(6)*MAT1(7)-MAT1(4)*MAT1(9))/DETR
      MAT2(5)=(MAT1(1)*MAT1(9)-MAT1(3)*MAT1(7))/DETR
      MAT2(6)=(MAT1(3)*MAT1(4)-MAT1(1)*MAT1(6))/DETR
      MAT2(7)=(MAT1(4)*MAT1(8)-MAT1(5)*MAT1(7))/DETR
      MAT2(8)=(MAT1(2)*MAT1(7)-MAT1(1)*MAT1(8))/DETR
      MAT2(9)=(MAT1(1)*MAT1(5)-MAT1(2)*MAT1(4))/DETR
C
C  RETURN TO CALLING SUBROUTINE.
      FLAG=1
  101 RETURN
  102 FLAG=0
      GO TO 101
C
      END
