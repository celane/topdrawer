      SUBROUTINE UG2DH6(PTA1,PTA2,PTB1,PTB2,PNTO,TOLR,PFLG)
C
C *******************  THE UNIFIED GRAPHICS SYSTEM  *******************
C *                   INTERSECT TWO STRAIGHT LINES                    *
C *                                                                   *
C *  THIS SUBROUTINE IS USED BY UG2DH3 TO INTERSECT TWO STRAIGHT      *
C *  LINES GIVEN BY THEIR END POINTS.  THE EXTENSIVE CHECKING DONE    *
C *  HERE IS NECESSARY IN SOME CASES AND DEPENDS ON THE WAY THIS      *
C *  SUBROUTINE IS CALLED.                                            *
C *                                                                   *
C *  THE CALLING SEQUENCE IS:                                         *
C *    CALL UG2DH6(PTA1,PTA2,PTB1,PTB2,PNTO,TOLR,PFLG)                *
C *                                                                   *
C *  THE PARAMETERS IN THE CALLING SEQUENCE ARE:                      *
C *    PTA1  THE FIRST END POINT OF THE FIRST LINE.                   *
C *    PTA2  THE SECOND END POINT OF THE FIRST LINE.                  *
C *    PTB1  THE FIRST END POINT OF THE SECOND LINE.                  *
C *    PTB2  THE SECOND END POINT OF THE SECOND LINE.                 *
C *    PNTO  THE COMPUTED INTERSECTION POINT.                         *
C *    TOLR  A TOLERANCE.                                             *
C *    PFLG  A FLAG INDICATING IF AN INTERSECTION POINT IS            *
C *          AVAILABLE.                                               *
C *                                                                   *
C *                          ROBERT C. BEACH                          *
C *                    COMPUTATION RESEARCH GROUP                     *
C *                STANFORD LINEAR ACCELERATOR CENTER                 *
C *                                                                   *
C *********************************************************************
C
      REAL          PTA1(2),PTA2(2),PTB1(2),PTB2(2),PNTO(2),TOLR
      LOGICAL       PFLG
C
      REAL          EQUA(3),EQUB(3),DETR
      REAL          TLSQ,POLA,POLB
C
C  SET UP THE EQUATIONS OF THE TWO LINES AND SOLVE THEM SIMULTANEOUSLY.
      TLSQ=TOLR*TOLR
      EQUA(1)=PTA1(2)-PTA2(2)
      EQUA(2)=PTA2(1)-PTA1(1)
      EQUA(3)=PTA1(1)*PTA2(2)-PTA2(1)*PTA1(2)
      EQUB(1)=PTB1(2)-PTB2(2)
      EQUB(2)=PTB2(1)-PTB1(1)
      EQUB(3)=PTB1(1)*PTB2(2)-PTB2(1)*PTB1(2)
      DETR=EQUA(1)*EQUB(2)-EQUB(1)*EQUA(2)
      IF (ABS(DETR).LE.TLSQ) THEN
        PFLG=.FALSE.
      ELSE
        PFLG=.TRUE.
        PNTO(1)=(EQUA(2)*EQUB(3)-EQUB(2)*EQUA(3))/DETR
        PNTO(2)=(EQUB(1)*EQUA(3)-EQUA(1)*EQUB(3))/DETR
        IF (PTB1(1).EQ.PTB2(1)) PNTO(1)=PTB1(1)
        IF ((PNTO(1)+TOLR).LT.PTA1(1)) THEN
          PFLG=.FALSE.
        ELSE IF ((PNTO(1)-TOLR).GT.PTA2(1)) THEN
          PFLG=.FALSE.
        ELSE IF ((PNTO(1)+TOLR).LT.MIN(PTB1(1),PTB2(1))) THEN
          PFLG=.FALSE.
        ELSE IF ((PNTO(1)-TOLR).GT.MAX(PTB1(1),PTB2(1))) THEN
          PFLG=.FALSE.
        ELSE IF ((PNTO(2)+TOLR).LT.MIN(PTB1(2),PTB2(2))) THEN
          PFLG=.FALSE.
        ELSE IF ((PNTO(2)-TOLR).GT.MAX(PTB1(2),PTB2(2))) THEN
          PFLG=.FALSE.
        ELSE
          POLA=EQUA(1)*PNTO(1)+EQUA(2)*PNTO(2)+EQUA(3)
          POLB=EQUB(1)*PNTO(1)+EQUB(2)*PNTO(2)+EQUB(3)
          IF ((POLA*POLA).GT.
     X        (TLSQ*(EQUA(1)*EQUA(1)+EQUA(2)*EQUA(2)))) THEN
            PFLG=.FALSE.
          ELSE IF ((POLB*POLB).GT.
     X        (TLSQ*(EQUB(1)*EQUB(1)+EQUB(2)*EQUB(2)))) THEN
            PFLG=.FALSE.
          END IF
        END IF
      END IF
C
C  RETURN TO CALLING SUBROUTINE.
      RETURN
C
      END
