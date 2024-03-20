      SUBROUTINE UGMES2(LSUB,PNT1,PNT2,PNTX)
C
C *******************  THE UNIFIED GRAPHICS SYSTEM  *******************
C *                        DRAW A LINE SEGMENT                        *
C *                                                                   *
C *  THIS SUBROUTINE IS USED BY UGMESH TO DRAW A SINGLE LINE          *
C *  SEGMENT.  REDUNDANT BLANK MOVEMENTS ARE SUPPRESSED.              *
C *                                                                   *
C *  THE CALLING SEQUENCE IS:                                         *
C *    CALL UGMES2(LSUB,PNT1,PNT2,PNTX)                               *
C *                                                                   *
C *  THE PARAMETERS IN THE CALLING SEQUENCE ARE:                      *
C *    LSUB  THE LINE SEGMENT END POINT SUBROUTINE.                   *
C *    PNT1  THE FIRST POINT.                                         *
C *    PNT2  THE SECOND POINT.                                        *
C *    PNTX  A POINT USED TO REMEMBER THE LAST DRAWN POINT.           *
C *                                                                   *
C *                          ROBERT C. BEACH                          *
C *                    COMPUTATION RESEARCH GROUP                     *
C *                STANFORD LINEAR ACCELERATOR CENTER                 *
C *                                                                   *
C *********************************************************************
C
      EXTERNAL      LSUB
      REAL          PNT1(2),PNT2(2),PNTX(2)
C
C  DRAW THE VECTOR AND SAVE CURRENT POSITION.
      IF ((PNT1(1).NE.PNTX(1)).OR.(PNT1(2).NE.PNTX(2)))
     X  CALL LSUB(PNT1(1),PNT1(2),0)
      CALL LSUB(PNT2(1),PNT2(2),1)
      PNTX(1)=PNT2(1)
      PNTX(2)=PNT2(2)
C
C  RETURN TO CALLING SUBROUTINE.
      RETURN
C
      END
