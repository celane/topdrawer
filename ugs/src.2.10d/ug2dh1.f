      SUBROUTINE UG2DH1(VSWT,PNT3,TRNS,PPNT)
C
C *******************  THE UNIFIED GRAPHICS SYSTEM  *******************
C *                  PROJECT A POINT ONTO THE SCREEN                  *
C *                                                                   *
C *  THIS SUBROUTINE IS USED BY UG2DHG TO PROJECT A POINT IN THE      *
C *  (V1-V2-V3) SPACE INTO THE SCREEN REFERENCE SYSTEM.               *
C *                                                                   *
C *  THE CALLING SEQUENCE IS:                                         *
C *    CALL UG2DH1(VSWT,PNT3,TRNS,PPNT)                               *
C *                                                                   *
C *  THE PARAMETERS IN THE CALLING SEQUENCE ARE:                      *
C *    VSWT  AN X/Y VARIABLE SWITCH.                                  *
C *    PNT3  THE POINT IN 3-DIMENSIONAL SPACE.                        *
C *    TRNS  THE PROJECTION TRANSFORMATION.                           *
C *    PPNT  THE PROJECTED POINT.                                     *
C *                                                                   *
C *                          ROBERT C. BEACH                          *
C *                    COMPUTATION RESEARCH GROUP                     *
C *                STANFORD LINEAR ACCELERATOR CENTER                 *
C *                                                                   *
C *********************************************************************
C
      INTEGER       VSWT
      REAL          PNT3(3)
      REAL          TRNS(31)
      REAL          PPNT(2)
C
      REAL          PT3D(3)
C
C  OBTAIN THE POINT AND PROJECT IT INTO THE VIEW PLANE.
      IF (VSWT.EQ.1) THEN
        PT3D(1)=PNT3(1)
        PT3D(2)=PNT3(2)
        PT3D(3)=PNT3(3)
      ELSE
        PT3D(1)=PNT3(2)
        PT3D(2)=PNT3(1)
        PT3D(3)=PNT3(3)
      END IF
      CALL UGPROJ(TRNS,PT3D,PPNT)
C
C  RETURN TO CALLING SUBROUTINE.
      RETURN
C
      END
