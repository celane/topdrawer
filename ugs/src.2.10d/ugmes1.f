      SUBROUTINE UGMES1(VSWT,V1IX,V2IX,ARAY,MDIM,NDIM,TRNS,PPNT)
C
C *******************  THE UNIFIED GRAPHICS SYSTEM  *******************
C *              OBTAIN A PROJECTED POINT ON THE SURFACE              *
C *                                                                   *
C *  THIS SUBROUTINE IS USED BY UGMESH TO EVALUATE A POINT ON THE     *
C *  MESH SURFACE AND PROJECT IT INTO THE VIEW PLANE.                 *
C *                                                                   *
C *  THE CALLING SEQUENCE IS:                                         *
C *    CALL UGMES1(VSWT,V1IX,V2IX,ARAY,MDIM,NDIM,TRNS,PPNT)           *
C *                                                                   *
C *  THE PARAMETERS IN THE CALLING SEQUENCE ARE:                      *
C *    VSWT  AN X/Y VARIABLE SWITCH.                                  *
C *    V1IX  THE INDEX IN THE NORMALIZED X DIRECTION.                 *
C *    V2IX  THE INDEX IN THE NORMALIZED Y DIRECTION.                 *
C *    ARAY  THE ARRAY DEFINING THE MESH SURFACE.                     *
C *    MDIM  THE EXTENT OF THE MESH SURFACE IN THE X DIRECTION.       *
C *    NDIM  THE EXTENT OF THE MESH SURFACE IN THE Y DIRECTION.       *
C *    TRNS  THE PROJECTION TRANSFORMATION.                           *
C *    PPNT  THE PROJECTED POINT.                                     *
C *                                                                   *
C *                          ROBERT C. BEACH                          *
C *                    COMPUTATION RESEARCH GROUP                     *
C *                STANFORD LINEAR ACCELERATOR CENTER                 *
C *                                                                   *
C *********************************************************************
C
      INTEGER       VSWT,V1IX,V2IX
      REAL          ARAY(MDIM,NDIM)
      INTEGER       MDIM,NDIM
      REAL          TRNS(31)
      REAL          PPNT(2)
C
      REAL          PT3D(3)
C
C  OBTAIN THE POINT AND PROJECT IT INTO THE VIEW PLANE.
      IF (VSWT.EQ.0) THEN
        PT3D(1)=ARAY(1,V1IX)
        PT3D(2)=ARAY(V2IX,1)
        PT3D(3)=ARAY(V2IX,V1IX)
      ELSE
        PT3D(1)=ARAY(1,V2IX)
        PT3D(2)=ARAY(V1IX,1)
        PT3D(3)=ARAY(V1IX,V2IX)
      END IF
      CALL UGPROJ(TRNS,PT3D,PPNT)
C
C  RETURN TO CALLING SUBROUTINE.
      RETURN
C
      END
