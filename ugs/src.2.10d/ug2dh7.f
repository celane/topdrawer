      SUBROUTINE UG2DH7(PSUB,ARAY,MDIM,NDIM,TRNS,FLAG,XIND,YIND)
C
C *******************  THE UNIFIED GRAPHICS SYSTEM  *******************
C *                    DRAW AN ACTUAL COLUMN FACE                     *
C *                                                                   *
C *  THIS SUBROUTINE IS USED BY UG2DHP TO DRAW THE FACE OF A COLUMN   *
C *  IN THE 2-D HISTOGRAM.                                            *
C *                                                                   *
C *  THE CALLING SEQUENCE IS:                                         *
C *    CALL UG2DH7(PSUB,ARAY,MDIM,NDIM,TRNS,FLAG,XIND,YIND)           *
C *                                                                   *
C *  THE PARAMETERS IN THE CALLING SEQUENCE ARE:                      *
C *    PSUB  THE POLYGON FILL SUBROUTINE.                             *
C *    ARAY  THE ARRAY DEFINING THE 2-D HISTOGRAM.                    *
C *    MDIM  THE EXTENT OF THE 2-D HISTOGRAM IN THE X DIRECTION.      *
C *    NDIM  THE EXTENT OF THE 2-D HISTOGRAM IN THE Y DIRECTION.      *
C *    TRNS  THE PROJECTION TRANSFORMATION.                           *
C *    FLAG  AN INDEX SPECIFYING THE FACE TO BE DRAWN (0 MEANS TOP,   *
C *          1 MEANS LOW X, 2 MEANS LOW Y, 3 MEANS HIGH X, AND 4      *
C *          MEANS HIGH Y).                                           *
C *    XIND  THE X INDEX OF THE COLUMN.                               *
C *    YIND  THE Y INDEX OF THE COLUMN.                               *
C *                                                                   *
C *                          ROBERT C. BEACH                          *
C *                    COMPUTATION RESEARCH GROUP                     *
C *                STANFORD LINEAR ACCELERATOR CENTER                 *
C *                                                                   *
C *********************************************************************
C
      EXTERNAL      PSUB
      REAL          ARAY(MDIM,NDIM)
      INTEGER       MDIM,NDIM
      REAL          TRNS(31)
      INTEGER       FLAG,XIND,YIND
C
      REAL          PT3A(3),PT3B(3),PT3C(3),PT3D(3)
      REAL          PT2A(2)
      REAL          XARY(5),YARY(5)
C
C  GENERATE THE SELECTED FACE OF THE COLUMN.
      IF (FLAG.EQ.1) THEN
        PT3A(1)=ARAY(1,XIND)
      ELSE IF (FLAG.EQ.2) THEN
        PT3A(2)=ARAY(YIND,1)
      ELSE IF (FLAG.EQ.3) THEN
        PT3A(1)=ARAY(1,XIND+1)
      ELSE IF (FLAG.EQ.4) THEN
        PT3A(2)=ARAY(YIND+1,1)
      ELSE
        PT3A(1)=ARAY(1,XIND)
        PT3A(2)=ARAY(YIND,1)
        PT3B(1)=ARAY(1,XIND+1)
        PT3B(2)=PT3A(2)
        PT3C(1)=PT3B(1)
        PT3C(2)=ARAY(YIND+1,1)
        PT3D(1)=PT3A(1)
        PT3D(2)=PT3C(2)
      END IF
      IF ((FLAG.EQ.1).OR.(FLAG.EQ.3)) THEN
        PT3A(2)=ARAY(YIND,1)
        PT3B(1)=PT3A(1)
        PT3B(2)=PT3A(2)
        PT3C(1)=PT3A(1)
        PT3C(2)=ARAY(YIND+1,1)
        PT3D(1)=PT3A(1)
        PT3D(2)=PT3C(2)
      END IF
      IF ((FLAG.EQ.2).OR.(FLAG.EQ.4)) THEN
        PT3A(1)=ARAY(1,XIND)
        PT3B(1)=PT3A(1)
        PT3B(2)=PT3A(2)
        PT3C(1)=ARAY(1,XIND+1)
        PT3C(2)=PT3A(2)
        PT3D(1)=PT3C(1)
        PT3D(2)=PT3A(2)
      END IF
      PT3B(3)=ARAY(YIND,XIND)
      PT3C(3)=PT3B(3)
      IF (FLAG.NE.0) THEN
        PT3A(3)=ARAY(1,1)
        PT3D(3)=PT3A(3)
      ELSE
        PT3A(3)=PT3B(3)
        PT3D(3)=PT3B(3)
      END IF
      CALL UGPROJ(TRNS,PT3A,PT2A)
      XARY(1)=PT2A(1)
      YARY(1)=PT2A(2)
      CALL UGPROJ(TRNS,PT3B,PT2A)
      XARY(2)=PT2A(1)
      YARY(2)=PT2A(2)
      CALL UGPROJ(TRNS,PT3C,PT2A)
      XARY(3)=PT2A(1)
      YARY(3)=PT2A(2)
      CALL UGPROJ(TRNS,PT3D,PT2A)
      XARY(4)=PT2A(1)
      YARY(4)=PT2A(2)
      XARY(5)=XARY(1)
      YARY(5)=YARY(1)
      CALL PSUB(XARY,YARY,FLAG)
C
C  RETURN TO CALLER.
      RETURN
C
      END
