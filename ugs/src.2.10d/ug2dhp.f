      SUBROUTINE UG2DHP(OPTN,PSUB,ARAY,MDIM,NDIM,TRNS)
C
C *******************  THE UNIFIED GRAPHICS SYSTEM  *******************
C *                2-D HISTOGRAM GENERATION SUBROUTINE                *
C *                                                                   *
C *  THIS SUBROUTINE MAY BE USED TO GENERATE A DESCRIPTION OF A 2-D   *
C *  HISTOGRAM USING THE POLYGON FILL GRAPHIC PRIMITIVE.  A USER      *
C *  SUPPLIED SUBROUTINE IS CALLED TO PROCESS THE POLYGON DATA.       *
C *                                                                   *
C *  THE CALLING SEQUENCE IS:                                         *
C *    CALL UG2DHP(OPTN,PSUB,ARAY,MDIM,NDIM,TRNS)                     *
C *                                                                   *
C *  THE PARAMETERS IN THE CALLING SEQUENCE ARE:                      *
C *    OPTN  THE OPTIONS LIST.                                        *
C *    PSUB  THE POLYGON FILL SUBROUTINE.                             *
C *    ARAY  THE ARRAY DEFINING THE 2-D HISTOGRAM.                    *
C *    MDIM  THE EXTENT OF THE 2-D HISTOGRAM IN THE X DIRECTION.      *
C *    NDIM  THE EXTENT OF THE 2-D HISTOGRAM IN THE Y DIRECTION.      *
C *    TRNS  THE PROJECTION TRANSFORMATION.                           *
C *                                                                   *
C *                          ROBERT C. BEACH                          *
C *                    COMPUTATION RESEARCH GROUP                     *
C *                STANFORD LINEAR ACCELERATOR CENTER                 *
C *                                                                   *
C *********************************************************************
C
      CHARACTER*(*) OPTN
      EXTERNAL      PSUB
      REAL          ARAY(MDIM,NDIM)
      INTEGER       MDIM,NDIM
      REAL          TRNS(31)
C
      INCLUDE       'UGSYSTEM:UGERRCBK.FOR'
C
      INTEGER       QUAD
      INTEGER       V1IX,V1LO,V1HI,V1DL
      INTEGER       V2RP,V2LA(2),V2HA(2),V2DA(2)
      INTEGER       V2IX,V2LO,V2HI,V2DL
      INTEGER       XIND,YIND
      INTEGER       IDX1(0:3),IDX2(0:3),IDX3(0:3)
C
      INTEGER       INT1,INT2
C
      DATA          IDX1/3,4,4,3/
      DATA          IDX2/1,2,2,1/
      DATA          IDX3/2,3,1,4/
C
C  INITIALIZE AND CHECK THE INPUT.
      IF ((MDIM.LT.3).OR.(NDIM.LT.3)) GO TO 401
C
C  COMPUTE THE NORMALIZED VIEW OF THE 2-D HISTOGRAM.
      IF ((ARAY(1,2)   *ARAY(2,1)
     X    +TRNS(13)    *ARAY(MDIM,1)
     X    +ARAY(1,NDIM)*TRNS(14)
     X    -ARAY(1,NDIM)*ARAY(MDIM,1)
     X    -TRNS(13)    *ARAY(2,1)
     X    -ARAY(1,2)   *TRNS(14)    ).GT.0.0) THEN
        QUAD=1
      ELSE
        QUAD=0
      END IF
      IF ((ARAY(1,2)   *ARAY(MDIM,1)
     X    +TRNS(13)    *ARAY(2,1)
     X    +ARAY(1,NDIM)*TRNS(14)
     X    -ARAY(1,NDIM)*ARAY(2,1)
     X    -TRNS(13)    *ARAY(MDIM,1)
     X    -ARAY(1,2)   *TRNS(14)    ).GT.0.0) QUAD=QUAD+2
      IF (QUAD.EQ.1) THEN
        V1LO=2
        V1HI=NDIM-1
        V1DL=1
      ELSE IF (QUAD.EQ.2) THEN
        V1LO=NDIM-1
        V1HI=2
        V1DL=-1
      ELSE IF (QUAD.EQ.3) THEN
        V1LO=2
        V1HI=MDIM-1
        V1DL=1
      ELSE
        V1LO=MDIM-1
        V1HI=2
        V1DL=-1
      END IF
      IF ((TRNS(9).NE.0.0).OR.(TRNS(10).NE.0.0).OR.
     X    (TRNS(11).NE.0.0).OR.(TRNS(12).EQ.0.0)) THEN
        IF ((QUAD.EQ.1).OR.(QUAD.EQ.2)) THEN
          DO 101 INT1=2,MDIM
            IF (TRNS(14).LT.ARAY(INT1,1)) THEN
              INT2=INT1
              GO TO 102
            END IF
  101     CONTINUE
          INT2=MDIM+1
  102     V2LA(2)=MDIM-1
        ELSE
          DO 103 INT1=2,NDIM
            IF (TRNS(13).LT.ARAY(1,INT1)) THEN
              INT2=INT1
              GO TO 104
            END IF
  103     CONTINUE
          INT2=NDIM+1
  104     V2LA(2)=NDIM-1
        END IF
        V2LA(1)=2
        V2HA(1)=INT2-2
        V2DA(1)=1
        V2HA(2)=MAX(INT2-1,2)
        V2DA(2)=-1
      ELSE
        IF ((QUAD.EQ.1).OR.(QUAD.EQ.2)) THEN
          IF (TRNS(23).GT.0.0) THEN
            V2LA(1)=MDIM-1
            V2HA(1)=2
            V2DA(1)=-1
          ELSE
            V2LA(1)=2
            V2HA(1)=MDIM-1
            V2DA(1)=1
          END IF
        ELSE
          IF (TRNS(22).GT.0.0) THEN
            V2LA(1)=NDIM-1
            V2HA(1)=2
            V2DA(1)=-1
          ELSE
            V2LA(1)=2
            V2HA(1)=NDIM-1
            V2DA(1)=1
          END IF
        END IF
        V2LA(2)=1
        V2HA(2)=0
        V2DA(2)=1
      END IF
C
C  DRAW THE INDIVIDUAL COLUMNS.
      DO 203 V1IX=V1LO,V1HI,V1DL
        DO 202 V2RP=1,2
          V2LO=V2LA(V2RP)
          V2HI=V2HA(V2RP)
          V2DL=V2DA(V2RP)
          DO 201 V2IX=V2LO,V2HI,V2DL
            IF ((QUAD.EQ.1).OR.(QUAD.EQ.2)) THEN
              XIND=V1IX
              YIND=V2IX
            ELSE
              XIND=V2IX
              YIND=V1IX
            END IF
            CALL UG2DH7(PSUB,ARAY,MDIM,NDIM,TRNS,0,XIND,YIND)
            IF (V2DL.GT.0) THEN
              INT1=IDX1(QUAD)
            ELSE
              INT1=IDX2(QUAD)
            END IF
            CALL UG2DH7(PSUB,ARAY,MDIM,NDIM,TRNS,INT1,XIND,YIND)
            CALL UG2DH7(PSUB,ARAY,MDIM,NDIM,TRNS,IDX3(QUAD),XIND,YIND)
  201     CONTINUE
  202   CONTINUE
  203 CONTINUE
C
C  RESET ERROR INDICATORS AND RETURN TO CALLER.
      UGELV=0
      UGENM='        '
      UGEIX=0
  301 RETURN
C
C  REPORT ERRORS TO THE UNIFIED GRAPHICS SYSTEM ERROR PROCESSOR.
  401 CALL UGRERR(3,'UG2DHP  ',1)
      GO TO 301
C
      END
