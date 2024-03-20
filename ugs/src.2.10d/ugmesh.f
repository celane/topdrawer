      SUBROUTINE UGMESH(OPTN,LSUB,ARAY,MDIM,NDIM,TRNS,WKAR,LDIM)
C
C *******************  THE UNIFIED GRAPHICS SYSTEM  *******************
C *                MESH SURFACE GENERATION SUBROUTINE                 *
C *                                                                   *
C *  THIS SUBROUTINE MAY BE USED TO GENERATE A DESCRIPTION OF A MESH  *
C *  SURFACE.  A USER SUPPLIED SUBROUTINE IS CALLED TO PROCESS THE    *
C *  LINE SEGMENT END POINTS.                                         *
C *                                                                   *
C *  THE CALLING SEQUENCE IS:                                         *
C *    CALL UGMESH(OPTN,LSUB,ARAY,MDIM,NDIM,TRNS,WKAR,LDIM)           *
C *                                                                   *
C *  THE PARAMETERS IN THE CALLING SEQUENCE ARE:                      *
C *    OPTN  THE OPTIONS LIST.                                        *
C *    LSUB  THE LINE SEGMENT END POINT SUBROUTINE.                   *
C *    ARAY  THE ARRAY DEFINING THE MESH SURFACE.                     *
C *    MDIM  THE EXTENT OF THE MESH SURFACE IN THE X DIRECTION.       *
C *    NDIM  THE EXTENT OF THE MESH SURFACE IN THE Y DIRECTION.       *
C *    TRNS  THE PROJECTION TRANSFORMATION.                           *
C *    WKAR  A WORK AREA.                                             *
C *    LDIM  THE NUMBER OF WORDS IN WKAR.                             *
C *                                                                   *
C *                          ROBERT C. BEACH                          *
C *                    COMPUTATION RESEARCH GROUP                     *
C *                STANFORD LINEAR ACCELERATOR CENTER                 *
C *                                                                   *
C *********************************************************************
C
      CHARACTER*(*) OPTN
      EXTERNAL      LSUB
      REAL          ARAY(MDIM,NDIM)
      INTEGER       MDIM,NDIM
      REAL          TRNS(31)
      REAL          WKAR(*)
      INTEGER       LDIM
C
      INCLUDE       'UGSYSTEM:UGERRCBK.FOR'
C
      INTEGER*4     EXST(3),EXLO,EXNB
      REAL*4        EXTL
      EQUIVALENCE   (EXLO,EXST(1)),      (EXNB,EXST(2)),
     X              (EXTL,EXST(3))
C
      REAL*4        HFB1
      INTEGER*2     HFP1(2)
      EQUIVALENCE   (HFP1(1),HFB1)
      INTEGER*4     HFCU,HFFL
C
      REAL          PNTX(2)
      REAL          PNT1(2),PNT2(2),PNT3(2),PNT4(2),PNT5(2),PNT6(2)
      INTEGER       VSWT,V1IX,V1LO,V1HI,V1DL,V1CD,V2IX,V2LO,V2HI,V2DL
      REAL          ULVL
      LOGICAL       VFLG
C
      REAL          FLT1,FLT2,FLT3,FLT4,FLT5
      INTEGER       INT1,INT2
      INTEGER       DESC_NUM
      PARAMETER    (DESC_NUM = 3)
      INTEGER*4     INST(4,DESC_NUM)
      CHARACTER*16  IFLAG(DESC_NUM)
C
      DATA  (INST(I,1),I=1,4) / 1,5,1,1 /, IFLAG(1) / 'LOWER' /
      DATA  (INST(I,2),I=1,4) / 1,6,2,1 /, IFLAG(2) / 'NOCOMN' /
      DATA  (INST(I,3),I=1,4) / 3,5,3,0 /, IFLAG(3) / 'TOLER' /
C
C
C  SCAN THE OPTIONS LIST.
      EXLO=0
      EXNB=0
      EXTL=0.00005
      CALL UGOPTION(OPTN,DESC_NUM,INST,IFLAG,EXST,EXSTR)
      PNTX(1)=1E10
      PNTX(2)=1E10
      IF (EXLO.EQ.0) THEN
        ULVL=1.0
      ELSE
        ULVL=-1.0
      END IF
      IF ((MDIM.LT.3).OR.(NDIM.LT.3)) GO TO 701
C
C  INITIALIZE THE HEIGHT FUNCTION.
      INT1=3*(MIN(LDIM,32767)/3)-2
      IF (INT1.LT.7) GO TO 702
      HFP1(1)=4
      HFP1(2)=0
      WKAR(1)=HFB1
      WKAR(2)=-1E10
      WKAR(3)=-1E10*ULVL
      HFP1(1)=0
      HFP1(2)=1
      WKAR(4)=HFB1
      WKAR(5)=1E10
      WKAR(6)=-1E10*ULVL
      DO 101 INT2=7,INT1,3
        IF (INT2.EQ.INT1) THEN
          HFP1(1)=0
        ELSE
          HFP1(1)=INT2+3
        END IF
        WKAR(INT2)=HFB1
  101 CONTINUE
      HFCU=1
      HFFL=7
C
C  COMPUTE THE NORMALIZED VIEW OF THE MESH SURFACE.
      PNT1(1)=TRNS(13)+TRNS(25)*TRNS(16)
      PNT1(2)=TRNS(14)+TRNS(25)*TRNS(17)
      FLT1=ARAY(1,NDIM)-ARAY(1,2)
      FLT2=(PNT1(1)-ARAY(1,2))*(ARAY(MDIM,1)-ARAY(2,1))
      FLT3=(PNT1(2)-ARAY(2,1))*FLT1-FLT2
      FLT4=(ARAY(1,2)+ARAY(1,NDIM))/2.0
      FLT5=(ARAY(2,1)+ARAY(MDIM,1))/2.0
      V1CD=0
      IF ((PNT1(2)-ARAY(MDIM,1))*FLT1+FLT2.LT.0.0) THEN
        IF (FLT3.GT.0.0) THEN
          VSWT=1
          V2LO=2
          V2HI=NDIM
          V2DL=1
          IF (PNT1(2).GT.FLT5) GO TO 202
          GO TO 201
        ELSE
          VSWT=0
          V2LO=2
          V2HI=MDIM
          V2DL=1
          IF (PNT1(1).LT.FLT4) GO TO 205
          GO TO 204
        END IF
      ELSE
        IF (FLT3.LT.0.0) THEN
          VSWT=1
          V2LO=NDIM
          V2HI=2
          V2DL=-1
          IF (PNT1(2).LT.FLT5) GO TO 201
          GO TO 202
        ELSE
          VSWT=0
          V2LO=MDIM
          V2HI=2
          V2DL=-1
          IF (PNT1(1).GT.FLT4) GO TO 204
          GO TO 205
        END IF
      END IF
  201 V1LO=MDIM
      V1HI=2
      V1DL=-1
      GO TO 203
  202 V1LO=2
      V1HI=MDIM
      V1DL=1
  203 IF (PNT1(2).GE.ARAY(MDIM,1)) V1CD=MDIM
      IF (PNT1(2).LE.ARAY(2,1)) V1CD=2
      GO TO 301
  204 V1LO=2
      V1HI=NDIM
      V1DL=1
      GO TO 206
  205 V1LO=NDIM
      V1HI=2
      V1DL=-1
  206 IF (PNT1(1).GE.ARAY(1,NDIM)) V1CD=NDIM
      IF (PNT1(1).LE.ARAY(1,2)) V1CD=2
C
C  INITIALIZE THE LOOP ON THE V2IX VARIABLE.
  301 V2IX=V2LO
C
C  MARCH ALONG THE V2IX=CONSTANT MESH LINE.
  401 V1IX=V1LO
      CALL UGMES1(VSWT,V1IX,V2IX,ARAY,MDIM,NDIM,TRNS,PNT2)
      PNT1(1)=PNT2(1)
      PNT1(2)=PNT2(2)
  402 V1IX=V1IX+V1DL
      CALL UGMES1(VSWT,V1IX,V2IX,ARAY,MDIM,NDIM,TRNS,PNT2)
      CALL UGMES3(PNT1,PNT2,ULVL,EXTL,WKAR,HFCU,PNT3,PNT4,VFLG)
      IF (VFLG) THEN
        IF ((V2IX.NE.V2LO).OR.(EXNB.EQ.0))
     X     CALL UGMES2(LSUB,PNT3,PNT4,PNTX)
        CALL UGMES4(PNT3,PNT4,EXTL,WKAR,HFCU,HFFL,INT1)
        IF (INT1.NE.0) GO TO 702
      END IF
      IF (V1IX.NE.V1HI) THEN
        PNT1(1)=PNT2(1)
        PNT1(2)=PNT2(2)
        GO TO 402
      END IF
C
C  CHECK FOR THE LAST V2IX=CONSTANT MESH LINE.
      IF (V2IX.EQ.V2HI) GO TO 601
C
C  MARCH BACK ALONG THE V2IX=CONSTANT MESH LINE.
      V1IX=V1HI
  501 CALL UGMES1(VSWT,V1IX,V2IX,ARAY,MDIM,NDIM,TRNS,PNT2)
      PNT1(1)=PNT2(1)
      PNT1(2)=PNT2(2)
      V2IX=V2IX+V2DL
      CALL UGMES1(VSWT,V1IX,V2IX,ARAY,MDIM,NDIM,TRNS,PNT2)
      V2IX=V2IX-V2DL
      CALL UGMES3(PNT1,PNT2,ULVL,EXTL,WKAR,HFCU,PNT3,PNT4,VFLG)
      IF (V1IX.EQ.V1CD) THEN
        PNT5(1)=PNT3(1)
        PNT5(2)=PNT3(2)
        PNT6(1)=PNT4(1)
        PNT6(2)=PNT4(2)
        PNT3(1)=PNT1(1)
        PNT3(2)=PNT1(2)
        PNT4(1)=PNT2(1)
        PNT4(2)=PNT2(2)
      END IF
      IF (((V1IX.EQ.V1CD).AND.(EXNB.EQ.0)).OR.(VFLG.AND.
     X  ((V1IX.NE.V1CD).OR.(EXNB.EQ.0))))
     X   CALL UGMES2(LSUB,PNT3,PNT4,PNTX)
      IF (V1IX.EQ.V1CD) THEN
        PNT3(1)=PNT5(1)
        PNT3(2)=PNT5(2)
        PNT4(1)=PNT6(1)
        PNT4(2)=PNT6(2)
      END IF
      IF (VFLG) THEN
        CALL UGMES4(PNT3,PNT4,EXTL,WKAR,HFCU,HFFL,INT1)
        IF (INT1.NE.0) GO TO 702
      END IF
      IF (V1IX.NE.V1LO) THEN
        V1IX=V1IX-V1DL
        GO TO 501
      END IF
C
C  INCREMENT IN THE V2IX DIRECTION.
      V2IX=V2IX+V2DL
      GO TO 401
C
C  RESET ERROR INDICATORS AND RETURN TO CALLER.
  601 UGELV=0
      UGENM='        '
      UGEIX=0
  602 RETURN
C
C  REPORT ERRORS TO THE UNIFIED GRAPHICS SYSTEM ERROR PROCESSOR.
  701 CALL UGRERR(3,'UGMESH  ',1)
      GO TO 602
  702 CALL UGRERR(3,'UGMESH  ',2)
      GO TO 602
C
      END
