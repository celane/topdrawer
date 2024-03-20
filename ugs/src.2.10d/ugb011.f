      SUBROUTINE UGB011(FLAG)
C
C *******************  THE UNIFIED GRAPHICS SYSTEM  *******************
C *      PROCESS THREE-DIMENSIONAL TRANSFORMATION DATA (PART 2)       *
C *                                                                   *
C *  THIS SUBROUTINE IS USED TO DO THE FINAL PROCESSING OF THREE-     *
C *  DIMENSIONAL TRANSFORMATION DATA.  IT IS CALLED WHENEVER A NEW    *
C *  THREE-DIMENSIONAL OBJECT VOLUME, EYE POINT, UPWARD DIRECTION,    *
C *  AND PROJECTION FLAG ARE AVAILABLE.                               *
C *                                                                   *
C *  THE CALLING SEQUENCE IS:                                         *
C *    CALL UGB011(FLAG)                                              *
C *                                                                   *
C *  THE PARAMETER IN THE CALLING SEQUENCE IS:                        *
C *    FLAG  AN ERROR FLAG (0 MEANS PROCESSING WAS COMPLETE, 1 MEANS  *
C *          THE UPWARD DIRECTION WAS INVALID).                       *
C *                                                                   *
C *                          ROBERT C. BEACH                          *
C *                    COMPUTATION RESEARCH GROUP                     *
C *                STANFORD LINEAR ACCELERATOR CENTER                 *
C *                                                                   *
C *********************************************************************
C
      INTEGER       FLAG
C
      INCLUDE       'UGSYSTEM:UGDDACBK.FOR'
C
      REAL          NOBV(3,2),NEPT(3),NUDR(3)
      REAL          COVL(3),VDIR(3),HORZ(3)
      REAL          XVLO,YVLO,XVHI,YVHI
      REAL          DST1,DST2
C
      REAL          TPNT(3)
      REAL          FLT1,FLT2,FLT3
      INTEGER       INT1,INT2
C
C  INITIALIZE THE SUBROUTINE.
      FLAG=0
C
C  INITIALIZE THE TRANSFORMATION DATA.
      DDA3T(1)=DDA3W(1,2)-DDA3W(1,1)
      DDA3T(2)=DDA3W(2,2)-DDA3W(2,1)
      DDA3T(3)=DDA3W(3,2)-DDA3W(3,1)
      IF (DDADF.EQ.3) THEN
        DDA3T(4)=REAL(DDA3D(1,2)-DDA3D(1,1))
        DDA3T(5)=REAL(DDA3D(2,2)-DDA3D(2,1))
        DDA3T(6)=REAL(DDA3D(3,2)-DDA3D(3,1))
        DDA3T(7)=REAL(DDA3D(1,1))
        DDA3T(8)=REAL(DDA3D(2,1))
        DDA3T(9)=REAL(DDA3D(3,1))
        DDA3T(10)=DDA3T(4)/DDA3T(1)
        DDA3T(11)=DDA3T(5)/DDA3T(2)
        DDA3T(12)=DDA3T(6)/DDA3T(3)
      END IF
      DDA3T(13)=(REAL(DDAXV(1,2)-DDAXV(1,1)))/(DDA3V(1,2)-DDA3V(1,1))
      DDA3T(14)=(REAL(DDAXV(2,2)-DDAXV(2,1)))/(DDA3V(2,2)-DDA3V(2,1))
      DDA3T(15)=REAL(DDAXV(1,1))
      DDA3T(16)=REAL(DDAXV(2,1))
      FLT3=REAL(DDABD(1,2)-DDABD(1,1))
      FLT1=DDABX*FLT3
      FLT2=DDABY*REAL(DDABD(2,2)-DDABD(2,1))
      IF (FLT2.LE.FLT1) FLT3=FLT3-(FLT1-FLT2)/DDABX
      DDA3T(17)=FLT3/DDA3T(13)
      DDA3T(18)=(DDABX*DDA3T(13))/(DDABY*DDA3T(14))
C
C  GENERATE THE NECESSARY TRANSFORMATION DATA.
C    FIRST, PROJECT THE DATA INTO NORMALIZED WORLD COORDINATES.
      CALL UGB006(DDA3O(1,1),DDA3O(2,1),DDA3O(3,1),
     X             NOBV(1,1), NOBV(2,1), NOBV(3,1))
      CALL UGB006(DDA3O(1,2),DDA3O(2,2),DDA3O(3,2),
     X             NOBV(1,2), NOBV(2,2), NOBV(3,2))
      CALL UGB006(DDA3E(1),DDA3E(2),DDA3E(3),
     X             NEPT(1), NEPT(2), NEPT(3))
      NUDR(1)=DDA3U(1)/DDA3T(1)
      NUDR(2)=DDA3U(2)/DDA3T(2)
      NUDR(3)=DDA3U(3)/DDA3T(3)
      CALL UGB014(NUDR,INT1)
C    COMPUTE THE CENTER OF THE OBJECT VOLUME.
      COVL(1)=0.5*(NOBV(1,2)+NOBV(1,1))
      COVL(2)=0.5*(NOBV(2,2)+NOBV(2,1))
      COVL(3)=0.5*(NOBV(3,2)+NOBV(3,1))
C    COMPUTE THE VIEWING DIRECTION AND THE HORIZONTAL DIRECTION.
      VDIR(1)=COVL(1)-NEPT(1)
      VDIR(2)=COVL(2)-NEPT(2)
      VDIR(3)=COVL(3)-NEPT(3)
      CALL UGB014(VDIR,INT1)
      CALL UGB015(VDIR,NUDR,HORZ)
      CALL UGB014(HORZ,INT1)
      IF (INT1.EQ.0) GO TO 202
      CALL UGB015(HORZ,VDIR,NUDR)
      CALL UGB014(NUDR,INT1)
      IF (INT1.EQ.0) GO TO 202
      IF (DDADF.EQ.2) THEN
C    COMPUTE THE PROJECTION MATRIX AUXILIARY DATA FOR A
C    TWO-DIMENSIONAL DEVICE.
        FLT1=DDABX*(DDAXV(1,2)-DDAXV(1,1))
        FLT2=DDABY*(DDAXV(2,2)-DDAXV(2,1))
        IF (FLT1.GE.FLT2) THEN
          FLT3=(FLT1-FLT2)*(DDA3V(1,2)-DDA3V(1,1))/(2.0*FLT1)
          XVLO=DDA3V(1,1)+FLT3
          YVLO=DDA3V(2,1)
          XVHI=DDA3V(1,2)-FLT3
          YVHI=DDA3V(2,2)
        ELSE
          FLT3=(FLT2-FLT1)*(DDA3V(2,2)-DDA3V(2,1))/(2.0*FLT2)
          XVLO=DDA3V(1,1)
          YVLO=DDA3V(2,1)+FLT3
          XVHI=DDA3V(1,2)
          YVHI=DDA3V(2,2)-FLT3
        END IF
        TPNT(1)=COVL(1)-NEPT(1)
        TPNT(2)=COVL(2)-NEPT(2)
        TPNT(3)=COVL(3)-NEPT(3)
        DST1=SQRT(TPNT(1)*TPNT(1)+TPNT(2)*TPNT(2)+TPNT(3)*TPNT(3))
        TPNT(1)=NOBV(1,2)-NOBV(1,1)
        TPNT(2)=NOBV(2,2)-NOBV(2,1)
        TPNT(3)=NOBV(3,2)-NOBV(3,1)
        DST2=0.5*SQRT(TPNT(1)*TPNT(1)+TPNT(2)*TPNT(2)+TPNT(3)*TPNT(3))
C    GENERATE THE ACTUAL PROJECTION MATRIX.  THE FOLLOWING MATRIX
C    TRANSFORMS FROM NORMALIZED THREE-DIMENSIONAL WORLD COORDINATES
C    TO THE THREE-DIMENSIONAL VIEW PORT IN USER COORDINATES.
        IF (DDA3N.GT.0.0) THEN
          DO 101 INT1=1,3
            DDA3M(1,INT1)=DST1*(XVHI-XVLO)*HORZ(INT1)+
     X                    DST2*(XVHI+XVLO)*VDIR(INT1)
            DDA3M(2,INT1)=DST1*(YVHI-YVLO)*NUDR(INT1)+
     X                    DST2*(YVHI+YVLO)*VDIR(INT1)
            DDA3M(3,INT1)=2.0*DST2*VDIR(INT1)
  101     CONTINUE
          DO 102 INT1=1,3
            DDA3M(INT1,4)=-(DDA3M(INT1,1)*NEPT(1)+
     X                      DDA3M(INT1,2)*NEPT(2)+
     X                      DDA3M(INT1,3)*NEPT(3))
  102     CONTINUE
        ELSE
          DO 103 INT1=1,3
            DDA3M(1,INT1)=(XVHI-XVLO)*HORZ(INT1)
            DDA3M(2,INT1)=(YVHI-YVLO)*NUDR(INT1)
            DDA3M(3,INT1)=0.0
  103     CONTINUE
          DDA3M(1,4)=DST2*(XVHI+XVLO)-(DDA3M(1,1)*COVL(1)+
     X                                 DDA3M(1,2)*COVL(2)+
     X                                 DDA3M(1,3)*COVL(3))
          DDA3M(2,4)=DST2*(YVHI+YVLO)-(DDA3M(2,1)*COVL(1)+
     X                                 DDA3M(2,2)*COVL(2)+
     X                                 DDA3M(2,3)*COVL(3))
          DDA3M(3,4)=2.0*DST2
        END IF
C    TRANSFORM THE MATRIX SO THAT IT PROJECTS FROM THE USER THREE-
C    DIMENSIONAL WORLD COORDINATE SYSTEM TO THE THREE-DIMENSIONAL
C    VIEW PORT IN USER COORDINATES.
        DO 105 INT1=1,3
          DO 104 INT2=1,3
            DDA3M(INT1,INT2)=DDA3M(INT1,INT2)/DDA3T(INT2)
            DDA3M(INT1,4)=DDA3M(INT1,4)-
     X                   (DDA3M(INT1,INT2)*DDA3W(INT2,1))
  104     CONTINUE
  105   CONTINUE
C    COMPUTE THE NEAR SCISSORING PLANE FOR A POINT PROJECTION.
        IF (DDA3N.GT.0.0) THEN
          TPNT(1)=NEPT(1)+DDA3N*(COVL(1)-NEPT(1))
          TPNT(2)=NEPT(2)+DDA3N*(COVL(2)-NEPT(2))
          TPNT(3)=NEPT(3)+DDA3N*(COVL(3)-NEPT(3))
          TPNT(1)=DDA3T(1)*TPNT(1)+DDA3W(1,1)
          TPNT(2)=DDA3T(2)*TPNT(2)+DDA3W(2,1)
          TPNT(3)=DDA3T(3)*TPNT(3)+DDA3W(3,1)
          DDA3P(1)=TPNT(1)-DDA3E(1)
          DDA3P(2)=TPNT(2)-DDA3E(2)
          DDA3P(3)=TPNT(3)-DDA3E(3)
          DDA3P(4)=-(DDA3P(1)*TPNT(1)+
     X               DDA3P(2)*TPNT(2)+
     X               DDA3P(3)*TPNT(3))
        END IF
      ELSE
C    TRANSFORM THE DATA INTO THREE-DIMENSIONAL DEVICE COORDINATES.
        CALL UGB007( NOBV(1,1), NOBV(2,1), NOBV(3,1),
     X              DDAXO(1,1),DDAXO(2,1),DDAXO(3,1))
        CALL UGB007( NOBV(1,2), NOBV(2,2), NOBV(3,2),
     X              DDAXO(1,2),DDAXO(2,2),DDAXO(3,2))
        CALL UGB007( NEPT(1), NEPT(2), NEPT(3),
     X              DDAXE(1),DDAXE(2),DDAXE(3))
        DDAXU(1)=NINT((2.0**30)*NUDR(1))
        DDAXU(2)=NINT((2.0**30)*NUDR(2))
        DDAXU(3)=NINT((2.0**30)*NUDR(3))
        DDAXN=NINT((2.0**30)*DDA3N)
      END IF
C
C  RETURN TO CALLER.
  201 RETURN
  202 FLAG=1
      GO TO 201
C
      END
