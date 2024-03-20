      SUBROUTINE UGSCIN(OPTN,LSUB,XARY,YARY,NPTS,TARY,NTEN,
     X                  BFLG,BXVL,BYVL,TFLG,TXVL,TYVL)
C
C *******************  THE UNIFIED GRAPHICS SYSTEM  *******************
C *               SMOOTH CURVE INTERPOLATION SUBROUTINE               *
C *                                                                   *
C *  THIS SUBROUTINE MAY BE USED TO INTERPOLATE A SMOOTH CURVE        *
C *  THROUGH A GIVEN SERIES OF POINTS.  A USER SUPPLIED SUBROUTINE    *
C *  IS CALLED TO PROCESS THE LINE SEGMENT END POINTS.                *
C *                                                                   *
C *  THE CALLING SEQUENCE IS:                                         *
C *    CALL UGSCIN(OPTN,LSUB,XARY,YARY,NPTS,TARY,NTEN,                *
C *                BFLG,BXVL,BYVL,TFLG,TXVL,TYVL)                     *
C *                                                                   *
C *  THE PARAMETERS IN THE CALLING SEQUENCE ARE:                      *
C *    OPTN  THE OPTIONS LIST.                                        *
C *    LSUB  THE LINE SEGMENT END POINT SUBROUTINE.                   *
C *    XARY  THE ARRAY OF X COORDINATES TO BE INTERPOLATED.           *
C *    YARY  THE ARRAY OF Y COORDINATES TO BE INTERPOLATED.           *
C *    NPTS  THE NUMBER OF POINTS IN XARY AND YARY.                   *
C *    TARY  THE ARRAY OF TENSION VALUES.                             *
C *    NTEN  THE NUMBER OF WORDS IN TARY.                             *
C *    BFLG  THE BEGINNING END POINT CONDITION FLAG.                  *
C *    BXVL  THE X VALUE OF THE BEGINNING END POINT CONDITION.        *
C *    BYVL  THE Y VALUE FOR THE BEGINNING END POINT CONDITION.       *
C *    TFLG  THE TERMINAL END POINT CONDITION FLAG.                   *
C *    TXVL  THE X VALUE OF THE TERMINAL END POINT CONDITION.         *
C *    TYVL  THE Y VALUE FOR THE TERMINAL END POINT CONDITION.        *
C *                                                                   *
C *                          ROBERT C. BEACH                          *
C *                    COMPUTATION RESEARCH GROUP                     *
C *                STANFORD LINEAR ACCELERATOR CENTER                 *
C *                                                                   *
C *********************************************************************
C
      CHARACTER*(*) OPTN
      EXTERNAL      LSUB
      REAL          XARY(*),YARY(*)
      INTEGER       NPTS
      REAL          TARY(*)
      INTEGER       NTEN
      INTEGER       BFLG
      REAL          BXVL,BYVL
      INTEGER       TFLG
      REAL          TXVL,TYVL
C
      INCLUDE       'UGSYSTEM:UGERRCBK.FOR'
C
      INTEGER*4     EXST(7),EXUN,EXGT
      REAL*4        EXXF,EXYF
      INTEGER*4     EXNP
      REAL*4        EXDP,EXTL
      EQUIVALENCE   (EXUN,EXST(1)),      (EXGT,EXST(2)),
     X              (EXXF,EXST(3)),      (EXYF,EXST(4)),
     X              (EXNP,EXST(5)),      (EXDP,EXST(6)),
     X              (EXTL,EXST(7))
C
      LOGICAL       BCUR,TCUR
C
      INTEGER       KSEG
      INTEGER       BTYP,TTYP
      INTEGER       KTEN
C
      INTEGER       FPRM
      INTEGER       NPRM,KPRM
      REAL          DPRM
C
      REAL          PNTS(4,2),DIST(3),TENS(4)
C
      REAL          D1P2,D1M2,D2P3,D2M3
      REAL          MTRX(4,4)
C
      REAL          TVEC(3),COEF(4)
      INTEGER       BBIT
      REAL          XCRD,YCRD,DELX,DELY
C
      INTEGER       DESC_NUM
      PARAMETER    (DESC_NUM = 7)
      INTEGER*4     INST(4,DESC_NUM)
      CHARACTER*16  IFLAG(DESC_NUM)
C
      DATA   (INST(I,1),I=1,4) / 1,7,1,1 /, IFLAG(1) /'UNIFORM'/
      DATA   (INST(I,2),I=1,4) / 1,6,2,1 /, IFLAG(2) /'GENTAN'/
      DATA   (INST(I,3),I=1,4) / 3,5,3,0 /, IFLAG(3) /'XFACT'/
      DATA   (INST(I,4),I=1,4) / 3,5,4,0 /, IFLAG(4) /'YFACT'/
      DATA   (INST(I,5),I=1,4) / 2,5,5,0 /, IFLAG(5) /'NPARM'/
      DATA   (INST(I,6),I=1,4) / 3,5,6,0 /, IFLAG(6) /'DPARM'/
      DATA   (INST(I,7),I=1,4) / 3,5,7,0 /, IFLAG(7) /'TOLER'/
C
C  CHECK THE INPUT DATA, SCAN THE OPTIONS LIST, AND INITIALIZE.
      IF (NPTS.LT.2) GO TO 501
      IF (NTEN.LT.1) GO TO 502
      EXUN=0
      EXGT=0
      EXXF=1.0
      EXYF=1.0
      EXNP=0
      EXDP=0.0
      EXTL=0.001
      CALL UGOPTION(OPTN,DESC_NUM,INST,IFLAG,EXST,EXSTR)
      IF (EXNP.GT.0) THEN
        FPRM=0
        NPRM=EXNP
      ELSE IF (EXDP.GT.0.0) THEN
        FPRM=1
        DPRM=EXDP
      ELSE
        FPRM=0
        NPRM=8
      END IF
      BCUR=.TRUE.
      TCUR=.FALSE.
      KSEG=0
      KTEN=0
      BBIT=0
C
C  OBTAIN THE NEXT CURVE SEGMENT.
  101 KSEG=KSEG+1
      IF (KSEG.EQ.NPTS) THEN
        TCUR=.TRUE.
        TVAL=1.0
        GO TO 301
      ELSE
        IF (KSEG.EQ.1) THEN
          IF (BFLG.EQ.0) THEN
            BTYP=0
            PNTS(1,1)=0.0
            PNTS(1,2)=0.0
          ELSE IF ((BFLG.EQ.1).OR.(BFLG.EQ.2)) THEN
            BTYP=BFLG
            PNTS(1,1)=BXVL
            PNTS(1,2)=BYVL
          ELSE
            GO TO 503
          END IF
          TENS(1)=0.0
          PNTS(2,1)=XARY(1)
          PNTS(2,2)=YARY(1)
          KTEN=KTEN+1
          IF (KTEN.GT.NTEN) KTEN=1
          TENS(2)=TARY(KTEN)
          PNTS(3,1)=XARY(2)
          PNTS(3,2)=YARY(2)
          KTEN=KTEN+1
          IF (KTEN.GT.NTEN) KTEN=1
          TENS(3)=TARY(KTEN)
          IF ((EXUN.NE.0).OR.(BTYP.NE.1)) THEN
            DIST(1)=1.0
          ELSE
            DIST(1)=SQRT((EXXF*(PNTS(2,1)-PNTS(1,1)))**2+
     X                   (EXYF*(PNTS(2,2)-PNTS(1,2)))**2)
            IF (ABS(DIST(1)).LT.EXTL) GO TO 504
          END IF
          IF (EXUN.NE.0) THEN
            DIST(2)=1.0
          ELSE
            DIST(2)=SQRT((EXXF*(PNTS(3,1)-PNTS(2,1)))**2+
     X                   (EXYF*(PNTS(3,2)-PNTS(2,2)))**2)
            IF (ABS(DIST(2)).LT.EXTL) GO TO 504
          END IF
        ELSE
          PNTS(1,1)=PNTS(2,1)
          PNTS(1,2)=PNTS(2,2)
          PNTS(2,1)=PNTS(3,1)
          PNTS(2,2)=PNTS(3,2)
          PNTS(3,1)=PNTS(4,1)
          PNTS(3,2)=PNTS(4,2)
          DIST(1)=DIST(2)
          DIST(2)=DIST(3)
          TENS(1)=TENS(2)
          TENS(2)=TENS(3)
          TENS(3)=TENS(4)
          BTYP=1
        END IF
        IF (KSEG.EQ.(NPTS-1)) THEN
          IF (TFLG.EQ.0) THEN
            TTYP=0
            PNTS(4,1)=0.0
            PNTS(4,2)=0.0
          ELSE IF ((TFLG.EQ.1).OR.(TFLG.EQ.2)) THEN
            TTYP=TFLG
            PNTS(4,1)=TXVL
            PNTS(4,2)=TYVL
          ELSE
            GO TO 503
          END IF
        ELSE
          PNTS(4,1)=XARY(KSEG+2)
          PNTS(4,2)=YARY(KSEG+2)
          KTEN=KTEN+1
          IF (KTEN.GT.NTEN) KTEN=1
          TENS(4)=TARY(KTEN)
          TTYP=1
        END IF
        IF ((EXUN.NE.0).OR.(TTYP.NE.1)) THEN
          DIST(3)=1.0
        ELSE
          DIST(3)=SQRT((EXXF*(PNTS(4,1)-PNTS(3,1)))**2+
     X                 (EXYF*(PNTS(4,2)-PNTS(3,2)))**2)
          IF (ABS(DIST(3)).LT.EXTL) GO TO 504
        END IF
      END IF
C
C  SET UP THE EQUATION OF THE SEGMENT.
      MTRX(1,1)=0.0
      MTRX(1,2)=0.0
      MTRX(1,3)=0.0
      MTRX(1,4)=0.0
      MTRX(2,1)=0.0
      MTRX(2,2)=0.0
      MTRX(2,3)=0.0
      MTRX(2,4)=1.0
      MTRX(3,1)=0.0
      MTRX(3,2)=0.0
      MTRX(3,3)=0.0
      MTRX(3,4)=0.0
      MTRX(4,1)=0.0
      MTRX(4,2)=0.0
      MTRX(4,3)=0.0
      MTRX(4,4)=0.0
      D1M2=TENS(2)*(DIST(1)-DIST(2))/DIST(1)
      D1P2=TENS(2)*DIST(1)/(DIST(1)+DIST(2))
      D2M3=TENS(3)*(DIST(2)-DIST(3))/DIST(3)
      D2P3=TENS(3)*DIST(3)/(DIST(2)+DIST(3))
      IF (BTYP.EQ.0) THEN
        IF (TTYP.EQ.0) THEN
          MTRX(2,3)=-1.0
          MTRX(3,3)=+1.0
        ELSE IF (TTYP.EQ.1) THEN
          MTRX(2,1)=         -0.5*D2P3+0.5
          MTRX(2,3)=         +0.5*D2P3-1.5
          MTRX(3,1)=-0.5*D2M3         -0.5
          MTRX(3,3)=+0.5*D2M3         +1.5
          MTRX(4,1)=+0.5*D2M3+0.5*D2P3
          MTRX(4,3)=-0.5*D2M3-0.5*D2P3
        ELSE
          MTRX(2,1)=+0.5
          MTRX(2,3)=-1.5
          MTRX(3,1)=-0.5
          MTRX(3,3)=+1.5
          MTRX(4,1)=+0.5*TENS(3)*DIST(2)
          MTRX(4,3)=-0.5*TENS(3)*DIST(2)
        END IF
      ELSE IF (BTYP.EQ.1) THEN
        IF (TTYP.EQ.0) THEN
          MTRX(1,1)=+0.5*D1M2-0.5*D1P2
          MTRX(1,2)=-1.5*D1M2+1.5*D1P2
          MTRX(1,3)=+    D1M2-    D1P2
          MTRX(2,1)=-0.5*D1M2         +0.5
          MTRX(2,2)=+1.5*D1M2         -1.5
          MTRX(2,3)=-    D1M2
          MTRX(3,1)=         +0.5*D1P2-0.5
          MTRX(3,2)=         -1.5*D1P2+1.5
          MTRX(3,3)=         +    D1P2
        ELSE IF (TTYP.EQ.1) THEN
          MTRX(1,1)=+    D1M2-    D1P2
          MTRX(1,2)=-2.0*D1M2+2.0*D1P2
          MTRX(1,3)=+    D1M2-    D1P2
          MTRX(2,1)=-    D1M2-    D2P3+2.0
          MTRX(2,2)=+2.0*D1M2+    D2P3-3.0
          MTRX(2,3)=-    D1M2
          MTRX(3,1)=-    D2M3+    D1P2-2.0
          MTRX(3,2)=+    D2M3-2.0*D1P2+3.0
          MTRX(3,3)=         +    D1P2
          MTRX(4,1)=+    D2M3+    D2P3
          MTRX(4,2)=-    D2M3-    D2P3
        ELSE
          MTRX(1,1)=+    D1M2-    D1P2
          MTRX(1,2)=-2.0*D1M2+2.0*D1P2
          MTRX(1,3)=+    D1M2-    D1P2
          MTRX(2,1)=-    D1M2         +2.0
          MTRX(2,2)=+2.0*D1M2         -3.0
          MTRX(2,3)=-    D1M2
          MTRX(3,1)=         +    D1P2-2.0
          MTRX(3,2)=         -2.0*D1P2+3.0
          MTRX(3,3)=         +    D1P2
          MTRX(4,1)=+    TENS(3)*DIST(2)
          MTRX(4,2)=-    TENS(3)*DIST(2)
        END IF
      ELSE
        IF (TTYP.EQ.0) THEN
          MTRX(1,1)=+0.5*TENS(2)*DIST(2)
          MTRX(1,2)=-1.5*TENS(2)*DIST(2)
          MTRX(1,3)=+    TENS(2)*DIST(2)
          MTRX(2,1)=+0.5
          MTRX(2,2)=-1.5
          MTRX(3,1)=-0.5
          MTRX(3,2)=+1.5
        ELSE IF (TTYP.EQ.1) THEN
          MTRX(1,1)=+    TENS(2)*DIST(2)
          MTRX(1,2)=-2.0*TENS(2)*DIST(2)
          MTRX(1,3)=+    TENS(2)*DIST(2)
          MTRX(2,1)=         -    D2P3+2.0
          MTRX(2,2)=         +    D2P3-3.0
          MTRX(3,1)=-    D2M3         -2.0
          MTRX(3,2)=+    D2M3         +3.0
          MTRX(4,1)=+    D2M3+    D2P3
          MTRX(4,2)=-    D2M3-    D2P3
        ELSE
          MTRX(1,1)=+    TENS(2)*DIST(2)
          MTRX(1,2)=-2.0*TENS(2)*DIST(2)
          MTRX(1,3)=+    TENS(2)*DIST(2)
          MTRX(2,1)=+2.0
          MTRX(2,2)=-3.0
          MTRX(3,1)=-2.0
          MTRX(3,2)=+3.0
          MTRX(4,1)=+    TENS(3)*DIST(2)
          MTRX(4,2)=-    TENS(3)*DIST(2)
        END IF
      END IF
C
C  OBTAIN THE NEXT PARAMETER VALUE ON THE CURVE.
  201 IF (BCUR) THEN
        BCUR=.FALSE.
        TVAL=0.0
        IF (FPRM.EQ.0) KPRM=0
      ELSE
        IF (FPRM.EQ.0) THEN
          KPRM=KPRM+1
          IF (KPRM.EQ.NPRM) THEN
            KPRM=-1
            GO TO 101
          ELSE
            TVAL=REAL(KPRM)/REAL(NPRM)
          END IF
        ELSE
          TVAL=TVAL+(DPRM/DIST(2))
          IF (TVAL.GT.(1.0-EXTL)) THEN
            TVAL=((TVAL-1.0)*DIST(2)-DPRM)/DIST(3)
            GO TO 101
          END IF
        END IF
      END IF
C
C  EVALUATE THE CURVE AT THE CURRENT VALUE OF THE PARAMETER.
  301 TVEC(3)=TVAL
      TVEC(2)=TVAL*TVAL
      TVEC(1)=TVAL*TVEC(2)
      COEF(1)=MTRX(1,1)*TVEC(1)+MTRX(1,2)*TVEC(2)+
     X        MTRX(1,3)*TVEC(3)+MTRX(1,4)
      COEF(2)=MTRX(2,1)*TVEC(1)+MTRX(2,2)*TVEC(2)+
     X        MTRX(2,3)*TVEC(3)+MTRX(2,4)
      COEF(3)=MTRX(3,1)*TVEC(1)+MTRX(3,2)*TVEC(2)+
     X        MTRX(3,3)*TVEC(3)+MTRX(3,4)
      COEF(4)=MTRX(4,1)*TVEC(1)+MTRX(4,2)*TVEC(2)+
     X        MTRX(4,3)*TVEC(3)+MTRX(4,4)
      XCRD=COEF(1)*PNTS(1,1)+COEF(2)*PNTS(2,1)+
     X     COEF(3)*PNTS(3,1)+COEF(4)*PNTS(4,1)
      YCRD=COEF(1)*PNTS(1,2)+COEF(2)*PNTS(2,2)+
     X     COEF(3)*PNTS(3,2)+COEF(4)*PNTS(4,2)
      IF (EXGT.EQ.0) THEN
        CALL LSUB(XCRD,YCRD,BBIT)
      ELSE
        TVEC(2)=2.0*TVAL
        TVEC(1)=3.0*TVAL*TVAL
        COEF(1)=MTRX(1,1)*TVEC(1)+MTRX(1,2)*TVEC(2)+MTRX(1,3)
        COEF(2)=MTRX(2,1)*TVEC(1)+MTRX(2,2)*TVEC(2)+MTRX(2,3)
        COEF(3)=MTRX(3,1)*TVEC(1)+MTRX(3,2)*TVEC(2)+MTRX(3,3)
        COEF(4)=MTRX(4,1)*TVEC(1)+MTRX(4,2)*TVEC(2)+MTRX(4,3)
        DELX=(COEF(1)*PNTS(1,1)+COEF(2)*PNTS(2,1)+
     X        COEF(3)*PNTS(3,1)+COEF(4)*PNTS(4,1))/DIST(2)
        DELY=(COEF(1)*PNTS(1,2)+COEF(2)*PNTS(2,2)+
     X        COEF(3)*PNTS(3,2)+COEF(4)*PNTS(4,2))/DIST(2)
        CALL LSUB(XCRD,YCRD,BBIT,DELX,DELY)
      END IF
      BBIT=1
      IF (.NOT.TCUR) GO TO 201
C
C  RESET ERROR INDICATORS AND RETURN TO CALLER.
      UGELV=0
      UGENM='        '
      UGEIX=0
  401 RETURN
C
C  REPORT ERRORS TO THE UNIFIED GRAPHICS SYSTEM ERROR PROCESSOR.
  501 CALL UGRERR(3,'UGSCIN  ',1)
      GO TO 401
  502 CALL UGRERR(3,'UGSCIN  ',2)
      GO TO 401
  503 CALL UGRERR(3,'UGSCIN  ',3)
      GO TO 401
  504 CALL UGRERR(3,'UGSCIN  ',4)
      GO TO 401
C
      END
