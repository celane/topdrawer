      SUBROUTINE UGXHCH(OPTN,LSUB,XARY,YARY,NPTS,WKAR,LDIM)
C
C *******************  THE UNIFIED GRAPHICS SYSTEM  *******************
C *                     CROSS HATCHING SUBROUTINE                     *
C *                                                                   *
C *  THIS SUBROUTINE MAY BE USED TO GENERATE A DESCRIPTION OF A       *
C *  CROSS HATCHED REGION.  A USER SUPPLIED SUBROUTINE IS CALLED TO   *
C *  PROCESS THE LINE SEGMENT END POINTS.                             *
C *                                                                   *
C *  THE CALLING SEQUENCE IS:                                         *
C *    CALL UGXHCH(OPTN,LSUB,XARY,YARY,NPTS,WKAR,LDIM)                *
C *                                                                   *
C *  THE PARAMETERS IN THE CALLING SEQUENCE ARE:                      *
C *    OPTN  THE OPTIONS LIST.                                        *
C *    LSUB  THE LINE SEGMENT END POINT SUBROUTINE.                   *
C *    XARY  THE ARRAY OF X COORDINATES DEFINING THE REGION.          *
C *    YARY  THE ARRAY OF Y COORDINATES DEFINING THE REGION.          *
C *    NPTS  THE NUMBER OF POINTS IN XARY AND YARY.                   *
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
      REAL          XARY(*),YARY(*)
      INTEGER       NPTS
      REAL          WKAR(*)
      INTEGER       LDIM
C
      INCLUDE       'UGSYSTEM:UGERRCBK.FOR'
C
      INTEGER*4     EXST(7)
      REAL*4        EXSP,EXAG,EXXC,EXYC,EXXF,EXYF,EXTL
      EQUIVALENCE   (EXSP,EXST(1)),      (EXAG,EXST(2)),
     X              (EXXC,EXST(3)),      (EXYC,EXST(4)),
     X              (EXXF,EXST(5)),      (EXYF,EXST(6)),
     X              (EXTL,EXST(7))
C
      REAL          SINA,COSA,CONX,CONY,COND
      INTEGER       MINL,MAXL,IPTS
      REAL          PNTX,PNTY,PARM
      REAL          TEM1,DST1,DST2
      LOGICAL       AFLG,SFLG
C
      INTEGER       INT1,INT2,INT3
      INTEGER       DESC_NUM
      PARAMETER    (DESC_NUM = 7)
      INTEGER*4     INST(4,DESC_NUM)
      CHARACTER*16  IFLAG(DESC_NUM)
C
      DATA     (INST(I,1),I=1,4) / 3,7,1,0 /, IFLAG(1) / 'SPACING' /
      DATA     (INST(I,2),I=1,4) / 3,5,2,0 /, IFLAG(2) / 'ANGLE' /
      DATA     (INST(I,3),I=1,4) / 3,1,3,0 /, IFLAG(3) / 'X' /
      DATA     (INST(I,4),I=1,4) / 3,1,4,0 /, IFLAG(4) / 'Y' /
      DATA     (INST(I,5),I=1,4) / 3,5,5,0 /, IFLAG(5) / 'XFACT' /
      DATA     (INST(I,6),I=1,4) / 3,5,6,0 /, IFLAG(6) / 'YFACT' /
      DATA     (INST(I,7),I=1,4) / 3,5,7,0 /, IFLAG(7) / 'TOLER' /
C
C  SCAN THE OPTIONS LIST.
      EXSP=0.02
      EXAG=45.0
      EXXC=XARY(1)
      EXYC=YARY(1)
      EXXF=1.0
      EXYF=1.0
      EXTL=0.0001
      CALL UGOPTION(OPTN,DESC_NUM,INST,IFLAG,EXST,EXSTR)
      EXAG=EXAG/57.2957795
      EXXC=EXXF*EXXC
      EXYC=EXYF*EXYC
      SINA=SIN(EXAG)
      COSA=COS(EXAG)
C
C  BRACKET THE REGION.
      MINL=999999
      MAXL=-999999
      IF (NPTS.LT.3) GO TO 401
      IF ((ABS(XARY(1)-XARY(NPTS)).GT.EXTL).OR.
     X    (ABS(YARY(1)-YARY(NPTS)).GT.EXTL)) GO TO 401
      DO 101 INT1=1,NPTS
        TEM1=(COSA*(EXYF*YARY(INT1)-EXYC)-
     X        SINA*(EXXF*XARY(INT1)-EXXC))/EXSP
        MINL=MIN(INT(TEM1),MINL)
        MAXL=MAX(INT(TEM1),MAXL)
  101 CONTINUE
C
C  PRODUCE THE CROSS HATCHING.
      AFLG=.TRUE.
C    GENERATE THE INTERSECTIONS.
      DO 206 INT1=MINL,MAXL
        IPTS=0
        TEM1=EXSP*REAL(INT1)
        CONX=EXXC-SINA*TEM1
        CONY=EXYC+COSA*TEM1
        COND=COSA*CONY-SINA*CONX
        DO 201 INT2=2,NPTS
          IF (INT2.EQ.2) THEN
            DST1=SINA*EXXF*XARY(1)-COSA*EXYF*YARY(1)+COND
          ELSE
            DST1=DST2
          END IF
          IF (INT2.EQ.NPTS) THEN
            INT3=1
          ELSE
            INT3=INT2
          END IF
          DST2=SINA*EXXF*XARY(INT3)-COSA*EXYF*YARY(INT3)+COND
          IF (.NOT.(((DST1.LT.0.0).AND.(DST2.LT.0.0)).OR.
     X              ((DST1.GE.0.0).AND.(DST2.GE.0.0)))) THEN
            TEM1=ABS(DST1)+ABS(DST2)
            PNTX=(ABS(DST1)*EXXF*XARY(INT3)+
     X            ABS(DST2)*EXXF*XARY(INT2-1))/TEM1
            PNTY=(ABS(DST1)*EXYF*YARY(INT3)+
     X            ABS(DST2)*EXYF*YARY(INT2-1))/TEM1
            PARM=COSA*(PNTX-CONX)+SINA*(PNTY-CONY)
            IPTS=IPTS+1
            IF (IPTS.GT.LDIM) GO TO 402
            WKAR(IPTS)=PARM
          END IF
  201   CONTINUE
C    SORT THE POINTS.
        IF (IPTS.GE.2) THEN
  202     SFLG=.FALSE.
          DO 204 INT2=2,IPTS
            IF (AFLG) THEN
              IF (WKAR(INT2-1).GE.WKAR(INT2)) GO TO 203
            ELSE
              IF (WKAR(INT2-1).LE.WKAR(INT2)) GO TO 203
            END IF
            SFLG=.TRUE.
            TEM1=WKAR(INT2-1)
            WKAR(INT2-1)=WKAR(INT2)
            WKAR(INT2)=TEM1
  203       CONTINUE
  204     CONTINUE
          IF (SFLG) GO TO 202
C    OUTPUT THE POINTS.
          DO 205 INT2=2,IPTS,2
            IF (ABS(WKAR(INT2-1)-WKAR(INT2)).GT.EXTL) THEN
              CALL LSUB((COSA*WKAR(INT2-1)+CONX)/EXXF,
     X                  (SINA*WKAR(INT2-1)+CONY)/EXYF,0)
              CALL LSUB((COSA*WKAR(INT2)+CONX)/EXXF,
     X                  (SINA*WKAR(INT2)+CONY)/EXYF,1)
            END IF
  205     CONTINUE
        END IF
        AFLG=.NOT.AFLG
  206 CONTINUE
C
C  RESET ERROR INDICATORS AND RETURN TO CALLER.
      UGELV=0
      UGENM='        '
      UGEIX=0
  301 RETURN
C
C  REPORT ERRORS TO THE UNIFIED GRAPHICS SYSTEM ERROR PROCESSOR.
  401 CALL UGRERR(3,'UGXHCH  ',1)
      GO TO 301
  402 CALL UGRERR(3,'UGXHCH  ',2)
      GO TO 301
C
      END
