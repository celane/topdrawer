      SUBROUTINE UGTRAN(OPTN,REFP,VDIR,HDIR,UDIR,SCRD,SCRZ,TRAN)
C
C *******************  THE UNIFIED GRAPHICS SYSTEM  *******************
C *          DEFINE A PROJECTIVE OR PARALLEL TRANSFORMATION           *
C *                                                                   *
C *  THIS SUBROUTINE MAY BE USED TO DEFINE A PROJECTIVE OR            *
C *  PARALLEL TRANSFORMATION FROM 3-SPACE INTO 2-SPACE.  SUBROUTINE   *
C *  UGPROJ USE THE TRANSFORMATION DEFINED BY THIS SUBROUTINE.        *
C *                                                                   *
C *  THE CALLING SEQUENCE IS:                                         *
C *    CALL UGTRAN(OPTN,REFP,VDIR,HDIR,UDIR,SCRD,SCRZ,TRAN)           *
C *                                                                   *
C *  THE PARAMETERS IN THE CALLING SEQUENCE ARE:                      *
C *    OPTN  THE OPTIONS LIST.                                        *
C *    REFP  THE REFERENCE POINT.                                     *
C *    VDIR  THE VIEWING DIRECTION.                                   *
C *    HDIR  THE "HORIZONTAL" DIRECTION.                              *
C *    UDIR  THE "UPWARD" DIRECTION.                                  *
C *    SCRD  THE SCREEN DISTANCE.                                     *
C *    SCRZ  THE SCREEN SIZE.                                         *
C *    TRAN  THE TRANSFORMATION.                                      *
C *                                                                   *
C *                          ROBERT C. BEACH                          *
C *                    COMPUTATION RESEARCH GROUP                     *
C *                STANFORD LINEAR ACCELERATOR CENTER                 *
C *                                                                   *
C *********************************************************************
C

* 19940803	KREYMER@FNAL.GOV
*   removed unused VCT2(3) variable

      CHARACTER*(*) OPTN
      REAL          REFP(3),VDIR(3),HDIR(3),UDIR(3)
      REAL          SCRD,SCRZ
      REAL          TRAN(31)
C
      INCLUDE       'UGSYSTEM:UGERRCBK.FOR'
C
      INTEGER*4     EXST(7),EXPL
      REAL*4        EXEY,EXXL,EXXH,EXYL,EXYH,EXTL
      EQUIVALENCE   (EXPL,EXST(1)),       (EXEY,EXST(2)),
     X              (EXXL,EXST(3)),       (EXXH,EXST(4)),
     X              (EXYL,EXST(5)),       (EXYH,EXST(6)),
     X              (EXTL,EXST(7))
C
      REAL          TTRN(31)
      REAL          VCT1(3)
      REAL          MAT1(3,3),MAT2(3,3)
      REAL          RECT(3,5)
C
      REAL          FLT1,FLT2,FLT3,FLT4,FLT5,FLT6
      INTEGER       INT1,INT2
C
      INTEGER       DESC_NUM
      PARAMETER    (DESC_NUM = 7)
      INTEGER*4     INST(4,DESC_NUM)
      CHARACTER*16  IFLAG(DESC_NUM)
C
      DATA   (INST(I,1),I=1,4) / 1,8,1,1 /, IFLAG(1) / 'PARALLEL' /
      DATA   (INST(I,2),I=1,4) / 3,4,2,0 /, IFLAG(2) / 'EYED' /
      DATA   (INST(I,3),I=1,4) / 3,3,3,0 /, IFLAG(3) / 'XLO' /
      DATA   (INST(I,4),I=1,4) / 3,3,4,0 /, IFLAG(4) / 'XHI' /
      DATA   (INST(I,5),I=1,4) / 3,3,5,0 /, IFLAG(5) / 'YLO' /
      DATA   (INST(I,6),I=1,4) / 3,3,6,0 /, IFLAG(6) / 'YHI' /
      DATA   (INST(I,7),I=1,4) / 3,5,7,0 /, IFLAG(7) / 'TOLER' /
C
C  SCAN THE OPTIONS LIST.
      EXPL=0
      EXEY=0.0
      EXXL=0.0
      EXXH=1.0
      EXYL=0.0
      EXYH=1.0
      EXTL=0.0001
      CALL UGOPTION(OPTN,DESC_NUM,INST,IFLAG,EXST,EXSTR)
      IF (EXPL.NE.0) EXEY=0.0
C
C  MOVE THE INPUT DATA TO THE INTERNAL ARRAY.
      DO 101 INT1=1,3
        TTRN(INT1+12)=REFP(INT1)
        TTRN(INT1+15)=HDIR(INT1)
        TTRN(INT1+18)=UDIR(INT1)
        TTRN(INT1+21)=VDIR(INT1)
  101 CONTINUE
      TTRN(25)=EXEY
      TTRN(26)=SCRD
      TTRN(27)=SCRZ
      TTRN(28)=EXXL
      TTRN(29)=EXXH
      TTRN(30)=EXYL
      TTRN(31)=EXYH
C
C  GENERATE THE TRIHEDRAL.
      IF ((ABS(TTRN(16))+ABS(TTRN(17))+ABS(TTRN(18))).EQ.0.0) THEN
        VCT1(1)=0.0
        VCT1(2)=0.0
        VCT1(3)=1.0
        CALL UGTRN1(TTRN(22),VCT1,TTRN(16))
      END IF
      IF ((ABS(TTRN(19))+ABS(TTRN(20))+ABS(TTRN(21))).EQ.0.0) THEN
        CALL UGTRN1(TTRN(16),TTRN(22),TTRN(19))
      ELSE
        CALL UGTRN1(TTRN(19),TTRN(16),VCT1)
        CALL UGTRN1(TTRN(16),VCT1,TTRN(19))
      END IF
      DO 201 INT1=16,22,3
        FLT1=SQRT(TTRN(INT1  )*TTRN(INT1  )+
     X            TTRN(INT1+1)*TTRN(INT1+1)+
     X            TTRN(INT1+2)*TTRN(INT1+2))
        IF (FLT1.LT.EXTL) GO TO 801
        TTRN(INT1)=TTRN(INT1)/FLT1
        TTRN(INT1+1)=TTRN(INT1+1)/FLT1
        TTRN(INT1+2)=TTRN(INT1+2)/FLT1
  201 CONTINUE
C
C  DO COMMON PROCESSING FOR THE TRANSFORMATIONS.
      CALL UGTRN2(TTRN(16),MAT1,EXTL,INT1)
      IF (INT1.EQ.0) GO TO 801
      FLT1=TTRN(27)/(TTRN(29)-TTRN(28))
      FLT2=TTRN(27)/(TTRN(31)-TTRN(30))
      FLT3=0.5*(TTRN(29)+TTRN(28))
      FLT4=0.5*(TTRN(31)+TTRN(30))
      FLT5=(FLT1*FLT3+TTRN(25))/(FLT1*TTRN(26))
      FLT6=FLT4/TTRN(26)
      DO 301 INT1=1,3
        MAT2(INT1,1)=MAT1(1,INT1)/FLT1+MAT1(3,INT1)*FLT5
        MAT2(INT1,2)=MAT1(2,INT1)/FLT2+MAT1(3,INT1)*FLT6
        MAT2(INT1,3)=MAT1(3,INT1)/TTRN(26)
  301 CONTINUE
      IF (EXPL.NE.0) GO TO 501
C
C  GENERATE THE PROJECTIVE TRANSFORMATION.
      DO 401 INT1=1,3
        TTRN(INT1)=MAT2(INT1,1)
        TTRN(INT1+4)=MAT2(INT1,2)
        TTRN(INT1+8)=MAT2(INT1,3)
        VCT1(INT1)=TTRN(INT1+12)+TTRN(25)*TTRN(INT1+15)
  401 CONTINUE
      DO 402 INT1=1,9,4
        TTRN(INT1+3)=-TTRN(INT1)*VCT1(1)-TTRN(INT1+1)*VCT1(2)
     X    -TTRN(INT1+2)*VCT1(3)
  402 CONTINUE
      GO TO 601
C
C  GENERATE THE PARALLEL TRANSFORMATION.
  501 DO 502 INT1=1,3
        RECT(INT1,1)=MAT2(1,INT1)
        RECT(INT1,2)=MAT2(2,INT1)
        RECT(INT1,3)=MAT2(3,INT1)
        RECT(INT1,4)=TTRN(22)*MAT2(1,INT1)+
     X               TTRN(23)*MAT2(2,INT1)+
     X               TTRN(24)*MAT2(3,INT1)
        RECT(INT1,5)=-TTRN(13)*MAT2(1,INT1)
     X               -TTRN(14)*MAT2(2,INT1)
     X               -TTRN(15)*MAT2(3,INT1)
  502 CONTINUE
      IF (ABS(RECT(3,4)).LT.EXTL) GO TO 801
      DO 503 INT1=1,2
        INT2=4*INT1
        TTRN(INT2-3)=RECT(INT1,1)*RECT(3,4)-RECT(INT1,4)*RECT(3,1)
        TTRN(INT2-2)=RECT(INT1,2)*RECT(3,4)-RECT(INT1,4)*RECT(3,2)
        TTRN(INT2-1)=RECT(INT1,3)*RECT(3,4)-RECT(INT1,4)*RECT(3,3)
        TTRN(INT2)=RECT(INT1,5)*RECT(3,4)-RECT(INT1,4)*(RECT(3,5)-1.0)
  503 CONTINUE
      TTRN(9)=0.0
      TTRN(10)=0.0
      TTRN(11)=0.0
      TTRN(12)=RECT(3,4)
C
C  PUT COMPUTED INFORMATION IN THE OUTPUT ARRAY.
  601 DO 602 INT1=1,31
        TRAN(INT1)=TTRN(INT1)
  602 CONTINUE
C
C  RESET ERROR INDICATORS AND RETURN TO CALLER.
      UGELV=0
      UGENM='        '
      UGEIX=0
  701 RETURN
C
C  REPORT ERRORS TO THE UNIFIED GRAPHICS SYSTEM ERROR PROCESSOR.
  801 CALL UGRERR(3,'UGTRAN  ', 1)
      GO TO 701
C
      END
