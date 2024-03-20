      SUBROUTINE UGLGAX(OPTN,LSUB,TSUB,TFLG,XCLO,YCLO,XCHI,YCHI,
     X                  LOLB,HILB,NLAB)
C
C *******************  THE UNIFIED GRAPHICS SYSTEM  *******************
C *                    AXIS GENERATION SUBROUTINE                     *
C *                                                                   *
C *  THIS SUBROUTINE MAY BE USED TO GENERATE A DESCRIPTION OF AN      *
C *  AXIS WITH LOGARITHMIC LABELING.  A PAIR OF USER SUPPLIED         *
C *  SUBROUTINES ARE CALLED TO PROCESS THE LINE SEGMENT END POINTS    *
C *  AND THE TEXT.                                                    *
C *                                                                   *
C *  THE CALLING SEQUENCE IS:                                         *
C *    CALL UGLGAX(OPTN,LSUB,TSUB,TFLG,XCLO,YCLO,XCHI,YCHI,           *
C *                LOLB,HILB,NLAB)                                    *
C *                                                                   *
C *  THE PARAMETERS IN THE CALLING SEQUENCE ARE:                      *
C *    OPTN  THE OPTIONS LIST.                                        *
C *    LSUB  THE LINE SEGMENT END POINT SUBROUTINE.                   *
C *    TSUB  THE LABEL SUBROUTINE.                                    *
C *    TFLG  A FLAG THAT IS PASSED TO TSUB.                           *
C *    XCLO  X COORDINATE OF THE LOW END OF THE AXIS.                 *
C *    YCLO  Y COORDINATE OF THE LOW END OF THE AXIS.                 *
C *    XCHI  X COORDINATE OF THE HIGH END OF THE AXIS.                *
C *    YCHI  Y COORDINATE OF THE HIGH END OF THE AXIS.                *
C *    LOLB  DATA VALUE AT THE LOW END OF THE AXIS.                   *
C *    HILB  DATA VALUE AT THE HIGH END OF THE AXIS.                  *
C *    NLAB  NUMBER OF LABELS ON THE AXIS.                            *
C *                                                                   *
C *                          ROBERT C. BEACH                          *
C *                    COMPUTATION RESEARCH GROUP                     *
C *                STANFORD LINEAR ACCELERATOR CENTER                 *
C *                                                                   *
C *********************************************************************
C
      CHARACTER*(*) OPTN
      EXTERNAL      LSUB,TSUB
      INTEGER       TFLG(*)
      REAL          XCLO,YCLO,XCHI,YCHI,LOLB,HILB
      INTEGER       NLAB
C
      INCLUDE       'UGSYSTEM:UGERRCBK.FOR'
C
      INTEGER*4     EXST(3),EXNT
      REAL*4        EXLS,EXRS
      EQUIVALENCE   (EXNT,EXST(1)),      (EXLS,EXST(2)),
     X              (EXRS,EXST(3))
C
      REAL          DLAX,DLAY
      REAL          DLTX,DLTY
      REAL          DLBX,DLBY,DLBM,DLBL
      REAL          XCRD,YCRD,VALU
      REAL          XCDS,YCDS
C
      REAL          FLT1
      INTEGER       INT1,INT2
      INTEGER       DESC_NUM
      PARAMETER     (DESC_NUM = 3)
      INTEGER*4     INST(4,DESC_NUM)
      CHARACTER*16  IFLAG(DESC_NUM)
C
      DATA   (INST(I,1),I=1,4) / 2,4,1,0 /, IFLAG(1) / 'NSTM' /
      DATA   (INST(I,2),I=1,4) / 3,4,2,0 /, IFLAG(2) / 'LSTM' /
      DATA   (INST(I,3),I=1,4) / 3,4,3,0 /, IFLAG(3) / 'RSTM' /
C
C  SCAN THE OPTIONS LIST.
      EXNT=0
      EXLS=0.01*MAX(ABS(XCHI-XCLO),ABS(YCHI-YCLO))
      EXRS=EXLS
      CALL UGOPTION(OPTN,DESC_NUM,INST,IFLAG,EXST,EXSTR)
C
C  DRAW THE AXIS STARTING AT THE LOW END.
      IF (NLAB.LT.2) GO TO 301
      CALL LSUB(XCLO,YCLO,0)
      CALL LSUB(XCHI,YCHI,1)
C
C  INITIALIZE THE TIC MARK AND LABEL LOOP.
      DLAX=XCHI-XCLO
      DLAY=YCHI-YCLO
      FLT1=SQRT(DLAX*DLAX+DLAY*DLAY)
      DLTX=DLAY/FLT1
      DLTY=-DLAX/FLT1
      FLT1=REAL(NLAB-1)
      DLBX=DLAX/FLT1
      DLBY=DLAY/FLT1
      DLBM=LOG10(HILB)
      DLBL=(DLBM-LOG10(LOLB))/FLT1
C
C  LOOP TO GENERATE THE TIC MARKS AND LABELS.
      DO 102 INT1=1,NLAB
C  GENERATE TIC MARK POSITION.
        FLT1=REAL(INT1-1)
        XCRD=XCHI-FLT1*DLBX
        YCRD=YCHI-FLT1*DLBY
C  GENERATE A LABEL.
        VALU=10**(DLBM-DLBL*FLT1)
        CALL TSUB(XCRD,YCRD,VALU,TFLG)
C  GENERATE A PRIMARY TIC MARK.
        CALL LSUB(XCRD+EXRS*DLTX,YCRD+EXRS*DLTY,0)
        CALL LSUB(XCRD-EXLS*DLTX,YCRD-EXLS*DLTY,1)
C  GENERATE SECONDARY TIC MARKS.
        IF ((INT1.NE.NLAB).AND.(EXNT.GE.1)) THEN
          DO 101 INT2=1,EXNT
            FLT1=LOG10(1.0-(REAL(INT2)/REAL(EXNT+1)))
            XCDS=XCRD+FLT1*DLBX
            YCDS=YCRD+FLT1*DLBY
            CALL LSUB(XCDS+0.75*EXRS*DLTX,YCDS+0.75*EXRS*DLTY,2)
            CALL LSUB(XCDS-0.75*EXLS*DLTX,YCDS-0.75*EXLS*DLTY,3)
  101     CONTINUE
        END IF
  102 CONTINUE
C
C  RESET ERROR INDICATORS AND RETURN TO CALLER.
      UGELV=0
      UGENM='        '
      UGEIX=0
  201 RETURN
C
C  REPORT ERRORS TO THE UNIFIED GRAPHICS SYSTEM ERROR PROCESSOR.
  301 CALL UGRERR(3,'UGLGAX  ',1)
      GO TO 201
C
      END
