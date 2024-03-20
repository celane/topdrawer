      SUBROUTINE UGD002(BBIT,XCRD,YCRD)
C
C *******************  THE UNIFIED GRAPHICS SYSTEM  *******************
C *                       LINE STRUCTURE MODULE                       *
C *                                                                   *
C *  THIS SUBROUTINE MAY BE USED TO SUPPLY A POINT TO THE LINE        *
C *  STRUCTURE MODULE.                                                *
C *                                                                   *
C *  THE CALLING SEQUENCE IS:                                         *
C *    CALL UGD002(BBIT,XCRD,YCRD)                                    *
C *                                                                   *
C *  THE PARAMETERS IN THE CALLING SEQUENCE ARE:                      *
C *    BBIT  THE BLANKING BIT: 0 MEANS MOVE WITHOUT DRAWING AND 1     *
C *          MEANS DRAW.                                              *
C *    XCRD  THE X COORDINATE OF A LINE END POINT.                    *
C *    YCRD  THE Y COORDINATE OF A LINE END POINT.                    *
C *                                                                   *
C *                          ROBERT C. BEACH                          *
C *                    COMPUTATION RESEARCH GROUP                     *
C *                STANFORD LINEAR ACCELERATOR CENTER                 *
C *                                                                   *
C *********************************************************************
C
      INTEGER       BBIT
      REAL          XCRD,YCRD
C
      INCLUDE       'UGSYSTEM:UGD00CBK.FOR'
C
      REAL          FLT1,FLT2,FLT3,FLT4,FLT5
C
C  SUPPLY AN END POINT TO THE LINE STRUCTURE MODULE.
      IF (BBIT.EQ.0) THEN
        PNT2(1)=XCRD
        PNT2(2)=YCRD
        TVL2=0.0
        TVAL=0.0
        FGLO=0
        FGAV=1
      ELSE
        FLT1=XCRD-PNT2(1)
        FLT2=YCRD-PNT2(2)
        FLT3=FLT1*CMUX
        FLT4=FLT2*CMUY
        FLT5=SQRT(FLT3*FLT3+FLT4*FLT4)
        IF (FLT5.GE.0.001) THEN
          PNT1(1)=PNT2(1)
          PNT1(2)=PNT2(2)
          TVL1=TVL2
          PNT2(1)=XCRD
          PNT2(2)=YCRD
          TVL2=TVL1+FLT5
          DELX=FLT1/FLT5
          DELY=FLT2/FLT5
          FGAV=1
        END IF
      END IF
C
C  RETURN TO CALLING SUBROUTINE.
      RETURN
C
      END
