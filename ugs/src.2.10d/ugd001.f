      SUBROUTINE UGD001(FGLS,XCMU,YCMU)
C
C *******************  THE UNIFIED GRAPHICS SYSTEM  *******************
C *                       LINE STRUCTURE MODULE                       *
C *                                                                   *
C *  THIS IS THE FIRST OF THREE SUBROUTINES THAT MAY BE USED TO       *
C *  GENERATE LINE STRUCTURE.  SOLID LINES MAY BE BROKEN DOWN INTO:   *
C *  (1) "DASHED" LINES, (2) "DOTTED" LINES, OR (3) "DOTDASH" LINES.  *
C *  SUBROUTINE UGD001 IS USED TO INITIALIZE THE PROCESS FOR A NEW    *
C *  SERIES OF LINE SEGMENTS, SUBROUTINE UGD002 IS USED TO SUPPLY     *
C *  AN END POINT TO THE LINE STRUCTURE GENERATING MODULES, AND       *
C *  SUBROUTINE UGD003 IS USED TO RETRIEVE A POINT OR LINE SEGMENT    *
C *  END POINT FROM THE LINE STRUCTURE GENERATING MODULES.  THE       *
C *  NORMAL SEQUENCE OF CALLS IS THE FOLLOWING: (1) UGD001 IS CALLED  *
C *  TO INITIALIZE PROCESSING, (2) UGD002 IS CALLED TO SUPPLY AN END  *
C *  POINT TO THE MODULES, (3) UGD003 IS CALLED REPEATEDLY TO         *
C *  RETRIEVE POINTS OR END POINTS OF LINES UNTIL IT SIGNALS THAT NO  *
C *  MORE DATA IS AVAILABLE, AND (4) STEP 2 IS REPEATED UNTIL NO      *
C *  MORE INPUT IS AVAILABLE.                                         *
C *                                                                   *
C *  THIS SUBROUTINE MAY BE USED TO INITIALIZE LINE STRUCTURE         *
C *  GENERATION.                                                      *
C *                                                                   *
C *  THE CALLING SEQUENCE IS:                                         *
C *    CALL UGD001(FGLS,XCMU,YCMU)                                    *
C *                                                                   *
C *  THE PARAMETERS IN THE CALLING SEQUENCE ARE:                      *
C *    FGLS  LINE STRUCTURE FLAG: 1 MEANS "DASHED"; 2 MEANS           *
C *          "DOTTED"; AND 3 MEANS "DOTDASH".                         *
C *    XCMU  CENTIMETERS PER UNIT DISTANCE IN THE X DIRECTION.        *
C *    YCMU  CENTIMETERS PER UNIT DISTANCE IN THE Y DIRECTION.        *
C *                                                                   *
C *                          ROBERT C. BEACH                          *
C *                    COMPUTATION RESEARCH GROUP                     *
C *                STANFORD LINEAR ACCELERATOR CENTER                 *
C *                                                                   *
C *********************************************************************
C
      INTEGER       FGLS
      REAL          XCMU,YCMU
C
      INCLUDE       'UGSYSTEM:UGD00CBK.FOR'
C
C  INITIALIZE THE LINE STRUCTURE MODULE.
      IFLS=FGLS
      CMUX=XCMU
      CMUY=YCMU
      FGAV=0
      IF (IFLS.EQ.1) THEN
        SZDS=0.333333333
        SZBK=0.333333333
      ELSE IF (IFLS.EQ.2) THEN
        SZBK=0.25
      ELSE
        SZDS=0.25
        SZBK=0.25
      END IF
C
C  RETURN TO CALLING SUBROUTINE.
      RETURN
C
      END
