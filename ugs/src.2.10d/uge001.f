      SUBROUTINE UGE001(FGDR,FGEQ,FGSC,FGMS,XCTR,YCTR,CSIZ,CANG,YFAC,
     X                  DATA,ERFG)
C
C *******************  THE UNIFIED GRAPHICS SYSTEM  *******************
C *                 CHARACTER STROKE GENERATOR MODULE                 *
C *                                                                   *
C *  THIS IS THE FIRST OF TWO SUBROUTINES THAT MAY BE USED TO BREAK   *
C *  A PRIMARY/SECONDARY PAIR OF CHARACTER STRINGS DOWN INTO          *
C *  INDIVIDUAL STROKES.  TWO DISTINCT OPERATIONS MAY BE PERFORMED:   *
C *    1.  GENERATE THE ACTUAL STROKE END POINTS AND RETURN THEM.     *
C *    2.  SCAN THE CHARACTERS AND RETURN THE COORDINATES OF THE END  *
C *        OF THE STRING.  THE END OF STRING MAY BE:                  *
C *        A.  THE CENTER OF THE LAST CHARACTER.                      *
C *        B.  THE CENTER OF THE NEXT CHARACTER.                      *
C *  SUBROUTINE UGE001 IS USED TO INITIALIZE THE PROCESS FOR A NEW    *
C *  PAIR OF STRINGS.  SUBROUTINE UGE002 IS USED TO OBTAIN THE NEXT   *
C *  STROKE END POINT.  IF THE STROKES ARE ONLY BEING SCANNED,        *
C *  UGE002 NEED ONLY BE CALLED ONCE FOLLOWING THE CALL TO UGE001.    *
C *                                                                   *
C *  THIS SUBROUTINE MAY BE USED TO INITIALIZE THE CHARACTER STROKE   *
C *  GENERATOR MODULE.                                                *
C *                                                                   *
C *  THE CALLING SEQUENCE IS:                                         *
C *    CALL UGE001(FGDR,FGEQ,FGSC,FGMS,XCTR,YCTR,CSIZ,CANG,YFAC,      *
C *                DATA,ERFG)                                         *
C *                                                                   *
C *  THE PARAMETERS IN THE CALLING SEQUENCE ARE:                      *
C *    FGDR  DRAW FLAG: 0 MEANS GENERATE STROKES; 1 MEANS SCAN/LAST;  *
C *          AND 2 MEANS SCAN/NEXT.                                   *
C *    FGEQ  EQUAL SPACING FLAG: 0 MEANS EQUAL SPACING; AND 1 MEANS   *
C *          PROPORTIONAL SPACING IF POSSIBLE.                        *
C *    FGSC  SPECIAL CHARACTER FLAG: 0 MEANS DO NOT PROCESS           *
C *          SUPERSCRIPT, SUBSCRIPT, ETC. CONTROL CHARACTERS; 1       *
C *          MEANS PROCESS THESE CHARACTERS.                          *
C *    FGMS  MISSING SECONDARY CHARACTERS FLAG: 0 MEANS CHRS WILL BE  *
C *          A DUMMY ARGUMENT; 1 MEANS IT WILL BE GIVEN.              *
C *    XCTR  THE X COORDINATE OF THE CENTER OF THE FIRST CHARACTER.   *
C *    YCTR  THE Y COORDINATE OF THE CENTER OF THE FIRST CHARACTER.   *
C *    CSIZ  THE SIZE OF THE CHARACTERS.                              *
C *    CANG  THE ANGLE THE CHARACTERS MAKE WITH THE HORIZONTAL.       *
C *    YFAC  THE Y FACTOR MULTIPLIER FOR THE OUTPUT.                  *
C *    DATA  THE BLOCK OF DATA DEFINING THE CHARACTER SET.            *
C *    ERFG  A FLAG WHICH WILL BE NON-ZERO IF DATA IS INVALID.        *
C *                                                                   *
C *                          ROBERT C. BEACH                          *
C *                    COMPUTATION RESEARCH GROUP                     *
C *                STANFORD LINEAR ACCELERATOR CENTER                 *
C *                                                                   *
C *********************************************************************
C
      INTEGER       FGDR,FGEQ,FGSC,FGMS
      REAL          XCTR,YCTR,CSIZ,CANG,YFAC
      INTEGER*2     DATA(*)
      INTEGER       ERFG
C
      INCLUDE       'UGSYSTEM:UGE00CBK.FOR'
C
C  INITIALIZE THE CHARACTER STROKE GENERATOR MODULE.
      NCHR=DATA(2)
      IF (NCHR.LE.0) THEN
        ERFG=1
      ELSE
        IFDR=FGDR
        IFEQ=FGEQ
        IFSC=FGSC
        IFMS=FGMS
        CHRO(1)=XCTR
        CHRO(2)=YCTR
        SIZE=CSIZ
        SNCS(1)=SIN(CANG/57.2957795)
        SNCS(2)=COS(CANG/57.2957795)
        XYFC=YFAC
        CSPC=REAL(DATA(4)+0)
        STEP=SIZE/CSPC
        XSZC=1.0
        SAV1(1)=CHRO(1)
        SAV1(2)=CHRO(2)
        SAV1(3)=STEP
        SAV1(4)=XSZC
        SAV2(1)=CHRO(1)
        SAV2(2)=CHRO(2)
        SAV2(3)=STEP
        SAV2(4)=XSZC
        SAV3(1)=CHRO(1)
        SAV3(2)=CHRO(2)
        SAV3(3)=STEP
        SAV3(4)=XSZC
        SAV4(1)=CHRO(1)
        SAV4(2)=CHRO(2)
        SAV4(3)=STEP
        SAV4(4)=XSZC
        ICHR=1
        PTOT=5
        PTST=PTOT+NCHR
        PTCT=PTST+DATA(3)
        ISTR=1
        NSTR=0
        ERFG=0
      END IF
C
C  RETURN TO CALLING SUBROUTINE.
      RETURN
C
      END
