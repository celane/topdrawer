      SUBROUTINE UGE003(XCRD,YCRD)
C
C *******************  THE UNIFIED GRAPHICS SYSTEM  *******************
C *                 CHARACTER STROKE GENERATOR MODULE                 *
C *                                                                   *
C *  THIS SUBROUTINE IS USED BY THE CHARACTER STROKE GENERATOR TO     *
C *  TRANSFORM POINTS.  THE RESULT OF THE TRANSFORMATION IS SAVED IN  *
C *  A COMMON BLOCK THAT IS SHARED BY THE OTHER SUBROUTINES IN THE    *
C *  CHARACTER STROKE GENERATOR MODULE.                               *
C *                                                                   *
C *  THE CALLING SEQUENCE IS:                                         *
C *    CALL UGE003(XCRD,YCRD)                                         *
C *                                                                   *
C *  THE PARAMETERS IN THE CALLING SEQUENCE ARE:                      *
C *    XCRD  THE X COORDINATE OF THE POINT TO BE TRANSFORMED.         *
C *    YCRD  THE Y COORDINATE OF THE POINT TO BE TRANSFORMED.         *
C *                                                                   *
C *                          ROBERT C. BEACH                          *
C *                    COMPUTATION RESEARCH GROUP                     *
C *                STANFORD LINEAR ACCELERATOR CENTER                 *
C *                                                                   *
C *********************************************************************
C
      REAL          XCRD,YCRD
C
      INCLUDE       'UGSYSTEM:UGE00CBK.FOR'
C
C  TRANSFORM THE POINT.
      CHRO(1)=     (SNCS(2)*XCRD-SNCS(1)*YCRD)+CHRO(1)
      CHRO(2)=XYFC*(SNCS(1)*XCRD+SNCS(2)*YCRD)+CHRO(2)
C
C  RETURN TO CALLING SUBROUTINE.
      RETURN
C
      END
