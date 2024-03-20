      SUBROUTINE UGCNVF(NUMB,NDEC,STRG,NLEN)
C
C *******************  THE UNIFIED GRAPHICS SYSTEM  *******************
C *       CONVERT A FLOATING POINT NUMBER TO A CHARACTER STRING       *
C *                                                                   *
C *  THIS SUBROUTINE MAY BE CALLED TO CONVERT A FLOATING POINT        *
C *  NUMBER TO A CHARACTER STRING.  THE LENGTH OF THE STRING IS       *
C *  LIMITED TO 12 AND THE NUMBER OF DIGITS TO THE RIGHT OF THE       *
C *  DECIMAL POINT IS LIMITED TO 10.  IF AN ERROR OCCURS, THE STRING  *
C *  IS SET TO ASTERISKS.                                             *
C *                                                                   *
C *  THE CALLING SEQUENCE IS:                                         *
C *    CALL UGCNVF(NUMB,NDEC,STRG,NLEN)                               *
C *                                                                   *
C *  THE PARAMETERS IN THE CALLING SEQUENCE ARE:                      *
C *    NUMB  THE NUMBER TO BE CONVERTED.                              *
C *    NDEC  THE NUMBER OF DECIMALS IN THE CONVERTED NUMBER.          *
C *    STRG  THE CHARACTER STRING WHICH WILL CONTAIN THE CONVERTED    *
C *          NUMBER.                                                  *
C *    NLEN  THE NUMBER OF NON-BLANK CHARACTERS IN THE CONVERTED      *
C *          NUMBER.                                                  *
C *                                                                   *
C *                          ROBERT C. BEACH                          *
C *                    COMPUTATION RESEARCH GROUP                     *
C *                STANFORD LINEAR ACCELERATOR CENTER                 *
C *                                                                   *
C *********************************************************************
C
      REAL          NUMB
      INTEGER       NDEC
      CHARACTER*(*) STRG
      INTEGER       NLEN
C
      INTEGER       FWID
      CHARACTER*1   DIGT(10)
      REAL          MULT(22)
      REAL          VALU,DIVZ
      INTEGER       QUOT
C
      INTEGER       INT1,INT2
C
      DATA          DIGT/'0','1','2','3','4','5','6','7','8','9'/
      DATA          MULT/ 1E+11, 1E+10, 1E+09, 1E+08, 1E+07,
     X                    1E+06, 1E+05, 1E+04, 1E+03, 1E+02,
     X                    1E+01, 1E+00, 1E-01, 1E-02, 1E-03,
     X                    1E-04, 1E-05, 1E-06, 1E-07, 1E-08,
     X                    1E-09, 1E-10/
C
C  VERIFY THAT FWID AND NDEC ARE VALID.
      FWID=LEN(STRG)
      IF (FWID.GT.12) GO TO 501
      IF (NDEC.GT.10) GO TO 501
      IF (NDEC.LT.0) GO TO 501
      IF (NDEC.GE.FWID) GO TO 501
C
C  COMPUTE THE RAW DIGITS.
      VALU=ABS(NUMB)+0.5*MULT(NDEC+12)
      DO 101 INT1=1,FWID
        DIVZ=MULT(INT1+12-FWID+NDEC)
        QUOT=INT(VALU/DIVZ)
        IF (QUOT.GT.9) GO TO 501
        STRG(INT1:INT1)=DIGT(QUOT+1)
        VALU=VALU-REAL(QUOT)*DIVZ
  101 CONTINUE
C
C  SET LEADING ZEROS TO BLANKS.
      NLEN=FWID
      INT1=FWID-NDEC-1
      IF (INT1.GE.1) THEN
        DO 201 INT2=1,INT1
          IF (STRG(INT2:INT2).NE.'0') GO TO 301
          STRG(INT2:INT2)=' '
          NLEN=NLEN-1
  201   CONTINUE
      END IF
C
C  INSERT DECIMAL POINT IF NECESSARY.
  301 IF (NDEC.GT.0) THEN
        NLEN=NLEN+1
        IF (STRG(1:1).NE.' ') GO TO 501
        INT1=FWID-NDEC-1
        IF (INT1.LT.1) GO TO 501
        DO 302 INT2=1,INT1
          STRG(INT2:INT2)=STRG(INT2+1:INT2+1)
  302   CONTINUE
        STRG(INT1+1:INT1+1)='.'
      END IF
C
C  INSERT MINUS SIGN IF NECESSARY.
      IF (NUMB.LT.0.0) THEN
        IF (NLEN.EQ.FWID) GO TO 501
        NLEN=NLEN+1
        STRG(FWID-NLEN+1:FWID-NLEN+1)='-'
      END IF
C
C  RETURN TO CALLING PROGRAM.
  401 RETURN
C
C  ERROR...SET STRING TO ASTERISKS.
  501 DO 502 INT1=1,FWID
        STRG(INT1:INT1)='*'
  502 CONTINUE
      NLEN=FWID
      GO TO 401
C
      END
