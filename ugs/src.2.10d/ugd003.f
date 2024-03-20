      SUBROUTINE UGD003(BBIT,XCRD,YCRD)
C
C *******************  THE UNIFIED GRAPHICS SYSTEM  *******************
C *                       LINE STRUCTURE MODULE                       *
C *                                                                   *
C *  THIS SUBROUTINE MAY BE USED TO RETRIEVE A POINT FROM THE LINE    *
C *  STRUCTURE MODULE.                                                *
C *                                                                   *
C *  THE CALLING SEQUENCE IS:                                         *
C *    CALL UGD003(BBIT,XCRD,YCRD)                                    *
C *                                                                   *
C *  THE PARAMETERS IN THE CALLING SEQUENCE ARE:                      *
C *    BBIT  THE BLANKING BIT OR TERMINATION FLAG: 0 MEANS MOVE       *
C *          WITHOUT DRAWING, 1 MEANS DRAW, 2 MEANS DRAW A POINT,     *
C *          AND -1 MEANS NO MORE DATA IS AVAILABLE.                  *
C *    XCRD  THE X COORDINATE OF A LINE END POINT OR A POINT.         *
C *    YCRD  THE Y COORDINATE OF A LINE END POINT OR A POINT.         *
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
      INTEGER       INT1
C
C  CHECK TO SEE IF ANY DATA IS AVAILABLE.
      IF (FGAV.EQ.0) THEN
        BBIT=-1
        GO TO 201
      END IF
C
C  PROCESS DATA ACCORDING TO LINE STRUCTURE AND LAST OPERATION.
      INT1=3*FGLO+IFLS
      GO TO (101,108,110,
     X       103,202,111,
     X       106,202,112,
     X       202,109,113), INT1
C  DASHED LINE, BLANK VECTOR SUPPLIED.
  101 FGLO=1
      BBIT=0
  102 FGAV=0
      XCRD=PNT2(1)
      YCRD=PNT2(2)
      GO TO 201
C  DASHED LINE, DREW BLANK.
  103 IF ((TVAL+SZDS).GT.TVL2) GO TO 105
      TVAL=TVAL+SZDS
      FGLO=2
      BBIT=1
  104 XCRD=DELX*(TVAL-TVL1)+PNT1(1)
      YCRD=DELY*(TVAL-TVL1)+PNT1(2)
      GO TO 201
  105 BBIT=1
      GO TO 102
C  DASHED LINE, DREW DASH.
  106 IF ((TVAL+SZBK).GT.TVL2) GO TO 107
      TVAL=TVAL+SZBK
      FGLO=1
      BBIT=0
      GO TO 104
  107 FGAV=0
      BBIT=-1
      GO TO 201
C  DOTTED LINE, BLANK VECTOR SUPPLIED.
  108 FGLO=3
      BBIT=2
      GO TO 102
C  DOTTED LINE, DREW POINT.
  109 IF ((TVAL+SZBK).GT.TVL2) GO TO 107
      TVAL=TVAL+SZBK
      BBIT=2
      GO TO 104
C  DOTDASH LINE, BLANK VECTOR SUPPLIED.
  110 FGLO=1
      BBIT=0
      GO TO 102
C  DOTDASH LINE, DREW BLANK.
  111 IF ((TVAL+SZDS).GT.TVL2) GO TO 105
      TVAL=TVAL+SZDS
      FGLO=2
      BBIT=1
      GO TO 104
C  DOTDASH LINE, DREW DASH.
  112 IF ((TVAL+SZBK).GT.TVL2) GO TO 107
      TVAL=TVAL+SZBK
      FGLO=3
      BBIT=2
      GO TO 104
C  DOTDASH LINE, DREW POINT.
  113 IF ((TVAL+SZBK).GT.TVL2) GO TO 107
      TVAL=TVAL+SZBK
      FGLO=1
      BBIT=0
      GO TO 104
C
C  RETURN TO CALLING SUBROUTINE.
  201 RETURN
C  TERMINATE ON IMPOSSIBLE TRANSFER.
  202 CALL UGZ001
      GO TO 201
C
      END
