      SUBROUTINE UGC003(BBIT,XCRD,YCRD)
C
C *******************  THE UNIFIED GRAPHICS SYSTEM  *******************
C *               LINE SCISSORING AND SHIELDING MODULE                *
C *                                                                   *
C *  THIS SUBROUTINE MAY BE USED TO SUPPLY A POINT TO THE LINE        *
C *  SCISSORING AND SHIELDING MODULE.                                 *
C *                                                                   *
C *  THE CALLING SEQUENCE IS:                                         *
C *    CALL UGC003(BBIT,XCRD,YCRD)                                    *
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
      INCLUDE       'UGSYSTEM:UGC00CBK.FOR'
C
      REAL          TSHD(2,2)
      INTEGER       KSHD
      INTEGER       KLIN,LLIN
C
      INTEGER       INT1
C
C  ACCEPT AN END POINT TO BE SCISSORED AND SHIELDED.
      KAVL=0
      NAVL=0
      XPNT(BBIT+1)=XCRD
      YPNT(BBIT+1)=YCRD
      IF (BBIT.EQ.0) THEN
        IFLG=0
        GO TO 201
      END IF
C
C  DO THE ACTUAL SCISSORING AND SHIELDING.
      CALL UGC005(NAVL)
      XPNT(1)=XPNT(2)
      YPNT(1)=YPNT(2)
      IF (NAVL.EQ.0) GO TO 201
      IF (NSHD.GT.0) THEN
        DO 105 KSHD=1,NSHD
          TSHD(1,1)=SHLD(KSHD,1,1)
          TSHD(1,2)=SHLD(KSHD,1,2)
          TSHD(2,1)=SHLD(KSHD,2,1)
          TSHD(2,2)=SHLD(KSHD,2,2)
          DO 103 KLIN=NAVL-1,1,-2
            DO 101 INT1=NAVL,KLIN+2,-1
              XSEG(INT1+2)=XSEG(INT1)
              YSEG(INT1+2)=YSEG(INT1)
  101       CONTINUE
            CALL UGC006(TSHD,KLIN,LLIN)
            IF (LLIN.EQ.4) THEN
              NAVL=NAVL+2
              GO TO 104
            ELSE
              DO 102 INT1=KLIN+2,NAVL
                XSEG(INT1+LLIN-2)=XSEG(INT1+2)
                YSEG(INT1+LLIN-2)=YSEG(INT1+2)
  102         CONTINUE
              NAVL=NAVL+LLIN-2
              IF (NAVL.EQ.0) GO TO 201
            END IF
  103     CONTINUE
  104     CONTINUE
  105   CONTINUE
      END IF
C
C  RETURN TO CALLING SUBROUTINE.
  201 RETURN
C
      END
