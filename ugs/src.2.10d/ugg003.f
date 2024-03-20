      SUBROUTINE    UGG003(BBIT,XCRD,YCRD,ZCRD)
C
C *******************  THE UNIFIED GRAPHICS SYSTEM  *******************
C *             THREE-DIMENSIONAL LINE SCISSORING MODULE              *
C *                                                                   *
C *  THIS SUBROUTINE MAY BE USED TO SUPPLY A POINT TO THE             *
C *  THREE-DIMENSIONAL LINE SCISSORING MODULE.                        *
C *                                                                   *
C *  THE CALLING SEQUENCE IS:                                         *
C *    CALL UGG003(BBIT,XCRD,YCRD,ZCRD)                               *
C *                                                                   *
C *  THE PARAMETERS IN THE CALLING SEQUENCE ARE:                      *
C *    BBIT  THE BLANKING BIT: 0 MEANS MOVE WITHOUT DRAWING AND 1     *
C *          MEANS DRAW.                                              *
C *    XCRD  THE X COORDINATE OF A LINE END POINT.                    *
C *    YCRD  THE Y COORDINATE OF A LINE END POINT.                    *
C *    ZCRD  THE Z COORDINATE OF A LINE END POINT.                    *
C *                                                                   *
C *                          ROBERT C. BEACH                          *
C *                    COMPUTATION RESEARCH GROUP                     *
C *                STANFORD LINEAR ACCELERATOR CENTER                 *
C *                                                                   *
C *********************************************************************
C
      INTEGER       BBIT
      REAL          XCRD,YCRD,ZCRD
C
      INCLUDE       'UGSYSTEM:UGG00CBK.FOR'
C
C  TEMPORARY X, Y, AND Z COORDINATES.
      REAL          TXCD,TYCD,TZCD
C  BOUNDARY CROSSING FLAGS.
      INTEGER       FLG1,FLG2,FLGT
      REAL          PVL1,PVL2,PVLS
C
C  ACCEPT AN END POINT TO BE SCISSORED.
      KAVL=0
      NAVL=0
      XPNT(BBIT+1)=XCRD
      YPNT(BBIT+1)=YCRD
      ZPNT(BBIT+1)=ZCRD
      IF (BBIT.EQ.0) THEN
        IFLG=0
        GO TO 301
      END IF
C
C  MOVE LINE INTO OUTPUT ARRAY.
      NAVL=0
      XSEG(1)=XPNT(1)
      XSEG(2)=XPNT(2)
      YSEG(1)=YPNT(1)
      YSEG(2)=YPNT(2)
      ZSEG(1)=ZPNT(1)
      ZSEG(2)=ZPNT(2)
C
C  INITIALIZE THE BOUNDARY CROSSING FLAGS.
      IF (ITYP.EQ.0) THEN
        PVL1=EQUN(1)*XSEG(1)+EQUN(2)*YSEG(1)+EQUN(3)*ZSEG(1)+EQUN(4)
        PVL2=EQUN(1)*XSEG(2)+EQUN(2)*YSEG(2)+EQUN(3)*ZSEG(2)+EQUN(4)
      ELSE
        CALL UGG005(LIMS,XSEG(1),YSEG(1),ZSEG(1),FLG1)
        CALL UGG005(LIMS,XSEG(2),YSEG(2),ZSEG(2),FLG2)
      END IF
C
C  NOW DO THE ACTUAL SCISSORING.
      IF (ITYP.EQ.0) THEN
        IF ((PVL1.LT.0.0).OR.(PVL2.LT.0.0)) THEN
          IF ((PVL1.LT.0.0).AND.(PVL2.LT.0.0)) GO TO 201
          PVLS=ABS(PVL1)+ABS(PVL2)
          TXCD=(ABS(PVL2)*XSEG(1)+ABS(PVL1)*XSEG(2))/PVLS
          TYCD=(ABS(PVL2)*YSEG(1)+ABS(PVL1)*YSEG(2))/PVLS
          TZCD=(ABS(PVL2)*ZSEG(1)+ABS(PVL1)*ZSEG(2))/PVLS
          IF (PVL1.LT.0.0) THEN
            XSEG(1)=TXCD
            YSEG(1)=TYCD
            ZSEG(1)=TZCD
          ELSE
            XSEG(2)=TXCD
            YSEG(2)=TYCD
            ZSEG(2)=TZCD
          END IF
        END IF
      ELSE
  101   IF ((FLG1.NE.0).OR.(FLG2.NE.0)) THEN
          IF (IAND(FLG1,FLG2).NE.0) GO TO 201
          IF (FLG1.NE.0) THEN
            FLGT=FLG1
          ELSE
            FLGT=FLG2
          END IF
          IF (IAND(FLGT,1).NE.0) THEN
            TXCD=LIMS(1,1)
            TYCD=((YSEG(2)-YSEG(1))*(TXCD-XSEG(1))/
     X            (XSEG(2)-XSEG(1)))+YSEG(1)
            TZCD=((ZSEG(2)-ZSEG(1))*(TXCD-XSEG(1))/
     X            (XSEG(2)-XSEG(1)))+ZSEG(1)
          ELSE IF (IAND(FLGT,2).NE.0) THEN
            TXCD=LIMS(1,2)
            TYCD=((YSEG(2)-YSEG(1))*(TXCD-XSEG(1))/
     X            (XSEG(2)-XSEG(1)))+YSEG(1)
            TZCD=((ZSEG(2)-ZSEG(1))*(TXCD-XSEG(1))/
     X            (XSEG(2)-XSEG(1)))+ZSEG(1)
          ELSE IF (IAND(FLGT,4).NE.0) THEN
            TYCD=LIMS(2,1)
            TXCD=((XSEG(2)-XSEG(1))*(TYCD-YSEG(1))/
     X            (YSEG(2)-YSEG(1)))+XSEG(1)
            TZCD=((ZSEG(2)-ZSEG(1))*(TYCD-YSEG(1))/
     X            (YSEG(2)-YSEG(1)))+ZSEG(1)
          ELSE IF (IAND(FLGT,8).NE.0) THEN
            TYCD=LIMS(2,2)
            TXCD=((XSEG(2)-XSEG(1))*(TYCD-YSEG(1))/
     X            (YSEG(2)-YSEG(1)))+XSEG(1)
            TZCD=((ZSEG(2)-ZSEG(1))*(TYCD-YSEG(1))/
     X            (YSEG(2)-YSEG(1)))+ZSEG(1)
          ELSE IF (IAND(FLGT,16).NE.0) THEN
            TZCD=LIMS(3,1)
            TXCD=((XSEG(2)-XSEG(1))*(TZCD-ZSEG(1))/
     X            (ZSEG(2)-ZSEG(1)))+XSEG(1)
            TYCD=((YSEG(2)-YSEG(1))*(TZCD-ZSEG(1))/
     X            (ZSEG(2)-ZSEG(1)))+YSEG(1)
          ELSE
            TZCD=LIMS(3,2)
            TXCD=((XSEG(2)-XSEG(1))*(TZCD-ZSEG(1))/
     X            (ZSEG(2)-ZSEG(1)))+XSEG(1)
            TYCD=((YSEG(2)-YSEG(1))*(TZCD-ZSEG(1))/
     X            (ZSEG(2)-ZSEG(1)))+YSEG(1)
          END IF
          CALL UGG005(LIMS,TXCD,TYCD,TZCD,FLGT)
          IF (FLG1.NE.0) THEN
            XSEG(1)=TXCD
            YSEG(1)=TYCD
            ZSEG(1)=TZCD
            FLG1=FLGT
          ELSE
            XSEG(2)=TXCD
            YSEG(2)=TYCD
            ZSEG(2)=TZCD
            FLG2=FLGT
          END IF
          GO TO 101
        END IF
      END IF
      NAVL=2
C
C  SAVE THE ORIGINAL POINT.
  201 XPNT(1)=XPNT(2)
      YPNT(1)=YPNT(2)
      ZPNT(1)=ZPNT(2)
C
C  RETURN TO CALLING SUBROUTINE.
  301 RETURN
C
      END
