      SUBROUTINE UGC006(CSHD,ILIN,NPTS)
C
C *******************  THE UNIFIED GRAPHICS SYSTEM  *******************
C *               LINE SCISSORING AND SHIELDING MODULE                *
C *                                                                   *
C *  THIS SUBROUTINE IS USED TO PERFORM THE ACTUAL SHIELDING          *
C *  OPERATION.  THE INPUT POINTS ARE IN THE COMMON ARRAYS XSEG AND   *
C *  YSEG.  THE OUTPUT LINE SEGMENTS, IF ANY, ARE IN THE COMMON       *
C *  ARRAYS XSEG AND YSEG.                                            *
C *                                                                   *
C *  THE CALLING SEQUENCE IS:                                         *
C *    CALL UGC006(CSHD,ILIN,NPTS)                                    *
C *                                                                   *
C *  THE PARAMETERS IN THE CALLING SEQUENCE ARE:                      *
C *    CSHD  AN ARRAY OF CONTAINING THE CURRENT SHIELD.               *
C *    ILIN  THE INDEX OF THE LINE IN XSEG AND YSEG.                  *
C *    NPTS  THE NUMBER OF END POINTS PUT INTO XSEG AND YSEG.  THIS   *
C *          NUMBER WILL BE 0, 2, OR 4.                               *
C *                                                                   *
C *                          ROBERT C. BEACH                          *
C *                    COMPUTATION RESEARCH GROUP                     *
C *                STANFORD LINEAR ACCELERATOR CENTER                 *
C *                                                                   *
C *********************************************************************
C
      REAL          CSHD(2,2)
      INTEGER       ILIN,NPTS
C
      INCLUDE       'UGSYSTEM:UGC00CBK.FOR'
C
      REAL          XCDS(2),YCDS(2)
      REAL          XCRD,YCRD
      INTEGER       IND1,IND2
      INTEGER       FLG1,FLG2,FLGT
C
C  INITIALIZE AND MOVE LINE INTO WORK ARRAY.
      NPTS=2
      XCDS(1)=XSEG(ILIN)
      XCDS(2)=XSEG(ILIN+1)
      YCDS(1)=YSEG(ILIN)
      YCDS(2)=YSEG(ILIN+1)
      CALL UGC007(CSHD,XCDS(1),YCDS(1),FLG1)
      CALL UGC007(CSHD,XCDS(2),YCDS(2),FLG2)
C
C  NOW DO THE ACTUAL SHIELDING.
      IF (IAND(FLG1,FLG2).NE.0) GO TO 201
      IF ((FLG1.EQ.0).AND.(FLG2.EQ.0)) THEN
        NPTS=0
        GO TO 201
      END IF
      IND1=0
      IND2=0
  101 IF ((FLG1.NE.0).OR.(FLG2.NE.0)) THEN
        IF (IAND(FLG1,FLG2).NE.0) GO TO 201
        IF (FLG1.NE.0) THEN
          FLGT=FLG1
        ELSE
          FLGT=FLG2
        END IF
        IF (IAND(FLGT,1).NE.0) THEN
          XCRD=CSHD(1,1)
          YCRD=((YCDS(2)-YCDS(1))*(XCRD-XCDS(1))/
     X          (XCDS(2)-XCDS(1)))+YCDS(1)
        ELSE IF (IAND(FLGT,2).NE.0) THEN
          XCRD=CSHD(1,2)
          YCRD=((YCDS(2)-YCDS(1))*(XCRD-XCDS(1))/
     X          (XCDS(2)-XCDS(1)))+YCDS(1)
        ELSE IF (IAND(FLGT,4).NE.0) THEN
          YCRD=CSHD(2,1)
          XCRD=((XCDS(2)-XCDS(1))*(YCRD-YCDS(1))/
     X          (YCDS(2)-YCDS(1)))+XCDS(1)
        ELSE
          YCRD=CSHD(2,2)
          XCRD=((XCDS(2)-XCDS(1))*(YCRD-YCDS(1))/
     X          (YCDS(2)-YCDS(1)))+XCDS(1)
        END IF
        CALL UGC007(CSHD,XCRD,YCRD,FLGT)
        IF (FLG1.NE.0) THEN
          XCDS(1)=XCRD
          YCDS(1)=YCRD
          FLG1=FLGT
          IND1=1
        ELSE
          XCDS(2)=XCRD
          YCDS(2)=YCRD
          FLG2=FLGT
          IND2=1
        END IF
        GO TO 101
      END IF
      IF ((IND1.NE.0).AND.(IND2.EQ.0)) THEN
        XSEG(ILIN+1)=XCDS(1)
        YSEG(ILIN+1)=YCDS(1)
      ELSE IF ((IND1.EQ.0).AND.(IND2.NE.0)) THEN
        XSEG(ILIN)=XCDS(2)
        YSEG(ILIN)=YCDS(2)
      ELSE
        XSEG(ILIN+3)=XSEG(ILIN+1)
        XSEG(ILIN+1)=XCDS(1)
        XSEG(ILIN+2)=XCDS(2)
        YSEG(ILIN+3)=YSEG(ILIN+1)
        YSEG(ILIN+1)=YCDS(1)
        YSEG(ILIN+2)=YCDS(2)
        NPTS=4
      END IF
C
C  RETURN TO CALLING SUBROUTINE.
  201 RETURN
C
      END
