      SUBROUTINE UGC005(NPTS)
C
C *******************  THE UNIFIED GRAPHICS SYSTEM  *******************
C *               LINE SCISSORING AND SHIELDING MODULE                *
C *                                                                   *
C *  THIS SUBROUTINE IS USED TO PERFORM THE ACTUAL SCISSORING         *
C *  OPERATION.  THE INPUT POINTS ARE IN THE COMMON ARRAYS XPNT AND   *
C *  YPNT.  THE SCISSORING LIMITS ARE IN THE COMMON ARRAY LIMS.       *
C *  THE OUTPUT LINE SEGMENT, IF ANY, IS IN THE COMMON ARRAYS XSEG    *
C *  AND YSEG.                                                        *
C *                                                                   *
C *  THE CALLING SEQUENCE IS:                                         *
C *    CALL UGC005(NPTS)                                              *
C *                                                                   *
C *  THE PARAMETER IN THE CALLING SEQUENCE IS:                        *
C *    NPTS  THE NUMBER OF END POINTS PUT INTO XSEG AND YSEG.  THIS   *
C *          NUMBER WILL BE 0 TO 2.                                   *
C *                                                                   *
C *                          ROBERT C. BEACH                          *
C *                    COMPUTATION RESEARCH GROUP                     *
C *                STANFORD LINEAR ACCELERATOR CENTER                 *
C *                                                                   *
C *********************************************************************
C
      INTEGER       NPTS
C
      INCLUDE       'UGSYSTEM:UGC00CBK.FOR'
C
      REAL          XCRD,YCRD
      INTEGER       FLG1,FLG2,FLGT
C
C  INITIALIZE AND MOVE LINE INTO OUTPUT ARRAY.
      NPTS=0
      XSEG(1)=XPNT(1)
      XSEG(2)=XPNT(2)
      YSEG(1)=YPNT(1)
      YSEG(2)=YPNT(2)
      CALL UGC007(LIMS,XSEG(1),YSEG(1),FLG1)
      CALL UGC007(LIMS,XSEG(2),YSEG(2),FLG2)
C
C  NOW DO THE ACTUAL SCISSORING.
  101 IF ((FLG1.NE.0).OR.(FLG2.NE.0)) THEN
        IF (IAND(FLG1,FLG2).NE.0) GO TO 201
        IF (FLG1.NE.0) THEN
          FLGT=FLG1
        ELSE
          FLGT=FLG2
        END IF
        IF (IAND(FLGT,1).NE.0) THEN
          XCRD=LIMS(1,1)
          YCRD=((YSEG(2)-YSEG(1))*(XCRD-XSEG(1))/
     X          (XSEG(2)-XSEG(1)))+YSEG(1)
        ELSE IF (IAND(FLGT,2).NE.0) THEN
          XCRD=LIMS(1,2)
          YCRD=((YSEG(2)-YSEG(1))*(XCRD-XSEG(1))/
     X          (XSEG(2)-XSEG(1)))+YSEG(1)
        ELSE IF (IAND(FLGT,4).NE.0) THEN
          YCRD=LIMS(2,1)
          XCRD=((XSEG(2)-XSEG(1))*(YCRD-YSEG(1))/
     X          (YSEG(2)-YSEG(1)))+XSEG(1)
        ELSE
          YCRD=LIMS(2,2)
          XCRD=((XSEG(2)-XSEG(1))*(YCRD-YSEG(1))/
     X          (YSEG(2)-YSEG(1)))+XSEG(1)
        END IF
        CALL UGC007(LIMS,XCRD,YCRD,FLGT)
        IF (FLG1.NE.0) THEN
          XSEG(1)=XCRD
          YSEG(1)=YCRD
          FLG1=FLGT
        ELSE
          XSEG(2)=XCRD
          YSEG(2)=YCRD
          FLG2=FLGT
        END IF
        GO TO 101
      END IF
      NPTS=2
C
C  RETURN TO CALLING SUBROUTINE.
  201 RETURN
C
      END
