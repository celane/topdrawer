      SUBROUTINE UGCNT3(FLAG,ISID,IROW,ICOL,NDIM,WKAR,OFLG)
C
C *******************  THE UNIFIED GRAPHICS SYSTEM  *******************
C *                   MARK/CHECK SIDES AS PROCESSED                   *
C *                                                                   *
C *  THIS SUBROUTINE IS USED BY UGCNTR TO MARK A GIVEN SIDE AS        *
C *  PROCESSED OR CHECK IF THE SIDE HAS BEEN PROCESSED.               *
C *                                                                   *
C *  THE CALLING SEQUENCE IS:                                         *
C *    CALL UGCNT3(FLAG,ISID,IROW,ICOL,NDIM,WKAR,OFLG)                *
C *                                                                   *
C *  THE PARAMETERS IN THE CALLING SEQUENCE ARE:                      *
C *    FLAG  0 MEANS MARK, 1 MEANS CHECK.                             *
C *    ISID  INDEX OF THE SIDE BEING PROCESSED.                       *
C *    IROW  INDEX OF THE ROW BEING PROCESSED.                        *
C *    ICOL  INDEX OF THE COLUMN BEING PROCESSED.                     *
C *    NDIM  THE EXTENT OF THE CONTOUR DATA IN THE Y DIRECTION.       *
C *    WKAR  A WORK ARRAY.                                            *
C *    OFLG  A RETURN VALUE INDICATING IF THE SIDE HAS BEEN           *
C *          PROCESSED.                                               *
C *                                                                   *
C *                          ROBERT C. BEACH                          *
C *                    COMPUTATION RESEARCH GROUP                     *
C *                STANFORD LINEAR ACCELERATOR CENTER                 *
C *                                                                   *
C *********************************************************************
C
      INTEGER       FLAG,ISID,IROW,ICOL,NDIM
      INTEGER*4     WKAR(*)
      INTEGER       OFLG
C
      INTEGER       KROW,KCOL,KSID
      INTEGER       XMNB,XMNW,XMNO,XMWD,XMLP,XMUP,XMP0,XMP1
C
C  PERFORM THE REQUIRED OPERATION.
      OFLG=FLAG
      KROW=IROW
      KCOL=ICOL
      KSID=ISID
      IF (KSID.EQ.2) THEN
        KSID=0
        KCOL=KCOL+1
      ELSE IF (KSID.EQ.3) THEN
        KSID=1
        KROW=KROW+1
      END IF
      XMNB=2*((KROW-3)*(NDIM-1)+(KCOL-3))+KSID
      XMNW=1+XMNB/30
      XMNO=  MOD(XMNB,30)
      XMP0=ISHFT(1,XMNO)
      XMWD=WKAR(XMNW)
      IF (OFLG.NE.0) THEN
        XMUP=XMWD/XMP0
        IF (MOD(XMUP,2).EQ.0) OFLG=0
      ELSE
        XMP1=ISHFT(1,XMNO+1)
        XMLP=MOD(XMWD,XMP0)
        XMUP=XMWD/XMP1
        WKAR(XMNW)=XMUP*XMP1+XMP0+XMLP
      END IF
C
C  RETURN TO CALLING SUBROUTINE.
      RETURN
C
      END
