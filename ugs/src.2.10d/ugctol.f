      SUBROUTINE UGCTOL(OPTN,XCRD,YCRD,TXTP,TXTS,
     X                  NSIZ,XARY,YARY,NCRD,BBTS)
C
C *******************  THE UNIFIED GRAPHICS SYSTEM  *******************
C *            CONVERT CHARACTER STRINGS TO LINE SEGMENTS             *
C *                                                                   *
C *  THIS SUBROUTINE MAY BE USED TO CONVERT A PRIMARY AND SECONDARY   *
C *  CHARACTER STRING TO LINE SEGMENTS AND THEIR BLANKING BITS.  THE  *
C *  RESULTING DATA MAY BE MODIFIED BEFORE IT IS PASSED ON TO         *
C *  SUBROUTINE UGPLIN.                                               *
C *                                                                   *
C *  THE CALLING SEQUENCE IS:                                         *
C *    CALL UGCTOL(OPTN,XCRD,YCRD,TXTP,TXTS,                          *
C *                NSIZ,XARY,YARY,NCRD,BBTS)                          *
C *                                                                   *
C *  THE PARAMETERS IN THE CALLING SEQUENCE ARE:                      *
C *    OPTN  THE OPTIONS LIST.                                        *
C *    XCRD  THE X COORDINATE OF THE FIRST CHARACTER.                 *
C *    YCRD  THE Y COORDINATE OF THE FIRST CHARACTER.                 *
C *    TXTP  THE PRIMARY CHARACTER STRING.                            *
C *    TXTS  THE SECONDARY CHARACTER STRING.                          *
C *    NSIZ  THE NUMBER OF ENTRIES IN XARY, YARY, AND BBTS.           *
C *    XARY  THE X COORDINATES OF THE LINE END POINTS.                *
C *    YARY  THE Y COORDINATES OF THE LINE END POINTS.                *
C *    NCRD  THE NUMBER OF ENTRIES STORED IN XARY, YARY, AND BBTS.    *
C *    BBTS  THE BLANKING BIT ARRAY.                                  *
C *                                                                   *
C *                          ROBERT C. BEACH                          *
C *                    COMPUTATION RESEARCH GROUP                     *
C *                STANFORD LINEAR ACCELERATOR CENTER                 *
C *                                                                   *
C *********************************************************************
C
      CHARACTER*(*) OPTN
      REAL          XCRD,YCRD
      CHARACTER*(*) TXTP,TXTS
      INTEGER       NSIZ
      REAL          XARY(*),YARY(*)
      INTEGER       NCRD
      INTEGER*4     BBTS(*)
C
      INCLUDE       'UGSYSTEM:UGMCACBK.FOR'
      INCLUDE       'UGSYSTEM:UGERRCBK.FOR'

      SAVE          /UGA013/
      COMMON        /UGA013/
     X              CHRTP3,
     X              CHRNC3,
     X              CHRSC3,
     X              CHRCS3,
     X              CHROT3,
     X              CHRLT3,
     X              CHRCT3,
     X              CHRID3
      INTEGER*2     CHRTP3
      INTEGER*2     CHRNC3
      INTEGER*2     CHRSC3
      INTEGER*2     CHRCS3
      INTEGER*2     CHROT3(  365)
      INTEGER*2     CHRLT3( 2572)
      CHARACTER*2   CHRCT3(  365)
      CHARACTER*8   CHRID3
 
      SAVE          /UGA014/
      COMMON        /UGA014/
     X              CHRTP4,
     X              CHRNC4,
     X              CHRSC4,
     X              CHRCS4,
     X              CHROT4,
     X              CHRLT4,
     X              CHRCT4,
     X              CHRID4
      INTEGER*2     CHRTP4
      INTEGER*2     CHRNC4
      INTEGER*2     CHRSC4
      INTEGER*2     CHRCS4
C     INTEGER*2     CHROT4(  365)
C     INTEGER*2     CHRLT4( 6453)
C     CHARACTER*2   CHRCT4(  365)
      INTEGER*2     CHROT4(  746)   ! for UBCFONT
      INTEGER*2     CHRLT4(13976)
      CHARACTER*2   CHRCT4(  746)
      CHARACTER*8   CHRID4

      INTEGER*4     EXST(5)
      REAL*4        EXSZ,EXAG
      INTEGER*4     EXJF,EXFX,EXLN
      EQUIVALENCE   (EXSZ,EXST(1)),       (EXAG,EXST(2)),
     X              (EXJF,EXST(3)),       (EXFX,EXST(4)),
     X              (EXLN,EXST(5))
C
      INTEGER       NTXT
      INTEGER       BBIT
      REAL          XVAL,YVAL,SZCH
      INTEGER       FLG1,FLG2
      INTEGER*4     BVAL
C
      REAL          FLT1,FLT2,FLT3
      INTEGER       INT1,INT2
C
      INTEGER       DESC_NUM
      PARAMETER     (DESC_NUM = 7)
      INTEGER*4     INST(4, DESC_NUM)
      CHARACTER*16  IFLAG(DESC_NUM)
C
      DATA     (INST(I,1),I=1,4) / 3,4,1,0 /, IFLAG(1) / 'SIZE' /
      DATA     (INST(I,2),I=1,4) / 3,5,2,0 /, IFLAG(2) / 'ANGLE'/
      DATA     (INST(I,3),I=1,4) / 1,5,3,1 /, IFLAG(3) / 'RIGHT'/
      DATA     (INST(I,4),I=1,4) / 1,6,3,2 /, IFLAG(4) / 'CENTER'/
      DATA     (INST(I,5),I=1,4) / 1,7,4,0 /, IFLAG(5) / 'FIXSIZE'/
      DATA     (INST(I,6),I=1,4) / 1,4,5,1 /, IFLAG(6) / 'LAST'/
      DATA     (INST(I,7),I=1,4) / 1,4,5,2 /, IFLAG(7) / 'NEXT'/

C  SCAN THE OPTIONS LIST.
      EXSZ=0.015
      EXAG=0.0
      EXJF=0
      EXFX=1
      EXLN=0
      CALL UGOPTION(OPTN,DESC_NUM,INST,IFLAG,EXST,EXSTR)
C
C  OBTAIN THE INDIVIDUAL STROKES.
      NTXT=MIN(LEN(TXTP),LEN(TXTS))
      IF (NTXT.LT.1) GO TO 301
      IF (NTXT.GT.1024) GO TO 301
      NCRD=0
      FLG1=EXLN
      FLG2=EXFX
      XVAL=XCRD
      YVAL=YCRD
      IF (EXJF.NE.0) THEN
        IF ( MCACN .EQ. 'SIMPLEX' ) THEN
          CALL UGE001(1,FLG2,1,1,0.0,0.0,EXSZ,EXAG,1.0,CHRTP3,INT1)
        ELSE
          CALL UGE001(1,FLG2,1,1,0.0,0.0,EXSZ,EXAG,1.0,CHRTP4,INT1)
        ENDIF
        IF (INT1.NE.0) THEN
           WRITE(6,'('' * UGCTOL * UGE001 INT1 is '',I10)' ) INT1
           GO TO 303
        END IF
        IF ( MCACN .EQ. 'SIMPLEX' ) THEN
          CALL UGE002(TXTP,TXTS,NTXT,CHRTP3,INT1,FLT1,FLT2,FLT3)
        ELSE
          CALL UGE002(TXTP,TXTS,NTXT,CHRTP4,INT1,FLT1,FLT2,FLT3)
        ENDIF
        XVAL=XVAL-0.5*FLT1
        YVAL=YVAL-0.5*FLT2
        IF (EXJF.NE.2) THEN
          XVAL=XVAL-0.5*FLT1
          YVAL=YVAL-0.5*FLT2
        END IF
      END IF
      IF ( MCACN .EQ. 'SIMPLEX' ) THEN
        CALL UGE001(FLG1,FLG2,1,1,XVAL,YVAL,EXSZ,EXAG,1.0,CHRTP3,INT1)
      ELSE
        CALL UGE001(FLG1,FLG2,1,1,XVAL,YVAL,EXSZ,EXAG,1.0,CHRTP4,INT1)
      ENDIF
      IF (INT1.NE.0) THEN
         WRITE(6,'('' * UGCTOL * INT1 from UGE001 2 is '',I10)' ) INT1
         GO TO 303
      END IF

  101 CONTINUE

      IF ( MCACN .EQ. 'SIMPLEX' ) THEN
        CALL UGE002(TXTP,TXTS,NTXT,CHRTP3,BBIT,XVAL,YVAL,SZCH)
      ELSE
        CALL UGE002(TXTP,TXTS,NTXT,CHRTP4,BBIT,XVAL,YVAL,SZCH)
      ENDIF
      IF (FLG1.EQ.0) THEN
        IF (BBIT.EQ.-1) GO TO 201
        IF (NCRD.GE.NSIZ) GO TO 302
        NCRD=NCRD+1
        XARY(NCRD)=XVAL
        YARY(NCRD)=YVAL
        IF (NCRD.GT.1) THEN
          INT1 = (NCRD+30) / 32
          INT2 =  NCRD+30  - 32*INT1
          IF (BBIT.NE.0) THEN
             BVAL = IBSET (BBTS(INT1), INT2 )
          ELSE
             BVAL = IBCLR (BBTS(INT1), INT2 )
          ENDIF
          BBTS(INT1)=BVAL
        END IF
        GO TO 101
      END IF
      XARY(1)=XVAL
      YARY(1)=YVAL
      XARY(2)=SZCH
C
C  RESET ERROR INDICATORS AND RETURN TO CALLER.
  201 UGELV=0
      UGENM='        '
      UGEIX=0
  202 RETURN
C
C  REPORT ERRORS TO THE UNIFIED GRAPHICS SYSTEM ERROR PROCESSOR.
  301 CALL UGRERR(2,'UGCTOL  ', 1)
      GO TO 202
  302 CALL UGRERR(2,'UGCTOL  ', 2)
      GO TO 202
  303 CALL UGRERR(3,'UGCTOL  ',14)
      GO TO 202
C
      END
