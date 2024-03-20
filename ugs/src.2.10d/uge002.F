      SUBROUTINE UGE002(CHRP,CHRS,CHRN,DATA,BBIT,XCRD,YCRD,SZCH)
C
C *******************  THE UNIFIED GRAPHICS SYSTEM  *******************
C *                 CHARACTER STROKE GENERATOR MODULE                 *
C *                                                                   *
C *  THIS SUBROUTINE MAY BE USED TO RETRIEVE A POINT FROM THE         *
C *  CHARACTER STROKE GENERATOR MODULE.                               *
C *                                                                   *
C *  THE CALLING SEQUENCE IS:                                         *
C *    CALL UGE002(CHRP,CHRS,CHRN,DATA,BBIT,XCRD,YCRD,SZCH)           *
C *                                                                   *
C *  THE PARAMETERS IN THE CALLING SEQUENCE ARE:                      *
C *    CHRP  THE PRIMARY CHARACTER STRING.                            *
C *    CHRS  THE SECONDARY CHARACTER STRING.                          *
C *    CHRN  THE NUMBER OF CHARACTERS IN THE STRINGS.                 *
C *    DATA  THE BLOCK OF DATA DEFINING THE CHARACTER SET.            *
C *    BBIT  THE BLANKING BIT OR TERMINATION FLAG: 0 MEANS MOVE       *
C *          WITHOUT DRAWING, 1 MEANS DRAW, AND -1 MEANS NO MORE      *
C *          DATA IS AVAILABLE.                                       *
C *    XCRD  THE X COORDINATE OF A STROKE END POINT OR THE END OF     *
C *          STRING X COORDINATE FOR A SCAN.                          *
C *    YCRD  THE Y COORDINATE OF A STROKE END POINT OR THE END OF     *
C *          STRING Y COORDINATE FOR A SCAN.                          *
C *    SZCH  THE SIZE CHANGE FACTOR.  THIS IS ONLY AVAILABLE WHEN     *
C *          BBIT IS -1.                                              *
C *                                                                   *
C *                          ROBERT C. BEACH                          *
C *                    COMPUTATION RESEARCH GROUP                     *
C *                STANFORD LINEAR ACCELERATOR CENTER                 *
C *                                                                   *
C *********************************************************************
C
      CHARACTER*(*) CHRP,CHRS
      INTEGER       CHRN
      INTEGER*2     DATA(*)
      INTEGER       BBIT
      REAL          XCRD,YCRD,SZCH
C
#include "UGSYSTEM:UGE00CBK.FOR"
C     INCLUDE       'UGSYSTEM:UGE00CBK.FOR'
C
      INTEGER*2     CHAR
      CHARACTER*2   CHRX
      EQUIVALENCE   (CHAR,CHRX)
C
      INTEGER       INT1,INT2,INT3
      INTEGER*2     J8 ,     J16      ! For IDIOT AIX compiler
      DATA          J8 /8/ , J16 /16/
C
C  CHECK TO SEE IF THE CHARACTER SET IS AVAILABLE.
      IF (NCHR.LE.0) GO TO 401
C
C  PROCESS THE NEXT STROKE IF ANY ARE AVAILABLE.
  101 IF (ISTR.GT.NSTR) THEN
        IF (NSTR.GT.0) THEN
          IF (IFEQ.EQ.0) CALL UGE003(-0.5*XDSP*STEP,0.0)
          CALL UGE003(CSPC*STEP,0.0)
        END IF
      ELSE
        INT1=DATA(ISTR)
        ISTR=ISTR+1
        BBIT=INT1/16384
        INT1=INT1-16384*BBIT
        CALL UGE003(STEP*REAL((INT1/128)-64),
     X              STEP*REAL(MOD(INT1,128)-64))
        IF ((IFDR.EQ.0).AND.(ISTR.LE.NSTR)) GO TO 402
        GO TO 101
      END IF
C
C  SELECT THE NEXT CHARACTER FROM THE CHARACTER STRINGS IF POSSIBLE.
  201 IF (ICHR.GT.CHRN) GO TO 401
      CHRX(1:1)=CHRP(ICHR:ICHR)
      IF (IFMS.EQ.0) THEN
        CHRX(2:2)=' '
      ELSE
        CHRX(2:2)=CHRS(ICHR:ICHR)
      END IF
      ICHR=ICHR+1
C
C  CHECK FOR SPECIAL CHARACTER PROCESSING.
      IF (IFSC.NE.0) THEN
        IF (CHRX(2:2).EQ.'X') THEN
          IF (CHRX(1:1).EQ.'0') THEN
C           ENTER SUBSCRIPT MODE.
            CALL UGE003(-0.111111111*CSPC*STEP,
     X                  -0.500000000*CSPC*STEP)
            STEP= 0.666666667*STEP
            XSZC= 0.666666667*XSZC
            GO TO 201
          ELSE IF (CHRX(1:1).EQ.'1') THEN
C           LEAVE SUBSCRIPT MODE.
            CALL UGE003( 0.333333333*CSPC*STEP,
     X                   0.750000000*CSPC*STEP)
            STEP= 1.500000000*STEP
            XSZC= 1.500000000*XSZC
            GO TO 201
          ELSE IF (CHRX(1:1).EQ.'2') THEN
C           ENTER SUPERSCRIPT MODE.
            CALL UGE003(-0.111111111*CSPC*STEP,
     X                   0.500000000*CSPC*STEP)
            STEP= 0.666666667*STEP
            XSZC= 0.666666667*XSZC
            GO TO 201
          ELSE IF (CHRX(1:1).EQ.'3') THEN
C           LEAVE SUPERSCRIPT MODE.
            CALL UGE003( 0.333333333*CSPC*STEP,
     X                  -0.750000000*CSPC*STEP)
            STEP= 1.500000000*STEP
            XSZC= 1.500000000*XSZC
            GO TO 201
          END IF
        ELSE IF (CHRX(2:2).EQ.'Y') THEN
          IF (CHRX(1:1).EQ.'0') THEN
C           INCREASE SIZE BY ONE HALF.
            CALL UGE003( 0.166666667*CSPC*STEP,
     X                   0.250000000*CSPC*STEP)
            STEP= 1.500000000*STEP
            XSZC= 1.500000000*XSZC
            GO TO 201
          ELSE IF (CHRX(1:1).EQ.'1') THEN
C           DECREASE SIZE BY ONE THIRD.
            CALL UGE003(-0.111111111*CSPC*STEP,
     X                  -0.166666667*CSPC*STEP)
            STEP= 0.666666667*STEP
            XSZC= 0.666666667*XSZC
            GO TO 201
          END IF
        ELSE IF (CHRX(2:2).EQ.'Z') THEN
          IF (CHRX(1:1).EQ.'0') THEN
C           PUT CURRENT STATE IN FIRST SAVE AREA.
            SAV1(1)=CHRO(1)
            SAV1(2)=CHRO(2)
            SAV1(3)=STEP
            SAV1(4)=XSZC
            GO TO 201
          ELSE IF (CHRX(1:1).EQ.'1') THEN
C           RESTORE STATE FROM FIRST SAVE AREA.
            CHRO(1)=SAV1(1)
            CHRO(2)=SAV1(2)
            STEP=SAV1(3)
            XSZC=SAV1(4)
            GO TO 201
          ELSE IF (CHRX(1:1).EQ.'2') THEN
C           PUT CURRENT STATE IN SECOND SAVE AREA.
            SAV2(1)=CHRO(1)
            SAV2(2)=CHRO(2)
            SAV2(3)=STEP
            SAV2(4)=XSZC
            GO TO 201
          ELSE IF (CHRX(1:1).EQ.'3') THEN
C           RESTORE STATE FROM SECOND SAVE AREA.
            CHRO(1)=SAV2(1)
            CHRO(2)=SAV2(2)
            STEP=SAV2(3)
            XSZC=SAV2(4)
            GO TO 201
          ELSE IF (CHRX(1:1).EQ.'4') THEN
C           PUT CURRENT STATE IN THIRD SAVE AREA.
            SAV3(1)=CHRO(1)
            SAV3(2)=CHRO(2)
            SAV3(3)=STEP
            SAV3(4)=XSZC
            GO TO 201
          ELSE IF (CHRX(1:1).EQ.'5') THEN
C           RESTORE STATE FROM THIRD SAVE AREA.
            CHRO(1)=SAV3(1)
            CHRO(2)=SAV3(2)
            STEP=SAV3(3)
            XSZC=SAV3(4)
            GO TO 201
          ELSE IF (CHRX(1:1).EQ.'6') THEN
C           PUT CURRENT STATE IN FOURTH SAVE AREA.
            SAV4(1)=CHRO(1)
            SAV4(2)=CHRO(2)
            SAV4(3)=STEP
            SAV4(4)=XSZC
            GO TO 201
          ELSE IF (CHRX(1:1).EQ.'7') THEN
C           RESTORE STATE FROM FOURTH SAVE AREA.
            CHRO(1)=SAV4(1)
            CHRO(2)=SAV4(2)
            STEP=SAV4(3)
            XSZC=SAV4(4)
            GO TO 201
          END IF
        END IF
      END IF
C
C  DO A BINARY SEARCH FOR THE CHARACTER IN THE DATA BLOCK.
      INT1=0
      INT2=NCHR-2
      IF (DATA(PTCT+INT1).NE.CHAR) THEN
  301   IF (DATA(PTCT+INT2).NE.CHAR) THEN
  302     IF (INT2.GT.(INT1+1)) THEN
            INT3=(INT1+INT2)/2
#if  ( defined(__LINUX_AOUT) || defined(__LINUX_ELF) || defined(__OSF1) )
c      This is a little-endian architecture
       IF (DATA(PTCT+INT3).GE.CHAR) THEN
#else
c      This is a big-endian architecture
       IF (ISHFTC(DATA(PTCT+INT3),J8,J16).GE.ISHFTC(CHAR,J8,J16)) THEN
#endif
              INT2=INT3
              GO TO 301
            ELSE
              INT1=INT3
              GO TO 302
            END IF
          ELSE
            INT2=NCHR-1
          END IF
        END IF
      ELSE
        INT2=INT1
      END IF
      INT1=DATA(PTOT+INT2)+PTST-1
      INT2=DATA(INT1)
      XDSP=REAL(MOD(INT2,128)-64)
      ISTR=INT1+1
      NSTR=(INT2/128)+ISTR-1
C
C  PROCESS THE CHARACTER.
      IF (IFEQ.EQ.0) CALL UGE003(-0.5*XDSP*STEP,0.0)
      GO TO 101
C
C  SIGNAL END OF PROCESSING AND RETURN TO CALLING SUBROUTINE.
  401 IF (IFDR.EQ.1) CALL UGE003(-CSPC*STEP,0.0)
      BBIT=-1
      SZCH=XSZC
  402 XCRD=CHRO(1)
      YCRD=CHRO(2)
      RETURN
C
      END
