      SUBROUTINE UGINFO(OPTN,STRG,IARY,XARY)
C
C *******************  THE UNIFIED GRAPHICS SYSTEM  *******************
C *                OBTAIN INFORMATION ABOUT THE SYSTEM                *
C *                                                                   *
C *  THIS SUBROUTINE MAY BE USED TO DETERMINE THE STATUS OF THE       *
C *  UNIFIED GRAPHICS SYSTEM AND MANY OF THE PROPERTIES OF THE        *
C *  ACTIVE GRAPHIC DEVICE.                                           *
C *                                                                   *
C *  THE CALLING SEQUENCE IS:                                         *
C *    CALL UGINFO(OPTN,STRG,IARY,XARY)                               *
C *                                                                   *
C *  THE PARAMETERS IN THE CALLING SEQUENCE ARE:                      *
C *    OPTN  THE OPTIONS LIST.                                        *
C *    STRG  A CHARACTER STRING FOR THE ANSWERS.                      *
C *    IARY  AN INTEGER ARRAY FOR THE ANSWERS.                        *
C *    XARY  A FLOATING POINT ARRAY FOR THE ANSWERS.                  *
C *                                                                   *
C *                          ROBERT C. BEACH                          *
C *                    COMPUTATION RESEARCH GROUP                     *
C *                STANFORD LINEAR ACCELERATOR CENTER                 *
C *                                                                   *
C *********************************************************************
C
      CHARACTER*(*) OPTN,STRG
      INTEGER       IARY(*)
      REAL          XARY(*)
C
      INCLUDE       'UGSYSTEM:UGMCACBK.FOR'
C
      INCLUDE       'UGSYSTEM:UGDDACBK.FOR'
C
      INCLUDE       'UGSYSTEM:UGERRCBK.FOR'
C
      INTEGER       DESC_NUM
      PARAMETER    (DESC_NUM = 12)
      INTEGER*4     INST(4,DESC_NUM)
      CHARACTER*16  IFLAG(DESC_NUM)

      INTEGER*4     EXST(DESC_NUM),EXOD,EXAD,			!JC
     X              EXAT,EXIL,EXDM,EXIC,EXEC,EXBE,EXDX,EXWX,
     X              EXSD,EXDP
      EQUIVALENCE   (EXOD,EXST( 1)),      (EXAD,EXST( 2)),
     X              (EXAT,EXST( 3)),      (EXIL,EXST( 4)),
     X              (EXDM,EXST( 5)),      (EXIC,EXST( 6)),
     X              (EXEC,EXST( 7)),      (EXBE,EXST( 8)),
     X              (EXDX,EXST( 9)),      (EXWX,EXST(10)),
     X              (EXSD,EXST(11)),	  (EXDP,EXST(12))	!JC
C
      INTEGER       INT1
C
*     DATA (INST(I,6),I=1,4)  / 1,9,6,1  /, IFLAG(6)  /'DIMENSION' /
C
      DATA (INST(I, 1),I=1,4) / 1, 7, 1,1  /, IFLAG( 1) /'OPENDEV'   /
      DATA (INST(I, 2),I=1,4) / 1, 6, 2,1  /, IFLAG( 2) /'ACTDEV'    /
      DATA (INST(I, 3),I=1,4) / 1, 7, 3,1  /, IFLAG( 3) /'DEVTYPE'   /
      DATA (INST(I, 4),I=1,4) / 1, 6, 4,1  /, IFLAG( 4) /'ILEVEL'    /
      DATA (INST(I, 5),I=1,4) / 1, 7, 5,1  /, IFLAG( 5) /'DMEDIUM'   /
      DATA (INST(I, 6),I=1,4) / 1, 8, 6,1  /, IFLAG( 6) /'CONTROLS'  /
      DATA (INST(I, 7),I=1,4) / 1, 9, 7,1  /, IFLAG( 7) /'ECONTROLS' /
      DATA (INST(I, 8),I=1,4) / 1,10, 8,1  /, IFLAG( 8) /'EXTENSIONS'/
      DATA (INST(I, 9),I=1,4) / 1, 8 ,9,1  /, IFLAG( 9) /'DSPCSIZE'  /
      DATA (INST(I,10),I=1,4) / 1, 8,10,1  /, IFLAG(10) /'WDOWSIZE'  /
      DATA (INST(I,11),I=1,4) / 1, 7,11,1  /, IFLAG(11) /'SEGDEFL'   /
      DATA (INST(I,12),I=1,4) / 1, 8,12,1  /, IFLAG(12) /'DPHYSIZE'  /


C     DATA          INST/NP,1, 7, 1,1,'OPEN','DEV ',
C    X                      1, 6, 2,1,'ACTD','EV  ',
C    X                      1, 7, 3,1,'DEVT','YPE ',
C    X                      1, 6, 4,1,'ILEV','EL  ',
C    X                      1, 7, 5,1,'DMED','IUM ',
C    X                      1, 8, 6,1,'CONT','ROLS',
C    X                      1, 9, 7,1,'ECON','TROL','S   ',
C    X                      1,10, 8,1,'EXTE','NSIO','NS  ',
C    X                      1, 8, 9,1,'DSPC','SIZE',
C    X                      1, 8,10,1,'WDOW','SIZE',
C    X                      1, 7,11,1,'SEGD','EFL ', 
C	1		    1, 8,12,1,'DPHY','SIZE'/		!JC

C
C  SCAN THE OPTIONS LIST.
      EXOD=0
      EXAD=0
      EXAT=0
      EXIL=0
      EXDM=0
      EXIC=0
      EXEC=0
      EXBE=0
      EXDX=0
      EXWX=0
      EXSD=0
      EXDP=0
      CALL UGOPTION(OPTN,DESC_NUM,INST,IFLAG,EXST,EXSTR)
C
C  PROCESS REQUESTS THAT DO NOT REQUIRE AN ACTIVE DEVICE.
C  PROCESS "OPENDEV" IF REQUESTED.
      IF (EXOD.NE.0) THEN
        IARY(1)=0
        DO 101 INT1=1,MCAZ1
          IF (MCAOI(INT1).NE.0) THEN
            IARY(1)=IARY(1)+1
            IARY(IARY(1)+1)=MCAOI(INT1)
          END IF
  101   CONTINUE
* * *
*      WRITE (*,*) '  opendev ',( IARY(J),J=1,IARY( IARY(1)+1 ) )
* * *
      END IF
C  PROCESS "ACTDEV" IF REQUESTED.
      IF (EXAD.NE.0) THEN
        IF (DDAAI.EQ.0) THEN
          IARY(1)=0
        ELSE
          IARY(1)=MCAOI(DDAAI)
        END IF
* * *
*      WRITE (*,*) '   actdev ',IARY(1)
* * *
      END IF
C  PROCESS "SEGDEFL" IF REQUESTED.
      IF (EXSD.NE.0) THEN
        CALL UGDEFL('GET',IARY,XARY)
* * *
*      WRITE (*,*) ' segdefl  * * * * * * * * * * * * * * * * * * * '
* * *
      END IF
C
C  PROCESS REQUESTS THAT REQUIRE AN ACTIVE DEVICE.
C  PROCESS "DEVTYPE" IF REQUESTED.
      IF (EXAT.NE.0) THEN
        IF (DDAAI.EQ.0) GO TO 401
        STRG(1:8)=DDAAT
* * *
*      WRITE (*,*) '  devtype ',STRG(:8)
* * *
      END IF
C  PROCESS "ILEVEL" IF REQUESTED.
      IF (EXIL.NE.0) THEN
        IF (DDAAI.EQ.0) GO TO 401
        IARY(1)=DDAIL
* * *
*      WRITE (*,*) '   ilevel ',IARY(1)
* * *
      END IF
C  PROCESS "DMEDIUM" IF REQUESTED.
      IF (EXDM.NE.0) THEN
        IF (DDAAI.EQ.0) GO TO 401
        IARY(2)=DDADM
* * *
*      WRITE (*,*) '  dmedium ',IARY(2)
* * *
      END IF
C  PROCESS "CONTROLS" IF REQUESTED.
      IF (EXIC.NE.0) THEN
        IF (DDAAI.EQ.0) GO TO 401
        DO 201 INT1=1,DDAZ1
          IARY(INT1)=DDAIC(INT1)
  201   CONTINUE
* * *
*      WRITE (*,*) ' controls ',(IARY(J),J=1,DDAZ1)
* * *
      END IF
C  PROCESS "ECONTROLS" IF REQUESTED.
      IF (EXEC.NE.0) THEN
        IF (DDAAI.EQ.0) GO TO 401
        DO 202 INT1=1,DDAZ1
          IARY(INT1)=DDABC(INT1)
  202   CONTINUE
* * *
*      WRITE (*,*) ' econtrols',(IARY(J),J=1,DDAZ1)
* * *
      END IF
C  PROCESS "EXTENSIONS" IF REQUESTED.
      IF (EXBE.NE.0) THEN
        IF (DDAAI.EQ.0) GO TO 401
        IARY(1)=DDABE(1,1)
        IARY(2)=DDABE(2,1)
        IARY(3)=DDABE(1,2)
        IARY(4)=DDABE(2,2)
* * *
*      WRITE (*,*) ' extension',IARY(1),IARY(2),IARY(3),IARY(4)
* * *
      END IF
C  PROCESS "DSPCSIZE" IF REQUESTED.
      IF (EXDX.NE.0) THEN
        IF (DDAAI.EQ.0) GO TO 401
        XARY(1)=DDADX
        XARY(2)=DDADY
* * *
*      WRITE (*,*) ' dspcsize ',XARY(1),XARY(2)
* * *
      END IF
C  PROCESS "WDOWSIZE" IF REQUESTED.
      IF (EXWX.NE.0) THEN
        IF (DDAAI.EQ.0) GO TO 401
        XARY(1)=DDAWX
        XARY(2)=DDAWY
* * *
*      WRITE (*,*) ' wdowsize ',XARY(1),XARY(2)
* * *
      END IF
C**************************************************************
C
C	Modified By. J. Clement, Rice Univ. To get PXEL size
C
C  PROCESS "DPHYSIZE" IF REQUESTED.
      IF (EXDP.NE.0) THEN
        IF (DDAAI.EQ.0) GO TO 401
        XARY(1)=DDABX
        XARY(2)=DDABY
        IARY(1)=DDABD(1,1)			!JC
        IARY(2)=DDABD(2,1)			!JC
        IARY(3)=DDABD(1,2)			!JC
        IARY(4)=DDABD(2,2)			!JC
* * *
*      WRITE (*,*) ' dphysize ',XARY(1),XARY(2)            
*      WRITE (*,*) ' dphysize ',IARY(1),IARY(2),IARY(3),IARY(4)
* * *
      END IF
C**************************************************************
C
C  RESET ERROR INDICATORS AND RETURN TO CALLER.
      UGELV=0
      UGENM='        '
      UGEIX=0
  301 RETURN
C
C  REPORT ERRORS TO THE UNIFIED GRAPHICS SYSTEM ERROR PROCESSOR.
  401 CALL UGRERR(3,'UGINFO  ',12)
      GO TO 301
C
      END
