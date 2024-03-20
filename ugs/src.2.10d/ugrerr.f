      SUBROUTINE UGRERR(LEVL,SNAM,INDX)
C
C *******************  THE UNIFIED GRAPHICS SYSTEM  *******************
C *                    ERROR PROCESSING SUBROUTINE                    *
C *                                                                   *
C *  THIS SUBROUTINE MAY BE USED TO REPORT ERRORS TO THE UNIFIED      *
C *  GRAPHICS SYSTEM.  ERROR MESSAGES MAY BE PRINTED AND THE PROGRAM  *
C *  MAY TERMINATE EXECUTION AND/OR PRODUCE A MEMORY DUMP.            *
C *                                                                   *
C *  THE CALLING SEQUENCE IS:                                         *
C *    CALL UGRERR(LEVL,SNAM,INDX)                                    *
C *                                                                   *
C *  THE PARAMETERS IN THE CALLING SEQUENCE ARE:                      *
C *    LEVL  THE LEVEL OF THE ERROR (1 MEANS SET UGERRD DATA ONLY, 2  *
C *          MEANS ALSO PRINT MESSAGE, 3 MEANS PRINT AND STOP, 4      *
C *          MEANS PRINT AND ABORT).                                  *
C *    SNAM  THE NAME OF THE SUBROUTINE DETECTING THE ERROR PADDED    *
C *          TO EIGHT CHARACTERS.                                     *
C *    INDX  THE INDEX OF THE ERROR.                                  *
C *                                                                   *
C *                          ROBERT C. BEACH                          *
C *                    COMPUTATION RESEARCH GROUP                     *
C *                STANFORD LINEAR ACCELERATOR CENTER                 *
C *                                                                   *
C *********************************************************************
C
      INTEGER       LEVL
      CHARACTER*8   SNAM
      INTEGER       INDX
C
      INCLUDE       'UGSYSTEM:UGERRCBK.FOR'
C
      INCLUDE       'UGSYSTEM:UGEMSCBK.FOR'
C
      INTEGER       TLVL,TIND
      CHARACTER*8   TNAM
      INTEGER       IMES
C
      INTEGER       INT1
C
C  SAVE ERROR INFORMATION.
      TLVL=LEVL
      TNAM=SNAM
      TIND=INDX
C
C  INCREMENT RECURSIVE USE FLAG AND CALL USER SUBROUTINE
C  FOR NON-RECURSIVE USE.
      EMMRC=EMMRC+1
      IF (EMMRC.EQ.1) CALL UGXERR(TLVL,TNAM,TIND)
C
C  SET COMMON BLOCK INDICATORS.
      UGELV=TLVL
      UGENM=TNAM
      UGEIX=TIND
C
C  PRINT ERROR MESSAGE IF NECESSARY.
  101 IF (TLVL.GE.2) THEN
        DO 102 INT1=1,EMMNM
          IF ((EMMSN(INT1).EQ.TNAM).AND.(EMMIX(INT1).EQ.TIND)) THEN
            IMES=INT1
            EMMPC(IMES)=EMMPC(IMES)+1
            IF (EMMPC(IMES).GT.EMMPM) GO TO 201
            GO TO 103
          END IF
  102   CONTINUE
        IMES=0
  103   WRITE (EMMEU,104) TNAM,TIND,TLVL
  104   FORMAT(//
     X    1H ,'******************************************************'/
     X    1H ,'*                                                    *'/
     X    1H ,'*     ERROR FOUND BY THE UNIFIED GRAPHICS SYSTEM     *'/
     X    1H ,'*                                                    *'/
     X    1H ,'*  SUBROUTINE: ',A8,'    INDEX:',I4,
     X        '      LEVEL:',I2,'  *')
        IF (IMES.NE.0) THEN
          WRITE (EMMEU,105)
  105     FORMAT(1H ,
     X      '*                                                    *')
          DO 107 INT1=1,EMMNL(IMES)
            WRITE (EMMEU,106) EMMTX(EMMOF(IMES)+INT1-1)
  106       FORMAT(1H ,'*  ',A48,'  *')
  107     CONTINUE
        END IF
        WRITE (EMMEU,108)
  108   FORMAT(
     X    1H ,'*                                                    *'/
     X    1H ,'******************************************************')
      END IF
C
C  CHECK FOR RECURSIVE USE AND TERMINATE FOR NON-RECURSIVE USE.
  201 IF (EMMRC.EQ.1) THEN
        EMMRC=0
        IF (TLVL.LT.3) RETURN
        IF (TLVL.LT.4) STOP
        CALL UGZ001
      END IF
C
C  GENERATE RECURSIVE USE ERROR MESSAGE.
      EMMRC=1
      TLVL=4
      TNAM='UGXERR  '
      TIND=1
      GO TO 101
C
      END
