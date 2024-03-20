      SUBROUTINE UG3TRN(OPTN,OVOL,EYPT,UDIR,PFLG)
C
C *******************  THE UNIFIED GRAPHICS SYSTEM  *******************
C *            DEFINE THE THREE-DIMENSIONAL TRANSFORMATION            *
C *                                                                   *
C *  THIS SUBROUTINE WILL DEFINE THE TRANSFORMATION FROM THREE-       *
C *  DIMENSIONAL SPACE TO THE THREE-DIMENSIONAL WINDOW.  THE CURRENT  *
C *  PARAMETERS MAY ALSO BE RETRIEVED.                                *
C *                                                                   *
C *  THE CALLING SEQUENCE IS:                                         *
C *    CALL UG3TRN(OPTN,OVOL,EYPT,UDIR,PFLG)                          *
C *                                                                   *
C *  THE PARAMETERS IN THE CALLING SEQUENCE ARE:                      *
C *    OPTN  THE OPTIONS LIST.                                        *
C *    OVOL  THE LIMITS OF THE OBJECT VOLUME.                         *
C *    EYPT  THE EYE POINT.                                           *
C *    UDIR  THE UPWARD DIRECTION.                                    *
C *    PFLG  THE PROJECTION FLAG.                                     *
C *                                                                   *
C *                          ROBERT C. BEACH                          *
C *                    COMPUTATION RESEARCH GROUP                     *
C *                STANFORD LINEAR ACCELERATOR CENTER                 *
C *                                                                   *
C *********************************************************************
C
      CHARACTER*(*) OPTN
      REAL          OVOL(3,2),EYPT(3),UDIR(3),PFLG
C
      INCLUDE       'UGSYSTEM:UGDDACBK.FOR'
C
      INCLUDE       'UGSYSTEM:UGERRCBK.FOR'
C
      INTEGER*4     EXST(2),EXGP,EXRM
      EQUIVALENCE   (EXGP,EXST(1)),       (EXRM,EXST(2))
C
      INTEGER       DDIN(2),DDEX(1)
      INTEGER       ERFG
C
      INTEGER       INT1
      INTEGER       DESC_NUM
      PARAMETER    (DESC_NUM = 3)
      INTEGER*4     INST(4,DESC_NUM)
      CHARACTER*16  IFLAG(DESC_NUM)
C
      DATA    (INST(I,1),I=1,4) / 1,3,1,1 /,IFLAG(1) /'GET'/
      DATA    (INST(I,2),I=1,4) / 1,3,1,2 /,IFLAG(2) /'PUT'/
      DATA    (INST(I,3),I=1,4) / 1,6,2,1 /,IFLAG(3) /'REMOTE'/
C
C  IS THERE AN ACTIVE DEVICE?
      IF (DDAAI.EQ.0) GO TO 307
C
C  SCAN THE OPTIONS LIST.
      EXGP=0
      EXRM=0
      CALL UGOPTION(OPTN,DESC_NUM,INST,IFLAG,EXST,EXSTR)
C
C  PERFORM THE REQUESTED OPERATION.
      IF (EXGP.EQ.1) THEN
        IF (EXRM.EQ.0) THEN
          ERFG=0
  101     OVOL(1,1)=DDA3O(1,1)
          OVOL(2,1)=DDA3O(2,1)
          OVOL(3,1)=DDA3O(3,1)
          OVOL(1,2)=DDA3O(1,2)
          OVOL(2,2)=DDA3O(2,2)
          OVOL(3,2)=DDA3O(3,2)
          EYPT(1)=DDA3E(1)
          EYPT(2)=DDA3E(2)
          EYPT(3)=DDA3E(3)
          UDIR(1)=DDA3U(1)
          UDIR(2)=DDA3U(2)
          UDIR(3)=DDA3U(3)
          PFLG=DDA3N
          IF (ERFG.NE.0) GO TO 306
        ELSE
          IF (DDADF.NE.3) THEN
            ERFG=1
            GO TO 101
          END IF
          DDIN(1)=15
          DDIN(2)=2
          CALL UGZ006(DDAAC,0,0,DDIN,'        ',DDEX)
          IF (DDEX(1).NE.0) THEN
            ERFG=1
            GO TO 101
          END IF
          CALL UGB009(DDAXO(1,1),DDAXO(2,1),DDAXO(3,1),
     X                 OVOL(1,1), OVOL(2,1), OVOL(3,1))
          CALL UGB009(DDAXO(1,2),DDAXO(2,2),DDAXO(3,2),
     X                 OVOL(1,2), OVOL(2,2), OVOL(3,2))
          CALL UGB009(DDAXE(1),DDAXE(2),DDAXE(3),
     X                 EYPT(1), EYPT(2), EYPT(3))
          UDIR(1)=REAL(DDAXU(1))/DDA3T(10)
          UDIR(2)=REAL(DDAXU(2))/DDA3T(11)
          UDIR(3)=REAL(DDAXU(3))/DDA3T(12)
          CALL UGB014(UDIR,INT1)
          PFLG=REAL(DDAXN)/(2.0**30)
        END IF
      ELSE IF (EXGP.EQ.2) THEN
C  CHECK INPUT DATA FOR CONSISTENCY.
        IF (OVOL(1,1).GE.OVOL(1,2)) GO TO 301
        IF (OVOL(2,1).GE.OVOL(2,2)) GO TO 301
        IF (OVOL(3,1).GE.OVOL(3,2)) GO TO 301
        IF (OVOL(1,1).LT.DDA3W(1,1)) GO TO 302
        IF (OVOL(1,2).GT.DDA3W(1,2)) GO TO 302
        IF (OVOL(2,1).LT.DDA3W(2,1)) GO TO 302
        IF (OVOL(2,2).GT.DDA3W(2,2)) GO TO 302
        IF (OVOL(3,1).LT.DDA3W(3,1)) GO TO 302
        IF (OVOL(3,2).GT.DDA3W(3,2)) GO TO 302
        IF (EYPT(1).LT.DDA3W(1,1)) GO TO 303
        IF (EYPT(1).GT.DDA3W(1,2)) GO TO 303
        IF (EYPT(2).LT.DDA3W(2,1)) GO TO 303
        IF (EYPT(2).GT.DDA3W(2,2)) GO TO 303
        IF (EYPT(3).LT.DDA3W(3,1)) GO TO 303
        IF (EYPT(3).GT.DDA3W(3,2)) GO TO 303
        IF (PFLG.LT.0.0) GO TO 305
        IF (PFLG.GE.1.0) GO TO 305
C  TRANSFER THE DATA TO THE DDA.
        DDA3O(1,1)=OVOL(1,1)
        DDA3O(2,1)=OVOL(2,1)
        DDA3O(3,1)=OVOL(3,1)
        DDA3O(1,2)=OVOL(1,2)
        DDA3O(2,2)=OVOL(2,2)
        DDA3O(3,2)=OVOL(3,2)
        DDA3E(1)=EYPT(1)
        DDA3E(2)=EYPT(2)
        DDA3E(3)=EYPT(3)
        DDA3U(1)=UDIR(1)
        DDA3U(2)=UDIR(2)
        DDA3U(3)=UDIR(3)
        DDA3N=PFLG
        CALL UGB011(INT1)
        IF (INT1.NE.0) GO TO 304
C  SEND THE PROJECTION PARAMETERS TO A THREE-DIMENSIONAL DEVICE.
        IF (DDADF.EQ.3) THEN
          DDIN(1)=15
          DDIN(2)=1
          CALL UGZ006(DDAAC,0,0,DDIN,'        ',DDEX)
        END IF
      END IF
C
C  RESET ERROR INDICATORS AND RETURN TO CALLER.
      UGELV=0
      UGENM='        '
      UGEIX=0
  201 RETURN
C
C  REPORT ERRORS TO THE UNIFIED GRAPHICS SYSTEM ERROR PROCESSOR.
  301 CALL UGRERR(3,'UG3TRN  ', 1)
      GO TO 201
  302 CALL UGRERR(3,'UG3TRN  ', 2)
      GO TO 201
  303 CALL UGRERR(3,'UG3TRN  ', 3)
      GO TO 201
  304 CALL UGRERR(3,'UG3TRN  ', 4)
      GO TO 201
  305 CALL UGRERR(3,'UG3TRN  ', 5)
      GO TO 201
  306 CALL UGRERR(2,'UG3TRN  ', 6)
      GO TO 201
  307 CALL UGRERR(3,'UG3TRN  ',12)
      GO TO 201
C
      END
