      SUBROUTINE UGDEFL(OPTN,IARY,XARY)
C
C *******************  THE UNIFIED GRAPHICS SYSTEM  *******************
C *                MANIPULATE THE PICTURE ITEM OPTIONS                *
C *                                                                   *
C *  THIS SUBROUTINE MAY BE USED TO SUPPLY, CHANGE, OR RETRIEVE THE   *
C *  DEFAULT VALUES FOR MANY OF THE OPTIONS ITEMS IN THE UGMARK,      *
C *  UGLINE, UGTEXT, ETC. SUBROUTINES.                                *
C *                                                                   *
C *  THE CALLING SEQUENCE IS:                                         *
C *    CALL UGDEFL(OPTN,IARY,XARY)                                    *
C *                                                                   *
C *  THE PARAMETER IN THE CALLING SEQUENCE IS:                        *
C *    OPTN  THE OPTIONS LIST.                                        *
C *    IARY  AN INTEGER ARRAY CONTAINING THE DEFAULT VALUES.          *
C *    XARY  A FLAOTING POINT ARRAY CONTAINING THE DEFAULT VALUES.    *
C *                                                                   *
C *                          ROBERT C. BEACH                          *
C *                    COMPUTATION RESEARCH GROUP                     *
C *                STANFORD LINEAR ACCELERATOR CENTER                 *
C *                                                                   *
C *********************************************************************
C
      CHARACTER*(*) OPTN
      INTEGER       IARY(*)
      REAL          XARY(*)

      INCLUDE       'UGSYSTEM:UGERRCBK.FOR'
      INCLUDE       'UGSYSTEM:UGPOTCBK.FOR'
      INCLUDE       'UGSYSTEM:UGPOTDCL.FOR'
C
      INTEGER        DESC_NUM
      PARAMETER     (DESC_NUM = 4)

      INTEGER*4     INST(4,DESC_NUM)
      CHARACTER*16  IFLAG(DESC_NUM)

      INTEGER*4      EXRS,EXSX(1)
      EQUIVALENCE   (EXRS,EXSX(1))
C
      DATA   (INST(I,1),I=1,4) / 1,3,1,1 /, IFLAG(1) /'GET '/
      DATA   (INST(I,2),I=1,4) / 1,3,1,2 /, IFLAG(2) /'PUT '/
      DATA   (INST(I,3),I=1,4) / 1,5,1,3 /, IFLAG(3) /'RESET '/
      DATA   (INST(I,4),I=1,4) / 1,3,1,2 /, IFLAG(4) /'SET '/
C
C  SCAN THE OPTIONS LIST.
      EXRS=0
      CALL UGOPTION(OPTN,DESC_NUM,INST,IFLAG,EXSX,EXSTR)
C
C  PROCESS THE "GET", "PUT", OR "RESET" OPTIONS.
      IF (EXRS.EQ.1) THEN
        IARY(1)=POTIL
        IARY(2)=POTCR
        IARY(3)=POTBL
        IARY(4)=POTMK
        IARY(5)=POTLS
        XARY(1)=POTSZ
        XARY(2)=POTDZ
        XARY(3)=POTAG
        IARY(6)=POTJF
        IARY(7)=POTCP
        IARY(8)=POTFX
        IARY(9)=POTCC
        IARY(10)=POTPI
      ELSE IF (EXRS.EQ.2) THEN
        POTIL=IARY(1)
        IF ((POTIL.LT.1).OR.(POTIL.GT.5)) POTIL=PODIL
        POTCR=IARY(2)
        IF ((POTCR.LT.1).OR.(POTCR.GT.8)) POTCR=PODCR
        POTBL=IARY(3)
        IF ((POTBL.LT.1).OR.(POTBL.GT.2)) POTBL=PODBL
        POTMK=IARY(4)
        IF ((POTMK.LT.-1).OR.(POTMK.GT.9)) POTMK=PODMK
        POTLS=IARY(5)
        IF ((POTLS.LT.1).OR.(POTLS.GT.4)) POTLS=PODLS
        IF ((XARY(1).GT.0.0).AND.(XARY(2).EQ.0.0)) THEN
          POTSZ=XARY(1)
          POTDZ=0.0
        ELSE IF ((XARY(1).EQ.0.0).AND.(XARY(2).GT.0.0)) THEN
          POTSZ=0.0
          POTDZ=XARY(2)
        ELSE
          POTSZ=0.0
          POTDZ=0.015
        END IF
        POTAG=XARY(3)
        POTJF=IARY(6)
        IF ((POTJF.LT.1).OR.(POTJF.GT.3)) POTJF=PODJF
        POTCP=IARY(7)
        IF ((POTCP.LT.1).OR.(POTCP.GT.3)) POTCP=PODCP
        POTFX=IARY(8)
        IF ((POTFX.LT.1).OR.(POTFX.GT.2)) POTFX=PODFX
        POTCC=IARY(9)
        IF ((POTCC.LT.0).OR.(POTCC.GT.3)) POTCC=PODCC
        POTPI=IARY(10)
      ELSE IF (EXRS.EQ.3) THEN
        POTIL=PODIL
        POTCR=PODCR
        POTBL=PODBL
        POTMK=PODMK
        POTLS=PODLS
        POTSZ=PODSZ
        POTDZ=PODDZ
        POTAG=PODAG
        POTJF=PODJF
        POTCP=PODCP
        POTFX=PODFX
        POTCC=PODCC
        POTPI=PODPI
      END IF
C
C  RE-SCAN THE OPTIONS LIST.
      EXIL=POTIL
      EXCR=POTCR
      EXBL=POTBL
      EXMK=POTMK
      EXLS=POTLS
      EXSZ=0.0
      EXDZ=0.0
      EXAG=POTAG
      EXJF=POTJF
      EXCP=POTCP
      EXFX=POTFX
      EXCC=POTCC
      EXPI=POTPI
      CALL UGOPTION(OPTN,DESC_NUM2,POTST,IFLAG2,EXST,EXSTR)
C
C  SET THE NEW DEFAULT VALUES.
      POTIL=EXIL
      POTCR=EXCR
      POTBL=EXBL
      POTMK=EXMK
      POTLS=EXLS
      IF (EXSZ.GT.0.0) THEN
        POTSZ=EXSZ
        POTDZ=0.0
      ELSE IF (EXDZ.GT.0.0) THEN
        POTSZ=0.0
        POTDZ=EXDZ
      END IF
      POTAG=EXAG
      POTJF=EXJF
      POTCP=EXCP
      POTFX=EXFX
      POTCC=EXCC
      POTPI=EXPI
C
C  RESET ERROR INDICATORS AND RETURN TO CALLER.
      UGELV=0
      UGENM='        '
      UGEIX=0
      RETURN
C
      END
