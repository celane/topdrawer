      SUBROUTINE UGB008(XWCS,YWCS,ZWCS,XDCS,YDCS,ZDCS)
C
C *******************  THE UNIFIED GRAPHICS SYSTEM  *******************
C *           TRANSFORM 3-D WORLD TO 3-D DEVICE COORDINATES           *
C *                                                                   *
C *  THIS SUBROUTINE MAY BE USED TO TRANSFORM A POINT IN THE THREE-   *
C *  DIMENSIONAL WORLD COORDINATE SYSTEM TO THE THREE-DIMENSIONAL     *
C *  DEVICE COORDINATE SYSTEM.                                        *
C *                                                                   *
C *  THE CALLING SEQUENCE IS:                                         *
C *    CALL UGB008(XWCS,YWCS,ZWCS,XDCS,YDCS,ZDCS)                     *
C *                                                                   *
C *  THE PARAMETERS IN THE CALLING SEQUENCE ARE:                      *
C *    XWCS  X COORDINATE IN THE THREE-DIMENSIONAL WORLD SYSTEM.      *
C *    YWCS  Y COORDINATE IN THE THREE-DIMENSIONAL WORLD SYSTEM.      *
C *    ZWCS  Z COORDINATE IN THE THREE-DIMENSIONAL WORLD SYSTEM.      *
C *    XDCS  X COORDINATE IN THE THREE-DIMENSIONAL DEVICE SYSTEM.     *
C *    YDCS  Y COORDINATE IN THE THREE-DIMENSIONAL DEVICE SYSTEM.     *
C *    ZDCS  Z COORDINATE IN THE THREE-DIMENSIONAL DEVICE SYSTEM.     *
C *                                                                   *
C *                          ROBERT C. BEACH                          *
C *                    COMPUTATION RESEARCH GROUP                     *
C *                STANFORD LINEAR ACCELERATOR CENTER                 *
C *                                                                   *
C *********************************************************************
C
      REAL          XWCS,YWCS,ZWCS
      INTEGER       XDCS,YDCS,ZDCS
C
      INCLUDE       'UGSYSTEM:UGDDACBK.FOR'
C
C  TRANSFORM THE POINT.
      XDCS=NINT(DDA3T(10)*(XWCS-DDA3W(1,1))+DDA3T(7))
      YDCS=NINT(DDA3T(11)*(YWCS-DDA3W(2,1))+DDA3T(8))
      ZDCS=NINT(DDA3T(12)*(ZWCS-DDA3W(3,1))+DDA3T(9))
C
C  RETURN TO CALLER.
      RETURN
C
      END
