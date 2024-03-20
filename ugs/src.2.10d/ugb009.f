      SUBROUTINE UGB009(XDCS,YDCS,ZDCS,XWCS,YWCS,ZWCS)
C
C *******************  THE UNIFIED GRAPHICS SYSTEM  *******************
C *           TRANSFORM 3-D DEVICE TO 3-D WORLD COORDINATES           *
C *                                                                   *
C *  THIS SUBROUTINE MAY BE USED TO TRANSFORM A POINT IN THE THREE-   *
C *  DIMENSIONAL DEVICE COORDINATE SYSTEM TO THE THREE-DIMENSIONAL    *
C *  WORLD COORDINATE SYSTEM.                                         *
C *                                                                   *
C *  THE CALLING SEQUENCE IS:                                         *
C *    CALL UGB009(XDCS,YDCS,ZDCS,XWCS,YWCS,ZWCS)                     *
C *                                                                   *
C *  THE PARAMETERS IN THE CALLING SEQUENCE ARE:                      *
C *    XDCS  X COORDINATE IN THE THREE-DIMENSIONAL DEVICE SYSTEM.     *
C *    YDCS  Y COORDINATE IN THE THREE-DIMENSIONAL DEVICE SYSTEM.     *
C *    ZDCS  Z COORDINATE IN THE THREE-DIMENSIONAL DEVICE SYSTEM.     *
C *    XWCS  X COORDINATE IN THE THREE-DIMENSIONAL WORLD SYSTEM.      *
C *    YWCS  Y COORDINATE IN THE THREE-DIMENSIONAL WORLD SYSTEM.      *
C *    ZWCS  Z COORDINATE IN THE THREE-DIMENSIONAL WORLD SYSTEM.      *
C *                                                                   *
C *                          ROBERT C. BEACH                          *
C *                    COMPUTATION RESEARCH GROUP                     *
C *                STANFORD LINEAR ACCELERATOR CENTER                 *
C *                                                                   *
C *********************************************************************
C
      INTEGER       XDCS,YDCS,ZDCS
      REAL          XWCS,YWCS,ZWCS
C
      INCLUDE       'UGSYSTEM:UGDDACBK.FOR'
C
C  TRANSFORM THE POINT.
      XWCS=((REAL(XDCS)-DDA3T(7))/DDA3T(10))+DDA3W(1,1)
      YWCS=((REAL(YDCS)-DDA3T(8))/DDA3T(11))+DDA3W(2,1)
      ZWCS=((REAL(ZDCS)-DDA3T(9))/DDA3T(12))+DDA3W(3,1)
C
C  RETURN TO CALLER.
      RETURN
C
      END
