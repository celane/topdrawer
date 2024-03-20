      SUBROUTINE UGB004(XDCS,YDCS,XWCS,YWCS)
C
C *******************  THE UNIFIED GRAPHICS SYSTEM  *******************
C *               TRANSFORM DEVICE TO WORLD COORDINATES               *
C *                                                                   *
C *  THIS SUBROUTINE MAY BE USED TO TRANSFORM A POINT IN THE DEVICE   *
C *  COORDINATE SYSTEM TO THE WORLD COORDINATE SYSTEM.                *
C *                                                                   *
C *  THE CALLING SEQUENCE IS:                                         *
C *    CALL UGB004(XDCS,YDCS,XWCS,YWCS)                               *
C *                                                                   *
C *  THE PARAMETERS IN THE CALLING SEQUENCE ARE:                      *
C *    XDCS  X COORDINATE IN THE DEVICE SYSTEM.                       *
C *    YDCS  Y COORDINATE IN THE DEVICE SYSTEM.                       *
C *    XWCS  X COORDINATE IN THE WORLD SYSTEM.                        *
C *    YWCS  Y COORDINATE IN THE WORLD SYSTEM.                        *
C *                                                                   *
C *                          ROBERT C. BEACH                          *
C *                    COMPUTATION RESEARCH GROUP                     *
C *                STANFORD LINEAR ACCELERATOR CENTER                 *
C *                                                                   *
C *********************************************************************
C
      INTEGER       XDCS,YDCS
      REAL          XWCS,YWCS
C
      INCLUDE       'UGSYSTEM:UGDDACBK.FOR'
C
C  TRANSFORM THE POINT.
      XWCS=((REAL(XDCS)-DDATR(3))/DDATR(1))+DDAWS(1,1)
      YWCS=((REAL(YDCS)-DDATR(4))/DDATR(2))+DDAWS(2,1)
C
C  RETURN TO CALLER.
      RETURN
C
      END
