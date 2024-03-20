      SUBROUTINE UGB003(XWCS,YWCS,XDCS,YDCS)
C
C *******************  THE UNIFIED GRAPHICS SYSTEM  *******************
C *               TRANSFORM WORLD TO DEVICE COORDINATES               *
C *                                                                   *
C *  THIS SUBROUTINE MAY BE USED TO TRANSFORM A POINT IN THE WORLD    *
C *  COORDINATE SYSTEM TO THE DEVICE COORDINATE SYSTEM.               *
C *                                                                   *
C *  THE CALLING SEQUENCE IS:                                         *
C *    CALL UGB003(XWCS,YWCS,XDCS,YDCS)                               *
C *                                                                   *
C *  THE PARAMETERS IN THE CALLING SEQUENCE ARE:                      *
C *    XWCS  X COORDINATE IN THE WORLD SYSTEM.                        *
C *    YWCS  Y COORDINATE IN THE WORLD SYSTEM.                        *
C *    XDCS  X COORDINATE IN THE DEVICE SYSTEM.                       *
C *    YDCS  Y COORDINATE IN THE DEVICE SYSTEM.                       *
C *                                                                   *
C *                          ROBERT C. BEACH                          *
C *                    COMPUTATION RESEARCH GROUP                     *
C *                STANFORD LINEAR ACCELERATOR CENTER                 *
C *                                                                   *
C *********************************************************************
C
      REAL          XWCS,YWCS
      INTEGER       XDCS,YDCS
C
      INCLUDE       'UGSYSTEM:UGDDACBK.FOR'
C
C  TRANSFORM THE POINT.
      XDCS=NINT(DDATR(1)*(XWCS-DDAWS(1,1))+DDATR(3))
      YDCS=NINT(DDATR(2)*(YWCS-DDAWS(2,1))+DDATR(4))
C
C  RETURN TO CALLER.
      RETURN
C
      END
