      SUBROUTINE UGB007(XNWC,YNWC,ZNWC,XDCS,YDCS,ZDCS)
C
C *******************  THE UNIFIED GRAPHICS SYSTEM  *******************
C *     TRANSFORM NORMALIZED 3-D WORLD TO 3-D DEVICE COORDINATES      *
C *                                                                   *
C *  THIS SUBROUTINE MAY BE USED TO TRANSFORM A POINT IN THE          *
C *  NORMALIZED THREE-DIMENSIONAL WORLD COORDINATE SYSTEM TO THE      *
C *  THREE-DIMENSIONAL DEVICE COORDINATE SYSTEM.                      *
C *                                                                   *
C *  THE CALLING SEQUENCE IS:                                         *
C *    CALL UGB007(XNWC,YNWC,ZNWC,XDCS,YDCS,ZDCS)                     *
C *                                                                   *
C *  THE PARAMETERS IN THE CALLING SEQUENCE ARE:                      *
C *    XNWC  X COORDINATE IN THE NORMALIZED THREE-DIMENSIONAL WORLD   *
C *          SYSTEM.                                                  *
C *    YNWC  Y COORDINATE IN THE NORMALIZED THREE-DIMENSIONAL WORLD   *
C *          SYSTEM.                                                  *
C *    ZNWC  Z COORDINATE IN THE NORMALIZED THREE-DIMENSIONAL WORLD   *
C *          SYSTEM.                                                  *
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
      REAL          XNWC,YNWC,ZNWC
      INTEGER       XDCS,YDCS,ZDCS
C
      INCLUDE       'UGSYSTEM:UGDDACBK.FOR'
C
C  TRANSFORM THE POINT.
      XDCS=NINT(DDA3T(4)*XNWC+DDA3T(7))
      YDCS=NINT(DDA3T(5)*YNWC+DDA3T(8))
      ZDCS=NINT(DDA3T(6)*ZNWC+DDA3T(9))
C
C  RETURN TO CALLER.
      RETURN
C
      END
