      SUBROUTINE UGB006(XWCS,YWCS,ZWCS,XNWC,YNWC,ZNWC)
C
C *******************  THE UNIFIED GRAPHICS SYSTEM  *******************
C *      TRANSFORM 3-D WORLD TO NORMALIZED 3-D WORLD COORDINATES      *
C *                                                                   *
C *  THIS SUBROUTINE MAY BE USED TO TRANSFORM A POINT IN THE THREE-   *
C *  DIMENSIONAL WORLD COORDINATE SYSTEM TO THE NORMALIZED THREE-     *
C *  DIMENSIONAL WORLD COORDINATE SYSTEM.                             *
C *                                                                   *
C *  THE CALLING SEQUENCE IS:                                         *
C *    CALL UGB006(XWCS,YWCS,ZWCS,XNWC,YNWC,ZNWC)                     *
C *                                                                   *
C *  THE PARAMETERS IN THE CALLING SEQUENCE ARE:                      *
C *    XWCS  X COORDINATE IN THE THREE-DIMENSIONAL WORLD SYSTEM.      *
C *    YWCS  Y COORDINATE IN THE THREE-DIMENSIONAL WORLD SYSTEM.      *
C *    ZWCS  Z COORDINATE IN THE THREE-DIMENSIONAL WORLD SYSTEM.      *
C *    XNWC  X COORDINATE IN THE NORMALIZED THREE-DIMENSIONAL WORLD   *
C *          SYSTEM.                                                  *
C *    YNWC  Y COORDINATE IN THE NORMALIZED THREE-DIMENSIONAL WORLD   *
C *          SYSTEM.                                                  *
C *    ZNWC  Z COORDINATE IN THE NORMALIZED THREE-DIMENSIONAL WORLD   *
C *          SYSTEM.                                                  *
C *                                                                   *
C *                          ROBERT C. BEACH                          *
C *                    COMPUTATION RESEARCH GROUP                     *
C *                STANFORD LINEAR ACCELERATOR CENTER                 *
C *                                                                   *
C *********************************************************************
C
      REAL          XWCS,YWCS,ZWCS
      REAL          XNWC,YNWC,ZNWC
C
      INCLUDE       'UGSYSTEM:UGDDACBK.FOR'
C
C  TRANSFORM THE POINT.
      XNWC=(XWCS-DDA3W(1,1))/DDA3T(1)
      YNWC=(YWCS-DDA3W(2,1))/DDA3T(2)
      ZNWC=(ZWCS-DDA3W(3,1))/DDA3T(3)
C
C  RETURN TO CALLER.
      RETURN
C
      END
