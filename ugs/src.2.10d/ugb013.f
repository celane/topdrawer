      SUBROUTINE UGB013(XUVP,YUVP,XDVP,YDVP)
C
C *******************  THE UNIFIED GRAPHICS SYSTEM  *******************
C * TRANSFORM USER 3-D VIEW PORT TO DEVICE 3-D VIEW PORT COORDINATES  *
C *                                                                   *
C *  THIS SUBROUTINE MAY BE USED TO TRANSFORM A POINT IN THE USER     *
C *  THREE-DIMENSIONAL VIEW PORT SYSTEM TO THE DEVICE THREE-          *
C *  DIMENSIONAL VIEW PORT COORDINATE SYSTEM.                         *
C *                                                                   *
C *  THE CALLING SEQUENCE IS:                                         *
C *    CALL UGB013(XUVP,YUVP,XDVP,YDVP)                               *
C *                                                                   *
C *  THE PARAMETERS IN THE CALLING SEQUENCE ARE:                      *
C *    XUVP  X COORDINATE IN THE USER THREE-DIMENSIONAL VIEW PORT.    *
C *    YUVP  Y COORDINATE IN THE USER THREE-DIMENSIONAL VIEW PORT.    *
C *    XDVP  X COORDINATE IN THE DEVICE THREE-DIMENSIONAL VIEW PORT.  *
C *    YDVP  Y COORDINATE IN THE DEVICE THREE-DIMENSIONAL VIEW PORT.  *
C *                                                                   *
C *                          ROBERT C. BEACH                          *
C *                    COMPUTATION RESEARCH GROUP                     *
C *                STANFORD LINEAR ACCELERATOR CENTER                 *
C *                                                                   *
C *********************************************************************
C
      REAL          XUVP,YUVP
      INTEGER       XDVP,YDVP
C
      INCLUDE       'UGSYSTEM:UGDDACBK.FOR'
C
C  TRANSFORM THE POINT.
      XDVP=NINT(DDA3T(13)*(XUVP-DDA3V(1,1))+DDA3T(15))
      YDVP=NINT(DDA3T(14)*(YUVP-DDA3V(2,1))+DDA3T(16))
C
C  RETURN TO CALLER.
      RETURN
C
      END
