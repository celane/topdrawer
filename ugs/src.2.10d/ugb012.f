      SUBROUTINE UGB012(XWCS,YWCS,ZWCS,XUVP,YUVP)
C
C *******************  THE UNIFIED GRAPHICS SYSTEM  *******************
C *       TRANSFORM 3-D WORLD TO USER 3-D VIEW PORT COORDINATES       *
C *                                                                   *
C *  THIS SUBROUTINE MAY BE USED TO TRANSFORM A POINT IN THE THREE-   *
C *  DIMENSIONAL WORLD COORDINATE SYSTEM TO THE USER THREE-           *
C *  DIMENSIONAL VIEW PORT COORDINATE SYSTEM.                         *
C *                                                                   *
C *  THE CALLING SEQUENCE IS:                                         *
C *    CALL UGB012(XWCS,YWCS,ZWCS,XUVP,YUVP)                          *
C *                                                                   *
C *  THE PARAMETERS IN THE CALLING SEQUENCE ARE:                      *
C *    XWCS  X COORDINATE IN THE THREE-DIMENSIONAL WORLD SYSTEM.      *
C *    YWCS  Y COORDINATE IN THE THREE-DIMENSIONAL WORLD SYSTEM.      *
C *    ZWCS  Z COORDINATE IN THE THREE-DIMENSIONAL WORLD SYSTEM.      *
C *    XUVP  X COORDINATE IN THE USER THREE-DIMENSIONAL VIEW PORT.    *
C *    YUVP  Y COORDINATE IN THE USER THREE-DIMENSIONAL VIEW PORT.    *
C *                                                                   *
C *                          ROBERT C. BEACH                          *
C *                    COMPUTATION RESEARCH GROUP                     *
C *                STANFORD LINEAR ACCELERATOR CENTER                 *
C *                                                                   *
C *********************************************************************
C
      REAL          XWCS,YWCS,ZWCS
      REAL          XUVP,YUVP
C
      REAL          DENM
C
      INCLUDE       'UGSYSTEM:UGDDACBK.FOR'
C
C  TRANSFORM THE POINT.
      DENM= DDA3M(3,1)*XWCS+DDA3M(3,2)*YWCS+DDA3M(3,3)*ZWCS+DDA3M(3,4)
      XUVP=(DDA3M(1,1)*XWCS+DDA3M(1,2)*YWCS+DDA3M(1,3)*ZWCS+DDA3M(1,4))
     X     /DENM
      YUVP=(DDA3M(2,1)*XWCS+DDA3M(2,2)*YWCS+DDA3M(2,3)*ZWCS+DDA3M(2,4))
     X     /DENM
C
C  RETURN TO CALLER.
      RETURN
C
      END
