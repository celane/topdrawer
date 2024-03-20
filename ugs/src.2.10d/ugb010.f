      SUBROUTINE UGB010
C
C *******************  THE UNIFIED GRAPHICS SYSTEM  *******************
C *      PROCESS THREE-DIMENSIONAL TRANSFORMATION DATA (PART 1)       *
C *                                                                   *
C *  THIS SUBROUTINE IS USED TO DO THE INITIAL PROCESSING OF THREE-   *
C *  DIMENSIONAL TRANSFORMATION DATA.  IT IS CALLED WHENEVER A NEW    *
C *  THREE-DIMENSIONAL VIEW PORT AND THREE-DIMENSIONAL WORLD VOLUME   *
C *  ARE AVAILABLE.                                                   *
C *                                                                   *
C *  THE CALLING SEQUENCE IS:                                         *
C *    CALL UGB010                                                    *
C *                                                                   *
C *  THERE ARE NO PARAMETERS IN THE CALLING SEQUENCE.                 *
C *                                                                   *
C *                          ROBERT C. BEACH                          *
C *                    COMPUTATION RESEARCH GROUP                     *
C *                STANFORD LINEAR ACCELERATOR CENTER                 *
C *                                                                   *
C *********************************************************************
C
      INCLUDE       'UGSYSTEM:UGDDACBK.FOR'
C
      REAL          FLT1,FLT2,FLT3
      INTEGER       INT1,INT2
C
C  INITIALIZE THE TRANSFORMATION DATA.
      FLT1=DDA3W(1,2)-DDA3W(1,1)
      FLT2=DDA3W(2,2)-DDA3W(2,1)
      FLT3=DDA3W(3,2)-DDA3W(3,1)
      DDA3O(1,1)=DDA3W(1,1)+0.45*FLT1
      DDA3O(2,1)=DDA3W(2,1)+0.45*FLT2
      DDA3O(3,1)=DDA3W(3,1)+0.45*FLT3
      DDA3O(1,2)=DDA3W(1,2)-0.45*FLT1
      DDA3O(2,2)=DDA3W(2,2)-0.45*FLT2
      DDA3O(3,2)=DDA3W(3,2)-0.45*FLT3
      DDA3E(1)=0.5*(DDA3W(1,2)+DDA3W(1,1))
      DDA3E(2)=0.5*(DDA3W(2,2)+DDA3W(2,1))
      DDA3E(3)=0.5*(DDA3W(3,2)+DDA3W(3,1))+0.25*FLT3
      DDA3U(1)=0.0
      DDA3U(2)=1.0
      DDA3U(3)=0.0
      DDA3N=0.01
      DO 102 INT1=1,2
        DO 101 INT2=1,2
          DDAXV(INT1,INT2)=NINT(REAL(DDADD(INT1,1))+
     X        (REAL(DDADD(INT1,   2)-DDADD(INT1,1))*
     X             (DDA3V(INT1,INT2)-DDADS(INT1,1))/
     X             (DDADS(INT1,   2)-DDADS(INT1,1))))
  101   CONTINUE
  102 CONTINUE
C
C  RETURN TO CALLER.
      RETURN
C
      END
