      SUBROUTINE UG2DH5(PNTI,PNT1,PNT2,PNTO)
C
C *******************  THE UNIFIED GRAPHICS SYSTEM  *******************
C *               PROJECT A POINT ONTO A STRAIGHT LINE                *
C *                                                                   *
C *  THIS SUBROUTINE IS USED BY UG2DH3 TO PROJECT A GIVEN POINT       *
C *  VERTICALLY ONTO A STRAIGHT LINE.  THE X COORDINATES OF THE ENDS  *
C *  OF THE STRAIGHT LINE MUST BE DISTINCT.                           *
C *                                                                   *
C *  THE CALLING SEQUENCE IS:                                         *
C *    CALL UG2DH5(PNTI,PNT1,PNT2,PNTO)                               *
C *                                                                   *
C *  THE PARAMETERS IN THE CALLING SEQUENCE ARE:                      *
C *    PNTI  THE GIVEN POINT THAT IS TO BE PROJECTED ONTO THE LINE.   *
C *    PNT1  THE FIRST END POINT OF THE LINE.                         *
C *    PNT2  THE SECOND END POINT OF THE LINE.                        *
C *    PNTO  THE COMPUTED POINT ON THE LINE.                          *
C *                                                                   *
C *                          ROBERT C. BEACH                          *
C *                    COMPUTATION RESEARCH GROUP                     *
C *                STANFORD LINEAR ACCELERATOR CENTER                 *
C *                                                                   *
C *********************************************************************
C
      REAL          PNTI(2),PNT1(2),PNT2(2),PNTO(2)
C
C  PROJECT THE POINT ONTO THE LINE.
      PNTO(1)=PNTI(1)
      PNTO(2)=(PNT1(2)*(PNT2(1)-PNTI(1))+
     X         PNT2(2)*(PNTI(1)-PNT1(1)))/(PNT2(1)-PNT1(1))
C
C  RETURN TO CALLING SUBROUTINE.
      RETURN
C
      END
