      SUBROUTINE    UGG002(SLIM)
C
C *******************  THE UNIFIED GRAPHICS SYSTEM  *******************
C *             THREE-DIMENSIONAL LINE SCISSORING MODULE              *
C *                                                                   *
C *  THIS SUBROUTINE MAY BE USED TO INITIALIZE THE SCISSORING OF      *
C *  THREE-DIMENSIONAL LINE SEGMENTS AGAINST A RECTANGULAR            *
C *  PARALLELOPIPED.                                                  *
C *                                                                   *
C *  THE CALLING SEQUENCE IS:                                         *
C *    CALL UGG002(SLIM)                                              *
C *                                                                   *
C *  THE PARAMETER IN THE CALLING SEQUENCE IS:                        *
C *    SLIM  A ARRAY OF DIMENSION 3 BY 2 WHICH CONTAINS THE           *
C *          SCISSORING LIMITS.  SLIM(1,1) IS THE LOW X VALUE,        *
C *          SLIM(2,1) IS THE LOW Y VALUE, SLIM(3,1) IS THE LOW Z     *
C *          VALUE, SLIM(1,2) IS THE HIGH X VALUE, SLIM(2,2) IS THE   *
C *          HIGH Y VALUE, AND SLIM(3,2) IS THE HIGH Z VALUE.         *
C *                                                                   *
C *                          ROBERT C. BEACH                          *
C *                    COMPUTATION RESEARCH GROUP                     *
C *                STANFORD LINEAR ACCELERATOR CENTER                 *
C *                                                                   *
C *********************************************************************
C
      REAL          SLIM(3,2)
C
      INCLUDE       'UGSYSTEM:UGG00CBK.FOR'
C
C  INITIALIZE FOR SCISSORING AGAINST A RECTANGULAR PARALLELOPIPED.
      ITYP=1
      LIMS(1,1)=SLIM(1,1)
      LIMS(1,2)=SLIM(1,2)
      LIMS(2,1)=SLIM(2,1)
      LIMS(2,2)=SLIM(2,2)
      LIMS(3,1)=SLIM(3,1)
      LIMS(3,2)=SLIM(3,2)
      IFLG=0
      NAVL=0
      KAVL=0
C
C  RETURN TO CALLING SUBROUTINE.
      RETURN
C
      END
