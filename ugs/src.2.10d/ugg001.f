      SUBROUTINE    UGG001(PLAN)
C
C *******************  THE UNIFIED GRAPHICS SYSTEM  *******************
C *             THREE-DIMENSIONAL LINE SCISSORING MODULE              *
C *                                                                   *
C *  THIS IS THE FIRST OF FOUR SUBROUTINES THAT MAY BE USED TO        *
C *  SCISSOR THREE-DIMENSIONAL LINE SEGMENTS.  THE LINE SEGMENTS MAY  *
C *  BE SCISSORED AGAINST A PLANE OR A RECTANGULAR PARALLELOPIPED.    *
C *  SUBROUTINE UGG001 IS USED TO INITIALIZE FOR SCISSORING AGAINST   *
C *  A PLANE WHILE UGG002 IS USED TO INITIALIZE FOR SCISSORING        *
C *  AGAINST A RECTANGULAR PARALLELOPIPED.  SUBROUTINE UGG003 IS      *
C *  USED TO SUPPLY A LINE SEGMENT END POINT TO THE SCISSORING        *
C *  MODULE AND SUBROUTINE UGG004 IS USED TO RETRIEVE A LINE SEGMENT  *
C *  END POINTS FROM THE MODULE.  THE NORMAL SEQUENCE OF CALLS IS     *
C *  THE FOLLOWING: (1) UGG001 OR UGG002 IS CALLED TO INITIALIZE      *
C *  PROCESSING, (2) UGG003 IS CALLED TO SUPPLY AN END POINT TO THE   *
C *  MODULES, (3) UGG004 IS CALLED REPEATEDLY TO RETRIEVE END POINTS  *
C *  OF LINES UNTIL IT SIGNALS THAT NO MORE DATA IS AVAILABLE, AND    *
C *  (4) STEP 2 IS REPEATED UNTIL NO MORE INPUT IS AVAILABLE.         *
C *                                                                   *
C *  THIS SUBROUTINE MAY BE USED TO INITIALIZE THE SCISSORING OF      *
C *  THREE-DIMENSIONAL LINE SEGMENTS AGAINST A PLANE.                 *
C *                                                                   *
C *  THE CALLING SEQUENCE IS:                                         *
C *    CALL UGG001(PLAN)                                              *
C *                                                                   *
C *  THE PARAMETER IN THE CALLING SEQUENCE IS:                        *
C *    PLAN  THE EQUATION OF THE SCISSORING PLANE.  THE POINTS ON     *
C *          THE POSITIVE SIDE OF THE PLANE ARE THE ONES WHICH WILL   *
C *          BE SAVED.                                                *
C *                                                                   *
C *                          ROBERT C. BEACH                          *
C *                    COMPUTATION RESEARCH GROUP                     *
C *                STANFORD LINEAR ACCELERATOR CENTER                 *
C *                                                                   *
C *********************************************************************
C
      REAL          PLAN(4)
C
      INCLUDE       'UGSYSTEM:UGG00CBK.FOR'
C
C  INITIALIZE FOR SCISSORING AGAINST A PLANE.
      ITYP=0
      EQUN(1)=PLAN(1)
      EQUN(2)=PLAN(2)
      EQUN(3)=PLAN(3)
      EQUN(4)=PLAN(4)
      IFLG=0
      NAVL=0
      KAVL=0
C
C  RETURN TO CALLING SUBROUTINE.
      RETURN
C
      END
