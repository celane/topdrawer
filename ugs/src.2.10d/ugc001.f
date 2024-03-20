      SUBROUTINE UGC001(SLIM)
C
C *******************  THE UNIFIED GRAPHICS SYSTEM  *******************
C *               LINE SCISSORING AND SHIELDING MODULE                *
C *                                                                   *
C *  THIS IS THE FIRST OF FOUR SUBROUTINES THAT MAY BE USED TO        *
C *  SCISSOR AND SHIELD LINE SEGMENTS.  SUBROUTINE UGC001 IS USED TO  *
C *  INITIALIZE THE PROCESS AND SUBROUTINE UGC002 MAY SUPPLY A        *
C *  NUMBER OF OPTIONAL SHIELD SPECIFICATIONS.  SUBROUTINE UGC003 IS  *
C *  USED TO SUPPLY A LINE SEGMENT END POINT TO THE SCISSORING AND    *
C *  SHIELDING MODULE, AND SUBROUTINE UGC004 IS USED TO RETRIEVE A    *
C *  LINE SEGMENT END POINT FROM THE MODULE.  THE NORMAL SEQUENCE OF  *
C *  CALLS IS THE FOLLOWING: (1) UGC001 IS CALLED TO INITIALIZE       *
C *  PROCESSING, (2) UGC002 IS CALLED A NUMBER OF TIMES TO SUPPLY     *
C *  THE OPTIONAL SHIELD SPECIFICATIONS, (3) UGC003 IS CALLED TO      *
C *  SUPPLY AND END POINT TO THE MODULES, (4) UGC004 IS CALLED        *
C *  REPEATEDLY TO RETRIEVE END POINTS OF LINES UNTIL IT SIGNALS      *
C *  THAT NO MORE DATA IS AVAILABLE, AND (5) STEP 3 IS REPEATED       *
C *  UNTIL NO MORE INPUT IS AVAILABLE.                                *
C *                                                                   *
C *  THIS SUBROUTINE MAY BE USED TO INITIALIZE SCISSORING AND         *
C *  SHIELDING FOR LINE SEGMENTS.                                     *
C *                                                                   *
C *  THE CALLING SEQUENCE IS:                                         *
C *    CALL UGC001(SLIM)                                              *
C *                                                                   *
C *  THE PARAMETER IN THE CALLING SEQUENCE IS:                        *
C *    SLIM  A ARRAY OF DIMENSION 2 BY 2 WHICH CONTAINS THE           *
C *          SCISSORING LIMITS.  SLIM(1,1) IS THE LOW X VALUE,        *
C *          SLIM(1,2) IS THE HIGH X VALUE, SLIM(2,1) IS THE LOW Y    *
C *          VALUE, AND SLIM(2,2) IS THE HIGH Y VALUE.                *
C *                                                                   *
C *                          ROBERT C. BEACH                          *
C *                    COMPUTATION RESEARCH GROUP                     *
C *                STANFORD LINEAR ACCELERATOR CENTER                 *
C *                                                                   *
C *********************************************************************
C
      REAL          SLIM(2,2)
C
      INCLUDE       'UGSYSTEM:UGC00CBK.FOR'
C
C  INITIALIZE THE LINE SCISSORING AND SHIELDING PROCESS.
      LIMS(1,1)=SLIM(1,1)
      LIMS(1,2)=SLIM(1,2)
      LIMS(2,1)=SLIM(2,1)
      LIMS(2,2)=SLIM(2,2)
      IFLG=0
      NSHD=0
      KAVL=0
      NAVL=0
C
C  RETURN TO CALLING SUBROUTINE.
      RETURN
C
      END
