      SUBROUTINE    UGF001(SLIM)
C
C *******************  THE UNIFIED GRAPHICS SYSTEM  *******************
C *                     POLYGON SCISSORING MODULE                     *
C *                                                                   *
C *  THIS IS THE FIRST OF THREE SUBROUTINES THAT MAY BE USED TO       *
C *  SCISSOR POLYGONS.  SUBROUTINE UGF001 IS USED TO INITIALIZE THE   *
C *  PROCESS, SUBROUTINE UGF002 IS USED TO SUPPLY A POLYGON TO THE    *
C *  POLYGON SCISSORING MODULE, AND UGF003 IS USED TO RETRIEVE A      *
C *  POLYGON FROM THE MODULE.  THE NORMAL SEQUENCE OF CALLS IS THE    *
C *  FOLLOWING: (1) UGF001 IS CALLED TO INITIALIZE PROCESSING, (2)    *
C *  UGF002 IS CALLED TO SUPPLY A POLYGON TO THE MODULES, (3) UGF003  *
C *  IS CALLED REPEATEDLY TO RETRIEVE POLYGONS UNTIL IT SIGNALS THAT  *
C *  NO MORE DATA IS AVAILABLE, AND (4) STEP 2 IS REPEATED UNTIL NO   *
C *  MORE INPUT IS AVAILABLE.                                         *
C *                                                                   *
C *  THIS SUBROUTINE MAY BE USED TO INITIALIZE THE POLYGON            *
C *  SCISSORING MODULE.                                               *
C *                                                                   *
C *  THE CALLING SEQUENCE IS:                                         *
C *    CALL UGF001(SLIM)                                              *
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
      INCLUDE       'UGSYSTEM:UGF00CBK.FOR'
C
C  INITIALIZE THE POLYGON SCISSORING PROCESS.
      LIMS(1,1)=SLIM(1,1)
      LIMS(1,2)=SLIM(1,2)
      LIMS(2,1)=SLIM(2,1)
      LIMS(2,2)=SLIM(2,2)
      NPOL=0
      MPOL=0
C
C  RETURN TO CALLING SUBROUTINE.
      RETURN
C
      END
