      SUBROUTINE UGC002(SLIM)
C
C *******************  THE UNIFIED GRAPHICS SYSTEM  *******************
C *               LINE SCISSORING AND SHIELDING MODULE                *
C *                                                                   *
C *  THIS SUBROUTINE MAY BE USED TO SUPPLY A SHIELD TO THE LINE       *
C *  SCISSORING AND SHIELDING MODULE.                                 *
C *                                                                   *
C *  THE CALLING SEQUENCE IS:                                         *
C *    CALL UGC002(SLIM)                                              *
C *                                                                   *
C *  THE PARAMETER IN THE CALLING SEQUENCE IS:                        *
C *    SLIM  A ARRAY OF DIMENSION 2 BY 2 WHICH CONTAINS THE SHIELD    *
C *          LIMITS.  SLIM(1,1) IS THE LOW X VALUE, SLIM(1,2) IS THE  *
C *          HIGH X VALUE, SLIM(2,1) IS THE LOW Y VALUE, AND          *
C *          SLIM(2,2) IS THE HIGH Y VALUE.                           *
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
C  SAVE A SHIELD SPECIFICATION.
      NSHD=NSHD+1
      SHLD(NSHD,1,1)=SLIM(1,1)
      SHLD(NSHD,1,2)=SLIM(1,2)
      SHLD(NSHD,2,1)=SLIM(2,1)
      SHLD(NSHD,2,2)=SLIM(2,2)
C
C  RETURN TO CALLING SUBROUTINE.
      RETURN
C
      END
