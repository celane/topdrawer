      SUBROUTINE UGIX01(DDIN,DDST,DDEX)
C
C *******************  THE UNIFIED GRAPHICS SYSTEM  *******************
C *               DUMMY DEVICE-DEPENDENT MODULE FOR THE               *
C *                    MODEL 8/300 PRINTER/PLOTTER                    *
C *                     OF THE IMAGEN CORPORATION                     *
C *     THE FILES PRODUCED BY THIS MODULE MUST BE SENT TO THE IBM     *
C *                      COMPUTER FOR PROCESSING                      *
C *                                                                   *
C *  THIS SUBROUTINE IS A DUMMY DEVICE-DEPENDENT MODULE.  IT WILL BE  *
C *  INCORPORATED INTO THE LOAD MODULE IF THE USER DOES NOT INCLUDE   *
C *  THE ACTUAL DEVICE-DEPENDENT MODULE.                              *
C *                                                                   *
C *  THE CALLING SEQUENCE IS:                                         *
C *    CALL UGIX01(DDIN,DDST,DDEX)                                    *
C *                                                                   *
C *  THE PARAMETERS IN THE CALLING SEQUENCE ARE:                      *
C *    DDIN  AN INTEGER INPUT ARRAY.                                  *
C *    DDST  A CHARACTER STRING FOR INPUT AND OUTPUT.                 *
C *    DDEX  AN INTEGER OUTPUT ARRAY.                                 *
C *                                                                   *
C *                          ROBERT C. BEACH                          *
C *                    COMPUTATION RESEARCH GROUP                     *
C *                STANFORD LINEAR ACCELERATOR CENTER                 *
C *                                                                   *
C *********************************************************************
C
      INTEGER       DDIN(*)
      CHARACTER*(*) DDST
      INTEGER       DDEX(*)
C
C  INDICATE THAT THE MODULE IS NOT AN ACTUAL DEVICE-DEPENDENT MODULE.
      DDEX(1)=-1
C
C  RETURN TO CALLER.
      RETURN
C
      END
