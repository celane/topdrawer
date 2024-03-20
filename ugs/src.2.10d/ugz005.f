
      SUBROUTINE UGZ005 ( DATA , DADR )
      REAL    DATA
      INTEGER DADR

*******************  THE UNIFIED GRAPHICS SYSTEM  *******************
*         SUBROUTINE TO DETERMINE A DATA ELEMENT'S ADDRESS          *
*                                                                   *
*  THIS SUBROUTINE MAY BE USED TO DETERMINE THE ADDRESS OF A DATA   *
*  ELEMENT (NOT A CHARACTER STRING) WITHIN THE CURRENT LOAD         *
*  MODULE.                                                          *
*                                                                   *
*  THE CALLING SEQUENCE IS:                                         *
*    CALL UGZ005(DATA,DADR)                                         *
*                                                                   *
*  THE PARAMETERS IN THE CALLING SEQUENCE ARE:                      *
*    DATA  THE DATA ELEMENT WHOSE ADDRESS IS NEEDED.                *
*    DADR  THE ADDRESS OF THE DATA ELEMENT.                         *
*                                                                   *
*                          ROBERT C. BEACH                          *
*                    COMPUTATION RESEARCH GROUP                     *
*                STANFORD LINEAR ACCELERATOR CENTER                 *
*                                                                   *
*********************************************************************

      DADR = LOC(DATA)
      RETURN
      END
