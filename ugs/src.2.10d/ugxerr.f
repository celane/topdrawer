      SUBROUTINE UGXERR(LEVL,SNAM,INDX)
C
C *******************  THE UNIFIED GRAPHICS SYSTEM  *******************
C *                 USER ERROR PROCESSING SUBROUTINE                  *
C *                                                                   *
C *  THIS SUBROUTINE IS A DUMMY SUBROUTINE WHICH WILL BE LOADED IF    *
C *  THE USER DOES NOT SUPPLY HIS OR HER OWN ERROR PROCESSING         *
C *  SUBROUTINE.                                                      *
C *                                                                   *
C *  THE CALLING SEQUENCE IS:                                         *
C *    CALL UGXERR(LEVL,SNAM,INDX)                                    *
C *                                                                   *
C *  THE PARAMETERS IN THE CALLING SEQUENCE ARE:                      *
C *    LEVL  THE LEVEL OF THE ERROR.                                  *
C *    SNAM  THE NAME OF THE SUBROUTINE DETECTING THE ERROR PADDED    *
C *          TO EIGHT CHARACTERS.                                     *
C *    INDX  THE INDEX OF THE ERROR.                                  *
C *                                                                   *
C *                          ROBERT C. BEACH                          *
C *                    COMPUTATION RESEARCH GROUP                     *
C *                STANFORD LINEAR ACCELERATOR CENTER                 *
C *                                                                   *
C *********************************************************************
C
      INTEGER       LEVL
      CHARACTER*8   SNAM
      INTEGER       INDX
C
C  RETURN TO CALLING SUBROUTINE.
      RETURN
C
      END
