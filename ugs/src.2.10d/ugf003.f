      SUBROUTINE    UGF003(XCDS,YCDS,NPTS)
C
C *******************  THE UNIFIED GRAPHICS SYSTEM  *******************
C *                     POLYGON SCISSORING MODULE                     *
C *                                                                   *
C *  THIS SUBROUTINE MAY BE USED TO RETRIEVE A POLYGON FROM THE       *
C *  POLYGON SCISSORING MODULE.  THE FIRST AND LAST VERTEX OF THE     *
C *  RETURNED POLYGON WILL ALWAYS BE IDENTICAL.                       *
C *                                                                   *
C *  THE CALLING SEQUENCE IS:                                         *
C *    CALL UGF003(XCDS,YCDS,NPTS)                                    *
C *                                                                   *
C *  THE PARAMETERS IN THE CALLING SEQUENCE ARE:                      *
C *    XCDS  THE X COORDINATES OF THE POLYGON VERTICES.               *
C *    YCDS  THE Y COORDINATES OF THE POLYGON VERTICES.               *
C *    NPTS  THE NUMBER OF POLYGON VERTICES OR A -1 IF NO MORE        *
C *          POLYGONS ARE AVAILABLE.                                  *
C *                                                                   *
C *                          ROBERT C. BEACH                          *
C *                    COMPUTATION RESEARCH GROUP                     *
C *                STANFORD LINEAR ACCELERATOR CENTER                 *
C *                                                                   *
C *********************************************************************
C
      REAL          XCDS(*),YCDS(*)
      INTEGER       NPTS
C
      INCLUDE       'UGSYSTEM:UGF00CBK.FOR'
C
C  PASS A POLYGON BACK TO THE CALLING SUBROUTINE.
      IF (NPOL.LE.MPOL) THEN
        NPTS=-1
      ELSE
        NPTS=0
  101   NPTS=NPTS+1
        XCDS(NPTS)=XPOL(MPOL)
        YCDS(NPTS)=YPOL(MPOL)
        MPOL=MPOL+1
        IF (.NOT.FPOL(MPOL-1)) GO TO 101
      END IF
C
C  RETURN TO CALLING SUBROUTINE.
      RETURN
C
      END
