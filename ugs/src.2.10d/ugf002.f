      SUBROUTINE    UGF002(XCDS,YCDS,NPTS,ERFG)
C
C *******************  THE UNIFIED GRAPHICS SYSTEM  *******************
C *                     POLYGON SCISSORING MODULE                     *
C *                                                                   *
C *  THIS SUBROUTINE MAY BE USED TO SUPPLY A POLYGON TO THE POLYGON   *
C *  SCISSORING MODULE.  THE FIRST AND LAST VERTICES OF THE POLYGON   *
C *  MUST BE IDENTICAL.                                               *
C *                                                                   *
C *  THE CALLING SEQUENCE IS:                                         *
C *    CALL UGF002(XCDS,YCDS,NPTS,ERFG)                               *
C *                                                                   *
C *  THE PARAMETERS IN THE CALLING SEQUENCE ARE:                      *
C *    XCDS  THE X COORDINATES OF THE POLYGON VERTICES.               *
C *    YCDS  THE Y COORDINATES OF THE POLYGON VERTICES.               *
C *    NPTS  THE NUMBER OF POLYGON VERTICES.                          *
C *    ERFG  AN ERROR FLAG (0 MEANS NO ERROR, 1 MEANS AN INTERNAL     *
C *          TABLE HAS OVERFLOWN).                                    *
C *                                                                   *
C *                          ROBERT C. BEACH                          *
C *                    COMPUTATION RESEARCH GROUP                     *
C *                STANFORD LINEAR ACCELERATOR CENTER                 *
C *                                                                   *
C *********************************************************************
C
      REAL          XCDS(*),YCDS(*)
      INTEGER       NPTS,ERFG
C
      INCLUDE       'UGSYSTEM:UGF00CBK.FOR'
C
      INTEGER       INT1
C
C  ACCEPT A POLYGON TO BE SCISSORED.
      NPOL=NPTS
      DO 101 INT1=1,NPOL
        XPOL(INT1)=XCDS(INT1)
        YPOL(INT1)=YCDS(INT1)
        FPOL(INT1)=.FALSE.
  101 CONTINUE
      FPOL(NPOL)=.TRUE.
      MPOL=1
C
C  DO THE ACTUAL SCISSORING.
      CALL UGF004( 1.0, 0.0,-LIMS(1,1),ERFG)
      IF (ERFG.NE.0) GO TO 201
      CALL UGF004(-1.0, 0.0, LIMS(1,2),ERFG)
      IF (ERFG.NE.0) GO TO 201
      CALL UGF004( 0.0, 1.0,-LIMS(2,1),ERFG)
      IF (ERFG.NE.0) GO TO 201
      CALL UGF004( 0.0,-1.0, LIMS(2,2),ERFG)
C
C  RETURN TO CALLING SUBROUTINE.
  201 RETURN
C
      END
