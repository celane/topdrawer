      SUBROUTINE UGB002(MODE,NMOD,NDAT,SEGM,INDX)
C
C *******************  THE UNIFIED GRAPHICS SYSTEM  *******************
C *           FIND SPACE IN A GRAPHIC SEGMENT FOR NEW DATA            *
C *                                                                   *
C *  THIS SUBROUTINE RECEIVES A MODE SPECIFICATION AND A DATA WORD    *
C *  COUNT.  IT THEN CHECKS TO SEE IF THIS DATA WILL FIT INTO A       *
C *  GRAPHIC SEGMENT.  IF IT WILL FIT, A NEW MODE SPECIFICATION IS    *
C *  INSERTED, IF NECESSARY, AND ALL POINTERS IN THE SEGMENT ARE      *
C *  UPDATED.  IF IT WILL NOT FIT, AN ERROR INDICATION IS RETURNED.   *
C *                                                                   *
C *  THE CALLING SEQUENCE IS:                                         *
C *    CALL UGB002(MODE,NMOD,NDAT,SEGM,INDX)                          *
C *                                                                   *
C *  THE PARAMETERS IN THE CALLING SEQUENCE ARE:                      *
C *    MODE  THE MODE SPECIFICATION BLOCK.                            *
C *    NMOD  THE NUMBER OF WORD IN THE MODE SPECIFICATION BLOCK.  IF  *
C *          THIS NUMBER IS NEGATIVE, IT INDICATES THAT A NEW MODE    *
C *          SPECIFICATION MUST BE INSERTED AND THE ABSOLUTE VALUE    *
C *          IS USED FOR THE COUNT.                                   *
C *    NDAT  THE NUMBER OF DATA WORDS TO BE ADDED.                    *
C *    SEGM  THE GRAPHIC SEGMENT WHICH IS TO HAVE THE DATA ADDED TO   *
C *          IT.                                                      *
C *    INDX  THE INDEX OF THE START OF THE DATA OR A ZERO IF THE      *
C *          DATA WILL NOT FIT.                                       *
C *                                                                   *
C *                          ROBERT C. BEACH                          *
C *                    COMPUTATION RESEARCH GROUP                     *
C *                STANFORD LINEAR ACCELERATOR CENTER                 *
C *                                                                   *
C *********************************************************************
C
      INTEGER*4     MODE(*)
      INTEGER       NMOD,NDAT
      INTEGER*4     SEGM(*)
      INTEGER       INDX
C
      INTEGER*4     CTMX,IXLB,KMOD
C
      INTEGER       INT1
C
C  INITIALIZE AND COMPARE OLD AND NEW MODE SPECIFICATIONS.
      CTMX=SEGM(SEGM(1)+1)
      IXLB=SEGM(3)
      KMOD=ABS(NMOD)
      IF (IXLB.EQ.0) GO TO 201
      IF (NMOD.LT.0) GO TO 201
      IF (SEGM(IXLB).NE.MODE(1)) GO TO 201
      DO 101 INT1=3,KMOD
        IF (SEGM(IXLB+INT1-1).NE.MODE(INT1)) GO TO 201
  101 CONTINUE
C
C  ADD DATA TO THE LAST EXISTING BLOCK.
      IF (SEGM(1)+NDAT.GT.CTMX) GO TO 401
      INDX=SEGM(1)+1
      SEGM(1)=SEGM(1)+NDAT
      SEGM(IXLB+1)=SEGM(IXLB+1)+NDAT
      SEGM(SEGM(1)+1)=CTMX
      GO TO 301
C
C  CREATE A NEW BLOCK IN THE SEGMENT.
  201 IF (SEGM(1)+KMOD+NDAT.GT.CTMX) GO TO 401
      IXLB=SEGM(1)+1
      INDX=IXLB+KMOD
      SEGM(1)=SEGM(1)+KMOD+NDAT
      SEGM(3)=IXLB
      DO 202 INT1=1,KMOD
        SEGM(IXLB+INT1-1)=MODE(INT1)
  202 CONTINUE
      SEGM(SEGM(1)+1)=CTMX
C
C  RETURN TO CALLER.
  301 RETURN
C
C  SET ERROR INDICATOR.
  401 INDX=0
      GO TO 301
C
      END
