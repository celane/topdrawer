      SUBROUTINE UGLGDX(LODA,HIDA,MINL,MAXL,LOLB,HILB,NLAB)
C
C *******************  THE UNIFIED GRAPHICS SYSTEM  *******************
C *               AUXILIARY AXIS GENERATION PROGRAM                   *
C *                                                                   *
C *  THIS SUBROUTINE IS AN AID IN USING SUBROUTINE UGLGAX.  IF A      *
C *  PROGRAMMER DETERMINES THE EXTENT OF THE DATA AT EXECUTION TIME,  *
C *  IT CAN BE A PROBLEM TO SUPPLY VALUES OF THE ARGUMENTS LOLB,      *
C *  HILB, AND NLAB WHICH RESULT IN "ROUNDED NUMBERS" FOR THE         *
C *  LABELS.  THIS SUBROUTINE ACCEPTS AS ITS INPUT THE EXTENT OF THE  *
C *  DATA AND THE APPROXIMATE NUMBER OF LABELS TO BE USED.  ITS       *
C *  OUTPUT IS THE THREE ARGUMENTS FOR UGLGAX.                        *
C *                                                                   *
C *  THE CALLING SEQUENCE IS:                                         *
C *    CALL UGLGDX(LODA,HIDA,MINL,MAXL,LOLB,HILB,NLAB)                *
C *                                                                   *
C *  THE PARAMETERS IN THE CALLING SEQUENCE ARE:                      *
C *    LODA  LOW LIMIT OF ACTUAL DATA.                                *
C *    HIDA  HIGH LIMIT OF ACTUAL DATA.                               *
C *    MINL  MINIMUM NUMBER OF LABELS.                                *
C *    MAXL  MAXIMUM NUMBER OF LABELS.                                *
C *    LOLB  COMPUTED LOW LIMIT OF AXIS.                              *
C *    HILB  COMPUTED HIGH LIMIT OF AXIS.                             *
C *    NLAB  COMPUTED NUMBER OF LABELS.                               *
C *                                                                   *
C *                          ROBERT C. BEACH                          *
C *                    COMPUTATION RESEARCH GROUP                     *
C *                STANFORD LINEAR ACCELERATOR CENTER                 *
C *                                                                   *
C *********************************************************************
C
      REAL          LODA,HIDA
      INTEGER       MINL,MAXL
      REAL          LOLB,HILB
      INTEGER       NLAB
C
      INCLUDE       'UGSYSTEM:UGERRCBK.FOR'
C
      INTEGER       PVAL(3)
      INTEGER       NPVL
C
      REAL          DLOX,DHIX
C
      INTEGER       INT1
C
      DATA          PVAL/2,5,10/
      DATA          NPVL/3/
C
C  ADJUST THE GIVEN DATA LIMITS.
      DHIX=AINT(LOG10(HIDA))
      IF ((10.0**DHIX).LT.HIDA) DHIX=DHIX+1.0
      DLOX=AINT(LOG10(LODA))
      IF ((10.0**DLOX).GT.LODA) DLOX=DLOX-1.0
C  COMPUTE TENTATIVE OUTPUT VALUES.
      NLAB=INT(DHIX-DLOX+1.5)
      LOLB=10.0**DLOX
      HILB=10.0**DHIX
C  ADJUST THE VALUES IF THEY ARE NOT ACCEPTABLE.
      INT1=1
      IF (NLAB.LT.MINL) THEN
  101   IF (INT1.GT.NPVL) GO TO 301
        IF ((NLAB*PVAL(INT1)).GE.MINL) THEN
          NLAB=NLAB*PVAL(INT1)
          GO TO 201
        END IF
        INT1=INT1+1
        GO TO 101
      ELSE IF (NLAB.GT.MAXL) THEN
  102   IF (INT1.GT.NPVL) GO TO 301
        IF ((NLAB/PVAL(INT1)).LE.MAXL) THEN
          NLAB=NLAB/PVAL(INT1)
          IF (NLAB.LE.1) GO TO 301
          GO TO 201
        END IF
        INT1=INT1+1
        GO TO 102
      END IF
C
C  RESET ERROR INDICATORS AND RETURN TO CALLER.
  201 UGELV=0
      UGENM='        '
      UGEIX=0
  202 RETURN
C
C  REPORT ERRORS TO THE UNIFIED GRAPHICS SYSTEM ERROR PROCESSOR.
  301 CALL UGRERR(3,'UGLGDX  ',1)
      GO TO 202
C
      END
