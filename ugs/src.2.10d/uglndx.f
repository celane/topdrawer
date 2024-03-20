      SUBROUTINE UGLNDX(LODA,HIDA,MINL,MAXL,LOLB,HILB,NLAB)
C
C *******************  THE UNIFIED GRAPHICS SYSTEM  *******************
C *               AUXILIARY AXIS GENERATION PROGRAM                   *
C *                                                                   *
C *  THIS SUBROUTINE IS AN AID IN USING SUBROUTINE UGLNAX.  IF A      *
C *  PROGRAMMER DETERMINES THE EXTENT OF THE DATA AT EXECUTION TIME,  *
C *  IT CAN BE A PROBLEM TO SUPPLY VALUES OF THE ARGUMENTS LOLB,      *
C *  HILB, AND NLAB WHICH RESULT IN "ROUNDED NUMBERS" FOR THE         *
C *  LABELS.  THIS SUBROUTINE ACCEPTS AS ITS INPUT THE EXTENT OF THE  *
C *  DATA AND THE APPROXIMATE NUMBER OF LABELS TO BE USED.  ITS       *
C *  OUTPUT IS THE THREE ARGUMENTS FOR UGLNAX.                        *
C *                                                                   *
C *  THE CALLING SEQUENCE IS:                                         *
C *    CALL UGLNDX(LODA,HIDA,MINL,MAXL,LOLB,HILB,NLAB)                *
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
      REAL          PVAL(3)
      INTEGER       NPVL
C
      REAL          DALO,DAHI,SCAL,DLOX,DHIX,SCLX,OVLP
      REAL          TVR1,TVR2
      INTEGER       IMIN,IMAX
C
      INTEGER       INT1,INT2,INT3,INT4
C
      DATA          PVAL/1.0,2.0,5.0/
      DATA          NPVL/3/
C
C  ADJUST THE GIVEN DATA LIMITS.
      IF (LODA.LT.HIDA) THEN
        DALO=LODA
        DAHI=HIDA
      ELSE
        DALO=HIDA-0.5
        DAHI=LODA+0.5
      END IF
      TVR1=0.0005*(DAHI-DALO)
      DALO=DALO+TVR1
      DAHI=DAHI-TVR1
C  INITIALIZE THE LOOP TO FIND THE BEST VALUES.
      OVLP=1E20
      IMIN=MAX(2,MINL)
      IMAX=MAX(IMIN,MAXL)
C  LOOP TO FIND THE BEST VALUES.
      DO 104 INT1=IMIN,IMAX
        SCAL=INT1-1
        TVR2=(DAHI-DALO)/SCAL
        TVR1=LOG10(TVR2)
        INT2=INT(TVR1)
        IF (TVR1.LT.0.0) INT2=INT2-1
        TVR2=TVR2/(10.0**INT2)
        IF (TVR2.GT.PVAL(NPVL)) THEN
          INT2=INT2+1
          TVR2=TVR2/10.0
        END IF
        DO 101 INT4=1,NPVL
          INT3=INT4
          IF (PVAL(INT3).GE.TVR2) GO TO 102
  101   CONTINUE
  102   SCLX=PVAL(INT3)*(10.0**INT2)
        TVR1=(DAHI+DALO-SCLX*SCAL)/(2.0*SCLX)
        DLOX=AINT(TVR1)*SCLX
        IF (TVR1.LT.0.0) DLOX=DLOX-SCLX
  103   DHIX=DLOX+SCLX*SCAL
        IF (DAHI.GT.DHIX) THEN
          DLOX=DLOX+SCLX
          IF ((DLOX-DALO).LE.(0.005*SCLX)) GO TO 103
          IF (NPVL.NE.INT3) THEN
            INT3=INT3+1
          ELSE
            INT3=1
            INT2=INT2+1
          END IF
          GO TO 102
        END IF
        IF ((DHIX-DLOX).LT.OVLP) THEN
          OVLP=DHIX-DLOX
          LOLB=DLOX
          HILB=DHIX
          NLAB=INT1
        END IF
  104 CONTINUE
C
C  RESET ERROR INDICATORS AND RETURN TO CALLER.
      UGELV=0
      UGENM='        '
      UGEIX=0
      RETURN
C
      END
