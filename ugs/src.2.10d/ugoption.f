      SUBROUTINE UGOPTION(OPTN,DESC_NUM,INST,IFLAG,EXST,EXSTR)
C
C *******************  THE UNIFIED GRAPHICS SYSTEM  *******************
C *                    OPTIONS SCANNING SUBROUTINE                    *
C *                                                                   *
C *  THIS SUBROUTINE MAY BE USED TO SCAN THE OPTIONS LIST.  THE       *
C *  OPTIONS LIST IS COMPARED WITH AN INPUT STRUCTURE AND AN OUTPUT   *
C *  DATA STRUCTURE IS PRODUCED.                                      *
C *                                                                   *
C *  THE CALLING SEQUENCE IS:                                         *
C *    CALL UGOPTION(OPTN,DESC_NUM,INST,IFLAG,EXST)                   *
C *                                                                   *
C *  THE PARAMETERS IN THE CALLING SEQUENCE ARE:                      *
C *    OPTN     THE OPTIONS STRING.                                   *
C *    DESC_NUM THE NUMBER OF DESCRIPTORS IN THE INPUT STRUCTURE.     *
C *    INST     THE INPUT STRUCTURE.                                  *
C *    IFLAG    THE FLAG.                                             *
C *    EXST     THE OUTPUT STRUCTURE.                                 *
C *    EXSTR    THE OUTPUT STRUCTURE FOR STRINGS.                     *
C *                                                                   *
C *                          DONNA M. REID                            *
C *                    PHYSICS ANALYSIS TOOLS GROUP                   *
C *                FERMI NATIONAL ACCELERATOR LABORATORY              *
C *                                                                   *
C *********************************************************************

* 19940803	KREYMEr
*    Removed unused variables M, MM, 

**   19940518	KREYMER
*   Support type 4 (strings) via new output structure

**   19940420	KREYMER
*   Return if no options
*   Reset DONE each time through loop
*   Check that matched option is not substring

      CHARACTER*(*)   OPTN
      INTEGER         DESC_NUM
      INTEGER*4       INST(4,DESC_NUM)
      INTEGER*4       EXST(*)
      CHARACTER*(*)   EXSTR(*)

      CHARACTER*20  FLTX
      CHARACTER*20  INTX
      INTEGER       I, J, K, KK, L
      INTEGER       ITEMP, INT_EXST, DONE
      REAL          FTEMP
      EQUIVALENCE   (FTEMP,INT_EXST)
      CHARACTER*(*)  IFLAG(DESC_NUM)
      CHARACTER*1   EQUAL1, COMMA1, SPACE1

      DATA EQUAL1 /'='/, COMMA1 /','/, SPACE1 /' '/
 
      L = LEN(OPTN)                     ! LENGTH OF OPTN STRING

      IF ( OPTN .EQ. ' ' ) RETURN
*      if (l.gt.40) WRITE (*,*) ' into option ',L,OPTN( :MIN(40,L) )

      DO 50 I=1,DESC_NUM                ! GO THRU LOOP FOR EACH DESCRIPTOR

         DONE = 0
         J = INDEX(OPTN,IFLAG(I)(:INST(2,I)))     ! CHECK FOR FLAG IN OPTN 
         IF (J.NE.0) THEN               ! IF FLAG IS IN OPTN STRING

C           Exact match required
            IF (J.GT.1) THEN
               IF ( OPTN(J-1:J-1) .NE. ',' .AND.
     +              OPTN(J-1:J-1) .NE. ' ' )     GOTO 50
            ENDIF

            IF (INST(1,I).EQ.1) THEN           ! IF DESCRIPTOR TYPE #1
               EXST(INST(3,I)) = INST(4,I)     ! MOVE TYPE DEP VALUE
               GO TO 50
            ENDIF

            IF (INST(1,I).EQ.2)  THEN          ! IF DESC TYPE #2 (INT)
               K=INDEX(OPTN(J:),EQUAL1)        ! CHECK FOR EQUAL SIGN
               IF (K.EQ.0) GO TO 50            ! IF NO EQUAL, CONTINUE
               K=K+J                           ! MOVE 1 PAST =
               KK=K                     
 30            IF (KK.LT.L) KK=KK+1            ! ADD 1 TO POINTER

               IF (OPTN(KK:KK).EQ.COMMA1.OR.OPTN(KK:KK).EQ.SPACE1) THEN
                  KK=KK-1                       ! IF COMMA OR SEPERATOR,
                  DONE=1                        ! DONE W/THIS DESCRIPTOR
               ENDIF

               IF (KK.EQ.L)  DONE=1                 ! CHECK FOR END OF OPTN

               IF (DONE.EQ.1) THEN
                  INTX = ' '
                  INTX(19+K-KK:) = OPTN(K:KK)
                  READ(INTX, '(I20)', ERR=88) ITEMP
                  EXST(INST(3,I))=ITEMP 
               ELSE
                  GO TO 30
               ENDIF
            ENDIF

            IF (INST(1,I).EQ.3) THEN          ! IF DESC TYPE #3 (FLT)
               K = INDEX(OPTN(J:),EQUAL1)     ! CHECK FOR EQUAL SIGN
               IF (K.EQ.0) GO TO 50           ! IF NO EQUAL, CONTINUE
               K=K+J                          ! MOVE 1 PAST =
               KK=K                       

 40            IF (KK.LT.L) KK=KK+1           ! ADD 1 TO POINTER

               IF (OPTN(KK:KK).EQ.COMMA1.OR.OPTN(KK:KK).EQ.SPACE1) THEN
                  KK=KK-1                     ! IF COMMA OR SEPERATOR, 
                  DONE=1                      ! DONE W/THIS DESCRIPTOR
               ENDIF

               IF (KK.EQ.L)  DONE=1          ! CHECK FOR END OF OPTN
                  
               IF (DONE.EQ.1) THEN           ! IF DONE
                  FLTX = ' '
                  FLTX(19+K-KK:) = OPTN(K:KK)
                  READ( FLTX , '(F20.10)' ,  ERR=99 ) FTEMP
                  EXST(INST(3,I))=INT_EXST          ! INT EQUIV TO FTEMP 
               ELSE
                  GO TO 40
               ENDIF
            ENDIF

            IF (INST(1,I).EQ.4) THEN            ! IF DESC TYPE #4 (CHAR)
               K=INDEX(OPTN(J:),EQUAL1)         ! CHECK FOR EQUAL SIGN
               IF (K.EQ.0) GO TO 50             ! IF NO EQUAL, CONTINUE
               K=K+J                            ! MOVE 1 PAST =
               KK=K                    
 45            IF (KK.LT.L) KK=KK+1             ! ADD 1 TO POINTER

               IF (OPTN(KK:KK).EQ.COMMA1.OR.OPTN(KK:KK).EQ.SPACE1) THEN
                  KK=KK-1                        ! IF COMMA OR SEPERATOR, 
                  DONE=1                         ! DONE W/THIS DESCRIPTOR
               ENDIF
                                    
               IF (KK.EQ.L) DONE=1                ! CHECK FOR END OF OPTN
                 
               IF (DONE.EQ.1) THEN                ! IF DONE
                  EXSTR ( INST(3,I) ) = OPTN ( K : KK )
               ELSE
                   GO TO 45
               ENDIF
            ENDIF

            IF (INST(1,I).EQ.5) THEN            ! IF DESC TYPE #5 (BITS)
               WRITE(6,49) 
 49            FORMAT(' ERROR  DESCRIPTOR TYPE 5 IS NOT SUPPORTED')
               GO TO 50
            ENDIF
         ENDIF
 50   CONTINUE 
      GO TO 100

 88   WRITE (*,*) ' ERROR CONVERTING INPUT STRING TO INTEGER'
      WRITE (*,*) LEN(OPTN)
      WRITE (*,*) OPTN ( : MIN (72,LEN(OPTN)) )
      JJJ = EXST(1 000 000 000)
      GO TO 100
 99   WRITE (*,*) ' ERROR CONVERTING INPUT STRING TO FLOAT'
      WRITE (*,*) LEN(OPTN)
      WRITE (*,*) OPTN ( : MIN (72,LEN(OPTN)) )
      JJJ = EXST(1 000 000 000)
      GO TO 100
 111  WRITE (*,*) ' ERROR INPUT STRING HAS MORE CHARACTERS THAN ALLOWED'
     
 100  CONTINUE
      RETURN
C
      END
