
      SUBROUTINE UGZ003(FLAG,SIZE,PNTR)

C *******************  THE UNIFIED GRAPHICS SYSTEM  *******************
C *       SUBROUTINE TO ALLOCATE/DE-ALLOCATE A BLOCK OF MEMORY        *
C *                                                                   *
C *  THIS SUBROUTINE MAY BE USED TO ALLOCATE OR DE-ALLOCATE A BLOCK   *
C *  OF MEMORY.  WHEN A BLOCK IS ALLOCATED, THE ADDRESS OF THE BLOCK  *
C *  IS RETURNED.  WHEN A BLOCK IS DE-ALLOCATED, ITS ADDRESS AND      *
C *  LENGTH MUST BE GIVEN.                                            *
C *                                                                   *
C *  THE CALLING SEQUENCE IS:                                         *
C *    CALL UGZ003(FLAG,SIZE,PNTR)                                    *
C *                                                                   *
C *  THE PARAMETERS IN THE CALLING SEQUENCE ARE:                      *
C *    FLAG  THE OPERATION INDICATOR (0 MEANS ALLOCATE AND 1 MEANS    *
C *          DE-ALLOCATE).                                            *
C *    SIZE  THE SIZE OF THE BLOCK OF MEMORY.  THIS VALUE MUST BE     *
C *          GIVEN IN FULL WORDS.                                     *
C *    PNTR  THE ADDRESS OF THE BLOCK OF MEMORY.                      *
C *                                                                   *
C *                          ROBERT C. BEACH                          *
C *                    COMPUTATION RESEARCH GROUP                     *
C *                STANFORD LINEAR ACCELERATOR CENTER                 *
C *                                                                   *
C *********************************************************************

      INTEGER       FLAG,SIZE,PNTR

      INTEGER       INT1,UGGETV

C  ALLOCATE/DEALLOCATE a block of memory

      IF (FLAG.EQ.0) THEN
        INT1=UGGETV(4*SIZE,PNTR)
      ELSE
        INT1=UGFREV(4*SIZE,PNTR)
      END IF

      IF ( IAND(INT1,1) .EQ. 0 ) THEN
          WRITE (*,*) ' Error allocating or deallocating memory '
	  WRITE (*,*) ' Flag, size, pointer ' , FLAG,SIZE,PNTR
          WRITE (*,*) ' Status ' , INT1
          CALL UGZ001
      ENDIF

      RETURN
      END
