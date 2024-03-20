      SUBROUTINE UGB001(ARY1,IND1,ARY2,IND2,LENG)
C
C *******************  THE UNIFIED GRAPHICS SYSTEM  *******************
C *                          MOVE AN ARRAY                            *
C *                                                                   *
C *  THIS SUBROUTINE WILL MOVE A SUBSET OF ONE ARRAY INTO ANOTHER     *
C *  ARRAY.                                                           *
C *                                                                   *
C *  THE CALLING SEQUENCE IS:                                         *
C *    CALL UGB001(ARY1,IND1,ARY2,IND2,LENG)                          *
C *                                                                   *
C *  THE PARAMETERS IN THE CALLING SEQUENCE ARE:                      *
C *    ARY1  THE TARGET ARRAY.                                        *
C *    IND1  THE STARTING INDEX IN THE TARGET ARRAY.                  *
C *    ARY2  THE SOURCE ARRAY.                                        *
C *    IND2  THE STARTING INDEX IN THE SOURCE ARRAY.                  *
C *    LENG  THE LENGTH OF THE ARRAY.                                 *
C *                                                                   *
C *                          ROBERT C. BEACH                          *
C *                    COMPUTATION RESEARCH GROUP                     *
C *                STANFORD LINEAR ACCELERATOR CENTER                 *
C *                                                                   *
C *********************************************************************
C
      INTEGER       ARY1(*),ARY2(*)
      INTEGER       IND1,IND2,LENG
C
      INTEGER       INT1
C
C  MOVE THE ARRAY.
      DO 101 INT1=1,LENG
        ARY1(INT1+IND1-1)=ARY2(INT1+IND2-1)
  101 CONTINUE
C
C  RETURN TO CALLER.
      RETURN
C
      END
