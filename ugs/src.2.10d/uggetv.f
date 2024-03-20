      INTEGER FUNCTION UGGETV ( LENG , IPTR )

C     POINTER ( IPTR , ARR )
C     DIMENSION        ARR(1)

      IPTR = MALLOC ( LENG )

      IF ( IPTR .NE. 0 ) THEN
         UGGETV = 1
      ELSE
         UGGETV = 0
      ENDIF

C     WRITE (*,*) ' * * * * WARNING, GET_VM CALLED * * * * '
         
      RETURN
      END
