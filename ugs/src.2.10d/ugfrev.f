      INTEGER FUNCTION UGFREV ( LENG , IPTR )
C     POINTER ( IPTR , ARR )
C     DIMENSION        ARR(1)

      CALL FREE ( IPTR )

      UGFREV = 1

      WRITE (*,*) ' * * * * WARNING, FREE_VM CALLED * * * * '

      RETURN
      END
