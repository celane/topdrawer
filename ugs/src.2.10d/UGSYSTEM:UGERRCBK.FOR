C  Error diagnostic information
      SAVE          /UGERRD/
      COMMON        /UGERRD/
     X              UGELV,UGENM,UGEIX
C  The error level value:
C    0 No error has been detected
C    1 Minor error
C    2 Error
C    3 Severe error
C    4 Terminal error
      INTEGER       UGELV
C  The name of the subroutine detecting the error
      CHARACTER*8   UGENM
C  The index of the error.
      INTEGER       UGEIX

C  Least significant bit in floating point
C  IEEE = 0
C  VAX  = 16
      INTEGER     LSBFLO
      PARAMETER ( LSBFLO = 0 )
