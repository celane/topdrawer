C  CONTROL BLOCK FOR THE DEVICE-DEPENDENT AREA EXTENSION FOR THE
C  UNPROCESSED GRAPHIC SEGMENT PSEUDO-DEVICE.
      SAVE          /UGPU00/
C  CONTROL BLOCK LENGTH.
      INTEGER       DDXZZ
      PARAMETER     (DDXZZ=5)
C  THE DECLARATION OF THE COMMON BLOCK.
      COMMON        /UGPU00/
     X              DDXID,
     X              DDXU2,
     X              DDXDC,
     X              DDXIO
C  DEVICE-DEPENDENT AREA EXTENSION IDENTIFICATION.
      CHARACTER*8   DDXID
C  POINTER TO SUBROUTINE UGPU02.
      INTEGER       DDXU2
C  DEFERRED CLEAR FLAG:
C    0 MEANS THE START OF A NEW PICTURE HAS NOT BEEN DEFERRED, AND
C    1 MEANS THE START OF A NEW PICTURE HAS BEEN DEFERRED.
      INTEGER       DDXDC
C  INPUT/OUTPUT IDENTIFICATION VALUE.
      INTEGER       DDXIO
C  THE FOLLOWING ARRAY OVERLAYS THE ENTIRE DDA EXTENSION.
      INTEGER       DDXRY(DDXZZ)
      EQUIVALENCE   (DDXRY(1),DDXID)
