C  Control block for the device-dependent area foundation.
      SAVE          /UGA003/
      COMMON        /UGA003/
     X              DDAID,DDALG,DDAPA,DDALX,DDAPX,DDACX,
     X              DDAAI,DDAAT,DDAAC,DDAAN,
     X              DDAIL,DDADM,DDADF,DDAIC,DDABC,
     X              DDAKX,DDAKY,DDAKS,DDAKN,DDAKF,
     X              DDABF,
     X              DDASL,DDAST,DDASN,
     X              DDABD,DDABE,DDABX,DDABY,
     X              DDADS,DDADD,DDADX,DDADY,DDADA,
     X              DDAWA,DDAWS,DDAWD,DDAWX,DDAWY,DDATR,
     X              DDASA,DDASF,DDASH,
     X              DDA3D,DDA3V,
     X              DDA3W,DDA3O,DDA3E,DDA3U,DDA3N,
     X              DDA3M,DDA3P,DDA3T,
     X              DDAXV,DDAXO,DDAXE,DDAXU,DDAXN,
     X              DDAFW

C  Maximum number of interactive controls.
      INTEGER       DDAZ1
      PARAMETER     (DDAZ1=8)
C  Maximum keyboard string length.
      INTEGER       DDAZ2
      PARAMETER     (DDAZ2=128)
C  Number of items in the button array.
      INTEGER       DDAZ3
      PARAMETER     (DDAZ3=2)
C  Maximum number of stroke segments.
      INTEGER       DDAZ4
      PARAMETER     (DDAZ4=128)
C  Maximum number of shields.
      INTEGER       DDAZ5
      PARAMETER     (DDAZ5=4)
C  Number of words in the device-dependent area foundation.
      INTEGER       DDAZZ
      PARAMETER     (DDAZZ=224)

C  Device-dependent area foundation identification.
      CHARACTER*8   DDAID
C  Number of words in the DDA foundation.
      INTEGER       DDALG
C  Pointer to the allocated block for the DDA foundation.
      INTEGER       DDAPA
C  Number of words in the device-dependent extension of the DDA.
      INTEGER       DDALX
C  Pointer to the allocated block for the device-dependent extension of DDA.
      INTEGER       DDAPX
C  Pointer to the actual common block for the device-dependent extension.
      INTEGER       DDACX
C  Index of the device in the MCA'S list of open devices.
      INTEGER       DDAAI
C  Device type as specified in SUBROUTINE UGOPEN.
      CHARACTER*8   DDAAT
C  Pointer to device-dependent subroutine.
      INTEGER       DDAAC
C  Name of device-dependent subroutine.
      CHARACTER*8   DDAAN

C  Interaction level of device:
C    1 Non-interactive
C    2 Slave-display
C    3 Fully interactive
      INTEGER       DDAIL

C  Drawing medium properties:
C    1 Non-erasable medium,
C    2 Raster-scan DEVICE
C    3 Refresh DISPLAY DEVICE.
      INTEGER       DDADM

C  Display dimension:
C    2 Two-dimensional device
C    3 Three-dimensional
      INTEGER       DDADF

C  Interactive controls flags
C  The entries in the array refer to the following control units:
C    1 KEYBOARD,
C    2 PICK,
C    3 BUTTON,
C    4 STROKE,
C    5 LOCATOR, AND
C    6 VALUATOR.
C  Any additional entries are not used.
C  The values of the entries are:
C    0 Not available
C    1 Available EXCEPT Button or Valuator (3,6)
C    n Buttons or valuators
      INTEGER       DDAIC(DDAZ1)
C  Enabled interactive controls
C  The entries in the array are the same as for DDAIC
C    0 Disabled
C    + Enabled
      INTEGER       DDABC(DDAZ1)

C  Keyboard X and Y Coordinates.
      INTEGER       DDAKX,DDAKY
C  Keyboard character string.
      CHARACTER*(DDAZ2) DDAKS
C  Keyboard character string length.
      INTEGER       DDAKN
C  Keyboard upper case translation flag:
C    0 Translate lower case to upper case
C    1 Do not translate
      INTEGER       DDAKF
C  Button flags:
C    0 BIT MEANS LIGHT IS OFF, AND
C    1 BIT MEANS LIGHT IS ON.
      INTEGER*4     DDABF(DDAZ3)

C  Maximum stroke length as a ratio of full screen (precision is (32,30))
      INTEGER       DDASL
C  Maximum stroke time (in hundredths of a second).
      INTEGER       DDAST
C  Stroke table length.
      INTEGER       DDASN
C  Basic two-dimensional drawing area.
      INTEGER       DDABD(2,2)
C  Extension possibilities:
C    0 No extension is possible
C    1    Extension is possible
      INTEGER       DDABE(2,2)
C  Centimeters per raster unit in the X and Y direction.
      REAL          DDABX,DDABY
C  Drawing space limits.
      REAL          DDADS(2,2)
C  Drawing space device limits.
      INTEGER       DDADD(2,2)
C  Centimeters per unit in the x and y direction in the drawing space.
      REAL          DDADX,DDADY
C  Affinity value.
      REAL          DDADA
C  Two-dimensional view port limits.
      REAL          DDAWA(2,2)
C  Two-dimensional window limits.
      REAL          DDAWS(2,2)
C  Two-dimensional window device limits.
      INTEGER       DDAWD(2,2)
C  Centimeters per unit in the x and y direction in the view port.
      REAL          DDAWX,DDAWY
C  Auxiliary two-dimensional transformation data.
      REAL          DDATR(8)

C  Number of currently active shields.
      INTEGER       DDASA
C  Flags for active shields:
C    0 Not available
C    1     available
      INTEGER       DDASF(DDAZ5)
C  Shield definitions.
      REAL          DDASH(2,2,DDAZ5)

C  Basic three-dimensional drawing volume
      INTEGER       DDA3D(3,2)
C  Three-dimensional view port.
      REAL          DDA3V(2,2)
C  Three-dimensional world volume.
      REAL          DDA3W(3,2)
C  Three-dimensional object volume.
      REAL          DDA3O(3,2)
C  Three-dimensional eye point.
      REAL          DDA3E(3)
C  Three-dimensional up direction.
      REAL          DDA3U(3)
C  Three-dimensional projection flag:
C    0.0 Parallel projection
C     +  Near scissoring value for a point projection.
      REAL          DDA3N
C  The projection matrix.
      REAL          DDA3M(3,4)
C  Equation of the near scissoring plane.
      REAL          DDA3P(4)
C  Auxiliary three-dimensional transformation data.
      REAL          DDA3T(24)
C  Three-dimensional view port in device units.
      INTEGER       DDAXV(2,2)
C  Three-dimensional object volume in device units.
      INTEGER       DDAXO(3,2)
C  Three-dimensional eye point in device units.
      INTEGER       DDAXE(3)
C  Three-dimensional up direction in device units (precision is (32,30)).
      INTEGER       DDAXU(3)
C  Three-dimensional projection flag in device units:
C    0 Parallel projection, and
C    + Near scissoring value for a point projection (precision is (32,30)).
      INTEGER       DDAXN

C  Initial write given flag:
C    0 First graphic segment has not been written
C    1 first graphic segment has     been written.
      INTEGER       DDAFW

C  The following array overlays the entire dda foundation.
      INTEGER       DDARY(DDAZZ)
      EQUIVALENCE   (DDARY(1),DDAID)
