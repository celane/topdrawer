C  CONTROL BLOCK FOR THE DEVICE-DEPENDENT AREA EXTENSION FOR
C  GENERATING DISPLAY FILES FOR THE QMS LASERGRAFIX 1200.
      SAVE          /UGQM00/
C  Maximum allowed number of characters in one output record:
      INTEGER       DDXZ1
C  Total available size of plotting area (inches)
      REAL          DDXZ2,DDXZ3
C  raster points per inch:
      INTEGER       DDXZ4
C  control block length:
      INTEGER       DDXZZ
      PARAMETER     (DDXZ1=132,
     X               DDXZ2=8.5,   DDXZ3=11.0,
     X               DDXZ4=300,
     X               DDXZZ=59)
      COMMON        /UGQM00/
     X              DDXID,
     X              DDXLU,
     X              DDXBF,DDXBN,
     X              DDXDC,
     X              DDXNR,DDXNC,DDXJR,DDXJC,
     X              DDXLM,DDXTM,DDXXM,DDXYM,
     X              DDXXG,DDXYG,
     X              DDXVM,
     X              DDXPW,DDXPR,DDXPM,
     X              DDXLX,DDXLY,DDXLB
C  Device Dependent Area Extension identifier:
      CHARACTER*8   DDXID
C  Fortran logical unit number for output:
      INTEGER       DDXLU
C  Storage buffer for the output record:
      CHARACTER*(DDXZ1) DDXBF
C  Pointer to the last character in the output record:
      INTEGER       DDXBN
C  Deferred clear flag:
      LOGICAL       DDXDC
C  Number of rows and columns of subplots:
      INTEGER       DDXNR,DDXNC
C  Row and column of the most recent subplot:
      INTEGER       DDXJR,DDXJC
C  Overall left and top margins (raster units):
      INTEGER       DDXLM,DDXTM
C  Maximum X and Y coordinate values (raster units):
      INTEGER       DDXXM,DDXYM
C  X and Y gaps between subplots (raster units):
      INTEGER       DDXXG,DDXYG
C  Vector mode flag:
      LOGICAL       DDXVM
C  Most recent pen width used:
      INTEGER       DDXPW
C  Latest pen width request:
      INTEGER       DDXPR
C  Mapping from intensity level (VDIM,DIM,MEDIUM,BRIGHT,VBRIGHT) to pen width:
      INTEGER       DDXPM(5)
C  Raster coordinates and blanking bit for the most recent vector move:
      INTEGER       DDXLX,DDXLY,DDXLB
C  The following array overlays the entire DDA extension:
      INTEGER       DDXRY(DDXZZ)
      EQUIVALENCE   (DDXRY(1),DDXID)
