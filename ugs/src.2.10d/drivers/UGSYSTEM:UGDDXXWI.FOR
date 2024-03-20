C  Control block for the device dependent area extension for the
C  XWindows system in the fully interactive mode.

      SAVE          /UGXI00/
      COMMON        /UGXI00/
     X              DDXID,
     X              DDXDS,DDXSC,DDXWD,
     X              DDXGC,DDXDP,DDXTY,DDXCT,DDXXZ,DDXYZ,
     X              DDXEX,DDXPM,DDXCI,DDXDM,
     X              XPNT,YPNT,DDXPN,DDXPF,
     X              DDXFI,DDXFW,DDXFH,
     X              DDXKX,DDXKY,DDXKS,DDXKN,
     X              DDXKO,DDXKV,
     X              DDXPX,DDXPY,
     X              DDXHF

C  Size of the line and point array.
      INTEGER       DDXZ1
      PARAMETER     (DDXZ1=64)
C  Number of fonts supported.
      INTEGER       DDXZ2
      PARAMETER     (DDXZ2=4)
C  Index of font to be used for keyboard input.
      INTEGER       DDXZ3
      PARAMETER     (DDXZ3=2)
C  Control block length.
      INTEGER       DDXZZ
      PARAMETER     (DDXZZ=140)
C  Device-dependent area extension identification.
      CHARACTER*8   DDXID
C  Identification of display.
      INTEGER*4     DDXDS
C  Identification of default screen of display.
      INTEGER*4     DDXSC
C  Identification of window.
      INTEGER*4     DDXWD
C  Identification of graphic context.
      INTEGER*4     DDXGC

C  Default depth of screen.
      INTEGER       DDXDP
C  Terminal type:
C    0 Monochrome
C    1 Color
      INTEGER       DDXTY
C  Hardware/Software flag for character generation:
C    0 Use hardware if possible
C    1 Always use software
      INTEGER       DDXCT
C  Size of the window in raster units
      INTEGER       DDXXZ,DDXYZ
C  Expose control flag:
C    0 Write to the window and do not try to handle exposures,
C    1 Let the system handle exposures
C    2 Write to both a pixmap and the window
C    3 Write to the pixmap and transfer the pixmap to the
C         window at the end of each segment
      INTEGER       DDXEX
C  Identification of pixel map for exposure recovery.
      INTEGER*4     DDXPM
C  Color indices
      INTEGER*4     DDXCI(8)
C  Segment drawing mode:
C    0 Draw
C    1 Erase
      INTEGER       DDXDM

C  Points/lines count
      INTEGER       DDXPN
C  Points/lines flag:
C    0 Points
C    1 Lines
      INTEGER       DDXPF
C  Points/lines data
**      RECORD        /X$POINT/              XPNT(DDXZ1)
      INTEGER       XPNT(DDXZ1) , YPNT(DDXZ1)
      INTEGER*4     DDXPT(DDXZ1)

C  The identification, width, and height of the fonts.
      INTEGER*4     DDXFI(DDXZ2)
      INTEGER       DDXFW(DDXZ2)
      INTEGER       DDXFH(DDXZ2)
C  Coordinates of the first character in the keyboard input buffer.
      INTEGER       DDXKX,DDXKY
C  Keyboard input buffer and length.
      CHARACTER*128 DDXKS
      INTEGER       DDXKN
C  Offset of the keyboard button signal.
      INTEGER       DDXKO
C  Value of the keyboard button signal.
      INTEGER       DDXKV
C  Coordinates of the pointer.
      INTEGER       DDXPX,DDXPY
C  The hibernation flag:
C   -2  MEANS THE PROGRAM IS HIBERNATING FOR A LOCATOR POSITION,
C   -1  MEANS THE PROGRAM IS HIBERNATING FOR A KEYBOARD OR BUTTON
C       EVENT,
C    0  MEANS THE PROGRAM IS NOT HIBERNATING,
C    1  MEANS A TIME OUT HAS OCCURRED,
C    2  MEANS A KEYBOARD EVENT IS AVAILABLE,
C    3  MEANS A BUTTON EVENT IS AVAILABLE, AND
C    4  MEANS A LOCATOR POSITION IS AVAILABLE.
      INTEGER       DDXHF

C  The following array overlays the entire dda extension.
      INTEGER       DDXRY(DDXZZ)
      EQUIVALENCE   (DDXRY(1),DDXID)
