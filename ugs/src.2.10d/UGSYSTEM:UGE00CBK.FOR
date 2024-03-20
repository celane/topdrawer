C  Communication area for the character stroke generator module
      SAVE          /UGE000/
      COMMON        /UGE000/
     X              IFDR,IFEQ,IFSC,IFMS,
     X              SIZE,CSPC,STEP,XYFC,
     X              CHRO,XSZC,SNCS,
     X              SAV1,SAV2,SAV3,SAV4,
     X              ICHR,
     X              XDSP,
     X              NCHR,PTCT,PTOT,PTST,
     X              ISTR,NSTR
C  Draw flag:
C    0 Generate strokes
C    1 Scan/last
C    2 Scan/next
      INTEGER       IFDR
C  Equal spacing flag:
C    0 Equal spacing
C    1 Proportional spacing
      INTEGER       IFEQ
C  Special character flag
C    0 Do not process superscript, subscript, etc. control characters
C    1 Process these characters
      INTEGER       IFSC
C  Missing secondary character flag:
C    0 Secondary characters are not given
C    1 They are given
      INTEGER       IFMS
C  The size of the characters in the window system
      REAL          SIZE
C  Character spacing in internal units
      REAL          CSPC
C  The size of an internal unit
      REAL          STEP
C  The Y factor multiplier for the output
      REAL          XYFC
C  The current coordinates of a stroke
      REAL          CHRO(2)
C  The current size change factor
      REAL          XSZC
C  The sine and cosine of the given angle
      REAL          SNCS(2)
C  Save areas for the current state
      REAL          SAV1(4),SAV2(4),SAV3(4),SAV4(4)
C  The index of the current character
      INTEGER       ICHR
C  X Displacement for fixed spacing
      REAL          XDSP
C  The number of characters in the current character set table
      INTEGER       NCHR
C  Starting index of the character pair table in the character set table
      INTEGER       PTCT
C  Starting index of the character offset table in the character set table
      INTEGER       PTOT
C  Starting index of the character stroke segment table in character set table
      INTEGER       PTST
C  Index of the current stroke
      INTEGER       ISTR
C  Number of strokes in the current character
      INTEGER       NSTR
