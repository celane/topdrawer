      BLOCK DATA UGNUCL
C
      INCLUDE       'UGSYSTEM:UGMCACBK.FOR'
C
      INCLUDE       'UGSYSTEM:UGPOTCBK.FOR'
C
      INCLUDE       'UGSYSTEM:UGDDACBK.FOR'
C
      SAVE          /UGA011/
C  THE DECLARATION OF THE COMMON BLOCK.
      COMMON        /UGA011/
     X              CHRTP1,
     X              CHRNC1,
     X              CHRSC1,
     X              CHRCS1,
     X              CHROT1,
     X              CHRLT1,
     X              CHRCT1,
     X              CHRID1
C

C  Size of character pair table
      INTEGER*2     CHRSC1
C  SYMBOL STROKE TABLE IDENTIFICATION.
      CHARACTER*8   CHRID1
C  SYMBOL STROKE TABLE TYPE.
      INTEGER*2     CHRTP1
C  NUMBER OF CHARACTERS.
      INTEGER*2     CHRNC1
C  CHARACTER SPACING.
      INTEGER*2     CHRCS1
C  CHARACTER PAIR TABLE.
      CHARACTER*2   CHRCT1(   11)
      CHARACTER*2   CHC011(11)
      EQUIVALENCE   (CHC011(1),CHRCT1(    1))
C  CHARACTER OFFSET TABLE.
      INTEGER*2     CHROT1(   11)
      INTEGER*2     CHO011(11)
      EQUIVALENCE   (CHO011(1),CHROT1(    1))
C  LINE SEGMENT TABLE.
      INTEGER*2     CHRLT1(  127)
      INTEGER*2     CHL011(80)
      EQUIVALENCE   (CHL011(1),CHRLT1(    1))
      INTEGER*2     CHL012(47)
      EQUIVALENCE   (CHL012(1),CHRLT1(   81))
C
      SAVE          /UGA012/
C  THE DECLARATION OF THE COMMON BLOCK.
      COMMON        /UGA012/
     X              CHRTP2,
     X              CHRNC2,
     X              CHRSC2,
     X              CHRCS2,
     X              CHROT2,
     X              CHRLT2,
     X              CHRCT2,
     X              CHRID2
C
C  Size of character pair table
      INTEGER*2     CHRSC2
C  SYMBOL STROKE TABLE IDENTIFICATION.
      CHARACTER*8   CHRID2
C  SYMBOL STROKE TABLE TYPE.
      INTEGER*2     CHRTP2
C  NUMBER OF CHARACTERS.
      INTEGER*2     CHRNC2
C  CHARACTER SPACING.
      INTEGER*2     CHRCS2
C  CHARACTER PAIR TABLE.
      CHARACTER*2   CHRCT2(   96)
      CHARACTER*2   CHC021(80)
      EQUIVALENCE   (CHC021(1),CHRCT2(    1))
      CHARACTER*2   CHC022(16)
      EQUIVALENCE   (CHC022(1),CHRCT2(   81))
C  CHARACTER OFFSET TABLE.
      INTEGER*2     CHROT2(   96)
      INTEGER*2     CHO021(80)
      EQUIVALENCE   (CHO021(1),CHROT2(    1))
      INTEGER*2     CHO022(16)
      EQUIVALENCE   (CHO022(1),CHROT2(   81))
C  LINE SEGMENT TABLE.
      INTEGER*2     CHRLT2(  617)
      INTEGER*2     CHL021(80)
      EQUIVALENCE   (CHL021(1),CHRLT2(    1))
      INTEGER*2     CHL022(80)
      EQUIVALENCE   (CHL022(1),CHRLT2(   81))
      INTEGER*2     CHL023(80)
      EQUIVALENCE   (CHL023(1),CHRLT2(  161))
      INTEGER*2     CHL024(80)
      EQUIVALENCE   (CHL024(1),CHRLT2(  241))
      INTEGER*2     CHL025(80)
      EQUIVALENCE   (CHL025(1),CHRLT2(  321))
      INTEGER*2     CHL026(80)
      EQUIVALENCE   (CHL026(1),CHRLT2(  401))
      INTEGER*2     CHL027(80)
      EQUIVALENCE   (CHL027(1),CHRLT2(  481))
      INTEGER*2     CHL028(57)
      EQUIVALENCE   (CHL028(1),CHRLT2(  561))
C
      INCLUDE       'UGSYSTEM:UGEMSCBK.FOR'
C
C
C *******************  THE UNIFIED GRAPHICS SYSTEM  *******************
C *                    THE MAIN COMMUNICATION AREA                    *
C *                                                                   *
C *  THIS BLOCK OF DATA CONTAINS INFORMATION ABOUT THE STATE OF THE   *
C *  UNIFIED GRAPHICS SYSTEM.                                         *
C *                                                                   *
C *                          ROBERT C. BEACH                          *
C *                    COMPUTATION RESEARCH GROUP                     *
C *                STANFORD LINEAR ACCELERATOR CENTER                 *
C *                                                                   *
C *********************************************************************
C
      DATA          MCAID/'MCA     '/
      DATA          MCACN/'SIMPLEX '/
      DATA          MCAOI/MCAZ1*0/
      DATA          MCAOP/MCAZ1*0/
      DATA          MCAIC/    0/
C
C
C *******************  THE UNIFIED GRAPHICS SYSTEM  *******************
C *                     THE PICTURE OPTIONS TABLE                     *
C *                                                                   *
C *  THIS BLOCK OF DATA DEFINES THE PICTURE OPTIONS TABLE.  THIS      *
C *  INCLUDES THE DEFAULT VALUES FOR THE PICTURE OPTIONS AND THE      *
C *  OPTIONS SCANNING TABLE.                                          *
C *                                                                   *
C *                          ROBERT C. BEACH                          *
C *                    COMPUTATION RESEARCH GROUP                     *
C *                STANFORD LINEAR ACCELERATOR CENTER                 *
C *                                                                   *
C *********************************************************************
C
      DATA          POTID/'POT     '/
      DATA          POTIL/PODIL/
      DATA          POTCR/PODCR/
      DATA          POTBL/PODBL/
      DATA          POTPI/PODPI/
      DATA          POTMK/PODMK/
      DATA          POTLS/PODLS/
      DATA          POTSZ/PODSZ/
      DATA          POTDZ/PODDZ/
      DATA          POTAG/PODAG/
      DATA          POTJF/PODJF/
      DATA          POTCP/PODCP/
      DATA          POTFX/PODFX/
      DATA          POTCC/PODCC/
C
      DATA  (POT001(I,1),I=1,4) / 1,4,1,1/, IFLAG2(1) / 'VDIM'/
      DATA  (POT001(I,2),I=1,4) / 1,3,1,2/, IFLAG2(2) / 'DIM'/
      DATA  (POT001(I,3),I=1,4) / 1,6,1,3/, IFLAG2(3) / 'MEDIUM'/
      DATA  (POT001(I,4),I=1,4) / 1,6,1,4/, IFLAG2(4) / 'BRIGHT'/
      DATA  (POT001(I,5),I=1,4) / 1,7,1,5/, IFLAG2(5) / 'VBRIGHT'/
      DATA  (POT001(I,6),I=1,4) / 1,6,1,6/, IFLAG2(6) / 'WIDTH6'/
      DATA  (POT001(I,7),I=1,4) / 1,6,1,7/, IFLAG2(7) / 'WIDTH7'/
      DATA  (POT001(I,8),I=1,4) / 1,6,1,8/, IFLAG2(8) / 'WIDTH8'/
      DATA  (POT001(I,9),I=1,4) / 1,6,1,9/, IFLAG2(9) / 'WIDTH9'/
      DATA  (POT001(I,10),I=1,4)/ 1,7,1,10/,IFLAG2(10)/ 'WIDTH10'/
C
      DATA  (POT002(I,1),I=1,4) / 1,5,2,1/, IFLAG2(11) / 'WHITE'/
      DATA  (POT002(I,2),I=1,4) / 1,3,2,2/, IFLAG2(12) / 'RED'/
      DATA  (POT002(I,3),I=1,4) / 1,5,2,3/, IFLAG2(13) / 'GREEN'/
      DATA  (POT002(I,4),I=1,4) / 1,4,2,4/, IFLAG2(14) / 'BLUE'/
      DATA  (POT002(I,5),I=1,4) / 1,6,2,5/, IFLAG2(15) / 'YELLOW'/
      DATA  (POT002(I,6),I=1,4) / 1,7,2,6/, IFLAG2(16) / 'MAGENTA'/
      DATA  (POT002(I,7),I=1,4) / 1,4,2,7/, IFLAG2(17) / 'CYAN'/
      DATA  (POT002(I,8),I=1,4) / 1,5,2,8/, IFLAG2(18) / 'BLACK'/
C
      DATA  (POT003(I,1),I=1,4) / 1,6,3,1/, IFLAG2(19) / 'STEADY'/
      DATA  (POT003(I,2),I=1,4) / 1,5,3,2/, IFLAG2(20) / 'BLINK'/    
C
      DATA  (POT004(I,1),I=1,4) / 2,6,4,0/, IFLAG2(21) / 'PICKID'/
C
      DATA  (POT005(I,1),I=1,4) / 2,4,5,0/, IFLAG2(22) / 'MARK'/
C
      DATA  (POT006(I,1),I=1,4) / 1,5,6,1/, IFLAG2(23) / 'SOLID'/
      DATA  (POT006(I,2),I=1,4) / 1,6,6,2/, IFLAG2(24) / 'DASHED'/
      DATA  (POT006(I,3),I=1,4) / 1,6,6,3/, IFLAG2(25) / 'DOTTED'/
      DATA  (POT006(I,4),I=1,4) / 1,7,6,4/, IFLAG2(26) / 'DOTDASH'/
C
      DATA  (POT007(I,1),I=1,4) / 3,4,7,0/, IFLAG2(27) / 'SIZE'/
C
      DATA  (POT008(I,1),I=1,4) / 3,5,8,0/, IFLAG2(28) / 'DSIZE'/
C
      DATA  (POT009(I,1),I=1,4) / 3,5,9,0/, IFLAG2(29) / 'ANGLE'/
C
      DATA  (POT010(I,1),I=1,4) / 1,4,10,1/, IFLAG2(30) / 'LEFT'/
      DATA  (POT010(I,2),I=1,4) / 1,5,10,2/, IFLAG2(31) / 'RIGHT'/
      DATA  (POT010(I,3),I=1,4) / 1,6,10,3/, IFLAG2(32) / 'CENTER'/
C
      DATA  (POT011(I,1),I=1,4) / 1,6,11,1/, IFLAG2(33) / 'NORMGN'/
      DATA  (POT011(I,2),I=1,4) / 1,6,11,2/, IFLAG2(34) / 'HARDGN'/
      DATA  (POT011(I,3),I=1,4) / 1,6,11,3/, IFLAG2(35) / 'SOFTGN'/
C
      DATA  (POT012(I,1),I=1,4) / 1,7,12,1/, IFLAG2(36) / 'NOFXSIZ'/
      DATA  (POT012(I,2),I=1,4) / 1,7,12,2/, IFLAG2(37) / 'FIXSIZE'/
C
      DATA  (POT013(I,1),I=1,4) / 1,7,13,0/, IFLAG2(38) / 'NOCONST'/
      DATA  (POT013(I,2),I=1,4) / 1,6,13,1/, IFLAG2(39) / 'XCONST'/
      DATA  (POT013(I,3),I=1,4) / 1,6,13,2/, IFLAG2(40) / 'YCONST'/
      DATA  (POT013(I,4),I=1,4) / 1,6,13,3/, IFLAG2(41) / 'ZCONST'/
C
C
C *******************  THE UNIFIED GRAPHICS SYSTEM  *******************
C *               THE DEVICE-DEPENDENT AREA FOUNDATION                *
C *                                                                   *
C *  THIS BLOCK OF DATA CONTAINS DEVICE-INDEPENDENT INFORMATION       *
C *  ABOUT THE STATE OF THE ACTIVE GRAPHIC DEVICE.                    *
C *                                                                   *
C *                          ROBERT C. BEACH                          *
C *                    COMPUTATION RESEARCH GROUP                     *
C *                STANFORD LINEAR ACCELERATOR CENTER                 *
C *                                                                   *
C *********************************************************************
C
      DATA          DDAID/'DDA/BASE'/
      DATA          DDALG/DDAZZ/
      DATA          DDAAI/    0/
C
C
C *******************  THE UNIFIED GRAPHICS SYSTEM  *******************
C *                        THE MARKER SYMBOLS                         *
C *                                                                   *
C *  THIS BLOCK OF DATA DEFINES THE MARKER SYMBOLS.                   *
C *                                                                   *
C *                          ROBERT C. BEACH                          *
C *                    COMPUTATION RESEARCH GROUP                     *
C *                STANFORD LINEAR ACCELERATOR CENTER                 *
C *                                                                   *
C *********************************************************************
C
      DATA          CHRTP1/    1/
      DATA          CHRNC1/   11/
      DATA          CHRSC1/  127/
      DATA          CHRCS1/    6/

      DATA          CHRID1/'MARKER  '/

      DATA          CHC011
     X  / '0 ', '1 ', '2 ', '3 ', '4 ', '5 ', '6 ', '7 ', '8 ', '9 ',
     X    '$$'/
      DATA          CHO011
     X  /    1,    7,   13,   20,   27,   41,   55,   69,   83,  101,
     X     112/
      DATA          CHL011
     X  /  704, 8000,25152, 8002,24636, 8258,  704, 7998,25156, 7744,
     X   25148, 8002,  832, 8000,24894,24898,24386,24382, 8512,  832,
     X    7998,25152,24644,24128,24636, 8514, 1728, 8000,24768,24769,
     X    8257,24639,24767, 8384,24512,24511, 8255,24641,24513, 8384,
     X    1728, 7998,24769,24642, 8129,24767,24896, 8385,24511,24638,
     X    8383,24513,24384, 8385, 1728, 8000,25152, 8002,24636, 8384,
     X   24384, 8129,24642, 8385,24896, 8383,24638, 8001, 1728, 7998,
     X   25156, 7744,25148, 8257,24511, 8000,24513, 8258,24769, 8512/
      DATA          CHL012
     X  /24767, 7999, 2240, 8128,24512, 8258,24767, 8384,24641, 8512,
     X   24511, 8255,24768, 8254,24513, 8128,24639, 8000,24769, 8385,
     X    1344, 7999,24642,24769,24896,24767,24638,24511,24384,24513,
     X    8513, 1984, 7997,25152, 8257,24128, 8257,25152, 8257,24128,
     X    8257,25152, 8257,24128, 8257,25152, 7997/
C
C
C *******************  THE UNIFIED GRAPHICS SYSTEM  *******************
C *                      THE BASIC CHARACTER SET                      *
C *                                                                   *
C *  THIS BLOCK OF DATA DEFINES THE BASIC CHARACTER SET.              *
C *                                                                   *
C *                          ROBERT C. BEACH                          *
C *                    COMPUTATION RESEARCH GROUP                     *
C *                STANFORD LINEAR ACCELERATOR CENTER                 *
C *                                                                   *
C *********************************************************************
C

      DATA          CHRTP2/    2/
      DATA          CHRNC2/   96/
      DATA          CHRSC2/  617/
      DATA          CHRCS2/    6/

      DATA          CHRID2/'BASIC   '/

      DATA          CHC021
     X  / '  ', '! ', '" ', '# ', '$ ', '% ', '& ',''' ', '( ', ') ',
     X    '* ', '+ ', ', ', '- ', '. ', '/ ', '0 ', '1 ', '2 ', '3 ',
     X    '4 ', '5 ', '6 ', '7 ', '8 ', '9 ', ': ', '; ', '< ', '= ',
     X    '> ', '? ', '@ ', 'A ', 'B ', 'C ', 'D ', 'E ', 'F ', 'G ',
     X    'H ', 'I ', 'J ', 'K ', 'L ', 'M ', 'N ', 'O ', 'P ', 'Q ',
     X    'R ', 'S ', 'T ', 'U ', 'V ', 'W ', 'X ', 'Y ', 'Z ', '[ ',
     X    '\ ', '] ', '^ ', '_ ', '` ', 'a ', 'b ', 'c ', 'd ', 'e ',
     X    'f ', 'g ', 'h ', 'i ', 'j ', 'k ', 'l ', 'm ', 'n ', 'o '/
      DATA          CHC022
     X  / 'p ', 'q ', 'r ', 's ', 't ', 'u ', 'v ', 'w ', 'x ', 'y ',
     X    'z ', '{ ', '| ', '} ', '~ ', '$$'/
      DATA          CHO021
     X  /  333,  417,  449,  485,  459,  513,  473,  443,  380,  386,
     X     345,  335,  372,  341,  365,  355,  223,  232,  239,  249,
     X     264,  270,  282,  296,  301,  319,  392,  404,  592,  359,
     X     597,  428,  495,    1,    8,   21,   31,   40,   48,   55,
     X      67,   75,   83,   92,  100,  105,  112,  118,  129,  138,
     X     151,  162,  176,  182,  190,  195,  202,  208,  215,  544,
     X     540,  550,  580,  531,  574,    1,    8,   21,   31,   40,
     X      48,   55,   67,   75,   83,   92,  100,  105,  112,  118/
      DATA          CHO022
     X  /  129,  138,  151,  162,  176,  182,  190,  195,  202,  208,
     X     215,  556,  527,  565,  535,  602/
      DATA          CHL021
     X  /  832, 7997,24902,24890, 7875,24896, 8128, 1600, 8000,25024,
     X   24769,24641,24513,24256,24634,25024,24769,24641,24513, 8128,
     X    1216, 8510,24511,24384,24513,24644,24769,24896,24767, 7998,
     X    1088, 7997,24646,25024,24767,24636,24511,24256, 8515,  960,
     X    8509,24128,24646,25152, 8125,24256, 8512,  832, 7997,24646,
     X   25152, 8125,24256, 8512, 1472, 8256,24896,24638,24511,24384,
     X   24513,24644,24769,24896,24767, 7998,  960, 7997,24646, 8253,
     X   25152, 8259,24634, 8003,  960, 8125,24896, 8128,24646, 8128/
      DATA          CHL022
     X  /24896, 8125, 1088, 7998,24767,24768,24769,24645, 8128,24896,
     X    7997,  960, 7997,24646, 8768,24124, 8385,25021, 8003,  576,
     X    8003,24634,25152, 8003,  832, 7997,24646,24893,24899,24634,
     X    8003,  704, 7997,24646,25146,24646, 7997, 1344, 8125,24896,
     X   24769,24644,24513,24384,24511,24636,24767, 8387, 1088, 7997,
     X   24646,25024,24767,24639,24511,24256, 8512, 1600, 8125,24896,
     X   24769,24644,24513,24384,24511,24636,24767, 8386,24894, 8003,
     X    1344, 7997,24646,25024,24767,24639,24511,24256, 8512,24893/
      DATA          CHL023
     X  / 8003, 1728, 7998,24767,24896,24769,24641,24513,24384,24513,
     X   24641,24769,24896,24767, 7998,  704, 8253,24646, 8000,25152,
     X    7997,  960, 8003,24635,24767,24896,24769,24645, 7997,  576,
     X    8003,24890,24902, 7997,  832, 8003,24762,24771,24765,24774,
     X    7997,  704, 7997,25158, 7744,25146, 8003,  832, 8003,24893,
     X   24899, 7997,24637, 8259,  960, 8509,24128,25158,24128, 8381,
     X   24896, 8128, 1088, 8125,24896,24771,24515,24384,24509,24765,
     X    8387,  832, 8125,24896, 8128,24646,24511, 8382, 1216, 8002/
      DATA          CHL024
     X  /24769,24896,24767,24638,24126,24639,25152, 8003, 1856, 8002,
     X   24769,24896,24767,24639,24511,24512, 8384,24767,24639,24511,
     X   24384,24513, 8514,  704, 8381,24646,24252,25152, 8001, 1472,
     X    7998,24767,24896,24769,24642,24513,24384,24511,24643,25152,
     X    7997, 1728, 8000,24769,24896,24767,24638,24511,24384,24513,
     X   24644,24769,24896,24767, 7998,  576, 7997,25158,24128, 8509,
     X    2240, 8128,24896,24767,24639,24511,24384,24513,24641,24769,
     X   24513,24641,24769,24896,24767,24639,24511, 8128, 1728, 7998/
      DATA          CHL025
     X  /24767,24896,24769,24644,24513,24384,24511,24638,24767,24896,
     X   24769, 8000,  192, 8256,  704, 8128,24896, 8129,24638, 8257,
     X     448, 8128,24896, 8128, 1216, 8127,24898, 8255,24384, 8383,
     X   24642, 8128,24894, 8129,  448, 7997,25158, 7997,  704, 7999,
     X   25152, 8258,24128, 8511,  832, 8125,24641,24768,24639,24512,
     X    8387,  960, 8124,24769,24641,24512,24639,24768, 8259,  704,
     X    8381,24513,24644,24769, 8125,  704, 8125,24769,24644,24513,
     X    8381, 1472, 8125,24768,24641,24512,24639, 8259,24768,24641/
      DATA          CHL026
     X  /24512,24639, 8384, 1600, 8124,24769,24641,24512,24639,24768,
     X    8131,24641,24768,24639,24512, 8384, 1344, 8125,24768,24641,
     X   24512,24639, 8258,24644,24768,24508, 8385, 1856, 8125,24768,
     X   24641,24512,24639, 8258,24641,24896,24769,24641,24513,24384,
     X   24511, 8510,  704, 8129,24642,24768,24510, 8383, 1216, 8001,
     X   24642,24768,24510, 8512,24642,24768,24510, 8255, 1728, 7999,
     X   24767,24896,24769,24130,24769,24896,24767, 8130,24634, 8000,
     X   24646, 8381, 1472, 8509,24260,24641,24769,24767,24253,24639/
      DATA          CHL027
     X  /24767,24896,24769, 8002, 1216, 7998,24900, 7999,25152, 8254,
     X   24128, 8511,24900, 7998, 2240, 8384,24511,24512,24641,24769,
     X   24768,24638,24769,24641,24513,24384,24511,24638,24767,24896,
     X   24769, 8001, 1728, 7997,25158, 7871,24512,24641,24768,24639,
     X    8508,24768,24639,24512,24641, 8130,  448, 8253,24646, 8253,
     X     448, 7996,25152, 8004,  576, 8000,25152,24639, 8001,  448,
     X    8003,25146, 8003,  704, 8381,24512,24646,24768, 8125,  704,
     X    8125,24768,24646,24512, 8381, 1088, 8380,24513,24642,24513/
      DATA          CHL028
     X  /24769,24642,24769, 8124, 1088, 8124,24769,24642,24769,24513,
     X   24642,24513, 8380,  704, 8129,24514,24768,24638, 8383, 1472,
     X    8253,24646, 8510,24513,24384,24511,24638,24767,24896,24769,
     X    8001,  576, 8510,24130,25154, 7998,  576, 7998,25154,24130,
     X    8510, 1984, 7997,25152, 8257,24128, 8257,25152, 8257,24128,
     X    8257,25152, 8257,24128, 8257,25152, 7997/
C
C
C *******************  THE UNIFIED GRAPHICS SYSTEM  *******************
C *                       ERROR MESSAGE MODULE                        *
C *                                                                   *
C *  THIS BLOCK OF DATA CONTAINS ALL OF THE ERROR MESSAGES FOR WHICH  *
C *  THE UNIFIED GRAPHICS SYSTEM WILL PRODUCE A FULL DESCRIPTION.     *
C *  IT ALSO INCLUDES A COUNTER FOR EACH MESSAGE GIVING THE NUMBER    *
C *  OF TIMES IT HAS BEEN PRODUCED, AND A MAXIMUM COUNT AFTER WHICH   *
C *  THE PRINTED MESSAGE WILL BE SUPPRESSED.                          *
C *                                                                   *
C *                          ROBERT C. BEACH                          *
C *                    COMPUTATION RESEARCH GROUP                     *
C *                STANFORD LINEAR ACCELERATOR CENTER                 *
C *                                                                   *
C *********************************************************************
C
      DATA          EMMID/'ERRMESSG'/
      DATA          EMMRC/    0/
      DATA          EMMEU/    6/
      DATA          EMMPM/    8/
      DATA          EMMNM/   96/
      DATA          EMN001
     X  /'UGINIT  ', 'UGMARK  ', 'UGLINE  ', 'UGPMRK  ', 'UGPLIN  ',
     X   'UGTEXT  ', 'UGTEXT  ', 'UGXTXT  ', 'UGXTXT  ', 'UGPFIL  ',
     X   'UGPFIL  ', 'UG3MRK  ', 'UG3LIN  ', 'UG3PMK  ', 'UG3PLN  ',
     X   'UG3TXT  ', 'UG3TXT  ', 'UGDDAT  ', 'UGDDAT  ', 'UGOPEN  ',
     X   'UGOPEN  ', 'UGOPEN  ', 'UGOPEN  ', 'UGOPEN  ', 'UGOPEN  ',
     X   'UGOPEN  ', 'UGCLOS  ', 'UGSLCT  ', 'UGINFO  ', 'UGMCTL  ',
     X   'UGMCTL  ', 'UGWRIT  ', 'UGWRIT  ', 'UGWRIT  ', 'UGWRIT  ',
     X   'UGWRIT  ', 'UGWRIT  ', 'UGWRIT  ', 'UGPICT  ', 'UGPICT  '/
      DATA          EMN002
     X  /'UGENAB  ', 'UGENAB  ', 'UGDSAB  ', 'UGDSAB  ', 'UGEVNT  ',
     X   'UGEVNT  ', 'UGECTL  ', 'UGECTL  ', 'UGECTL  ', 'UGDSPC  ',
     X   'UGDSPC  ', 'UGDSPC  ', 'UGDSPC  ', 'UGWDOW  ', 'UGWDOW  ',
     X   'UGWDOW  ', 'UGWDOW  ', 'UGSHLD  ', 'UGSHLD  ', 'UGSHLD  ',
     X   'UG3WRD  ', 'UG3WRD  ', 'UG3WRD  ', 'UG3WRD  ', 'UG3WRD  ',
     X   'UG3TRN  ', 'UG3TRN  ', 'UG3TRN  ', 'UG3TRN  ', 'UG3TRN  ',
     X   'UG3TRN  ', 'UG3TRN  ', 'UGCTOL  ', 'UGCTOL  ', 'UGCTOL  ',
     X   'UGXERR  ', 'UGTRAN  ', 'UGSCIN  ', 'UGSCIN  ', 'UGSCIN  '/
      DATA          EMN003
     X  /'UGSCIN  ', 'UGXHCH  ', 'UGXHCH  ', 'UGLNAX  ', 'UGLGAX  ',
     X   'UGLGDX  ', 'UGCNTR  ', 'UGCNTR  ', 'UGCNTR  ', 'UGQCTR  ',
     X   'UGMESH  ', 'UGMESH  ', 'UG2DHG  ', 'UG2DHG  ', 'UG2DHG  ',
     X   'UG2DHP  '/
      DATA          EMI001
     X  /    1,   11,   11,   11,   11,    1,   11,    1,   11,    1,
     X      11,   11,   11,   11,   11,    1,   11,    1,   11,    1,
     X       2,    3,    4,    5,    6,    7,   12,    1,   12,   12,
     X      13,    1,    2,    3,    4,   12,   13,   14,   12,   13,
     X      12,   13,   12,   13,   12,   13,   12,   13,   15,    1,
     X       2,    3,   12,    1,    2,    3,   12,    1,    2,   12,
     X       1,    2,    3,    4,   12,    1,    2,    3,    4,    5,
     X       6,   12,    1,    2,   14,    1,    1,    1,    2,    3/
      DATA          EMI002
     X  /    4,    1,    2,    1,    1,    1,    1,    2,    3,    1,
     X       1,    2,    1,    2,    3,    1/
      DATA          EMMPC/   96*0/
      DATA          EMO001
     X  /    1,    4,    4,    4,    4,    8,    4,   10,    4,   12,
     X       4,    4,    4,    4,    4,    8,    4,    8,    4,   15,
     X      17,   19,   22,   25,   28,   30,   33,   34,   33,   33,
     X      36,   38,   41,   44,   45,   33,   36,   48,   33,   36,
     X      33,   36,   33,   36,   33,   36,   33,   36,   51,   53,
     X      57,   59,   33,   62,   64,   67,   33,   69,   71,   33,
     X      53,   62,   64,   73,   33,   76,   79,   81,   84,   86,
     X      88,   33,   10,   92,   48,   95,   97,   99,  101,  103/
      DATA          EMO002
     X  /  105,  107,  110,  111,  111,  113,  115,  110,  117,  115,
     X     115,  110,  119,  110,  122,  119/
      DATA          EML001
     X  /    3,    4,    4,    4,    4,    2,    4,    2,    4,    3,
     X       4,    4,    4,    4,    4,    2,    4,    2,    4,    2,
     X       2,    3,    3,    3,    2,    3,    1,    2,    1,    1,
     X       2,    3,    3,    1,    3,    1,    2,    3,    1,    2,
     X       1,    2,    1,    2,    1,    2,    1,    2,    2,    4,
     X       2,    3,    1,    2,    3,    2,    1,    2,    2,    1,
     X       4,    2,    3,    3,    1,    3,    2,    3,    2,    2,
     X       4,    1,    2,    3,    3,    2,    2,    2,    2,    2/
      DATA          EML002
     X  /    2,    3,    1,    2,    2,    2,    2,    1,    2,    2,
     X       2,    1,    3,    1,    1,    3/
      DATA          EMT001
     X  /'THE LENGTH OF THE GRAPHIC SEGMENT IS TOO SMALL. ',
     X   'THE MINIMUM SIZE IS 16, BUT IT MUST BE MUCH     ',
     X   'LARGER THAN THIS TO BE USEFUL.                  ',
     X   'NOT ENOUGH ROOM IS AVAILABLE IN THE GRAPHIC     ',
     X   'SEGMENT TO CONTAIN THE NEW DATA.  IN THIS CASE, ',
     X   'NOTHING HAS BEEN ADDED TO THE GRAPHIC SEGMENT,  ',
     X   'AND IT REMAINS UNCHANGED.                       ',
     X   'THE STRING LENGTH IS INVALID.  THE LENGTH MUST  '/
      DATA          EMT002
     X  /'BE AT LEAST 1 AND AT MOST 1024.                 ',
     X   'THE STRING LENGTHS ARE INVALID.  THE LENGTHS    ',
     X   'MUST BE AT LEAST 1 AND AT MOST 1024.            ',
     X   'THE POLYGON IS INVALID.  THE NUMBER OF VERTICES ',
     X   'MUST BE AT LEAST 4 AND AT MOST 32 (INCLUDING THE',
     X   'CLOSING VERTEX).                                ',
     X   'A VALID GRAPHIC DEVICE WAS NOT SPECIFIED.  CHECK',
     X   'YOUR OPTIONS PARAMETER.                         '/
      DATA          EMT003
     X  /'INVALID IDENTIFICATION.  THE IDENTIFICATION     ',
     X   'PARAMETER MUST HAVE A NONZERO VALUE.            ',
     X   'DUPLICATE IDENTIFICATION.  IF YOU ARE TRYING TO ',
     X   'OPEN MORE THAN ONE GRAPHIC DEVICE, THEN EACH    ',
     X   'DEVICE MUST HAVE A UNIQUE IDENTIFICATION.       ',
     X   'TOO MANY GRAPHIC DEVICES.  A MAXIMUM OF 32      ',
     X   'GRAPHIC DEVICES MAY BE OPEN AT ONCE BY A SINGLE ',
     X   'PROGRAM.                                        '/
      DATA          EMT004
     X  /'THE DEVICE-DEPENDENT CODE FOR THE SELECTED      ',
     X   'DEVICE IS NOT AVAILABLE.  THE EXECUTABLE LOAD   ',
     X   'MODULE WILL HAVE TO BE RECREATED.               ',
     X   'THE GRAPHIC DEVICE CANNOT BE OPENED.  CHECK YOUR',
     X   'SPECIFICATION STATEMENTS FOR THE OUTPUT FILE.   ',
     X   'YOU HAVE OPENED MORE THAN ONE FULLY INTERACTIVE ',
     X   'GRAPHIC DEVICE.  THIS MAY NOT WORK              ',
     X   'SATISFACTORILY.                                 '/
      DATA          EMT005
     X  /'NO GRAPHIC DEVICE IS ACTIVE AT PRESENT.         ',
     X   'THE GIVEN IDENTIFICATION IS INVALID; NO SUCH    ',
     X   'GRAPHIC DEVICE IS OPEN AT PRESENT.              ',
     X   'AN INVALID OPERATION WAS REQUESTED FOR THE      ',
     X   'ACTIVE GRAPHIC DEVICE.                          ',
     X   'THE DISPLAY FILE IS NOT LARGE ENOUGH TO HOLD ALL',
     X   'OF THE GRAPHIC SEGMENTS.  YOU WILL HAVE TO      ',
     X   'SIMPLIFY THE PICTURE.                           '/
      DATA          EMT006
     X  /'THE GRAPHIC SEGMENT IS TOO LARGE FOR THIS       ',
     X   'GRAPHIC DEVICE.  TRY USING SMALLER GRAPHIC      ',
     X   'SEGMENTS.                                       ',
     X   'THE GRAPHIC SEGMENT CONTAINS INVALID DATA.      ',
     X   'AN INTERNAL ARRAY HAS OVERFLOWN WHILE TRYING TO ',
     X   'SCISSOR A POLYGON-FILL AREA TO THE CURRENT      ',
     X   'WINDOW.  USE SIMPLER POLYGONS.                  ',
     X   'THE EXTENDED CHARACTER MODULE IS NOT AVAILABLE. '/
      DATA          EMT007
     X  /'THE EXECUTABLE LOAD MODULE WILL HAVE TO BE      ',
     X   'RECREATED.                                      ',
     X   'AN ATTEMPT WAS MADE TO READ A DISABLED CONTROL  ',
     X   'UNIT ON THE ACTIVE GRAPHIC DEVICE.              ',
     X   'THE SUBROUTINE WAS CALLED AT AN INCORRECT TIME. ',
     X   'IT MUST BE CALLED BEFORE THE FIRST GRAPHIC      ',
     X   'SEGMENT IN A PICTURE IS TRANSMITTED TO THE      ',
     X   'GRAPHIC DEVICE.                                 '/
      DATA          EMT008
     X  /'THE PICTURE SIZES ARE INCORRECT.  THEY MUST BOTH',
     X   'BE POSITIVE.                                    ',
     X   'THE AFFINITY PARAMETER IS INCORRECT.  IT MUST BE',
     X   'GREATER THAN OR EQUAL TO ZERO AND LESS THAN OR  ',
     X   'EQUAL TO ONE.                                   ',
     X   'THE VIEW PORT PARAMETER IS INCORRECT.  THE LOW X',
     X   'AND Y VALUES MUST BE LESS THAN THE HIGH VALUES. ',
     X   'AN INCORRECT OVERLAY OF THE VIEW PORT ONTO THE  '/
      DATA          EMT009
     X  /'DRAWING SPACE HAS BEEN SPECIFIED.  THE VIEW PORT',
     X   'MUST LIE ENTIRELY WITHIN THE DRAWING SPACE.     ',
     X   'THE WINDOW PARAMETER IS INCORRECT.  THE LOW X   ',
     X   'AND Y VALUES MUST BE LESS THAN THE HIGH VALUES. ',
     X   'THE SHIELD PARAMETER IS INCORRECT.  THE LOW X   ',
     X   'AND Y VALUES MUST BE LESS THAN THE HIGH VALUES. ',
     X   'THE INDEX OF THE SELECTED SHIELD IS INCORRECT.  ',
     X   'IT MUST BE BETWEEN ONE AND FOUR.                '/
      DATA          EMT010
     X  /'THE WORLD VOLUME PARAMETER IS INCORRECT.  THE   ',
     X   'LOW X, Y, AND Z VALUES MUST BE LESS THAN THE    ',
     X   'HIGH VALUES.                                    ',
     X   'THE OBJECT VOLUME PARAMETER IS INCORRECT.  THE  ',
     X   'LOW X, Y, AND Z VALUES MUST BE LESS THAN THE    ',
     X   'HIGH VALUES.                                    ',
     X   'THE OBJECT VOLUME PARAMETER IS INCORRECT.  IT   ',
     X   'MUST LIE COMPLETELY WITHIN THE WORLD VOLUME.    '/
      DATA          EMT011
     X  /'THE EYE POINT PARAMETER IS INCORRECT.  IT MUST  ',
     X   'BE INSIDE THE WORLD VOLUME AND OUTSIDE THE      ',
     X   'OBJECT VOLUME.                                  ',
     X   'THE UP DIRECTION PARAMETER FOR THE              ',
     X   'THREE-DIMENSIONAL VIEW IS INVALID.              ',
     X   'THE PROJECTION FLAG PARAMETER IS INCORRECT.  IT ',
     X   'MUST BE ZERO OR GREATER AND LESS THAN ONE.      ',
     X   'THE ACTIVE GRAPHIC DEVICE CANNOT RETURN THE     '/
      DATA          EMT012
     X  /'CURRENT THREE-DIMENSIONAL VIEW INFORMATION.     ',
     X   'INSTEAD, THE INFORMATION AVAILABLE ON THE HOST  ',
     X   'COMPUTER HAS BEEN RETURNED.                     ',
     X   'THERE IS NOT ENOUGH SPACE AVAILABLE IN THE      ',
     X   'OUTPUT ARRAYS TO CONTAIN THE GENERATED LINE     ',
     X   'SEGMENT DATA.                                   ',
     X   'AN ATTEMPT HAS BEEN MADE TO USE THE SUBROUTINE  ',
     X   'UGXERR RECURSIVELY.                             '/
      DATA          EMT013
     X  /'THE REQUESTED TRANSFORMATION IS SINGULAR AND    ',
     X   'CANNOT BE GENERATED.                            ',
     X   'THE NUMBER OF POINTS TO BE INTERPOLATED MUST BE ',
     X   'AT LEAST TWO.                                   ',
     X   'THE NUMBER OF TENSION VALUES MUST BE AT LEAST   ',
     X   'ONE.                                            ',
     X   'THE INITIAL OR TERMINAL CONTROL OF THE          ',
     X   'INTERPOLATING CURVE IS INVALID.                 '/
      DATA          EMT014
     X  /'THE DISTANCE BETWEEN TWO GIVEN POINTS IS TOO    ',
     X   'SMALL.                                          ',
     X   'EITHER THE FIRST AND LAST POINTS IN THE REGION  ',
     X   'DEFINITION ARE NOT THE SAME, OR THERE ARE FEWER ',
     X   'THAN THREE POINTS GIVEN.                        ',
     X   'THE WORK AREA ARRAY IS NOT LARGE ENOUGH.        ',
     X   'THE NUMBER OF LABELS AND PRIMARY TIC MARKS IS   ',
     X   'TOO SMALL.  IT MUST BE AT LEAST TWO.            '/
      DATA          EMT015
     X  /'ROUND NUMBERS COULD NOT BE FOUND FOR THE AXIS   ',
     X   'WITHIN THE IMPOSED CONSTRAINTS.                 ',
     X   'THE BOUNDS OF THE TWO-DIMENSIONAL ARRAY MUST BE ',
     X   'AT LEAST 3 BY 3 TO DEFINE A VALID SURFACE.      ',
     X   'THERE IS SOMETHING SUBSTANTIALLY WRONG WITH THE ',
     X   'DEFINITION OF THE SURFACE.                      ',
     X   'THE BOUNDS OF THE TWO-DIMENSIONAL ARRAY MUST BE ',
     X   'AT LEAST 3 BY 3 TO DEFINE A VALID               '/
      DATA          EMT016
     X  /'TWO-DIMENSIONAL HISTOGRAM.                      ',
     X   'THE TRANSFORMATION IS NOT VALID.                '/
C
      END
