      BLOCK DATA UGSIMP
C
C *******************  THE UNIFIED GRAPHICS SYSTEM  *******************
C *                THE EXTENDED/SIMPLEX CHARACTER SET                 *
C *                                                                   *
C *  THIS BLOCK OF DATA DEFINES THE EXTENDED/SIMPLEX CHARACTER SET.   *
C *                                                                   *
C *                          ROBERT C. BEACH                          *
C *                    COMPUTATION RESEARCH GROUP                     *
C *                STANFORD LINEAR ACCELERATOR CENTER                 *
C *                                                                   *
C *********************************************************************
C
      SAVE          /UGA013/
C  THE DECLARATION OF THE COMMON BLOCK.
      COMMON        /UGA013/
     X              CHRTP3,
     X              CHRNC3,
     X              CHRSC3,
     X              CHRCS3,
     X              CHROT3,
     X              CHRLT3,
     X              CHRCT3,
     X              CHRID3
C
C  SYMBOL STROKE TABLE IDENTIFICATION.
      CHARACTER*8   CHRID3

C  SYMBOL STROKE TABLE TYPE.
      INTEGER*2     CHRTP3
C  NUMBER OF CHARACTERS.
      INTEGER*2     CHRNC3
C  Size of character pair table
      INTEGER*2     CHRSC3
C  CHARACTER SPACING.
      INTEGER*2     CHRCS3
C  CHARACTER PAIR TABLE.
      CHARACTER*2   CHRCT3(  365)
      CHARACTER*2   CHC001(80)
      EQUIVALENCE   (CHC001(1),CHRCT3(    1))
      CHARACTER*2   CHC002(80)
      EQUIVALENCE   (CHC002(1),CHRCT3(   81))
      CHARACTER*2   CHC003(80)
      EQUIVALENCE   (CHC003(1),CHRCT3(  161))
      CHARACTER*2   CHC004(80)
      EQUIVALENCE   (CHC004(1),CHRCT3(  241))
      CHARACTER*2   CHC005(45)
      EQUIVALENCE   (CHC005(1),CHRCT3(  321))
C  CHARACTER OFFSET TABLE.
      INTEGER*2     CHROT3(  365)
      INTEGER*2     CHO001(80)
      EQUIVALENCE   (CHO001(1),CHROT3(    1))
      INTEGER*2     CHO002(80)
      EQUIVALENCE   (CHO002(1),CHROT3(   81))
      INTEGER*2     CHO003(80)
      EQUIVALENCE   (CHO003(1),CHROT3(  161))
      INTEGER*2     CHO004(80)
      EQUIVALENCE   (CHO004(1),CHROT3(  241))
      INTEGER*2     CHO005(45)
      EQUIVALENCE   (CHO005(1),CHROT3(  321))
C  LINE SEGMENT TABLE.
      INTEGER*2     CHRLT3( 2572)
      INTEGER*2     CHL001(80)
      EQUIVALENCE   (CHL001(1),CHRLT3(    1))
      INTEGER*2     CHL002(80)
      EQUIVALENCE   (CHL002(1),CHRLT3(   81))
      INTEGER*2     CHL003(80)
      EQUIVALENCE   (CHL003(1),CHRLT3(  161))
      INTEGER*2     CHL004(80)
      EQUIVALENCE   (CHL004(1),CHRLT3(  241))
      INTEGER*2     CHL005(80)
      EQUIVALENCE   (CHL005(1),CHRLT3(  321))
      INTEGER*2     CHL006(80)
      EQUIVALENCE   (CHL006(1),CHRLT3(  401))
      INTEGER*2     CHL007(80)
      EQUIVALENCE   (CHL007(1),CHRLT3(  481))
      INTEGER*2     CHL008(80)
      EQUIVALENCE   (CHL008(1),CHRLT3(  561))
      INTEGER*2     CHL009(80)
      EQUIVALENCE   (CHL009(1),CHRLT3(  641))
      INTEGER*2     CHL010(80)
      EQUIVALENCE   (CHL010(1),CHRLT3(  721))
      INTEGER*2     CHL011(80)
      EQUIVALENCE   (CHL011(1),CHRLT3(  801))
      INTEGER*2     CHL012(80)
      EQUIVALENCE   (CHL012(1),CHRLT3(  881))
      INTEGER*2     CHL013(80)
      EQUIVALENCE   (CHL013(1),CHRLT3(  961))
      INTEGER*2     CHL014(80)
      EQUIVALENCE   (CHL014(1),CHRLT3( 1041))
      INTEGER*2     CHL015(80)
      EQUIVALENCE   (CHL015(1),CHRLT3( 1121))
      INTEGER*2     CHL016(80)
      EQUIVALENCE   (CHL016(1),CHRLT3( 1201))
      INTEGER*2     CHL017(80)
      EQUIVALENCE   (CHL017(1),CHRLT3( 1281))
      INTEGER*2     CHL018(80)
      EQUIVALENCE   (CHL018(1),CHRLT3( 1361))
      INTEGER*2     CHL019(80)
      EQUIVALENCE   (CHL019(1),CHRLT3( 1441))
      INTEGER*2     CHL020(80)
      EQUIVALENCE   (CHL020(1),CHRLT3( 1521))
      INTEGER*2     CHL021(80)
      EQUIVALENCE   (CHL021(1),CHRLT3( 1601))
      INTEGER*2     CHL022(80)
      EQUIVALENCE   (CHL022(1),CHRLT3( 1681))
      INTEGER*2     CHL023(80)
      EQUIVALENCE   (CHL023(1),CHRLT3( 1761))
      INTEGER*2     CHL024(80)
      EQUIVALENCE   (CHL024(1),CHRLT3( 1841))
      INTEGER*2     CHL025(80)
      EQUIVALENCE   (CHL025(1),CHRLT3( 1921))
      INTEGER*2     CHL026(80)
      EQUIVALENCE   (CHL026(1),CHRLT3( 2001))
      INTEGER*2     CHL027(80)
      EQUIVALENCE   (CHL027(1),CHRLT3( 2081))
      INTEGER*2     CHL028(80)
      EQUIVALENCE   (CHL028(1),CHRLT3( 2161))
      INTEGER*2     CHL029(80)
      EQUIVALENCE   (CHL029(1),CHRLT3( 2241))
      INTEGER*2     CHL030(80)
      EQUIVALENCE   (CHL030(1),CHRLT3( 2321))
      INTEGER*2     CHL031(80)
      EQUIVALENCE   (CHL031(1),CHRLT3( 2401))
      INTEGER*2     CHL032(80)
      EQUIVALENCE   (CHL032(1),CHRLT3( 2481))
      INTEGER*2     CHL033(12)
      EQUIVALENCE   (CHL033(1),CHRLT3( 2561))
C
      DATA          CHRTP3/    3/
      DATA          CHRNC3/  365/
      DATA          CHRSC3/ 2572/
      DATA          CHRCS3/    6/

      DATA          CHRID3/'SIMPLEX '/

      DATA          CHC001
     X  / '  ', '! ', '" ', '# ', '$ ', '% ', '& ',''' ', '( ', ') ',
     X    '* ', '+ ', ', ', '- ', '. ', '/ ', '0 ', '1 ', '2 ', '3 ',
     X    '4 ', '5 ', '6 ', '7 ', '8 ', '9 ', ': ', '; ', '< ', '= ',
     X    '> ', '? ', '@ ', 'A ', 'B ', 'C ', 'D ', 'E ', 'F ', 'G ',
     X    'H ', 'I ', 'J ', 'K ', 'L ', 'M ', 'N ', 'O ', 'P ', 'Q ',
     X    'R ', 'S ', 'T ', 'U ', 'V ', 'W ', 'X ', 'Y ', 'Z ', '[ ',
     X    '\ ', '] ', '^ ', '_ ', '` ', 'a ', 'b ', 'c ', 'd ', 'e ',
     X    'f ', 'g ', 'h ', 'i ', 'j ', 'k ', 'l ', 'm ', 'n ', 'o '/
      DATA          CHC002
     X  / 'p ', 'q ', 'r ', 's ', 't ', 'u ', 'v ', 'w ', 'x ', 'y ',
     X    'z ', '{ ', '| ', '} ', '~ ', '*A', '0A', '1A', '2A', '3A',
     X    '4A', '5A', '6A', '7A', '8A', '9A', 'AA', 'BA', 'CA', 'EA',
     X    'HA', 'JA', 'KA', 'MA', 'NA', 'OA', 'PA', 'QA', 'SA', 'TA',
     X    'UA', 'VA', 'WA', 'XA', 'YA', '1B', '2B', '3B', '4B', '5B',
     X    '6B', 'AB', 'BB', 'CB', 'DB', 'EB', 'FB', 'GB', 'HB', 'IB',
     X    'JB', 'KB', 'LB', 'MB', 'NB', 'OB', 'PB', 'QB', 'RB', 'SB',
     X    'TB', 'UB', 'VB', 'WB', 'XB', 'YB', 'ZB', '1C', '2C', '3C'/
      DATA          CHC003
     X  / '4C', '5C', '6C', 'AC', 'BC', 'CC', 'DC', 'EC', 'FC', 'GC',
     X    'HC', 'IC', 'JC', 'KC', 'LC', 'MC', 'NC', 'OC', 'PC', 'QC',
     X    'RC', 'SC', 'TC', 'UC', 'VC', 'WC', 'XC', 'YC', 'ZC', 'OD',
     X    'UD', 'AF', 'BF', 'CF', 'DF', 'EF', 'FF', 'GF', 'HF', 'IF',
     X    'KF', 'LF', 'MF', 'NF', 'OF', 'PF', 'QF', 'RF', 'SF', 'TF',
     X    'UF', 'WF', 'XF', 'YF', 'ZF', 'AG', 'BG', 'CG', 'DG', 'EG',
     X    'FG', 'GG', 'HG', 'IG', 'KG', 'LG', 'MG', 'NG', 'OG', 'PG',
     X    'QG', 'RG', 'SG', 'TG', 'UG', 'WG', 'XG', 'YG', 'ZG', 'HK'/
      DATA          CHC004
     X  / 'LK', 'AL', 'BL', 'CL', 'DL', 'EL', 'FL', 'GL', 'HL', 'IL',
     X    'JL', 'KL', 'LL', 'ML', 'NL', 'OL', 'PL', 'QL', 'RL', 'SL',
     X    'TL', 'UL', 'VL', 'WL', 'XL', 'YL', 'ZL', '(M', ')M', '*M',
     X    '+M', '-M', '.M', '/M', '0M', '2M', '=M', 'AM', 'BM', 'CM',
     X    'DM', 'EM', 'GM', 'HM', 'IM', 'JM', 'LM', 'MM', 'NM', 'PM',
     X    'RM', 'SM', 'TM', 'XM', 'YM', 'ZM', '0O', '1O', '2O', '3O',
     X    '4O', '5O', '6O', '7O', '8O', '9O', ',P', '.P', 'AP', 'DP',
     X    'EP', 'FP', 'IP', 'PP', 'QP', 'UP', '(S', ')S', '+S', '/S'/
      DATA          CHC005
     X  / '0S', 'AS', 'BS', 'CS', 'DS', 'ES', 'LS', 'NS', 'PS', 'RS',
     X    'TS', 'US', 'VS', 'WS', 'XS', 'AT', 'ET', 'FT', 'GT', 'IT',
     X    'KT', 'LT', 'MT', 'NT', 'UT', ' U', '0U', '1U', '2U', '3U',
     X    '4U', '5U', '6U', '1V', '2V', '3V', '4V', '5V', '6V', 'BW',
     X    'DW', 'LW', 'RW', 'UW', '$$'/
      DATA          CHO001
     X  / 1326, 1410, 1460, 1534, 1496, 1562, 1522, 1454, 1373, 1379,
     X    1338, 1328, 1365, 1334, 1358, 1348, 1216, 1225, 1232, 1242,
     X    1257, 1263, 1275, 1289, 1294, 1312, 1385, 1397, 1723, 1352,
     X    1728, 1421, 1544,    1,    8,   21,   31,   40,   48,   55,
     X      67,   75,   83,   92,  100,  105,  112,  118,  129,  138,
     X     151,  162,  176,  182,  190,  195,  202,  208,  215, 1599,
     X    1595, 1605, 1645, 1586, 1639,  223,  237,  249,  259,  271,
     X     283,  292,  305,  314,  321,  328,  336,  342,  354,  363/
      DATA          CHO002
     X  /  374,  386,  399,  407,  417,  425,  433,  438,  445,  451,
     X     458, 1611, 1576, 1620, 1590, 2192, 2265, 2274, 2286, 2296,
     X    2308, 2324, 2339, 2353, 2365, 2374, 2384, 2402, 2180, 2090,
     X    2054, 2116, 2238, 2065, 2147, 2169, 2159, 2246, 2125, 2253,
     X    2135, 2079, 2105, 2202, 2220,  861,  893,  902,  910,  940,
     X     949,    1,  808,  886,  818,   40,  530,  466,  202,  855,
     X     976,  870,  879,  105,   67,  118,  504,  919,  129,   21,
     X     176,  208,    8,  961,  828,  929,  840, 1056, 1126, 1135/
      DATA          CHO003
     X  / 1143, 1171, 1179,  223,  987, 1119, 1016,  271, 1106, 1011,
     X     445, 1050, 1206, 1064, 1073, 1079, 1086,  363, 1094, 1152,
     X     374,  249, 1100,  451, 1000, 1191, 1025, 1161, 1037, 2525,
     X    2521,    1,    8,  547,  472,   40,  530,  466,   67,   75,
     X      92,  491,  105,  112,  118,  504,  478,  129,  512,  176,
     X     521,  565,  496,  553,  215,  577,  587,  779,  611,  625,
     X     766,  602,  647,  670,  676,  683,  690,  699,  363,  724,
     X     657,  733,  744,  754,  761,  795,  706,  787,  637, 2034/
      DATA          CHO004
     X  / 2045,  223,  237,  249,  259,  271,  283,  292,  305,  314,
     X     321,  328,  336,  342,  354,  363,  374,  386,  399,  407,
     X     417,  425,  433,  438,  445,  451,  458, 1873, 1878, 1692,
     X    1707, 1715, 1650, 1663, 1893, 1809, 1755, 1763, 1883, 1773,
     X    1815, 1888, 1728, 1740, 1826, 1836, 1723, 1733, 1747, 1677,
     X    1787, 1781, 1803, 1657, 1853, 1867, 2410, 2416, 2422, 2429,
     X    2436, 2450, 2464, 2478, 2492, 2510, 1397, 1385, 1454, 1482,
     X    1410, 1488, 1436, 1470, 1460, 1421, 1599, 1605, 1522, 1595/
      DATA          CHO005
     X  / 1562, 1544, 1629, 1510, 1496, 1634, 1611, 1590, 1534, 1620,
     X    1645, 1586, 1576, 1580, 1639, 1915, 1907, 1986, 1968, 1944,
     X    1976, 1960, 1922, 1932, 1952, 2529, 2531, 2533, 2535, 2537,
     X    2539, 2541, 2543, 2545, 2547, 2549, 2551, 2553, 2555, 2024,
     X    2003, 2010, 2017, 1996, 2557/
      DATA          CHL001
     X  /  832, 7997,24902,24890, 7875,24896, 8128, 1600, 8000,25024,
     X   24769,24641,24513,24256,24634,25024,24769,24641,24513, 8128,
     X    1216, 8510,24511,24384,24513,24644,24769,24896,24767, 7998,
     X    1088, 7997,24646,25024,24767,24636,24511,24256, 8515,  960,
     X    8509,24128,24646,25152, 8125,24256, 8512,  832, 7997,24646,
     X   25152, 8125,24256, 8512, 1472, 8256,24896,24638,24511,24384,
     X   24513,24644,24769,24896,24767, 7998,  959, 7997,24646, 8253,
     X   25024, 8259,24634, 8003,  958, 7997,24896, 8128,24646, 8128/
      DATA          CHL002
     X  /24896, 7997, 1088, 7998,24767,24768,24769,24645, 8128,24896,
     X    7997,  960, 7997,24646, 8768,24124, 8385,25021, 8003,  575,
     X    8003,24634,25024, 8003,  832, 7997,24646,24893,24899,24634,
     X    8003,  704, 7997,24646,25146,24646, 7997, 1344, 8125,24896,
     X   24769,24644,24513,24384,24511,24636,24767, 8387, 1088, 7997,
     X   24646,25024,24767,24639,24511,24256, 8512, 1600, 8125,24896,
     X   24769,24644,24513,24384,24511,24636,24767, 8386,24894, 8003,
     X    1344, 7997,24646,25024,24767,24639,24511,24256, 8512,24893/
      DATA          CHL003
     X  / 8003, 1728, 7998,24767,24896,24769,24641,24513,24384,24513,
     X   24641,24769,24896,24767, 7998,  704, 8253,24646, 8000,25152,
     X    7997,  960, 8003,24635,24767,24896,24769,24645, 7997,  576,
     X    8003,24890,24902, 7997,  832, 8003,24762,24771,24765,24774,
     X    7997,  704, 7997,25158, 7744,25146, 8003,  832, 8003,24893,
     X   24899, 7997,24637, 8259,  960, 8509,24128,25158,24128, 8381,
     X   24896, 8128, 1727, 8001,24896,24767,24637, 8257,24511,24512,
     X   24513,24641,24769,24768,24767, 8001, 1471, 7997,24646, 8253/
      DATA          CHL004
     X  /24769,24768,24767,24638,24511,24512,24513, 8386, 1215, 8382,
     X   24511,24512,24513,24642,24769,24768,24767, 8000, 1471, 8381,
     X   24646, 8253,24513,24512,24511,24638,24767,24768,24769, 8002,
     X    1471, 7999,25024,24641,24513,24512,24511,24638,24767,24768,
     X   24769, 8002, 1087, 7997,24645,24769,24768,24767, 8126,24384,
     X    8384, 1599, 8382,24511,24512,24513,24642,24769,24768,24767,
     X    8257,24635,24511, 8133, 1087, 7997,24646, 8253,24769,24768,
     X   24767,24637, 8003,  830, 7997,24896, 8128,24644,24512, 8255/
      DATA          CHL005
     X  /  830, 7995,24768,24769,24645,24512, 8127,  959, 7997,24646,
     X    8638,24253, 8385,24894, 8003,  702, 7997,24896, 8128,24646,
     X    8125, 1472, 7997,24644, 8255,24769,24767,24637, 8259,24769,
     X   24767,24637, 8003, 1087, 7997,24644, 8255,24769,24768,24767,
     X   24637, 8003, 1343, 8125,24768,24769,24642,24513,24512,24511,
     X   24638,24767, 8259, 1471, 7998,24767,24768,24769,24642,24513,
     X   24512,24511, 8257,24634, 8389, 1599, 8382,24511,24512,24513,
     X   24642,24769,24768,24767, 8257,24635,24767, 7877,  959, 7997/
      DATA          CHL006
     X  /24644, 8255,24769,24768,24767, 8000, 1215, 7998,24767,24768,
     X   24769,24258,24769,24768,24767, 8000,  959, 8001,25024, 8001,
     X   24636,24767,24769, 8002,  959, 8001,24637,24767,24768,24769,
     X   24643, 7999,  575, 8001,24764,24900, 7999,  832, 8001,24764,
     X   24770,24766,24772, 7999,  703, 7997,25028, 7872,25020, 8003,
     X     831, 8253,24388, 8640,24638,24380, 8261,  959, 8127,24768,
     X    8002,25024,24252,25024, 8003,  704, 7997,24646,25152,24639,
     X    7998,  704, 7997,24902,24890,24128, 8515, 1600, 8125,24896/
      DATA          CHL007
     X  /24769,24644,24513,24384,24511,24636,24767, 8131,25152, 8000,
     X     576, 7997,24902,24890, 8003,  960, 7997,25152, 8131,24384,
     X    8131,25152, 7997,  960, 8125,24646, 8128,25152, 8128,24634,
     X    8131, 1088, 8510,24639,24128,24899,24387,25152,24639, 7998,
     X    1088, 8002,24769,24767,24769,24767, 8000,24635, 8259, 2112,
     X    8125,24896, 8128,24646, 8128,24896, 8255,24767,24638,24511,
     X   24384,24513,24642,24769,24896, 8126,  704, 7997,25158, 7744,
     X   25146, 8003, 1472, 8125,24896, 8128,24646, 8384,24384, 8126/
      DATA          CHL008
     X  /24766,24896,24770, 7999, 1472, 7997,24768,24515,24642,24769,
     X   24896,24767,24638,24509,24768, 8003, 1216, 8513,24380,24512,
     X   24513,24642,24769,24768,24892, 8003, 1855, 7997,24643,24770,
     X   24768,24767,24511,24383, 8255,24767,24768,24769,24641,24513,
     X    8128, 1086, 8002,24894,24638,24511,24513,24642,24898, 7998,
     X    1727, 8128,24768,24767,24639,24511,24512,24513,24641,24769,
     X   24513,24769,24768, 8126, 1471, 7999,24896, 8383,24511,24512,
     X   24513,24642,24769,24768,24767, 8000, 1215, 7998,24767,24897/
      DATA          CHL009
     X  /24386,24770,24767,24511,24386, 8382, 1216, 8125,24771,24513,
     X   24511, 8512,24769,24767,24508, 8132, 1599, 7998,24642,24769,
     X   24768,24767,24638,24511,24512,24513, 8257,25024, 8001,  702,
     X    8254,24511,24513,24771, 8127,  830, 7997,24644, 8512,24382,
     X   24894, 8003,  831, 7997,24898, 8003,24767,24892, 8003, 1088,
     X    7996,24773,24637,24767,24768,24769,24643, 7999,  831, 8001,
     X   24768,24508,24897,24771, 7999, 2239, 7996,24896,24769,24513,
     X   24512,24513,24769,24896,24639,24384,24513,24898,24768,24639/
      DATA          CHL010
     X  /24384,24513, 8382, 1088, 8125,24644, 8127,24769,25024, 8128,
     X   24636, 8131, 1343, 7998,24767,24768,24769,24642,24513,24512,
     X   24510,24637, 8388, 1216, 8385,24637,24511,24512,24513,24642,
     X   24897,24896, 7999,  831, 8125,24772, 7999,24769,24896, 7999,
     X     575, 8001,24764,24900, 7999, 1599, 8125,24768,24769,24642,
     X   24513,24512,24511,24638,24767, 8255,24774, 8126,  959, 7997,
     X   25028, 7872,24768,24764,24768, 8003,  959, 8125,24773, 7999,
     X   24766,24768,24770, 7999, 1600, 8129,24511,24638,24767,24769/
      DATA          CHL011
     X  /24642, 8254,24767,24769,24642,24513, 8127, 1216, 8000,25024,
     X   24767,24639,24511,24256,24646,25152, 7997, 1216, 7996,24641,
     X   24770,24772,24896,24633, 7745,25152, 8003, 1474, 7997,24899,
     X   24387, 8509,24896, 8125,24646, 8640,24381,24893, 8003, 1856,
     X    7998,24767,24896,24769,24641,24513,24512, 8002,24769,24896,
     X   24767,24639,24511, 8128,  704, 8003,24634,25158,24634, 8003,
     X    1088, 8003,24634,25158,24634, 7879,24767,24769, 8124, 1088,
     X    7997,24646, 8253,24896, 8515,24381,24893, 8003,  832, 7997/
      DATA          CHL012
     X  /24770,24772,24896,24634, 8003,  831, 8381,24256,24646, 8640,
     X   24633, 8004, 1087, 8003,24637,24767,24768,24769, 8259,24634,
     X    8003,  960, 8003,24634,25152,24646, 8000,24634, 8259, 1088,
     X    8003,24634,25152, 8000,24646, 8512,24633, 8004, 1216, 8128,
     X   24896,24767,24639,24511,24384,24646,24512, 8509, 1344, 8000,
     X   24896,24767,24639,24511,24384,24646, 8768,24634, 8003, 1087,
     X    8000,24896,24767,24639,24511,24384,24646, 8381, 1472, 7998,
     X   24767,24896,24769,24644,24513,24384,24511, 8510,24896, 8000/
      DATA          CHL013
     X  / 1857, 7997,24646, 8253,24768, 8254,24767,24896,24769,24644,
     X   24513,24384,24511,24636, 8514, 1344, 7997,24899, 8512,24256,
     X   24513,24641,24769,25024,24634, 8003, 1599, 7999,24769,24768,
     X   24767,24639,24511,24512,24513,24642,24769,24897, 7998, 1343,
     X    7999,24896,24767,24511,24384,24644,24896,24767,24511, 8129,
     X     575, 7997,24644,25024, 7999, 1087, 7996,24641,24772,24896,
     X   24635, 7873,25024, 8003, 1472, 7997,24770,24514, 8382,24896,
     X    8126,24644, 8512,24510,24766, 8003, 1599, 7998,24767,24768/
      DATA          CHL014
     X  /24769,24513,24512, 8129,24769,24768,24767,24511, 8129,  703,
     X    8001,24636,25028,24636, 8003,  959, 8130,24768, 7999,24636,
     X   25028,24636, 8003, 1087, 7997,24644, 8254,24768,24898, 7998,
     X   24894, 8003,  703, 7997,24772,24896,24636, 8003,  832, 7997,
     X   24644,24894,24898,24636, 8003,  959, 7997,24644, 8254,25024,
     X    8258,24636, 8003,  703, 7997,24644,25024,24636, 8003,  704,
     X    8253,24644, 8000,25152, 7999, 1600, 8125,24896,24769,24642,
     X   24513,24384,24511,24638,24767, 8383,24646, 8254,  831, 8381/
      DATA          CHL015
     X  /24256,24644, 8640,24635, 8004, 1087, 8001,24638,24767,24768,
     X   24769, 8258,24636, 8003,  960, 8001,24636,25152,24644, 8000,
     X   24636, 8259, 1088, 8001,24636,25152, 8000,24644, 8512,24635,
     X    8004, 1087, 8001,24768,24636,24768,24769,24513,24512, 8257,
     X    1215, 7999,24768,24767,24511,24512,24644, 8640,24636, 8003,
     X     958, 7999,24768,24767,24511,24512,24644, 8255, 1471, 7998,
     X   24767,24768,24769,24642,24513,24512,24511, 8383,24896, 8001,
     X    1856, 7997,24644, 8254,24768, 8255,24767,24768,24769,24642/
      DATA          CHL016
     X  /24513,24512,24511,24638, 8386, 1215, 7997,24898, 8384,24384,
     X   24513,24769,24896,24636, 8003, 1088, 8125,24896,24771,24515,
     X   24384,24509,24765, 8387,  830, 7997,24896, 8128,24646,24511,
     X    8254, 1216, 8002,24769,24896,24767,24638,24126,24639,25152,
     X    8003, 1856, 8002,24769,24896,24767,24639,24511,24512, 8384,
     X   24767,24639,24511,24384,24513, 8514,  704, 8381,24646,24252,
     X   25152, 8001, 1472, 7998,24767,24896,24769,24642,24513,24384,
     X   24511,24643,25152, 7997, 1728, 8000,24769,24896,24767,24638/
      DATA          CHL017
     X  /24511,24384,24513,24644,24769,24896,24767, 7998,  576, 7997,
     X   25158,24128, 8509, 2240, 8128,24896,24767,24639,24511,24384,
     X   24513,24641,24769,24513,24641,24769,24896,24767,24639,24511,
     X    8128, 1728, 7998,24767,24896,24769,24644,24513,24384,24511,
     X   24638,24767,24896,24769, 8000,  190, 8000,  702, 8000,24896,
     X    8129,24638, 8129,  446, 8000,24896, 8000, 1214, 7999,24898,
     X    8255,24384, 8383,24642, 8128,24894, 8001,  447, 7997,25030,
     X    7997,  703, 7999,25024, 8257,24256, 8384,  829, 7997,24641/
      DATA          CHL018
     X  /24768,24639,24512, 8131,  957, 7996,24769,24641,24512,24639,
     X   24768, 8003,  701, 8125,24513,24644,24769, 7997,  701, 7997,
     X   24769,24644,24513, 8125, 1469, 7997,24768,24641,24512,24639,
     X    8259,24768,24641,24512,24639, 8128, 1597, 7996,24769,24641,
     X   24512,24639,24768, 8131,24641,24768,24639,24512, 8128, 1341,
     X    7997,24768,24641,24512,24639, 8258,24644,24768,24508, 8129,
     X    1855, 8125,24768,24641,24512,24639, 8258,24641,24768,24769,
     X   24641,24513,24512,24511, 8382, 2239, 8125,24768,24641,24512/
      DATA          CHL019
     X  /24639, 8258,24645,24768,24507, 8257,24768,24769,24641,24513,
     X   24512,24511, 8382,  701, 8001,24642,24768,24510, 8127, 1215,
     X    8001,24642,24768,24510, 8512,24642,24768,24510, 8127, 1470,
     X    7998,24767,24769,24386,24769, 8385,24513,24511,24894,24511,
     X    8129,  702, 8126,24644, 8127,24896, 7999,  958, 8126,24644,
     X    8383,24384, 8254,24896, 8001, 1727, 7999,24767,24768,24769,
     X   24258,24769,24768,24767, 8130,24634, 8128,24646, 8253, 1471,
     X    8254,24645, 8382,24513,24512,24511,24639,24767,24768,24769/
      DATA          CHL020
     X  / 8000, 1472, 8509,24260,24641,24769,24767,24253,24639,24767,
     X   24896,24769, 8002, 1215, 7998,24772, 8127,25024, 8254,24256,
     X    8511,24772, 7998, 2240, 8384,24511,24512,24641,24769,24768,
     X   24638,24769,24641,24513,24384,24511,24638,24767,24896,24769,
     X    8001, 1727, 7997,25030, 7999,24512,24641,24768,24639, 8380,
     X   24768,24639,24512,24641, 8130,  444, 7997,24646, 7997,  701,
     X    7997,24646, 8384,24634, 8003,  447, 7996,25024, 8004,  575,
     X    8000,25024,24639, 8001,  447, 8003,25018, 8003,  701, 8125/
      DATA          CHL021
     X  /24512,24646,24768, 7997,  701, 7997,24768,24646,24512, 8125,
     X    1086, 8252,24513,24642,24513,24769,24642,24769, 7996, 1086,
     X    7996,24769,24642,24769,24513,24642,24513, 8252,  574, 8253,
     X   24387,24899, 7997,  574, 7997,24899,24387, 8253,  701, 8129,
     X   24514,24768,24638, 7999,  574, 8000,24769,24767, 8000,  829,
     X    7999,24641,24768,24639,24512, 8129,  702, 7999,24898, 8000,
     X   24894, 8001, 1727, 8126,24768,24641,24512,24639, 8130,25024,
     X    8129,24512,24641,24768,24639, 8127, 1856, 7999,24767,24896/
      DATA          CHL022
     X  /24769,24642,24513,24384,24511,24638, 8512,24642, 8127,24896,
     X    8128, 1856, 7999,24767,24896,24769,24642,24513,24384,24511,
     X   24638, 8384,24898, 8000,24894, 8129,  958, 7999,24896, 8129,
     X   24642, 8127,24896, 7999,  958, 8002,24896, 8127,24638, 8129,
     X   24896, 8000,  575, 8382,24258,25026, 7998,  575, 7998,25026,
     X   24258, 8382,  831, 7999,25024, 8257,24257,25025, 7998,  831,
     X    7999,25024, 7873,25025,24257, 8382,  959, 8000,25024, 8257,
     X   24256, 8513,24509, 8257,  959, 7999,25024, 8257,24256, 8257/
      DATA          CHL023
     X  /25024, 7999, 1216, 7998,25152, 8257,24128, 8257,24769,24895,
     X   24769, 7999,  960, 7999,25152, 7745,24769,24895,24769, 7999,
     X     704, 7999,24769,24895,24769, 8000, 1985, 8383,24641,24769,
     X   24768, 7999,24513,24512,24511,24639,24767,24768,24769,24767,
     X   24768, 8002,  704, 7999,25152, 8000,24643, 8254,  704, 8000,
     X   24768,24765,24904, 7995, 1343, 8257,24769,24641,24513,24512,
     X   24511,24639,24767,24768, 8127, 1215, 7997,24767,24768,24769,
     X   24262,24769,24768,24767, 7997, 2111, 7997,24767,24768,24769/
      DATA          CHL024
     X  /24262,24769,24768,24767, 8253,24511,24512,24513,24769,24768,
     X   24767, 8000, 1727, 8383,24513,24512,24511,24639,24767,24768,
     X   24769,24643,24513,24512,24511, 8383,  704, 8254,24900,24128,
     X   24892, 8258,  573, 8125,24512,24646, 8125,  573, 7997,24768,
     X   24646, 7997,  573, 7997,24646,24768, 7997,  573, 8125,24646,
     X   24512, 8125, 1730, 8384,24767,24768,24769,24513,24512,24382,
     X   24512,24513,24769,24768,24767, 8384,  959, 7997,25024,24646,
     X   24256, 8253,25024, 8000,  832, 8003,24890,24902, 7869,24896/
      DATA          CHL025
     X  / 8128, 1215, 8000,25024, 8254,24384,24513,24642,24769,24896,
     X    7998, 1471, 7997,25030, 8255,24384,24511,24638,24767,24896,
     X    7874,25024, 8000,  959, 7998,24643,24769,24768,24767,24637,
     X    8002,  959, 8002,24637,24767,24768,24769,24643, 7998,  960,
     X    8510,24256,24513,24641,24769,25024, 7999,  960, 7998,25024,
     X   24769,24641,24513,24256, 8511, 1216, 7998,25152, 8257,24256,
     X   24513,24641,24769,25024, 7998, 1216, 8002,25024,24767,24639,
     X   24511,24256, 8255,25152, 8002,  830, 8126,24644,24767,24384/
      DATA          CHL026
     X  /24769, 8126,  830, 8130,24636,24769,24384,24767, 8130,  832,
     X    8000,24769,24638,24513,25152, 8000,  832, 8000,25152,24511,
     X   24642,24767, 8000, 1217, 8000,24767,24642,24511,25280,24513,
     X   24638,24769, 8000, 1344, 8125,24646, 8512,24254, 8383,24769,
     X   24768,24767,24637, 8003, 1087, 7997,24898, 8001,25026, 7872,
     X   24767,24892, 8003, 1344, 7999,24642,24769,24896,24767,24638,
     X   24511,24384,24513, 8513, 1726, 7998,24896, 8127,24642, 8128,
     X   24896,24642,24384,24638, 8259,24767,24769, 7998, 1342, 7999/
      DATA          CHL027
     X  /24896, 8127,24642, 8128,24896,24642,24384,24638, 8256, 1856,
     X    7999,24642,24769,24896,24767,24638,24511,24384,24513, 8257,
     X   25152, 8002,24636, 8258, 1344, 8256,24638,24384,24642,24896,
     X   24898, 8128,24768,24639, 7999, 1088, 8382,24644, 7872,24767,
     X   24639,24511,25152, 8001, 1215, 7998,24645, 8253,24769,24768,
     X   24767,24510,24767, 8003, 1470, 7998,24896,24642,24384,24638,
     X    8386,24642, 8127,24769,24767, 7999, 1472, 8126,24896, 8127,
     X   24645, 8000,24638,24767,24896,24769,24642, 7998, 1215, 8000/
      DATA          CHL028
     X  /24896,24769,24641,24513,24384,24634,25024, 8003, 1343, 8126,
     X   24896,24385,24642,24897,24384,24511,24638,24767, 8258, 1472,
     X    8256,24638,24384,24642,24896,24898, 8000,24510, 8383,24897,
     X    8000, 1216, 8126,24900, 8000,24892, 8385,24130, 8254,25154,
     X    7999, 2240, 8127,24512,24639,24768,24641,24513,24641,24769,
     X   24896,24767,24639,24511,24639,24768,24641,24512, 8129, 2240,
     X    8129,24512,24641,24768,24639,24511,24639,24767,24896,24769,
     X   24641,24513,24641,24768,24639,24512, 8127,  960, 8256,24638/
      DATA          CHL029
     X  /24384,24642,24896,24898, 7998,  831, 7998,25024,24643,24256,
     X   24637, 8386, 1473, 8255,24638,24384,24642,24896,24769,24896,
     X   24642,24384,24638, 8256, 1088, 8254,24643,24513,24511, 8512,
     X   24769,24767, 7999, 1470, 8000,24896,24638,24384,24642, 8258,
     X   24639,24767,24769,24641, 7998, 1216, 7998,25152, 7872,24644,
     X   24512,25152, 8128,24636, 8130, 1473, 7999,25280,24642,24384,
     X   24638, 8131,24638,24384,24642,25280, 7998, 1984, 8127,24512,
     X   24639,24768,24641,24513,24641,24769,24896,24767,24639,24511/
      DATA          CHL030
     X  /24639,24768, 8002, 1856, 8001,24769,24636, 8259,24769,24636,
     X    8259,24769,24635, 8260,24769,24637,24382, 8259, 1728, 7998,
     X   25152, 7745,24768,24513,24641,24769,24896,24767,24639,24511,
     X   24768, 8001, 1472, 8001,24769,24636, 8259,24769,24636, 8259,
     X   24769,24636,24769, 8001, 1088, 7998,25156, 8255,24641,24512,
     X    7998,24896, 8128, 1216, 8001,24769,24764,24642,24896,24642,
     X   24511,24637, 8130, 2240, 7998,24769,24639,24769,24639,24769,
     X   24639,24769, 7745,24769,24639,24769,24639,24769,24639,24769/
      DATA          CHL031
     X  / 7999,  960, 7998,25156, 8126,24384, 8130,25148, 8002,  704,
     X    8000,25152, 8002,24636, 8258,  704, 7998,25156, 7744,25148,
     X    8002,  832, 8000,24894,24898,24386,24382, 8512,  832, 7998,
     X   25152,24644,24128,24636, 8514, 1728, 8000,24768,24769, 8257,
     X   24639,24767, 8384,24512,24511, 8255,24641,24513, 8384, 1728,
     X    7998,24769,24642, 8129,24767,24896, 8385,24511,24638, 8383,
     X   24513,24384, 8385, 1728, 8000,25152, 8002,24636, 8384,24384,
     X    8129,24642, 8385,24896, 8383,24638, 8001, 1728, 7998,25156/
      DATA          CHL032
     X  / 7744,25148, 8257,24511, 8000,24513, 8258,24769, 8512,24767,
     X    7999, 2240, 8128,24512, 8258,24767, 8384,24641, 8512,24511,
     X    8255,24768, 8254,24513, 8128,24639, 8000,24769, 8385, 1344,
     X    7999,24642,24769,24896,24767,24638,24511,24384,24513, 8513,
     X     448, 7739,25408, 8005,  448, 7749,25408, 7995,  186, 7488,
     X     182, 6976,  189, 7872,  183, 7104,  188, 7744,  184, 7232,
     X     187, 7616,  185, 7360,  186, 7491,  186, 7485,  186, 7490,
     X     186, 7486,  186, 7489,  186, 7487, 1984, 7997,25152, 8257/
      DATA          CHL033
     X  /24128, 8257,25152, 8257,24128, 8257,25152, 8257,24128, 8257,
     X   25152, 7997/
C
      END
