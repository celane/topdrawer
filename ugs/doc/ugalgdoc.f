      PROGRAM       ALGDOC                                                 1.   
C                                                                          2.   
C *******************  THE UNIFIED GRAPHICS SYSTEM  *******************    3.   
C *                                                                   *    4.   
C *  THIS PROGRAM WILL GENERATE THE FIGURES FOR THE UNIFIED GRAPHICS  *    5.   
C *  SYSTEM ALGORITHMS MANUAL.                                        *    6.   
C *                                                                   *    7.   
C *                          ROBERT C. BEACH                          *    8.   
C *                    COMPUTATION RESEARCH GROUP                     *    9.   
C *                STANFORD LINEAR ACCELERATOR CENTER                 *   10.   
C *                                                                   *   11.   
C *********************************************************************   12.   
C                                                                         13.   
C  PICTURE MAGNIFICATION FACTOR.                                          14.   
      REAL          FACT                                                  15.   
      PARAMETER     (FACT=1.00)                                           16.   
C                                                                         17.   
C  COMMON BLOCK FOR THE GRAPHIC SEGMENT.                                  18.   
      SAVE          /FGGCBK/                                              19.   
C  NUMBER OF WORDS IN THE GRAPHIC SEGMENT.                                20.   
      INTEGER       NSEG                                                  21.   
      PARAMETER     (NSEG=1024)                                           22.   
C  THE DECLARATION OF THE COMMON BLOCK.                                   23.   
      COMMON        /FGGCBK/                                              24.   
     X              SEGM                                                  25.   
C  THE GRAPHIC SEGMENT.                                                   26.   
      INTEGER*4     SEGM(NSEG)                                            27.   
C                                                                         28.   
C  DECLARATIONS FOR FIGURE SHOWING POINT PROJECTION SCHEME.               29.   
      REAL          P221(2,2)                                             30.   
C                                                                         31.   
C  DECLARATIONS FOR FIGURE SHOWING CROSS-HATCHING EXAMPLE.                32.   
      EXTERNAL      SLN241,DLD241                                         33.   
      REAL          P241(2,2)                                             34.   
      REAL          U241(25),V241(25)                                     35.   
      SAVE          /FGCBKX/                                              36.   
      COMMON        /FGCBKX/                                              37.   
     X              N241,                                                 38.   
     X              X241,Y241                                             39.   
      INTEGER       N241                                                  40.   
      REAL          X241(250),Y241(250)                                   41.   
C                                                                         42.   
C  DECLARATIONS FOR FIGURE SHOWING SIMPLE GRAPHING EXAMPLE.               43.   
      EXTERNAL      DAX251,DTX251                                         44.   
      REAL          P251(2,2)                                             45.   
      REAL          V251(2,2),W251(2,2)                                   46.   
      REAL          X251(91),Y251(91)                                     47.   
C                                                                         48.   
C  DECLARATIONS FOR FIGURE SHOWING CONTOUR PLOT EXAMPLE.                  49.   
      EXTERNAL      DAX261,DTX261,DLN261                                  50.   
      REAL          P261(2,2)                                             51.   
      REAL          A261(20,30)                                           52.   
C                                                                         53.   
C  DECLARATIONS FOR FIGURE SHOWING MESH SURFACE EXAMPLE.                  54.   
      EXTERNAL      DLD271,DLN271                                         55.   
      REAL          P271(2,2)                                             56.   
      REAL          X271(11),Y271(11)                                     57.   
      INTEGER*4     B271                                                  58.   
      REAL          R271(3),V271(3),Z271(3)                               59.   
      REAL          M271(31)                                              60.   
      REAL          A271(32,52)                                           61.   
C                                                                         62.   
C  DECLARATIONS FOR FIGURE SHOWING TWO-DIMENSIONAL HISTOGRAM EXAMPLE.     63.   
      EXTERNAL      DLN281                                                64.   
      REAL          P281(2,2)                                             65.   
      REAL          R281(3),V281(3),Z281(3)                               66.   
      REAL          M281(31)                                              67.   
      REAL          A281(10,12)                                           68.   
      REAL          X281(100),Y281(100),S281(3),T281(2)                   69.   
      INTEGER*4     B281(4)                                               70.   
C                                                                         71.   
      REAL          WKSP(780)                                             72.   
      REAL          FLT1,FLT2,FLT3                                        73.   
      INTEGER       INT1,INT2                                             74.   
C                                                                         75.   
      DATA          P221/ 0.000, 0.120, 1.000, 0.870/                     76.   
      DATA          P241/ 0.000,-0.250, 1.000, 0.250/                     77.   
      DATA          U241/ 0.950, 0.890, 0.700, 0.575, 0.400,              78.   
     X                    0.225, 0.075, 0.150, 0.300, 0.450,              79.   
     X                    0.475, 0.375, 0.300, 0.375, 0.475,              80.   
     X                    0.450, 0.300, 0.150, 0.075, 0.225,              81.   
     X                    0.400, 0.575, 0.700, 0.890, 0.950/              82.   
      DATA          V241/ 0.000,-0.100, 0.000, 0.125, 0.200,              83.   
     X                    0.200, 0.090, 0.000,-0.050, 0.000,              84.   
     X                    0.075, 0.120, 0.000,-0.120,-0.075,              85.   
     X                    0.000, 0.050, 0.000,-0.090,-0.200,              86.   
     X                   -0.200,-0.125, 0.000, 0.100, 0.000/              87.   
      DATA          P251/ 0.000, 0.000,13.000,10.000/                     88.   
      DATA          V251/ 2.600, 1.500,11.700, 8.500/                     89.   
      DATA          X251/                                                 90.   
     X         3.08702, 3.08748, 3.08914, 3.08934, 3.08938, 3.08988,      91.   
     X         3.09008, 3.09020, 3.09090, 3.09106, 3.09120, 3.09134,      92.   
     X         3.09150, 3.09222, 3.09274, 3.09278, 3.09296, 3.09312,      93.   
     X         3.09380, 3.09384, 3.09396, 3.09414, 3.09416, 3.09418,      94.   
     X         3.09420, 3.09424, 3.09424, 3.09426, 3.09428, 3.09428,      95.   
     X         3.09440, 3.09448, 3.09448, 3.09450, 3.09460, 3.09464,      96.   
     X         3.09470, 3.09470, 3.09472, 3.09476, 3.09476, 3.09478,      97.   
     X         3.09482, 3.09484, 3.09484, 3.09490, 3.09492, 3.09494,      98.   
     X         3.09498, 3.09498, 3.09502, 3.09504, 3.09506, 3.09510,      99.   
     X         3.09510, 3.09510, 3.09512, 3.09512, 3.09514, 3.09520,     100.   
     X         3.09524, 3.09526, 3.09528, 3.09542, 3.09550, 3.09576,     101.   
     X         3.09576, 3.09580, 3.09584, 3.09592, 3.09606, 3.09606,     102.   
     X         3.09610, 3.09610, 3.09618, 3.09618, 3.09620, 3.09628,     103.   
     X         3.09662, 3.09678, 3.09700, 3.09702, 3.09750, 3.09778,     104.   
     X         3.09780, 3.09810, 3.09898, 3.10082, 3.10198, 3.11122,     105.   
     X         3.12954/                                                  106.   
      DATA          Y251/                                                107.   
     X           26.64,   23.88,   19.12,   28.29,   24.32,   18.36,     108.   
     X           18.29,   27.48,   34.18,   27.47,   29.80,   43.95,     109.   
     X           35.34,  126.90,  260.20,  276.67,  293.51,  279.88,     110.   
     X          761.47, 1121.26, 1124.99, 1401.76, 1237.14, 1787.48,     111.   
     X         2006.46, 2072.16, 2116.46, 1576.38, 1474.64, 2159.31,     112.   
     X         1426.50, 1755.02, 1715.47, 1864.18, 1648.21, 2309.35,     113.   
     X         2254.99, 2390.31, 2375.27, 1693.46, 2015.47, 2043.66,     114.   
     X         1798.43, 2560.76, 1808.28, 3163.26, 2376.27, 2329.92,     115.   
     X         2614.89, 2272.29, 2436.82, 2812.99, 1969.30, 2432.32,     116.   
     X         2301.46, 1933.90, 2769.29, 2881.40, 3344.97, 2053.62,     117.   
     X         1985.32, 2017.90, 2367.41, 2434.28, 2233.71, 1869.32,     118.   
     X         2018.33, 1538.77, 1591.03, 2067.37, 1409.14, 1762.93,     119.   
     X         1688.89, 1450.55, 1307.14, 1576.81, 1389.69, 1139.31,     120.   
     X         1190.66,  560.21,  796.24,  580.49,  476.03,  291.48,     121.   
     X          295.05,  331.34,  141.51,  102.40,  115.01,   56.67,     122.   
     X           39.80/                                                  123.   
      DATA          P261/-0.300,-0.200, 1.150, 1.000/                    124.   
      DATA          P271/ 0.000, 0.000, 1.000, 0.800/                    125.   
      DATA          X271/ 0.000, 1.000, 1.000, 0.000, 0.000, 0.050,      126.   
     X                    0.050, 0.950, 0.950, 0.050, 0.000/             127.   
      DATA          Y271/ 0.000, 0.000, 0.800, 0.800, 0.000, 0.050,      128.   
     X                    0.750, 0.750, 0.050, 0.050, 0.000/             129.   
      DATA          B271/Z'F0000000'/                                    130.   
      DATA          R271/ 50.00, 75.00, 40.00/                           131.   
      DATA          V271/-50.00,-75.00,-30.00/                           132.   
      DATA          Z271/  0.00,  0.00,  0.00/                           133.   
      DATA          P281/ 0.000, 0.000, 1.000, 0.800/                    134.   
      DATA          A281/                                                135.   
     X          0.0, 0.0, 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0,        136.   
     X          0.0, 0.5, 1.0, 0.5, 1.0, 2.0, 3.5, 2.0, 1.0,99.0,        137.   
     X          1.0, 1.0, 2.0, 1.0, 2.0, 3.0, 4.0, 3.0, 1.5,99.0,        138.   
     X          2.0, 3.0, 3.5, 3.0, 4.0, 4.5, 6.0, 4.5, 2.0,99.0,        139.   
     X          3.0, 2.0, 3.0, 3.5, 2.0, 3.0, 4.0, 3.0, 2.5,99.0,        140.   
     X          4.0, 2.5, 4.0, 3.0, 2.0, 2.5, 3.0, 2.5, 2.0,99.0,        141.   
     X          5.0, 4.0, 5.0, 5.0, 7.0, 4.0, 5.0, 6.0, 5.0,99.0,        142.   
     X          6.0, 4.5, 5.5, 9.0, 6.0, 4.5, 9.0, 7.0, 6.0,99.0,        143.   
     X          7.0, 4.0, 5.0, 6.0, 7.0, 5.0, 6.0, 6.5, 5.0,99.0,        144.   
     X          8.0, 3.0, 4.0, 5.0, 5.5, 4.0, 4.5, 4.0, 3.0,99.0,        145.   
     X          9.0, 2.0, 3.0, 4.0, 4.5, 4.0, 4.0, 3.0, 2.0,99.0,        146.   
     X         10.0,99.0,99.0,99.0,99.0,99.0,99.0,99.0,99.0,99.0/        147.   
      DATA          R281/-14.00, -9.00, 10.00/                           148.   
      DATA          V281/ 19.00, 13.00, -6.00/                           149.   
      DATA          Z281/  0.00,  0.00,  0.00/                           150.   
C                                                                        151.   
C  INITIALIZE THE PROGRAM.                                               152.   
C     CALL UGOPEN('IMGN300,GENIL',99)                                    153.   
      CALL UGOPEN('POSTSCR,GENIL',99)                                    153.   
C                                                                        154.   
C  PRODUCE FIGURE SHOWING POINT PROJECTION SCHEME.                       155.   
      CALL UGFONT('DUPLEX')                                              156.   
      CALL PCINIT(P221,16.5*FACT,1,SEGM,NSEG)                            157.   
C    DRAW THE SCREEN.                                                    158.   
      CALL UGLINE(' ',0.560,0.260,0,SEGM)                                159.   
      CALL UGLINE(' ',0.860,0.360,1,SEGM)                                160.   
      CALL UGLINE(' ',0.860,0.790,1,SEGM)                                161.   
      CALL UGLINE(' ',0.560,0.720,1,SEGM)                                162.   
      CALL UGLINE('VBRIGHT',0.560,0.720,0,SEGM)                          163.   
      CALL UGLINE('VBRIGHT',0.560,0.260,1,SEGM)                          164.   
C    DRAW THE SCREEN AXES.                                               165.   
      CALL DLNSEG('VDIM',0.560,0.490,0.860,0.580,0.860,0.730,SEGM)       166.   
      CALL DLNSEG('VBRIGHT',0.560,0.490,0.860,0.580,0.730,0.630,SEGM)    167.   
      CALL DARROW('VBRIGHT',0.620,0.508,0.860,0.580,SEGM)                168.   
      CALL UGXTXT('SIZE=0.02',0.640,0.490,'HDIR','    ',SEGM)            169.   
      CALL DLNSEG('VDIM',0.560,0.490,0.860,0.580,0.620,0.560,SEGM)       170.   
      CALL UGLINE(' ',0.730,0.317,0,SEGM)                                171.   
      CALL UGLINE(' ',0.730,0.541,1,SEGM)                                172.   
      CALL UGLINE('VBRIGHT',0.730,0.541,0,SEGM)                          173.   
      CALL UGLINE('VBRIGHT',0.730,0.680,1,SEGM)                          174.   
      CALL DARROW('VBRIGHT',0.730,0.680,0.730,0.317,SEGM)                175.   
      CALL UGXTXT('SIZE=0.02',0.745,0.690,'UDIR','    ',SEGM)            176.   
      CALL UGLINE(' ',0.730,0.680,0,SEGM)                                177.   
      CALL UGLINE(' ',0.730,0.760,1,SEGM)                                178.   
C    DRAW THE SCREEN LABELS.                                             179.   
      CALL DLNSEG('VDIM',0.560,0.720,0.860,0.790,0.870,0.900,SEGM)       180.   
      CALL UGXTXT('SIZE=0.02',0.915,0.800,'YHI','   ',SEGM)              181.   
      CALL DLNSEG('VDIM',0.560,0.260,0.860,0.360,0.870,0.900,SEGM)       182.   
      CALL UGXTXT('SIZE=0.02',0.915,0.370,'YLO','   ',SEGM)              183.   
      CALL UGLINE('VDIM',0.860,0.350,0,SEGM)                             184.   
      CALL UGLINE('VDIM',0.860,0.310,1,SEGM)                             185.   
      CALL UGXTXT('SIZE=0.02',0.840,0.290,'XLO','   ',SEGM)              186.   
      CALL DARROW('VDIM',0.860,0.335,0.560,0.235,SEGM)                   187.   
      CALL DLNSEG('VDIM',0.860,0.335,0.560,0.235,0.860,0.780,SEGM)       188.   
      CALL UGXTXT('SIZE=0.02',0.715,0.290,'SCRZ','    ',SEGM)            189.   
      CALL DLNSEG('VDIM',0.860,0.335,0.560,0.235,0.700,0.560,SEGM)       190.   
      CALL DARROW('VDIM',0.560,0.235,0.860,0.335,SEGM)                   191.   
      CALL UGLINE('VDIM',0.560,0.250,0,SEGM)                             192.   
      CALL UGLINE('VDIM',0.560,0.210,1,SEGM)                             193.   
      CALL UGXTXT('SIZE=0.02',0.545,0.190,'XHI','   ',SEGM)              194.   
      CALL UGLINE('VDIM',0.680,0.300,0,SEGM)                             195.   
      CALL UGLINE('VDIM',0.680,0.350,1,SEGM)                             196.   
      CALL UGXTXT('SIZE=0.02',0.680,0.370,'U','L',SEGM)                  197.   
      CALL UGLINE('VDIM',0.680,0.380,0,SEGM)                             198.   
      CALL UGLINE('VDIM',0.680,0.420,1,SEGM)                             199.   
      CALL DPOINT(0.680,0.420,SEGM)                                      200.   
      CALL UGXTXT('SIZE=0.02',0.680,0.440,'Q',' ',SEGM)                  201.   
      CALL DLNSEG('VDIM',0.680,0.420,0.860,0.475,0.680,0.770,SEGM)       202.   
      CALL UGXTXT('SIZE=0.02',0.785,0.450,'T','L',SEGM)                  203.   
      CALL DLNSEG('VDIM',0.680,0.420,0.860,0.475,0.790,0.860,SEGM)       204.   
C    DRAW THE PROJECTION LINE.                                           205.   
      CALL DLNSEG('VDIM',0.090,0.660,0.680,0.420,0.680,0.670,SEGM)       206.   
      CALL DLNSEG('VDIM',0.090,0.660,0.680,0.420,0.660,0.650,SEGM)       207.   
      CALL DLNSEG('VDIM',0.090,0.660,0.680,0.420,0.640,0.630,SEGM)       208.   
      CALL DLNSEG('VDIM',0.090,0.660,0.680,0.420,0.620,0.610,SEGM)       209.   
      CALL DLNSEG('VDIM',0.090,0.660,0.680,0.420,0.600,0.590,SEGM)       210.   
      CALL DLNSEG('VDIM',0.090,0.660,0.680,0.420,0.580,0.570,SEGM)       211.   
      CALL DLNSEG('VDIM',0.090,0.660,0.680,0.420,0.560,0.360,SEGM)       212.   
      CALL DPOINT(0.350,0.554,SEGM)                                      213.   
      CALL UGXTXT('SIZE=0.02',0.340,0.530,'P',' ',SEGM)                  214.   
      CALL DLNSEG('VDIM',0.090,0.660,0.680,0.420,0.340,0.090,SEGM)       215.   
      CALL DPOINT(0.090,0.660,SEGM)                                      216.   
C    DRAW THE EYE COMPLEX.                                               217.   
      CALL DLNSEG('VDIM',0.090,0.660,0.160,0.680,0.090,0.160,SEGM)       218.   
      CALL DPOINT(0.160,0.680,SEGM)                                      219.   
      CALL DLNSEG('VDIM',0.090,0.660,0.160,0.680,0.170,0.180,SEGM)       220.   
      CALL DLNSEG('VDIM',0.090,0.660,0.160,0.680,0.190,0.200,SEGM)       221.   
      CALL DLNSEG('VDIM',0.090,0.660,0.160,0.680,0.210,0.220,SEGM)       222.   
      CALL DLNSEG('VDIM',0.090,0.660,0.160,0.680,0.230,0.240,SEGM)       223.   
      CALL UGXTXT('SIZE=0.02',0.235,0.750,'EYED','    ',SEGM)            224.   
      CALL DLNSEG('VDIM',0.090,0.710,0.160,0.730,0.160,0.220,SEGM)       225.   
      CALL DARROW('VDIM',0.160,0.730,0.230,0.750,SEGM)                   226.   
      CALL UGLINE('VDIM',0.160,0.750,0,SEGM)                             227.   
      CALL UGLINE('VDIM',0.160,0.690,1,SEGM)                             228.   
      CALL UGLINE('VDIM',0.090,0.670,0,SEGM)                             229.   
      CALL UGLINE('VDIM',0.090,0.730,1,SEGM)                             230.   
      CALL DARROW('VDIM',0.090,0.710,0.020,0.690,SEGM)                   231.   
      CALL DLNSEG('VDIM',0.090,0.710,0.160,0.730,0.090,0.050,SEGM)       232.   
      CALL UGXTXT('SIZE=0.02',0.075,0.635,'E',' ',SEGM)                  233.   
      CALL UGXTXT('SIZE=0.02',0.090,0.570,'REFP','    ',SEGM)            234.   
      CALL UGLINE('VDIM',0.115,0.585,0,SEGM)                             235.   
      CALL UGLINE('VDIM',0.155,0.670,1,SEGM)                             236.   
      CALL DARROW('VDIM',0.155,0.670,0.115,0.585,SEGM)                   237.   
C    DRAW THE VDIR LINE.                                                 238.   
      CALL DLNSEG('VBRIGHT',0.160,0.680,0.730,0.541,0.160,0.280,SEGM)    239.   
      CALL DARROW('VBRIGHT',0.290,0.648,0.160,0.680,SEGM)                240.   
      CALL UGXTXT('SIZE=0.02',0.250,0.620,'VDIR','    ',SEGM)            241.   
      CALL DLNSEG('VDIM',0.160,0.680,0.730,0.541,0.280,0.560,SEGM)       242.   
      CALL DLNSEG('VDIM',0.160,0.680,0.730,0.541,0.570,0.580,SEGM)       243.   
      CALL DLNSEG('VDIM',0.160,0.680,0.730,0.541,0.590,0.600,SEGM)       244.   
      CALL DLNSEG('VDIM',0.160,0.680,0.730,0.541,0.610,0.620,SEGM)       245.   
      CALL DLNSEG('VDIM',0.160,0.680,0.730,0.541,0.630,0.640,SEGM)       246.   
      CALL DLNSEG('VDIM',0.160,0.680,0.730,0.541,0.650,0.660,SEGM)       247.   
      CALL DLNSEG('VDIM',0.160,0.680,0.730,0.541,0.670,0.680,SEGM)       248.   
      CALL DLNSEG('VDIM',0.160,0.680,0.730,0.541,0.690,0.700,SEGM)       249.   
      CALL DLNSEG('VDIM',0.160,0.680,0.730,0.541,0.710,0.720,SEGM)       250.   
C    DRAW THE SCRD LINE.                                                 251.   
      CALL DARROW('VDIM',0.778,0.555,0.230,0.700,SEGM)                   252.   
      CALL DLNSEG('VDIM',0.780,0.555,0.230,0.700,0.778,0.750,SEGM)       253.   
      CALL DLNSEG('VDIM',0.780,0.555,0.230,0.700,0.740,0.730,SEGM)       254.   
      CALL DLNSEG('VDIM',0.780,0.555,0.230,0.700,0.720,0.710,SEGM)       255.   
      CALL DLNSEG('VDIM',0.780,0.555,0.230,0.700,0.700,0.690,SEGM)       256.   
      CALL DLNSEG('VDIM',0.780,0.555,0.230,0.700,0.680,0.670,SEGM)       257.   
      CALL DLNSEG('VDIM',0.780,0.555,0.230,0.700,0.660,0.650,SEGM)       258.   
      CALL DLNSEG('VDIM',0.780,0.555,0.230,0.700,0.640,0.630,SEGM)       259.   
      CALL DLNSEG('VDIM',0.780,0.555,0.230,0.700,0.620,0.610,SEGM)       260.   
      CALL DLNSEG('VDIM',0.780,0.555,0.230,0.700,0.600,0.590,SEGM)       261.   
      CALL DLNSEG('VDIM',0.780,0.555,0.230,0.700,0.580,0.570,SEGM)       262.   
      CALL DLNSEG('VDIM',0.780,0.555,0.230,0.700,0.560,0.460,SEGM)       263.   
      CALL UGXTXT('SIZE=0.02',0.395,0.650,'SCRD','    ',SEGM)            264.   
      CALL DLNSEG('VDIM',0.780,0.555,0.230,0.700,0.380,0.230,SEGM)       265.   
      CALL DARROW('VDIM',0.230,0.700,0.780,0.555,SEGM)                   266.   
C    DRAW X-Y-Z TRIHEDRAL.                                               267.   
      CALL UGLINE('VBRIGHT',0.280,0.270,0,SEGM)                          268.   
      CALL UGLINE('VBRIGHT',0.150,0.200,1,SEGM)                          269.   
      CALL DARROW('VBRIGHT',0.150,0.200,0.280,0.270,SEGM)                270.   
      CALL UGXTXT('SIZE=0.02',0.130,0.200,'X',' ',SEGM)                  271.   
      CALL UGLINE('VBRIGHT',0.280,0.270,0,SEGM)                          272.   
      CALL UGLINE('VBRIGHT',0.450,0.270,1,SEGM)                          273.   
      CALL DARROW('VBRIGHT',0.450,0.270,0.280,0.270,SEGM)                274.   
      CALL UGXTXT('SIZE=0.02',0.450,0.290,'Y',' ',SEGM)                  275.   
      CALL UGLINE('VBRIGHT',0.280,0.270,0,SEGM)                          276.   
      CALL UGLINE('VBRIGHT',0.280,0.460,1,SEGM)                          277.   
      CALL DARROW('VBRIGHT',0.280,0.460,0.280,0.270,SEGM)                278.   
      CALL UGXTXT('SIZE=0.02',0.260,0.460,'Z',' ',SEGM)                  279.   
      CALL UGWRIT(' ',0,SEGM)                                            280.   
C                                                                        281.   
C  PRODUCE FIGURE SHOWING CROSS-HATCHING EXAMPLE.                        282.   
      CALL PCINIT(P241,16.5*FACT,1,SEGM,NSEG)                            283.   
C    GENERATE THE OUTLINE.                                               284.   
      CALL UGSCIN(' ',SLN241,U241,V241,25,1.05,1,                        285.   
     X  1,U241(24),V241(24),1,U241(2),V241(2))                           286.   
C    DRAW THE OUTLINE.                                                   287.   
      DO 101 INT1=1,24                                                   288.   
        CALL DPOINT(U241(INT1),V241(INT1),SEGM)                          289.   
  101 CONTINUE                                                           290.   
      CALL UGPLIN(' ',X241,Y241,N241,1,1,SEGM)                           291.   
C    DRAW THE CROSS-HATCHING.                                            292.   
      CALL UGXHCH('SPACING=0.0125',DLD241,X241,Y241,N241,WKSP,780)       293.   
      CALL UGWRIT(' ',0,SEGM)                                            294.   
C                                                                        295.   
C  PRODUCE FIGURE SHOWING SIMPLE GRAPHING EXAMPLE.                       296.   
      CALL UGFONT('DUPLEX')                                              297.   
      CALL PCINIT(P251,16.5*FACT,1,SEGM,NSEG)                            298.   
C    DRAW THE TITLES.                                                    299.   
      CALL UGXTXT('CENTER,SIZE=0.4',6.5,9.25,                            300.   
     X  'THE DISCOVERY OF Y(3095)',                                      301.   
     X  ' LL  LLLLLLLL LL G      ',SEGM)                                 302.   
      CALL UGXTXT('CENTER,SIZE=0.3',                                     303.   
     X  0.5*(V251(1,1)+V251(1,2)),0.8,                                   304.   
     X  'ENERGY (GEV)',                                                  305.   
     X  ' LLLLL   L  ',SEGM)                                             306.   
      CALL UGXTXT('CENTER,SIZE=0.3,ANGLE=90',                            307.   
     X  1.0,0.5*(V251(2,1)+V251(2,2)),                                   308.   
     X  'CROSS SECTION (NB)',                                            309.   
     X  ' LLLL  LLLLLL  LL ',SEGM)                                       310.   
C    DRAW THE AXES.                                                      311.   
      CALL UGLNAX('RSTM=0',DAX251,DTX251,1,                              312.   
     X  V251(1,1),V251(2,1),V251(1,2),V251(2,1),                         313.   
     X  3.085,3.130,10)                                                  314.   
      CALL UGLNAX('LSTM=0',DAX251,DTX251,0,                              315.   
     X  V251(1,1),V251(2,2),V251(1,2),V251(2,2),                         316.   
     X  3.085,3.130,10)                                                  317.   
      CALL UGLGAX('LSTM=0,NSTM=4',DAX251,DTX251,2,                       318.   
     X  V251(1,1),V251(2,1),V251(1,1),V251(2,2),                         319.   
     X  10.0,10000.0,4)                                                  320.   
      CALL UGLGAX('RSTM=0,NSTM=4',DAX251,DTX251,0,                       321.   
     X  V251(1,2),V251(2,1),V251(1,2),V251(2,2),                         322.   
     X  10.0,10000.0,4)                                                  323.   
      CALL UGWRIT(' ',0,SEGM)                                            324.   
C    DRAW THE CURVE.                                                     325.   
      W251(1,1)=3.085                                                    326.   
      W251(1,2)=3.130                                                    327.   
      W251(2,1)=LOG10(10.0)                                              328.   
      W251(2,2)=LOG10(10000.0)                                           329.   
      CALL UGWDOW('PUT,WINDOW',V251,W251)                                330.   
      CALL UGINIT('CLEAR',SEGM,NSEG)                                     331.   
      DO 201 INT1=1,91                                                   332.   
        CALL UGLINE(' ',X251(INT1),LOG10(Y251(INT1)),1,                  333.   
     X    SEGM)                                                          334.   
  201 CONTINUE                                                           335.   
      CALL UGWRIT(' ',0,SEGM)                                            336.   
C                                                                        337.   
C  PRODUCE FIGURE SHOWING CONTOUR PLOT EXAMPLE.                          338.   
      CALL UGFONT('DUPLEX')                                              339.   
      CALL PCINIT(P261,16.5*FACT,1,SEGM,NSEG)                            340.   
C    GENERATE THE SURFACE.                                               341.   
      DO 302 INT1=2,30                                                   342.   
        FLT1=(REAL(INT1)-2.00)/(30.00-2.00)                              343.   
        A261(1,INT1)=FLT1                                                344.   
        DO 301 INT2=2,20                                                 345.   
          FLT2=(REAL(INT2)-2.00)/(26.00-2.00)                            346.   
          A261(INT2,1)=FLT2                                              347.   
          A261(INT2,INT1)=7.0*FLT1*FLT2-3.0                              348.   
     X      -2.0*EXP(-15.0*((FLT1-0.50)**2+(FLT2-0.50)**2))              349.   
     X      +5.0*EXP(-5.0*(FLT1**2+(FLT2-0.25)**2))                      350.   
  301   CONTINUE                                                         351.   
  302 CONTINUE                                                           352.   
C    DRAW THE TITLE.                                                     353.   
      CALL UGXTXT('CENTER,SIZE=0.05',0.45,0.90,                          354.   
     X  'CONTOUR PLOT EXAMPLE',                                          355.   
     X  ' LLLLLL  LLL  LLLLLL',SEGM)                                     356.   
C    DRAW THE CONTOUR BOX OUTLINE.                                       357.   
      CALL UGLINE(' ',0.00,0.00,0,SEGM)                                  358.   
      CALL UGLINE(' ',1.00,0.00,1,SEGM)                                  359.   
      CALL UGLINE(' ',1.00,0.75,1,SEGM)                                  360.   
      CALL UGLINE(' ',0.00,0.75,1,SEGM)                                  361.   
      CALL UGLINE(' ',0.00,0.00,1,SEGM)                                  362.   
C    DRAW THE AXES.                                                      363.   
      CALL UGLNAX('NSTM=3',DAX261,DTX261,1,                              364.   
     X  0.00,-0.10,1.00,-0.10,0.00,1.00,6)                               365.   
      CALL UGLNAX('NSTM=4',DAX261,DTX261,0,                              366.   
     X  -0.15,0.00,-0.15,0.75,0.00,0.75,4)                               367.   
C    DRAW THE CONTOUR.                                                   368.   
      CALL UGCNTR('NSCL=3',DLN261,DTX261,                                369.   
     X  A261,20,30,-3.0,3.0,7,WKSP,780)                                  370.   
      CALL UGWRIT(' ',0,SEGM)                                            371.   
C                                                                        372.   
C  PRODUCE FIGURE SHOWING MESH SURFACE EXAMPLE.                          373.   
      CALL UGFONT('DUPLEX')                                              374.   
      CALL PCINIT(P271,16.5*FACT,0,SEGM,NSEG)                            375.   
C    GENERATE THE SURFACE.                                               376.   
      DO 402 INT1=2,52                                                   377.   
        FLT1=INT1-27                                                     378.   
        A271(1,INT1)=FLT1                                                379.   
        DO 401 INT2=2,32                                                 380.   
          FLT2=INT2-17                                                   381.   
          A271(INT2,1)=FLT2                                              382.   
          FLT3=SQRT(FLT1**2+FLT2**2)                                     383.   
          A271(INT2,INT1)=                                               384.   
     X      ((750.0/(FLT3**2+75.0))+5.0)*COS(0.4*FLT3)                   385.   
  401   CONTINUE                                                         386.   
  402 CONTINUE                                                           387.   
      CALL UGTRAN(' ',R271,V271,Z271,Z271,100.0,75.0,M271)               388.   
C    DRAW THE BORDER.                                                    389.   
      CALL UGPLIN('VBRIGHT',X271,Y271,10,B271,-5,SEGM)                   390.   
      CALL UGXHCH(' ',DLD271,X271,Y271,11,WKSP,780)                      391.   
C    DRAW THE TITLE.                                                     392.   
      CALL UGXTXT('CENTER,SIZE=0.04',0.5,0.65,                           393.   
     X  'MESH SURFACE EXAMPLE',                                          394.   
     X  ' LLL  LLLLLL  LLLLLL',SEGM)                                     395.   
C    DRAW THE SURFACE.                                                   396.   
      CALL UGMESH('UPPER',DLN271,A271,32,52,                             397.   
     X  M271,WKSP,780)                                                   398.   
      CALL UGMESH('LOWER,NOCOMN',DLD271,A271,32,52,                      399.   
     X  M271,WKSP,780)                                                   400.   
      CALL UGWRIT(' ',0,SEGM)                                            401.   
C                                                                        402.   
C  PRODUCE FIGURE SHOWING TWO-DIMENSIONAL HISTOGRAM EXAMPLE.             403.   
      CALL UGFONT('DUPLEX')                                              404.   
      CALL PCINIT(P281,16.5*FACT,1,SEGM,NSEG)                            405.   
      CALL UGTRAN('PARALLEL,XLO=0.2,YHI=0.8',                            406.   
     X  R281,V281,Z281,Z281,25.0,15.0,M281)                              407.   
C    DRAW THE TITLES.                                                    408.   
      CALL UGXTXT('CENTER,SIZE=0.035',0.20,0.70,                         409.   
     X  'TWO',' LL',SEGM)                                                410.   
      CALL UGXTXT('CENTER,SIZE=0.035',0.20,0.60,                         411.   
     X  'DIMENSIONAL',' LLLLLLLLLL',SEGM)                                412.   
      CALL UGXTXT('CENTER,SIZE=0.035',0.20,0.50,                         413.   
     X  'HISTOGRAM',' LLLLLLLL',SEGM)                                    414.   
      CALL UGXTXT('CENTER,SIZE=0.035',0.20,0.40,                         415.   
     X  'EXAMPLE',' LLLLLL',SEGM)                                        416.   
C    DRAW THE AXES LABELS.                                               417.   
      CALL UGCTOL('SIZE=0.5,CENTER',5.0,-1.0,                            418.   
     X  'X-AXIS','   LLL',100,X281,Y281,INT1,B281)                       419.   
      DO 501 INT2=1,INT1                                                 420.   
        S281(1)=X281(INT2)                                               421.   
        S281(2)=0.0                                                      422.   
        S281(3)=Y281(INT2)                                               423.   
        CALL UGPROJ(M281,S281,T281)                                      424.   
        X281(INT2)=T281(1)                                               425.   
        Y281(INT2)=T281(2)                                               426.   
  501 CONTINUE                                                           427.   
      CALL UGPLIN(' ',X281,Y281,INT1,B281,-INT1,SEGM)                    428.   
      CALL UGCTOL('SIZE=0.5,CENTER',4.0,-1.0,                            429.   
     X  'Y-AXIS','   LLL',100,X281,Y281,INT1,B281)                       430.   
      DO 502 INT2=1,INT1                                                 431.   
        S281(1)=0.0                                                      432.   
        S281(2)=8.0-X281(INT2)                                           433.   
        S281(3)=Y281(INT2)                                               434.   
        CALL UGPROJ(M281,S281,T281)                                      435.   
        X281(INT2)=T281(1)                                               436.   
        Y281(INT2)=T281(2)                                               437.   
  502 CONTINUE                                                           438.   
      CALL UGPLIN(' ',X281,Y281,INT1,B281,-INT1,SEGM)                    439.   
C    DRAW THE HISTOGRAM.                                                 440.   
      CALL UG2DHG(' ',DLN281,A281,10,12,M281,WKSP,780)                   441.   
      CALL UGWRIT(' ',0,SEGM)                                            442.   
C                                                                        443.   
C  TERMINATE THE PROGRAM.                                                444.   
      CALL UGCLOS(' ')                                                   445.   
      STOP                                                               446.   
C                                                                        447.   
      END                                                                448.   
      SUBROUTINE SLN241(XCRD,YCRD,BBIT)                                  449.   
C                                                                        450.   
C *********************************************************************  451.   
C *                                                                   *  452.   
C *  SUBROUTINE TO SAVE THE ACTUAL POINTS IN THE CROSS-HATCHING       *  453.   
C *  EXAMPLE.                                                         *  454.   
C *                                                                   *  455.   
C *  THE CALLING SEQUENCE IS:                                         *  456.   
C *    CALL SLN241(XCRD,YCRD,BBIT)                                    *  457.   
C *                                                                   *  458.   
C *  THE PARAMETERS IN THE CALLING SEQUENCE ARE:                      *  459.   
C *    XCRD  THE X COORDINATE OF THE LINE END POINT.                  *  460.   
C *    YCRD  THE Y COORDINATE OF THE LINE END POINT.                  *  461.   
C *    BBIT  THE BLANKING BIT (0 MEANS BEGINNING OF LINE AND 1 MEANS  *  462.   
C *          END OF LINE).                                            *  463.   
C *                                                                   *  464.   
C *********************************************************************  465.   
C                                                                        466.   
      REAL          XCRD,YCRD                                            467.   
      INTEGER       BBIT                                                 468.   
C                                                                        469.   
      SAVE          /FGCBKX/                                             470.   
      COMMON        /FGCBKX/                                             471.   
     X              N241,                                                472.   
     X              X241,Y241                                            473.   
      INTEGER       N241                                                 474.   
      REAL          X241(250),Y241(250)                                  475.   
C                                                                        476.   
      IF (BBIT.EQ.0) THEN                                                477.   
        N241=1                                                           478.   
        X241(1)=XCRD                                                     479.   
        Y241(1)=YCRD                                                     480.   
      ELSE                                                               481.   
        IF (N241.GE.250) STOP 9999                                       482.   
        N241=N241+1                                                      483.   
        X241(N241)=XCRD                                                  484.   
        Y241(N241)=YCRD                                                  485.   
      END IF                                                             486.   
      RETURN                                                             487.   
C                                                                        488.   
      END                                                                489.   
      SUBROUTINE DLD241(XCRD,YCRD,BBIT)                                  490.   
C                                                                        491.   
C *********************************************************************  492.   
C *                                                                   *  493.   
C *  SUBROUTINE TO DRAW LINES IN THE DIM MODE FOR THE CROSS-HATCHING  *  494.   
C *  EXAMPLE.                                                         *  495.   
C *                                                                   *  496.   
C *  THE CALLING SEQUENCE IS:                                         *  497.   
C *    CALL DLD241(XCRD,YCRD,BBIT)                                    *  498.   
C *                                                                   *  499.   
C *  THE PARAMETERS IN THE CALLING SEQUENCE ARE:                      *  500.   
C *    XCRD  THE X COORDINATE OF THE LINE END POINT.                  *  501.   
C *    YCRD  THE Y COORDINATE OF THE LINE END POINT.                  *  502.   
C *    BBIT  THE BLANKING BIT (0 MEANS BLANK AND 1 MEANS DRAW).       *  503.   
C *                                                                   *  504.   
C *********************************************************************  505.   
C                                                                        506.   
      REAL          XCRD,YCRD                                            507.   
      INTEGER       BBIT                                                 508.   
C                                                                        509.   
C  COMMON BLOCK FOR THE GRAPHIC SEGMENT.                                 510.   
      SAVE          /FGGCBK/                                             511.   
C  NUMBER OF WORDS IN THE GRAPHIC SEGMENT.                               512.   
      INTEGER       NSEG                                                 513.   
      PARAMETER     (NSEG=1024)                                          514.   
C  THE DECLARATION OF THE COMMON BLOCK.                                  515.   
      COMMON        /FGGCBK/                                             516.   
     X              SEGM                                                 517.   
C  THE GRAPHIC SEGMENT.                                                  518.   
      INTEGER*4     SEGM(NSEG)                                           519.   
C                                                                        520.   
      CALL UGLINE('VDIM',XCRD,YCRD,BBIT,SEGM)                            521.   
      RETURN                                                             522.   
C                                                                        523.   
      END                                                                524.   
      SUBROUTINE DAX251(XCRD,YCRD,BBIT)                                  525.   
C                                                                        526.   
C *********************************************************************  527.   
C *                                                                   *  528.   
C *  SUBROUTINE TO DRAW THE AXES IN THE FIGURE ILLUSTRATING SIMPLE    *  529.   
C *  GRAPH PLOTTING.                                                  *  530.   
C *                                                                   *  531.   
C *  THE CALLING SEQUENCE IS:                                         *  532.   
C *    CALL DAX251(XCRD,YCRD,BBIT)                                    *  533.   
C *                                                                   *  534.   
C *  THE PARAMETERS IN THE CALLING SEQUENCE ARE:                      *  535.   
C *    XCRD  THE X COORDINATE OF THE LINE END POINT.                  *  536.   
C *    YCRD  THE Y COORDINATE OF THE LINE END POINT.                  *  537.   
C *    BBIT  THE BLANKING BIT (0 OR 2 MEANS BLANK AND 1 OR 3 MEANS    *  538.   
C *          DRAW).                                                   *  539.   
C *                                                                   *  540.   
C *********************************************************************  541.   
C                                                                        542.   
      REAL          XCRD,YCRD                                            543.   
      INTEGER       BBIT                                                 544.   
C                                                                        545.   
C  COMMON BLOCK FOR THE GRAPHIC SEGMENT.                                 546.   
      SAVE          /FGGCBK/                                             547.   
C  NUMBER OF WORDS IN THE GRAPHIC SEGMENT.                               548.   
      INTEGER       NSEG                                                 549.   
      PARAMETER     (NSEG=1024)                                          550.   
C  THE DECLARATION OF THE COMMON BLOCK.                                  551.   
      COMMON        /FGGCBK/                                             552.   
     X              SEGM                                                 553.   
C  THE GRAPHIC SEGMENT.                                                  554.   
      INTEGER*4     SEGM(NSEG)                                           555.   
C                                                                        556.   
      IF (BBIT.LE.1) THEN                                                557.   
        CALL UGLINE(' ',XCRD,YCRD,BBIT,SEGM)                             558.   
      ELSE                                                               559.   
        CALL UGLINE('VDIM',XCRD,YCRD,BBIT-2,SEGM)                        560.   
      END IF                                                             561.   
      RETURN                                                             562.   
C                                                                        563.   
      END                                                                564.   
      SUBROUTINE DTX251(XCRD,YCRD,VALU,FLAG)                             565.   
C                                                                        566.   
C *********************************************************************  567.   
C *                                                                   *  568.   
C *  SUBROUTINE TO DRAW THE LABELS ON THE AXES IN THE FIGURE          *  569.   
C *  ILLUSTRATING SIMPLE GRAPH PLOTTING.                              *  570.   
C *                                                                   *  571.   
C *  THE CALLING SEQUENCE IS:                                         *  572.   
C *    CALL DTX251(XCRD,YCRD,VALU,FLAG)                               *  573.   
C *                                                                   *  574.   
C *  THE PARAMETERS IN THE CALLING SEQUENCE ARE:                      *  575.   
C *    XCRD  THE X COORDINATE OF THE LABEL.                           *  576.   
C *    YCRD  THE Y COORDINATE OF THE LABEL.                           *  577.   
C *    VALU  THE VALUE OF THE LABEL.                                  *  578.   
C *    FLAG  THE POSITION FLAG (1 MEANS HORIZONTAL AXIS AND 2 MEANS   *  579.   
C *          VERTICAL AXIS).                                          *  580.   
C *                                                                   *  581.   
C *********************************************************************  582.   
C                                                                        583.   
      REAL          XCRD,YCRD,VALU                                       584.   
      INTEGER       FLAG                                                 585.   
C                                                                        586.   
C  COMMON BLOCK FOR THE GRAPHIC SEGMENT.                                 587.   
      SAVE          /FGGCBK/                                             588.   
C  NUMBER OF WORDS IN THE GRAPHIC SEGMENT.                               589.   
      INTEGER       NSEG                                                 590.   
      PARAMETER     (NSEG=1024)                                          591.   
C  THE DECLARATION OF THE COMMON BLOCK.                                  592.   
      COMMON        /FGGCBK/                                             593.   
     X              SEGM                                                 594.   
C  THE GRAPHIC SEGMENT.                                                  595.   
      INTEGER*4     SEGM(NSEG)                                           596.   
C                                                                        597.   
      CHARACTER*10  STRG                                                 598.   
      INTEGER       LENG                                                 599.   
C                                                                        600.   
      IF (FLAG.EQ.1) THEN                                                601.   
        CALL UGCNVF(VALU,3,STRG,LENG)                                    602.   
        CALL UGTEXT('SIZE=0.15,CENTER',XCRD,YCRD-0.2,                    603.   
     X    STRG(11-LENG:10),SEGM)                                         604.   
      ELSE IF (FLAG.EQ.2) THEN                                           605.   
        CALL UGCNVF(VALU,0,STRG,LENG)                                    606.   
        CALL UGTEXT('SIZE=0.15,RIGHT',XCRD-0.2,YCRD,                     607.   
     X    STRG(11-LENG:10),SEGM)                                         608.   
      END IF                                                             609.   
      RETURN                                                             610.   
C                                                                        611.   
      END                                                                612.   
      SUBROUTINE DAX261(XCRD,YCRD,BBIT)                                  613.   
C                                                                        614.   
C *********************************************************************  615.   
C *                                                                   *  616.   
C *  SUBROUTINE TO DRAW THE AXES IN THE CONTOUR PLOT EXAMPLE.         *  617.   
C *                                                                   *  618.   
C *  THE CALLING SEQUENCE IS:                                         *  619.   
C *    CALL DAX261(XCRD,YCRD,BBIT)                                    *  620.   
C *                                                                   *  621.   
C *  THE PARAMETERS IN THE CALLING SEQUENCE ARE:                      *  622.   
C *    XCRD  THE X COORDINATE OF THE LINE END POINT.                  *  623.   
C *    YCRD  THE Y COORDINATE OF THE LINE END POINT.                  *  624.   
C *    BBIT  THE BLANKING BIT (0 OR 2 MEANS BLANK AND 1 OR 3 MEANS    *  625.   
C *          DRAW).                                                   *  626.   
C *                                                                   *  627.   
C *********************************************************************  628.   
C                                                                        629.   
      REAL          XCRD,YCRD                                            630.   
      INTEGER       BBIT                                                 631.   
C                                                                        632.   
C  COMMON BLOCK FOR THE GRAPHIC SEGMENT.                                 633.   
      SAVE          /FGGCBK/                                             634.   
C  NUMBER OF WORDS IN THE GRAPHIC SEGMENT.                               635.   
      INTEGER       NSEG                                                 636.   
      PARAMETER     (NSEG=1024)                                          637.   
C  THE DECLARATION OF THE COMMON BLOCK.                                  638.   
      COMMON        /FGGCBK/                                             639.   
     X              SEGM                                                 640.   
C  THE GRAPHIC SEGMENT.                                                  641.   
      INTEGER*4     SEGM(NSEG)                                           642.   
C                                                                        643.   
      IF (BBIT.LE.1) THEN                                                644.   
        CALL UGLINE(' ',XCRD,YCRD,BBIT,SEGM)                             645.   
      ELSE                                                               646.   
        CALL UGLINE('VDIM',XCRD,YCRD,BBIT-2,SEGM)                        647.   
      END IF                                                             648.   
      RETURN                                                             649.   
C                                                                        650.   
      END                                                                651.   
      SUBROUTINE DTX261(XCRD,YCRD,VALU,FLAG)                             652.   
C                                                                        653.   
C *********************************************************************  654.   
C *                                                                   *  655.   
C *  SUBROUTINE TO DRAW THE LABELS ON THE AXES AND CONTOUR PLOTS IN   *  656.   
C *  THE CONTOUR PLOT EXAMPLE.                                        *  657.   
C *                                                                   *  658.   
C *  THE CALLING SEQUENCE IS:                                         *  659.   
C *    CALL DTX261(XCRD,YCRD,VALU,FLAG)                               *  660.   
C *                                                                   *  661.   
C *  THE PARAMETERS IN THE CALLING SEQUENCE ARE:                      *  662.   
C *    XCRD  THE X COORDINATE OF THE LABEL.                           *  663.   
C *    YCRD  THE Y COORDINATE OF THE LABEL.                           *  664.   
C *    VALU  THE VALUE OF THE LABEL.                                  *  665.   
C *    FLAG  THE POSITION FLAG (0 MEANS LEFT, 1 MEANS BELOW, 2 MEANS  *  666.   
C *          RIGHT, AND 3 MEANS ABOVE).                               *  667.   
C *                                                                   *  668.   
C *********************************************************************  669.   
C                                                                        670.   
      REAL          XCRD,YCRD,VALU                                       671.   
      INTEGER       FLAG                                                 672.   
C                                                                        673.   
C  COMMON BLOCK FOR THE GRAPHIC SEGMENT.                                 674.   
      SAVE          /FGGCBK/                                             675.   
C  NUMBER OF WORDS IN THE GRAPHIC SEGMENT.                               676.   
      INTEGER       NSEG                                                 677.   
      PARAMETER     (NSEG=1024)                                          678.   
C  THE DECLARATION OF THE COMMON BLOCK.                                  679.   
      COMMON        /FGGCBK/                                             680.   
     X              SEGM                                                 681.   
C  THE GRAPHIC SEGMENT.                                                  682.   
      INTEGER*4     SEGM(NSEG)                                           683.   
C                                                                        684.   
      CHARACTER*10  STRG                                                 685.   
      INTEGER       LENG                                                 686.   
C                                                                        687.   
      CALL UGCNVF(VALU,2,STRG,LENG)                                      688.   
      IF (FLAG.EQ.0) THEN                                                689.   
        CALL UGTEXT('SIZE=0.015,RIGHT',XCRD-0.02,YCRD,                   690.   
     X    STRG(11-LENG:10),SEGM)                                         691.   
      ELSE IF (FLAG.EQ.1) THEN                                           692.   
        CALL UGTEXT('SIZE=0.015,CENTER',XCRD,YCRD-0.02,                  693.   
     X    STRG(11-LENG:10),SEGM)                                         694.   
      ELSE IF (FLAG.EQ.2) THEN                                           695.   
        CALL UGTEXT('SIZE=0.015,LEFT',XCRD+0.02,YCRD,                    696.   
     X    STRG(11-LENG:10),SEGM)                                         697.   
      ELSE                                                               698.   
        CALL UGTEXT('SIZE=0.015,CENTER',XCRD,YCRD+0.02,                  699.   
     X    STRG(11-LENG:10),SEGM)                                         700.   
      END IF                                                             701.   
      RETURN                                                             702.   
C                                                                        703.   
      END                                                                704.   
      SUBROUTINE DLN261(XCRD,YCRD,BBIT)                                  705.   
C                                                                        706.   
C *********************************************************************  707.   
C *                                                                   *  708.   
C *  SUBROUTINE TO DRAW THE ACTUAL CONTOUR LINES IN THE CONTOUR       *  709.   
C *  PLOT EXAMPLE.                                                    *  710.   
C *                                                                   *  711.   
C *  THE CALLING SEQUENCE IS:                                         *  712.   
C *    CALL DLN261(XCRD,YCRD,BBIT)                                    *  713.   
C *                                                                   *  714.   
C *  THE PARAMETERS IN THE CALLING SEQUENCE ARE:                      *  715.   
C *    XCRD  THE X COORDINATE OF THE LINE END POINT.                  *  716.   
C *    YCRD  THE Y COORDINATE OF THE LINE END POINT.                  *  717.   
C *    BBIT  THE BLANKING BIT (0 OR 2 MEANS BLANK AND 1 OR 3 MEANS    *  718.   
C *          DRAW).                                                   *  719.   
C *                                                                   *  720.   
C *********************************************************************  721.   
C                                                                        722.   
      REAL          XCRD,YCRD                                            723.   
      INTEGER       BBIT                                                 724.   
C                                                                        725.   
C  COMMON BLOCK FOR THE GRAPHIC SEGMENT.                                 726.   
      SAVE          /FGGCBK/                                             727.   
C  NUMBER OF WORDS IN THE GRAPHIC SEGMENT.                               728.   
      INTEGER       NSEG                                                 729.   
      PARAMETER     (NSEG=1024)                                          730.   
C  THE DECLARATION OF THE COMMON BLOCK.                                  731.   
      COMMON        /FGGCBK/                                             732.   
     X              SEGM                                                 733.   
C  THE GRAPHIC SEGMENT.                                                  734.   
      INTEGER*4     SEGM(NSEG)                                           735.   
C                                                                        736.   
      IF (BBIT.LE.1) THEN                                                737.   
        CALL UGLINE(' ',XCRD,YCRD,BBIT,SEGM)                             738.   
      ELSE                                                               739.   
        CALL UGLINE('VDIM,DASHED',XCRD,YCRD,BBIT-2,SEGM)                 740.   
      END IF                                                             741.   
      RETURN                                                             742.   
C                                                                        743.   
      END                                                                744.   
      SUBROUTINE DLD271(XCRD,YCRD,BBIT)                                  745.   
C                                                                        746.   
C *********************************************************************  747.   
C *                                                                   *  748.   
C *  SUBROUTINE TO DRAW LINES IN THE DIM MODE FOR THE MESH SURFACE    *  749.   
C *  EXAMPLE.                                                         *  750.   
C *                                                                   *  751.   
C *  THE CALLING SEQUENCE IS:                                         *  752.   
C *    CALL DLD271(XCRD,YCRD,BBIT)                                    *  753.   
C *                                                                   *  754.   
C *  THE PARAMETERS IN THE CALLING SEQUENCE ARE:                      *  755.   
C *    XCRD  THE X COORDINATE OF THE LINE END POINT.                  *  756.   
C *    YCRD  THE Y COORDINATE OF THE LINE END POINT.                  *  757.   
C *    BBIT  THE BLANKING BIT (0 MEANS BLANK AND 1 MEANS DRAW).       *  758.   
C *                                                                   *  759.   
C *********************************************************************  760.   
C                                                                        761.   
      REAL          XCRD,YCRD                                            762.   
      INTEGER       BBIT                                                 763.   
C                                                                        764.   
C  COMMON BLOCK FOR THE GRAPHIC SEGMENT.                                 765.   
      SAVE          /FGGCBK/                                             766.   
C  NUMBER OF WORDS IN THE GRAPHIC SEGMENT.                               767.   
      INTEGER       NSEG                                                 768.   
      PARAMETER     (NSEG=1024)                                          769.   
C  THE DECLARATION OF THE COMMON BLOCK.                                  770.   
      COMMON        /FGGCBK/                                             771.   
     X              SEGM                                                 772.   
C  THE GRAPHIC SEGMENT.                                                  773.   
      INTEGER*4     SEGM(NSEG)                                           774.   
C                                                                        775.   
      CALL UGLINE('VDIM',XCRD,YCRD,BBIT,SEGM)                            776.   
      RETURN                                                             777.   
C                                                                        778.   
      END                                                                779.   
      SUBROUTINE DLN271(XCRD,YCRD,BBIT)                                  780.   
C                                                                        781.   
C *********************************************************************  782.   
C *                                                                   *  783.   
C *  SUBROUTINE TO DRAW LINES IN THE NORMAL MODE FOR THE MESH         *  784.   
C *  SURFACE EXAMPLE.                                                 *  785.   
C *                                                                   *  786.   
C *  THE CALLING SEQUENCE IS:                                         *  787.   
C *    CALL DLN271(XCRD,YCRD,BBIT)                                    *  788.   
C *                                                                   *  789.   
C *  THE PARAMETERS IN THE CALLING SEQUENCE ARE:                      *  790.   
C *    XCRD  THE X COORDINATE OF THE LINE END POINT.                  *  791.   
C *    YCRD  THE Y COORDINATE OF THE LINE END POINT.                  *  792.   
C *    BBIT  THE BLANKING BIT (0 MEANS BLANK AND 1 MEANS DRAW).       *  793.   
C *                                                                   *  794.   
C *********************************************************************  795.   
C                                                                        796.   
      REAL          XCRD,YCRD                                            797.   
      INTEGER       BBIT                                                 798.   
C                                                                        799.   
C  COMMON BLOCK FOR THE GRAPHIC SEGMENT.                                 800.   
      SAVE          /FGGCBK/                                             801.   
C  NUMBER OF WORDS IN THE GRAPHIC SEGMENT.                               802.   
      INTEGER       NSEG                                                 803.   
      PARAMETER     (NSEG=1024)                                          804.   
C  THE DECLARATION OF THE COMMON BLOCK.                                  805.   
      COMMON        /FGGCBK/                                             806.   
     X              SEGM                                                 807.   
C  THE GRAPHIC SEGMENT.                                                  808.   
      INTEGER*4     SEGM(NSEG)                                           809.   
C                                                                        810.   
      CALL UGLINE(' ',XCRD,YCRD,BBIT,SEGM)                               811.   
      RETURN                                                             812.   
C                                                                        813.   
      END                                                                814.   
      SUBROUTINE DLN281(XCRD,YCRD,BBIT)                                  815.   
C                                                                        816.   
C *********************************************************************  817.   
C *                                                                   *  818.   
C *  SUBROUTINE TO DRAW LINES IN THE NORMAL MODE FOR THE TWO-         *  819.   
C *  DIMENSIONAL HISTOGRAM EXAMPLE.                                   *  820.   
C *                                                                   *  821.   
C *  THE CALLING SEQUENCE IS:                                         *  822.   
C *    CALL DLN281(XCRD,YCRD,BBIT)                                    *  823.   
C *                                                                   *  824.   
C *  THE PARAMETERS IN THE CALLING SEQUENCE ARE:                      *  825.   
C *    XCRD  THE X COORDINATE OF THE LINE END POINT.                  *  826.   
C *    YCRD  THE Y COORDINATE OF THE LINE END POINT.                  *  827.   
C *    BBIT  THE BLANKING BIT (0 MEANS BLANK AND 1 MEANS DRAW).       *  828.   
C *                                                                   *  829.   
C *********************************************************************  830.   
C                                                                        831.   
      REAL          XCRD,YCRD                                            832.   
      INTEGER       BBIT                                                 833.   
C                                                                        834.   
C  COMMON BLOCK FOR THE GRAPHIC SEGMENT.                                 835.   
      SAVE          /FGGCBK/                                             836.   
C  NUMBER OF WORDS IN THE GRAPHIC SEGMENT.                               837.   
      INTEGER       NSEG                                                 838.   
      PARAMETER     (NSEG=1024)                                          839.   
C  THE DECLARATION OF THE COMMON BLOCK.                                  840.   
      COMMON        /FGGCBK/                                             841.   
     X              SEGM                                                 842.   
C  THE GRAPHIC SEGMENT.                                                  843.   
      INTEGER*4     SEGM(NSEG)                                           844.   
C                                                                        845.   
      CALL UGLINE(' ',XCRD,YCRD,BBIT,SEGM)                               846.   
      RETURN                                                             847.   
C                                                                        848.   
      END                                                                849.   
      SUBROUTINE PCINIT(PLIM,XSIZ,BFLG,SEGM,NSEG)                        850.   
C                                                                        851.   
C *********************************************************************  852.   
C *                                                                   *  853.   
C *  SUBROUTINE TO BEGIN A NEW PICTURE, INITIALIZE THE DRAWING        *  854.   
C *  SPACE, AND (OPTIONALLY) DRAW A BORDER.                           *  855.   
C *                                                                   *  856.   
C *  THE CALLING SEQUENCE IS:                                         *  857.   
C *    CALL PCINIT(PLIM,XSIZ,BFLG,SEGM,NSEG)                          *  858.   
C *                                                                   *  859.   
C *  THE PARAMETERS IN THE CALLING SEQUENCE ARE:                      *  860.   
C *    PLIM  THE PROGRAMMER LIMITS FOR THE PICTURE.                   *  861.   
C *    XSIZ  THE ACTUAL WIDTH OF THE PICTURE IN CENTIMETERS.          *  862.   
C *    BFLG  THE BORDER FLAG (0 MEANS NO BORDER AND 1 MEANS DRAW THE  *  863.   
C *          BORDER).                                                 *  864.   
C *    SEGM  THE GRAPHIC SEGMENT.                                     *  865.   
C *    NSEG  THE DIMENSION OF THE GRAPHIC SEGMENT.                    *  866.   
C *                                                                   *  867.   
C *********************************************************************  868.   
C                                                                        869.   
      REAL          PLIM(2,2)                                            870.   
      REAL          XSIZ                                                 871.   
      INTEGER       BFLG                                                 872.   
      INTEGER*4     SEGM(*)                                              873.   
      INTEGER       NSEG                                                 874.   
C                                                                        875.   
      REAL          DSPC(2,2)                                            876.   
      REAL          YSIZ,XRTO,YRTO                                       877.   
      CHARACTER*1   STRG                                                 878.   
      INTEGER       IARY(1)                                              879.   
      REAL          XARY(2)                                              880.   
C                                                                        881.   
C  START A NEW PICTURE.                                                  882.   
      CALL UGPICT('CLEAR',0)                                             883.   
C                                                                        884.   
C  OBTAIN A DRAWING SPACE WITH THE CORRECT ASPECT RATIO AND SIZE.        885.   
      CALL UGDSPC('PUT',1.0,1.0,0.0)                                     886.   
      CALL UGINFO('DSPCSIZE',STRG,IARY,XARY)                             887.   
      YSIZ=XSIZ*(PLIM(2,2)-PLIM(2,1))/(PLIM(1,2)-PLIM(1,1))              888.   
      XRTO=XSIZ/XARY(1)                                                  889.   
      YRTO=YSIZ/XARY(2)                                                  890.   
      IF (MAX(XRTO,YRTO).GT.1.0) THEN                                    891.   
        IF (XRTO.GT.YRTO) THEN                                           892.   
          YRTO=YRTO/XRTO                                                 893.   
          XRTO=1.0                                                       894.   
        ELSE                                                             895.   
          XRTO=XRTO/YRTO                                                 896.   
          YRTO=1.0                                                       897.   
        END IF                                                           898.   
      END IF                                                             899.   
      DSPC(1,1)=0.5*(1.0-XRTO)                                           900.   
      DSPC(1,2)=1.0-DSPC(1,1)                                            901.   
      DSPC(2,1)=0.5*(1.0-YRTO)                                           902.   
      DSPC(2,2)=1.0-DSPC(2,1)                                            903.   
      CALL UGWDOW('PUT',DSPC,PLIM)                                       904.   
C                                                                        905.   
C  CLEAR THE GRAPHIC SEGMENT AND DRAW A BORDER IF NECESSARY.             906.   
      CALL UGINIT('CLEAR',SEGM,NSEG)                                     907.   
      IF (BFLG.NE.0) THEN                                                908.   
        CALL UGLINE('VBRIGHT',PLIM(1,1)+0.0001,                          909.   
     X                        PLIM(2,1)+0.0001,0,SEGM)                   910.   
        CALL UGLINE('VBRIGHT',PLIM(1,2)-0.0001,                          911.   
     X                        PLIM(2,1)+0.0001,1,SEGM)                   912.   
        CALL UGLINE('VBRIGHT',PLIM(1,2)-0.0001,                          913.   
     X                        PLIM(2,2)-0.0001,1,SEGM)                   914.   
        CALL UGLINE('VBRIGHT',PLIM(1,1)+0.0001,                          915.   
     X                        PLIM(2,2)-0.0001,1,SEGM)                   916.   
        CALL UGLINE('VBRIGHT',PLIM(1,1)+0.0001,                          917.   
     X                        PLIM(2,1)+0.0001,1,SEGM)                   918.   
      END IF                                                             919.   
C                                                                        920.   
C  RETURN TO CALLING ROUTINE.                                            921.   
      RETURN                                                             922.   
C                                                                        923.   
      END                                                                924.   
      SUBROUTINE DLNSEG(INTN,XCD1,YCD1,XCD2,YCD2,XCLO,XCHI,SEGM)         925.   
C                                                                        926.   
C *********************************************************************  927.   
C *                                                                   *  928.   
C *  SUBROUTINE TO DRAW ONE SEGMENT OF A LONGER LINE.                 *  929.   
C *                                                                   *  930.   
C *  THE CALLING SEQUENCE IS:                                         *  931.   
C *    CALL DLNSEG(INTN,XCD1,YCD1,XCD2,YCD2,XCLO,XCHI,SEGM)           *  932.   
C *                                                                   *  933.   
C *  THE PARAMETERS IN THE CALLING SEQUENCE ARE:                      *  934.   
C *    INTN  THE INTENSITY LEVEL OF THE LINE.                         *  935.   
C *    XCD1  THE X COORDINATE OF ONE END OF THE LINE.                 *  936.   
C *    YCD1  THE Y COORDINATE OF ONE END OF THE LINE.                 *  937.   
C *    XCD2  THE X COORDINATE OF THE OTHER END OF THE LINE.           *  938.   
C *    YCD2  THE Y COORDINATE OF THE OTHER END OF THE LINE.           *  939.   
C *    XCLO  THE X COORDINATE OF ONE END OF THE SEGMENT.              *  940.   
C *    XCHI  THE X COORDINATE OF THE OTHER END OF THE SEGMENT.        *  941.   
C *    SEGM  THE GRAPHIC SEGMENT.                                     *  942.   
C *                                                                   *  943.   
C *********************************************************************  944.   
C                                                                        945.   
      CHARACTER*(*) INTN                                                 946.   
      REAL          XCD1,YCD1,XCD2,YCD2,XCLO,XCHI                        947.   
      INTEGER*4     SEGM(*)                                              948.   
C                                                                        949.   
      CALL UGLINE(INTN,                                                  950.   
     X  XCLO,((YCD2-YCD1)*(XCLO-XCD1)/(XCD2-XCD1))+YCD1,0,SEGM)          951.   
      CALL UGLINE(INTN,                                                  952.   
     X  XCHI,((YCD2-YCD1)*(XCHI-XCD1)/(XCD2-XCD1))+YCD1,1,SEGM)          953.   
      RETURN                                                             954.   
C                                                                        955.   
      END                                                                956.   
      SUBROUTINE DPOINT(XCRD,YCRD,SEGM)                                  957.   
C                                                                        958.   
C *********************************************************************  959.   
C *                                                                   *  960.   
C *  SUBROUTINE TO DRAW A LARGE POINT.                                *  961.   
C *                                                                   *  962.   
C *  THE CALLING SEQUENCE IS:                                         *  963.   
C *    CALL DPOINT(XCRD,YCRD,SEGM)                                    *  964.   
C *                                                                   *  965.   
C *  THE PARAMETERS IN THE CALLING SEQUENCE ARE:                      *  966.   
C *    XCRD  THE X COORDINATE OF THE POINT.                           *  967.   
C *    YCRD  THE Y COORDINATE OF THE POINT.                           *  968.   
C *    SEGM  THE GRAPHIC SEGMENT.                                     *  969.   
C *                                                                   *  970.   
C *********************************************************************  971.   
C                                                                        972.   
      REAL          XCRD,YCRD                                            973.   
      INTEGER*4     SEGM(*)                                              974.   
C                                                                        975.   
      CALL UGMARK('MARK=9,SIZE=0.010',XCRD,YCRD,SEGM)                    976.   
      CALL UGMARK('MARK=9,SIZE=0.005',XCRD,YCRD,SEGM)                    977.   
      RETURN                                                             978.   
C                                                                        979.   
      END                                                                980.   
      SUBROUTINE DARROW(INTN,XCD1,YCD1,XCD2,YCD2,SEGM)                   981.   
C                                                                        982.   
C *********************************************************************  983.   
C *                                                                   *  984.   
C *  SUBROUTINE TO DRAW AN ARROW HEAD.                                *  985.   
C *                                                                   *  986.   
C *  THE CALLING SEQUENCE IS:                                         *  987.   
C *    CALL DARROW(INTN,XCD1,YCD1,XCD2,YCD2,SEGM)                     *  988.   
C *                                                                   *  989.   
C *  THE PARAMETERS IN THE CALLING SEQUENCE ARE:                      *  990.   
C *    INTN  THE INTENSITY LEVEL OF THE LINE.                         *  991.   
C *    XCD1  THE X COORDINATE OF THE HEAD OF THE ARROW.               *  992.   
C *    YCD1  THE Y COORDINATE OF THE HEAD OF THE ARROW.               *  993.   
C *    XCD2  THE X COORDINATE OF THE OTHER END OF THE ARROW.          *  994.   
C *    YCD2  THE Y COORDINATE OF THE OTHER END OF THE ARROW.          *  995.   
C *    SEGM  THE GRAPHIC SEGMENT.                                     *  996.   
C *                                                                   *  997.   
C *********************************************************************  998.   
C                                                                        999.   
      CHARACTER*(*) INTN                                                1000.   
      REAL          XCD1,YCD1,XCD2,YCD2                                 1001.   
      INTEGER*4     SEGM(*)                                             1002.   
C                                                                       1003.   
      REAL          ARGU                                                1004.   
C                                                                       1005.   
      ARGU=ATAN2(YCD2-YCD1,XCD2-XCD1)                                   1006.   
      CALL UGLINE(INTN,XCD1,YCD1,0,SEGM)                                1007.   
      CALL UGLINE(INTN,                                                 1008.   
     X  XCD1+0.02*COS(ARGU+0.25),YCD1+0.02*SIN(ARGU+0.25),1,SEGM)       1009.   
      CALL UGLINE(INTN,                                                 1010.   
     X  XCD1+0.02*COS(ARGU-0.25),YCD1+0.02*SIN(ARGU-0.25),1,SEGM)       1011.   
      CALL UGLINE(INTN,XCD1,YCD1,1,SEGM)                                1012.   
      RETURN                                                            1013.   
C                                                                       1014.   
      END                                                               1015.   
      SUBROUTINE UGXERR(LEVL,SNAM,INDX)                                 1016.   
C                                                                       1017.   
C ********************************************************************* 1018.   
C *                                                                   * 1019.   
C *  SUBROUTINE TO PROCESS GRAPHIC SEGMENT OVERFLOWS.                 * 1020.   
C *                                                                   * 1021.   
C *  THE CALLING SEQUENCE IS:                                         * 1022.   
C *    CALL UGXERR(LEVL,SNAM,INDX)                                    * 1023.   
C *                                                                   * 1024.   
C *  THE PARAMETERS IN THE CALLING SEQUENCE ARE:                      * 1025.   
C *    LEVL  THE LEVEL OF THE ERROR.                                  * 1026.   
C *    SNAM  THE NAME OF THE SUBROUTINE DETECTING THE ERROR.          * 1027.   
C *    INDX  THE INDEX OF THE ERROR.                                  * 1028.   
C *                                                                   * 1029.   
C ********************************************************************* 1030.   
C                                                                       1031.   
      INTEGER       LEVL                                                1032.   
      CHARACTER*8   SNAM                                                1033.   
      INTEGER       INDX                                                1034.   
C                                                                       1035.   
C  COMMON BLOCK FOR THE GRAPHIC SEGMENT.                                1036.   
      SAVE          /FGGCBK/                                            1037.   
C  NUMBER OF WORDS IN THE GRAPHIC SEGMENT.                              1038.   
      INTEGER       NSEG                                                1039.   
      PARAMETER     (NSEG=1024)                                         1040.   
C  THE DECLARATION OF THE COMMON BLOCK.                                 1041.   
      COMMON        /FGGCBK/                                            1042.   
     X              SEGM                                                1043.   
C  THE GRAPHIC SEGMENT.                                                 1044.   
      INTEGER*4     SEGM(NSEG)                                          1045.   
C                                                                       1046.   
C  CLEAR THE GRAPHIC SEGMENT IF IT HAS OVERFLOWED.                      1047.   
      IF (INDX.EQ.11) THEN                                              1048.   
        CALL UGWRIT(' ',0,SEGM)                                         1049.   
        CALL UGINIT('CONTINUE',SEGM,NSEG)                               1050.   
        LEVL=0                                                          1051.   
      END IF                                                            1052.   
C                                                                       1053.   
C  RETURN TO CALLING ROUTINE.                                           1054.   
      RETURN                                                            1055.   
C                                                                       1056.   
      END                                                               1057.   
