      PROGRAM       INTDOC                                                 1.   
C                                                                          2.   
C *******************  THE UNIFIED GRAPHICS SYSTEM  *******************    3.   
C *                                                                   *    4.   
C *  THIS PROGRAM WILL GENERATE THE FIGURES FOR THE UNIFIED GRAPHICS  *    5.   
C *  SYSTEM INTERNALS MANUAL.                                         *    6.   
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
C  DECLARATIONS FOR FIGURE SHOWING LINE SCISSORING SCHEME.                29.   
      REAL          P711(2,2)                                             30.   
C                                                                         31.   
C  DECLARATIONS FOR FIGURE SHOWING POLYGON SCISSORING EXAMPLE.            32.   
      REAL          P712(2,2)                                             33.   
C                                                                         34.   
C  DECLARATIONS FOR FIGURE SHOWING POINT PROJECTION SCHEME.               35.   
      REAL          P741(2,2)                                             36.   
C                                                                         37.   
C  DECLARATIONS FOR FIGURE SHOWING SMOOTH CURVE INTERPOLATION.            38.   
      EXTERNAL      DLN751                                                39.   
      REAL          P751(2,2)                                             40.   
      REAL          X751(4),Y751(4)                                       41.   
      SAVE          /FGCBKX/                                              42.   
      COMMON        /FGCBKX/                                              43.   
     X              A751                                                  44.   
      REAL          A751(2,5)                                             45.   
C                                                                         46.   
C  DECLARATIONS FOR FIGURE SHOWING THE CROSS-HATCHING SCHEME.             47.   
      REAL          P761(2,2)                                             48.   
C                                                                         49.   
C  DECLARATIONS FOR FIGURE SHOWING CONTOUR PLOT LABELING SCHEME.          50.   
      REAL          P781(2,2)                                             51.   
C                                                                         52.   
C  DECLARATIONS FOR FIGURE SHOWING CONTOUR PLOT PROBLEM.                  53.   
      REAL          P782(2,2)                                             54.   
C                                                                         55.   
C  DECLARATIONS FOR FIGURE SHOWING MESH SURFACE HEIGHT FUNCTION.          56.   
      REAL          P791(2,2)                                             57.   
C                                                                         58.   
C  DECLARATIONS FOR FIGURE SHOWING MESH SURFACE PROBLEM.                  59.   
      REAL          P792(2,2)                                             60.   
      REAL          X792(24),Y792(24)                                     61.   
C                                                                         62.   
C  DECLARATIONS FOR FIGURE SHOWING WORKSTATION PROJECTION SCHEME.         63.   
      REAL          P9X1(2,2)                                             64.   
C                                                                         65.   
      REAL          FLT1,FLT2                                             66.   
      INTEGER       INT1,INT2                                             67.   
C                                                                         68.   
      DATA          P711/ 0.000, 0.000, 1.000, 0.470/                     69.   
      DATA          P712/ 0.000, 0.000, 1.000, 0.450/                     70.   
      DATA          P741/ 0.000, 0.120, 1.000, 0.870/                     71.   
      DATA          P751/ 0.000, 0.000, 1.000, 0.550/                     72.   
      DATA          X751/ 0.075, 0.225, 0.750, 0.900/                     73.   
      DATA          Y751/ 0.075, 0.350, 0.350, 0.075/                     74.   
      DATA          P761/ 0.000, 0.000, 1.000, 0.700/                     75.   
      DATA          P781/ 0.000, 0.000, 1.000, 0.650/                     76.   
      DATA          P782/ 0.000, 0.000, 1.000, 0.450/                     77.   
      DATA          P791/ 0.000, 0.000, 1.000, 0.400/                     78.   
      DATA          P792/ 0.000, 0.000, 1.000, 0.650/                     79.   
      DATA          X792/-0.200,-0.150,-0.100,-0.050, 0.000, 0.050,       80.   
     X                    0.000,-0.050,-0.100,-0.150,-0.100,-0.050,       81.   
     X                    0.050, 0.100, 0.150, 0.100, 0.050, 0.000,       82.   
     X                   -0.050, 0.000, 0.050, 0.100, 0.150, 0.200/       83.   
      DATA          Y792/ 0.000,-0.025,-0.050,-0.075,-0.100,-0.075,       84.   
     X                   -0.050,-0.025, 0.000, 0.025, 0.050, 0.025,       85.   
     X                   -0.025,-0.050,-0.025, 0.000, 0.025, 0.075,       86.   
     X                    0.075, 0.100, 0.075, 0.050, 0.025, 0.000/       87.   
      DATA          P9X1/ 0.000, 0.120, 1.000, 0.870/                     88.   
C                                                                         89.   
C  INITIALIZE THE PROGRAM.                                                90.   
C     CALL UGOPEN('IMGN300,GENIL',99)                                     91.   
      CALL UGOPEN('POSTSCR,GENIL',99)                                     91.   
C                                                                         92.   
C  PRODUCE FIGURE SHOWING LINE SCISSORING SCHEME.                         93.   
      CALL UGFONT('DUPLEX')                                               94.   
      CALL PCINIT(P711,16.5*FACT,1,SEGM,NSEG)                             95.   
C    DRAW THE BIT ORDER DIAGRAM.                                          96.   
      CALL UGLINE(' ',0.075,0.275,0,SEGM)                                 97.   
      CALL UGLINE(' ',0.375,0.275,1,SEGM)                                 98.   
      CALL UGLINE(' ',0.375,0.325,1,SEGM)                                 99.   
      CALL UGLINE(' ',0.075,0.325,1,SEGM)                                100.   
      CALL UGLINE(' ',0.075,0.275,1,SEGM)                                101.   
      CALL UGLINE(' ',0.150,0.275,0,SEGM)                                102.   
      CALL UGLINE(' ',0.150,0.325,1,SEGM)                                103.   
      CALL UGLINE(' ',0.225,0.275,0,SEGM)                                104.   
      CALL UGLINE(' ',0.225,0.325,1,SEGM)                                105.   
      CALL UGLINE(' ',0.300,0.275,0,SEGM)                                106.   
      CALL UGLINE(' ',0.300,0.325,1,SEGM)                                107.   
      CALL UGXTXT('SIZE=0.02,CENTER',0.112,0.300,'YHI','   ',SEGM)       108.   
      CALL UGXTXT('SIZE=0.02,CENTER',0.187,0.300,'YLO','   ',SEGM)       109.   
      CALL UGXTXT('SIZE=0.02,CENTER',0.262,0.300,'XHI','   ',SEGM)       110.   
      CALL UGXTXT('SIZE=0.02,CENTER',0.337,0.300,'XLO','   ',SEGM)       111.   
      CALL UGXTXT('SIZE=0.02,CENTER',0.225,0.200,                        112.   
     X  'THE ORDER OF THE',                                              113.   
     X  ' LL  LLLL LL LLL',SEGM)                                         114.   
      CALL UGXTXT('SIZE=0.02,CENTER',0.225,0.150,                        115.   
     X  'BITS IN THE FLAG',                                              116.   
     X  ' LLL LL LLL  LLL',SEGM)                                         117.   
C    DRAW THE BIT VALUE DIAGRAM.                                         118.   
      CALL UGLINE(' ',0.550,0.325,0,SEGM)                                119.   
      CALL UGLINE(' ',0.925,0.325,1,SEGM)                                120.   
      CALL UGLINE(' ',0.550,0.250,0,SEGM)                                121.   
      CALL UGLINE(' ',0.925,0.250,1,SEGM)                                122.   
      CALL UGLINE(' ',0.675,0.175,0,SEGM)                                123.   
      CALL UGLINE(' ',0.675,0.400,1,SEGM)                                124.   
      CALL UGLINE(' ',0.800,0.175,0,SEGM)                                125.   
      CALL UGLINE(' ',0.800,0.400,1,SEGM)                                126.   
      CALL UGXTXT('SIZE=0.02,CENTER',0.612,0.362,'1001','    ',SEGM)     127.   
      CALL UGXTXT('SIZE=0.02,CENTER',0.737,0.362,'1000','    ',SEGM)     128.   
      CALL UGXTXT('SIZE=0.02,CENTER',0.862,0.362,'1010','    ',SEGM)     129.   
      CALL UGXTXT('SIZE=0.02,RIGHT',0.525,0.325,'YHI','   ',SEGM)        130.   
      CALL UGXTXT('SIZE=0.02,CENTER',0.612,0.287,'0001','    ',SEGM)     131.   
      CALL UGXTXT('SIZE=0.02,CENTER',0.737,0.287,'0000','    ',SEGM)     132.   
      CALL UGXTXT('SIZE=0.02,CENTER',0.862,0.287,'0010','    ',SEGM)     133.   
      CALL UGXTXT('SIZE=0.02,RIGHT',0.525,0.250,'YLO','   ',SEGM)        134.   
      CALL UGXTXT('SIZE=0.02,CENTER',0.612,0.212,'0101','    ',SEGM)     135.   
      CALL UGXTXT('SIZE=0.02,CENTER',0.737,0.212,'0100','    ',SEGM)     136.   
      CALL UGXTXT('SIZE=0.02,CENTER',0.862,0.212,'0110','    ',SEGM)     137.   
      CALL UGXTXT('SIZE=0.02,CENTER',0.675,0.150,'XLO','   ',SEGM)       138.   
      CALL UGXTXT('SIZE=0.02,CENTER',0.800,0.150,'XHI','   ',SEGM)       139.   
      CALL UGXTXT('SIZE=0.02,CENTER',0.712,0.075,                        140.   
     X  'THE VALUES OF THE FLAG',                                        141.   
     X  ' LL  LLLLL LL LLL  LLL',SEGM)                                   142.   
      CALL UGWRIT(' ',0,SEGM)                                            143.   
C                                                                        144.   
C  PRODUCE FIGURE SHOWING POLYGON SCISSORING EXAMPLE.                    145.   
      CALL UGFONT('DUPLEX')                                              146.   
      CALL PCINIT(P712,16.5*FACT,1,SEGM,NSEG)                            147.   
C    DRAW THE SCISSORING LINE.                                           148.   
      CALL UGLINE(' ',0.075,0.200,0,SEGM)                                149.   
      CALL UGLINE(' ',0.925,0.200,1,SEGM)                                150.   
      CALL UGXTXT('SIZE=0.02',0.700,0.350,                               151.   
     X  'KEEP SIDE',                                                     152.   
     X  ' LLL  LLL',SEGM)                                                153.   
      CALL UGXTXT('SIZE=0.02',0.675,0.225,                               154.   
     X  'SCISSORING LINE',                                               155.   
     X  ' LLLLLLLLL  LLL',SEGM)                                          156.   
      CALL UGXTXT('SIZE=0.02',0.650,0.100,                               157.   
     X  'REJECT SIDE',                                                   158.   
     X  ' LLLLL  LLL',SEGM)                                              159.   
C    DRAW THE POLYGON.                                                   160.   
      CALL UGLINE(' ',0.100,0.250,0,SEGM)                                161.   
      CALL UGLINE(' ',0.275,0.075,1,SEGM)                                162.   
      CALL UGLINE(' ',0.500,0.075,1,SEGM)                                163.   
      CALL UGLINE(' ',0.500,0.275,1,SEGM)                                164.   
      CALL UGLINE(' ',0.425,0.300,1,SEGM)                                165.   
      CALL UGLINE(' ',0.450,0.225,1,SEGM)                                166.   
      CALL UGLINE(' ',0.350,0.125,1,SEGM)                                167.   
      CALL UGLINE(' ',0.250,0.150,1,SEGM)                                168.   
      CALL UGLINE(' ',0.250,0.300,1,SEGM)                                169.   
      CALL UGLINE(' ',0.300,0.225,1,SEGM)                                170.   
      CALL UGLINE(' ',0.300,0.175,1,SEGM)                                171.   
      CALL UGLINE(' ',0.400,0.225,1,SEGM)                                172.   
      CALL UGLINE(' ',0.400,0.350,1,SEGM)                                173.   
      CALL UGLINE(' ',0.550,0.300,1,SEGM)                                174.   
      CALL UGLINE(' ',0.550,0.125,1,SEGM)                                175.   
      CALL UGLINE(' ',0.650,0.275,1,SEGM)                                176.   
      CALL UGLINE(' ',0.525,0.375,1,SEGM)                                177.   
      CALL UGLINE(' ',0.275,0.375,1,SEGM)                                178.   
      CALL UGLINE(' ',0.100,0.250,1,SEGM)                                179.   
      CALL UGXTXT('SIZE=0.02,CENTER',0.325,0.325,'I',' ',SEGM)           180.   
      CALL UGXTXT('SIZE=0.02,CENTER',0.475,0.250,'II','  ',SEGM)         181.   
C    DRAW THE INTERSECTION POINTS.                                       182.   
      CALL DPOINT(0.150,0.200,SEGM)                                      183.   
      CALL DPOINT(0.250,0.200,SEGM)                                      184.   
      CALL DPOINT(0.300,0.200,SEGM)                                      185.   
      CALL DPOINT(0.350,0.200,SEGM)                                      186.   
      CALL DPOINT(0.425,0.200,SEGM)                                      187.   
      CALL DPOINT(0.500,0.200,SEGM)                                      188.   
      CALL DPOINT(0.550,0.200,SEGM)                                      189.   
      CALL DPOINT(0.600,0.200,SEGM)                                      190.   
      CALL UGXTXT('SIZE=0.02',0.137,0.182,'P',' ',SEGM)                  191.   
      CALL UGXTXT('SIZE=0.02',0.225,0.220,'Q',' ',SEGM)                  192.   
      CALL UGWRIT(' ',0,SEGM)                                            193.   
C                                                                        194.   
C  PRODUCE FIGURE SHOWING POINT PROJECTION SCHEME.                       195.   
      CALL UGFONT('DUPLEX')                                              196.   
      CALL PCINIT(P741,16.5*FACT,1,SEGM,NSEG)                            197.   
C    DRAW THE SCREEN.                                                    198.   
      CALL UGLINE(' ',0.560,0.260,0,SEGM)                                199.   
      CALL UGLINE(' ',0.860,0.360,1,SEGM)                                200.   
      CALL UGLINE(' ',0.860,0.790,1,SEGM)                                201.   
      CALL UGLINE(' ',0.560,0.720,1,SEGM)                                202.   
      CALL UGLINE('VBRIGHT',0.560,0.720,0,SEGM)                          203.   
      CALL UGLINE('VBRIGHT',0.560,0.260,1,SEGM)                          204.   
C    DRAW THE SCREEN AXES.                                               205.   
      CALL DLNSEG('VDIM',0.560,0.490,0.860,0.580,0.860,0.730,SEGM)       206.   
      CALL DLNSEG('VBRIGHT',0.560,0.490,0.860,0.580,0.730,0.630,SEGM)    207.   
      CALL DARROW('VBRIGHT',0.620,0.508,0.860,0.580,SEGM)                208.   
      CALL UGXTXT('SIZE=0.02',0.640,0.490,'HDIR','    ',SEGM)            209.   
      CALL DLNSEG('VDIM',0.560,0.490,0.860,0.580,0.620,0.560,SEGM)       210.   
      CALL UGLINE(' ',0.730,0.317,0,SEGM)                                211.   
      CALL UGLINE(' ',0.730,0.541,1,SEGM)                                212.   
      CALL UGLINE('VBRIGHT',0.730,0.541,0,SEGM)                          213.   
      CALL UGLINE('VBRIGHT',0.730,0.680,1,SEGM)                          214.   
      CALL DARROW('VBRIGHT',0.730,0.680,0.730,0.317,SEGM)                215.   
      CALL UGXTXT('SIZE=0.02',0.745,0.690,'UDIR','    ',SEGM)            216.   
      CALL UGLINE(' ',0.730,0.680,0,SEGM)                                217.   
      CALL UGLINE(' ',0.730,0.760,1,SEGM)                                218.   
C    DRAW THE SCREEN LABELS.                                             219.   
      CALL DLNSEG('VDIM',0.560,0.720,0.860,0.790,0.870,0.900,SEGM)       220.   
      CALL UGXTXT('SIZE=0.02',0.915,0.800,'YHI','   ',SEGM)              221.   
      CALL DLNSEG('VDIM',0.560,0.260,0.860,0.360,0.870,0.900,SEGM)       222.   
      CALL UGXTXT('SIZE=0.02',0.915,0.370,'YLO','   ',SEGM)              223.   
      CALL UGLINE('VDIM',0.860,0.350,0,SEGM)                             224.   
      CALL UGLINE('VDIM',0.860,0.310,1,SEGM)                             225.   
      CALL UGXTXT('SIZE=0.02',0.840,0.290,'XLO','   ',SEGM)              226.   
      CALL DARROW('VDIM',0.860,0.335,0.560,0.235,SEGM)                   227.   
      CALL DLNSEG('VDIM',0.860,0.335,0.560,0.235,0.860,0.780,SEGM)       228.   
      CALL UGXTXT('SIZE=0.02',0.715,0.290,'SCRZ','    ',SEGM)            229.   
      CALL DLNSEG('VDIM',0.860,0.335,0.560,0.235,0.700,0.560,SEGM)       230.   
      CALL DARROW('VDIM',0.560,0.235,0.860,0.335,SEGM)                   231.   
      CALL UGLINE('VDIM',0.560,0.250,0,SEGM)                             232.   
      CALL UGLINE('VDIM',0.560,0.210,1,SEGM)                             233.   
      CALL UGXTXT('SIZE=0.02',0.545,0.190,'XHI','   ',SEGM)              234.   
      CALL UGLINE('VDIM',0.680,0.300,0,SEGM)                             235.   
      CALL UGLINE('VDIM',0.680,0.350,1,SEGM)                             236.   
      CALL UGXTXT('SIZE=0.02',0.680,0.370,'U','L',SEGM)                  237.   
      CALL UGLINE('VDIM',0.680,0.380,0,SEGM)                             238.   
      CALL UGLINE('VDIM',0.680,0.420,1,SEGM)                             239.   
      CALL DPOINT(0.680,0.420,SEGM)                                      240.   
      CALL UGXTXT('SIZE=0.02',0.680,0.440,'Q',' ',SEGM)                  241.   
      CALL DLNSEG('VDIM',0.680,0.420,0.860,0.475,0.680,0.770,SEGM)       242.   
      CALL UGXTXT('SIZE=0.02',0.785,0.450,'T','L',SEGM)                  243.   
      CALL DLNSEG('VDIM',0.680,0.420,0.860,0.475,0.790,0.860,SEGM)       244.   
C    DRAW THE PROJECTION LINE.                                           245.   
      CALL DLNSEG('VDIM',0.090,0.660,0.680,0.420,0.680,0.670,SEGM)       246.   
      CALL DLNSEG('VDIM',0.090,0.660,0.680,0.420,0.660,0.650,SEGM)       247.   
      CALL DLNSEG('VDIM',0.090,0.660,0.680,0.420,0.640,0.630,SEGM)       248.   
      CALL DLNSEG('VDIM',0.090,0.660,0.680,0.420,0.620,0.610,SEGM)       249.   
      CALL DLNSEG('VDIM',0.090,0.660,0.680,0.420,0.600,0.590,SEGM)       250.   
      CALL DLNSEG('VDIM',0.090,0.660,0.680,0.420,0.580,0.570,SEGM)       251.   
      CALL DLNSEG('VDIM',0.090,0.660,0.680,0.420,0.560,0.360,SEGM)       252.   
      CALL DPOINT(0.350,0.554,SEGM)                                      253.   
      CALL UGXTXT('SIZE=0.02',0.340,0.530,'P',' ',SEGM)                  254.   
      CALL DLNSEG('VDIM',0.090,0.660,0.680,0.420,0.340,0.090,SEGM)       255.   
      CALL DPOINT(0.090,0.660,SEGM)                                      256.   
C    DRAW THE EYE COMPLEX.                                               257.   
      CALL DLNSEG('VDIM',0.090,0.660,0.160,0.680,0.090,0.160,SEGM)       258.   
      CALL DPOINT(0.160,0.680,SEGM)                                      259.   
      CALL DLNSEG('VDIM',0.090,0.660,0.160,0.680,0.170,0.180,SEGM)       260.   
      CALL DLNSEG('VDIM',0.090,0.660,0.160,0.680,0.190,0.200,SEGM)       261.   
      CALL DLNSEG('VDIM',0.090,0.660,0.160,0.680,0.210,0.220,SEGM)       262.   
      CALL DLNSEG('VDIM',0.090,0.660,0.160,0.680,0.230,0.240,SEGM)       263.   
      CALL UGXTXT('SIZE=0.02',0.235,0.750,'EYED','    ',SEGM)            264.   
      CALL DLNSEG('VDIM',0.090,0.710,0.160,0.730,0.160,0.220,SEGM)       265.   
      CALL DARROW('VDIM',0.160,0.730,0.230,0.750,SEGM)                   266.   
      CALL UGLINE('VDIM',0.160,0.750,0,SEGM)                             267.   
      CALL UGLINE('VDIM',0.160,0.690,1,SEGM)                             268.   
      CALL UGLINE('VDIM',0.090,0.670,0,SEGM)                             269.   
      CALL UGLINE('VDIM',0.090,0.730,1,SEGM)                             270.   
      CALL DARROW('VDIM',0.090,0.710,0.020,0.690,SEGM)                   271.   
      CALL DLNSEG('VDIM',0.090,0.710,0.160,0.730,0.090,0.050,SEGM)       272.   
      CALL UGXTXT('SIZE=0.02',0.075,0.635,'E',' ',SEGM)                  273.   
      CALL UGXTXT('SIZE=0.02',0.090,0.570,'REFP','    ',SEGM)            274.   
      CALL UGLINE('VDIM',0.115,0.585,0,SEGM)                             275.   
      CALL UGLINE('VDIM',0.155,0.670,1,SEGM)                             276.   
      CALL DARROW('VDIM',0.155,0.670,0.115,0.585,SEGM)                   277.   
C    DRAW THE VDIR LINE.                                                 278.   
      CALL DLNSEG('VBRIGHT',0.160,0.680,0.730,0.541,0.160,0.280,SEGM)    279.   
      CALL DARROW('VBRIGHT',0.290,0.648,0.160,0.680,SEGM)                280.   
      CALL UGXTXT('SIZE=0.02',0.250,0.620,'VDIR','    ',SEGM)            281.   
      CALL DLNSEG('VDIM',0.160,0.680,0.730,0.541,0.280,0.560,SEGM)       282.   
      CALL DLNSEG('VDIM',0.160,0.680,0.730,0.541,0.570,0.580,SEGM)       283.   
      CALL DLNSEG('VDIM',0.160,0.680,0.730,0.541,0.590,0.600,SEGM)       284.   
      CALL DLNSEG('VDIM',0.160,0.680,0.730,0.541,0.610,0.620,SEGM)       285.   
      CALL DLNSEG('VDIM',0.160,0.680,0.730,0.541,0.630,0.640,SEGM)       286.   
      CALL DLNSEG('VDIM',0.160,0.680,0.730,0.541,0.650,0.660,SEGM)       287.   
      CALL DLNSEG('VDIM',0.160,0.680,0.730,0.541,0.670,0.680,SEGM)       288.   
      CALL DLNSEG('VDIM',0.160,0.680,0.730,0.541,0.690,0.700,SEGM)       289.   
      CALL DLNSEG('VDIM',0.160,0.680,0.730,0.541,0.710,0.720,SEGM)       290.   
C    DRAW THE SCRD LINE.                                                 291.   
      CALL DARROW('VDIM',0.778,0.555,0.230,0.700,SEGM)                   292.   
      CALL DLNSEG('VDIM',0.780,0.555,0.230,0.700,0.778,0.750,SEGM)       293.   
      CALL DLNSEG('VDIM',0.780,0.555,0.230,0.700,0.740,0.730,SEGM)       294.   
      CALL DLNSEG('VDIM',0.780,0.555,0.230,0.700,0.720,0.710,SEGM)       295.   
      CALL DLNSEG('VDIM',0.780,0.555,0.230,0.700,0.700,0.690,SEGM)       296.   
      CALL DLNSEG('VDIM',0.780,0.555,0.230,0.700,0.680,0.670,SEGM)       297.   
      CALL DLNSEG('VDIM',0.780,0.555,0.230,0.700,0.660,0.650,SEGM)       298.   
      CALL DLNSEG('VDIM',0.780,0.555,0.230,0.700,0.640,0.630,SEGM)       299.   
      CALL DLNSEG('VDIM',0.780,0.555,0.230,0.700,0.620,0.610,SEGM)       300.   
      CALL DLNSEG('VDIM',0.780,0.555,0.230,0.700,0.600,0.590,SEGM)       301.   
      CALL DLNSEG('VDIM',0.780,0.555,0.230,0.700,0.580,0.570,SEGM)       302.   
      CALL DLNSEG('VDIM',0.780,0.555,0.230,0.700,0.560,0.460,SEGM)       303.   
      CALL UGXTXT('SIZE=0.02',0.395,0.650,'SCRD','    ',SEGM)            304.   
      CALL DLNSEG('VDIM',0.780,0.555,0.230,0.700,0.380,0.230,SEGM)       305.   
      CALL DARROW('VDIM',0.230,0.700,0.780,0.555,SEGM)                   306.   
C    DRAW X-Y-Z TRIHEDRAL.                                               307.   
      CALL UGLINE('VBRIGHT',0.280,0.270,0,SEGM)                          308.   
      CALL UGLINE('VBRIGHT',0.150,0.200,1,SEGM)                          309.   
      CALL DARROW('VBRIGHT',0.150,0.200,0.280,0.270,SEGM)                310.   
      CALL UGXTXT('SIZE=0.02',0.130,0.200,'X',' ',SEGM)                  311.   
      CALL UGLINE('VBRIGHT',0.280,0.270,0,SEGM)                          312.   
      CALL UGLINE('VBRIGHT',0.450,0.270,1,SEGM)                          313.   
      CALL DARROW('VBRIGHT',0.450,0.270,0.280,0.270,SEGM)                314.   
      CALL UGXTXT('SIZE=0.02',0.450,0.290,'Y',' ',SEGM)                  315.   
      CALL UGLINE('VBRIGHT',0.280,0.270,0,SEGM)                          316.   
      CALL UGLINE('VBRIGHT',0.280,0.460,1,SEGM)                          317.   
      CALL DARROW('VBRIGHT',0.280,0.460,0.280,0.270,SEGM)                318.   
      CALL UGXTXT('SIZE=0.02',0.260,0.460,'Z',' ',SEGM)                  319.   
      CALL UGWRIT(' ',0,SEGM)                                            320.   
C                                                                        321.   
C  PRODUCE FIGURE SHOWING SMOOTH CURVE INTERPOLATION.                    322.   
      CALL UGFONT('DUPLEX')                                              323.   
      CALL PCINIT(P751,16.5*FACT,1,SEGM,NSEG)                            324.   
C    DRAW THE INTERPOLATION CURVE.                                       325.   
      CALL UGSCIN('GENTAN,NPARM=32',DLN751,X751(2),Y751(2),2,1.0,1,      326.   
     X  1,X751(1),Y751(1),1,X751(4),Y751(4))                             327.   
      CALL UGXTXT('SIZE=0.02',A751(1,3)+0.025,A751(2,3)+0.025,           328.   
     X  'K(T)',                                                          329.   
     X  '  L ',SEGM)                                                     330.   
C    DRAW THE CONTROL POLYGON.                                           331.   
      CALL UGPLIN('VDIM',X751,Y751,4,1,1,SEGM)                           332.   
      CALL UGXTXT('SIZE=0.02,RIGHT',                                     333.   
     X  0.5*(X751(1)+X751(2))-0.025,0.5*(Y751(1)+Y751(2)),               334.   
     X  'D1','  ',SEGM)                                                  335.   
      CALL UGXTXT('SIZE=0.02,CENTER',                                    336.   
     X  0.5*(X751(2)+X751(3)),0.5*(Y751(2)+Y751(3))+0.025,               337.   
     X  'D2','  ',SEGM)                                                  338.   
      CALL UGXTXT('SIZE=0.02',                                           339.   
     X  0.5*(X751(3)+X751(4))+0.025,0.5*(Y751(3)+Y751(4)),               340.   
     X  'D3','  ',SEGM)                                                  341.   
C    DRAW THE CONTROL POINTS.                                            342.   
      CALL DPOINT(X751(1),Y751(1),SEGM)                                  343.   
      CALL DPOINT(X751(2),Y751(2),SEGM)                                  344.   
      CALL DPOINT(X751(3),Y751(3),SEGM)                                  345.   
      CALL DPOINT(X751(4),Y751(4),SEGM)                                  346.   
      CALL UGXTXT('SIZE=0.02',X751(1)+0.025,Y751(1),                     347.   
     X  'K0','  ',SEGM)                                                  348.   
      CALL UGXTXT('SIZE=0.02',X751(2)+0.025,Y751(2)-0.025,               349.   
     X  'K1,A1,T=0',                                                     350.   
     X  '      L  ',SEGM)                                                351.   
      CALL UGXTXT('SIZE=0.02,RIGHT',X751(3)-0.025,Y751(3)-0.025,         352.   
     X  'K2,A2,T=1',                                                     353.   
     X  '      L  ',SEGM)                                                354.   
      CALL UGXTXT('SIZE=0.02,RIGHT',X751(4)-0.025,Y751(4),               355.   
     X  'K3','  ',SEGM)                                                  356.   
C    DRAW THE TANGENT VECTORS.                                           357.   
      A751(1,4)=X751(2)+0.200*A751(1,1)                                  358.   
      A751(2,4)=Y751(2)+0.200*A751(2,1)                                  359.   
      A751(1,5)=X751(3)+0.200*A751(1,2)                                  360.   
      A751(2,5)=Y751(3)+0.200*A751(2,2)                                  361.   
      CALL UGLINE(' ',X751(2),Y751(2),0,SEGM)                            362.   
      CALL UGLINE(' ',A751(1,4),A751(2,4),1,SEGM)                        363.   
      CALL DARROW(' ',A751(1,4),A751(2,4),X751(2),Y751(2),SEGM)          364.   
      CALL UGLINE(' ',X751(3),Y751(3),0,SEGM)                            365.   
      CALL UGLINE(' ',A751(1,5),A751(2,5),1,SEGM)                        366.   
      CALL DARROW(' ',A751(1,5),A751(2,5),X751(3),Y751(3),SEGM)          367.   
      CALL UGXTXT('SIZE=0.02',A751(1,4)+0.025,A751(2,4),                 368.   
     X  'T1','  ',SEGM)                                                  369.   
      CALL UGXTXT('SIZE=0.02',A751(1,5)+0.025,A751(2,5),                 370.   
     X  'T2','  ',SEGM)                                                  371.   
      CALL UGWRIT(' ',0,SEGM)                                            372.   
C                                                                        373.   
C  PRODUCE FIGURE SHOWING THE CROSS-HATCHING SCHEME.                     374.   
      CALL UGFONT('DUPLEX')                                              375.   
      CALL PCINIT(P761,16.5*FACT,1,SEGM,NSEG)                            376.   
C    DRAW THE CROSS-HATCH LINES.                                         377.   
      CALL UGLINE(' ',0.100,0.075,0,SEGM)                                378.   
      CALL UGLINE(' ',0.800,0.250,1,SEGM)                                379.   
      CALL UGXTXT('SIZE=0.02',0.825,0.250,'I=-1','    ',SEGM)            380.   
      CALL UGLINE(' ',0.100,0.200,0,SEGM)                                381.   
      CALL UGLINE(' ',0.800,0.375,1,SEGM)                                382.   
      CALL UGXTXT('SIZE=0.02',0.825,0.375,'I=0','   ',SEGM)              383.   
      CALL UGLINE(' ',0.100,0.325,0,SEGM)                                384.   
      CALL UGLINE(' ',0.800,0.500,1,SEGM)                                385.   
      CALL UGXTXT('SIZE=0.02',0.825,0.500,'I=1','   ',SEGM)              386.   
      CALL UGLINE(' ',0.100,0.450,0,SEGM)                                387.   
      CALL UGLINE(' ',0.800,0.625,1,SEGM)                                388.   
      CALL UGXTXT('SIZE=0.02',0.825,0.625,'I=2','   ',SEGM)              389.   
C    DRAW THE REFERENCE POINTS.                                          390.   
      CALL DPOINT(0.341,0.510,SEGM)                                      391.   
      CALL DPOINT(0.371,0.393,SEGM)                                      392.   
      CALL DPOINT(0.400,0.275,SEGM)                                      393.   
      CALL DPOINT(0.429,0.157,SEGM)                                      394.   
C    DRAW THE I=0 LABELS.                                                395.   
      CALL UGLINE('VDIM',0.375,0.275,0,SEGM)                             396.   
      CALL UGLINE('VDIM',0.325,0.275,1,SEGM)                             397.   
      CALL UGXTXT('SIZE=0.02,RIGHT',0.305,0.275,'YCRD','    ',SEGM)      398.   
      CALL UGLINE('VDIM',0.400,0.260,0,SEGM)                             399.   
      CALL UGLINE('VDIM',0.400,0.235,1,SEGM)                             400.   
      CALL UGXTXT('SIZE=0.02,CENTER',0.400,0.215,'XCRD','    ',SEGM)     401.   
      CALL UGLINE('VDIM',0.425,0.275,0,SEGM)                             402.   
      CALL UGLINE('VDIM',0.675,0.275,1,SEGM)                             403.   
      CALL UGXTXT('SIZE=0.02',0.600,0.300,'ANGL','    ',SEGM)            404.   
C    DRAW THE SPACE LABELS.                                              405.   
      CALL DARROW('VDIM',0.500,0.300,0.471,0.418,SEGM)                   406.   
      CALL UGLINE('VDIM',0.500,0.300,0,SEGM)                             407.   
      CALL UGLINE('VDIM',0.471,0.418,1,SEGM)                             408.   
      CALL DARROW('VDIM',0.471,0.418,0.500,0.300,SEGM)                   409.   
      CALL UGXTXT('SIZE=0.02',0.510,0.375,'SPAC','    ',SEGM)            410.   
      CALL DARROW('VDIM',0.500,0.425,0.471,0.543,SEGM)                   411.   
      CALL UGLINE('VDIM',0.500,0.425,0,SEGM)                             412.   
      CALL UGLINE('VDIM',0.471,0.543,1,SEGM)                             413.   
      CALL DARROW('VDIM',0.471,0.543,0.500,0.425,SEGM)                   414.   
      CALL UGXTXT('SIZE=0.02',0.510,0.500,'SPAC','    ',SEGM)            415.   
C    DRAW THE REFERENCE POINT LABELS.                                    416.   
      CALL DARROW('VDIM',0.361,0.403,0.125,0.625,SEGM)                   417.   
      CALL UGLINE('VDIM',0.361,0.403,0,SEGM)                             418.   
      CALL UGLINE('VDIM',0.125,0.625,1,SEGM)                             419.   
      CALL UGLINE('VDIM',0.275,0.625,1,SEGM)                             420.   
      CALL DARROW('VDIM',0.331,0.520,0.225,0.625,SEGM)                   421.   
      CALL UGLINE('VDIM',0.331,0.520,0,SEGM)                             422.   
      CALL UGLINE('VDIM',0.225,0.625,1,SEGM)                             423.   
      CALL UGXTXT('SIZE=0.02',0.300,0.625,'REFERENCE POINTS',            424.   
     X                                    ' LLLLLLLL  LLLLL',SEGM)       425.   
      CALL UGWRIT(' ',0,SEGM)                                            426.   
C                                                                        427.   
C  PRODUCE FIGURE SHOWING CONTOUR PLOT LABELING SCHEME.                  428.   
      CALL UGFONT('DUPLEX')                                              429.   
      CALL PCINIT(P781,16.5*FACT,1,SEGM,NSEG)                            430.   
C    DRAW THE SURFACE PATCH.                                             431.   
      CALL UGLINE(' ',0.150,0.125,0,SEGM)                                432.   
      CALL UGLINE(' ',0.850,0.125,1,SEGM)                                433.   
      CALL UGLINE(' ',0.850,0.525,1,SEGM)                                434.   
      CALL UGLINE(' ',0.150,0.525,1,SEGM)                                435.   
      CALL UGLINE(' ',0.150,0.125,1,SEGM)                                436.   
      CALL DPOINT(0.150,0.125,SEGM)                                      437.   
C    DRAW THE CORNER LABELS.                                             438.   
      CALL UGXTXT('SIZE=0.02,CENTER',0.150,0.075,                        439.   
     X  '(IROW-1,ICOL-1)',                                               440.   
     X  '               ',SEGM)                                          441.   
      CALL DPOINT(0.850,0.125,SEGM)                                      442.   
      CALL UGXTXT('SIZE=0.02,CENTER',0.850,0.075,                        443.   
     X  '(IROW-1,ICOL)',                                                 444.   
     X  '             ',SEGM)                                            445.   
      CALL DPOINT(0.850,0.525,SEGM)                                      446.   
      CALL UGXTXT('SIZE=0.02,CENTER',0.850,0.575,                        447.   
     X  '(IROW,ICOL)',                                                   448.   
     X  '           ',SEGM)                                              449.   
      CALL DPOINT(0.150,0.525,SEGM)                                      450.   
      CALL UGXTXT('SIZE=0.02,CENTER',0.150,0.575,                        451.   
     X  '(IROW,ICOL-1)',                                                 452.   
     X  '             ',SEGM)                                            453.   
C    DRAW THE SIDE LABELS.                                               454.   
      CALL UGXTXT('SIZE=0.02,CENTER',0.500,0.165,'ISID=1',               455.   
     X                                           '      ',SEGM)          456.   
      CALL UGXTXT('SIZE=0.02,RIGHT',0.810,0.325,'ISID=2',                457.   
     X                                          '      ',SEGM)           458.   
      CALL UGXTXT('SIZE=0.02,CENTER',0.500,0.485,'ISID=3',               459.   
     X                                           '      ',SEGM)          460.   
      CALL UGXTXT('SIZE=0.02',0.190,0.325,'ISID=0',                      461.   
     X                                    '      ',SEGM)                 462.   
      CALL UGWRIT(' ',0,SEGM)                                            463.   
C                                                                        464.   
C  PRODUCE FIGURE SHOWING CONTOUR PLOT PROBLEM.                          465.   
      CALL UGFONT('DUPLEX')                                              466.   
      CALL PCINIT(P782,16.5*FACT,1,SEGM,NSEG)                            467.   
C    DRAW THE FIRST CONFIGURATION.                                       468.   
      CALL UGLINE(' ',0.075,0.150,0,SEGM)                                469.   
      CALL UGLINE(' ',0.325,0.150,1,SEGM)                                470.   
      CALL UGLINE(' ',0.325,0.400,1,SEGM)                                471.   
      CALL UGLINE(' ',0.075,0.400,1,SEGM)                                472.   
      CALL UGLINE(' ',0.075,0.150,1,SEGM)                                473.   
      CALL DPOINT(0.075,0.250,SEGM)                                      474.   
      CALL DPOINT(0.175,0.400,SEGM)                                      475.   
      CALL DPOINT(0.225,0.150,SEGM)                                      476.   
      CALL DPOINT(0.325,0.300,SEGM)                                      477.   
      CALL UGLINE(' ',0.075,0.250,0,SEGM)                                478.   
      CALL UGLINE(' ',0.175,0.400,1,SEGM)                                479.   
      CALL UGLINE(' ',0.225,0.150,0,SEGM)                                480.   
      CALL UGLINE(' ',0.325,0.300,1,SEGM)                                481.   
C    DRAW THE SECOND CONFIGURATION.                                      482.   
      CALL UGLINE(' ',0.375,0.150,0,SEGM)                                483.   
      CALL UGLINE(' ',0.625,0.150,1,SEGM)                                484.   
      CALL UGLINE(' ',0.625,0.400,1,SEGM)                                485.   
      CALL UGLINE(' ',0.375,0.400,1,SEGM)                                486.   
      CALL UGLINE(' ',0.375,0.150,1,SEGM)                                487.   
      CALL DPOINT(0.375,0.250,SEGM)                                      488.   
      CALL DPOINT(0.475,0.400,SEGM)                                      489.   
      CALL DPOINT(0.525,0.150,SEGM)                                      490.   
      CALL DPOINT(0.625,0.300,SEGM)                                      491.   
      CALL UGLINE(' ',0.375,0.250,0,SEGM)                                492.   
      CALL UGLINE(' ',0.525,0.150,1,SEGM)                                493.   
      CALL UGLINE(' ',0.475,0.400,0,SEGM)                                494.   
      CALL UGLINE(' ',0.625,0.300,1,SEGM)                                495.   
C    DRAW THE THIRD CONFIGURATION.                                       496.   
      CALL UGLINE(' ',0.675,0.150,0,SEGM)                                497.   
      CALL UGLINE(' ',0.925,0.150,1,SEGM)                                498.   
      CALL UGLINE(' ',0.925,0.400,1,SEGM)                                499.   
      CALL UGLINE(' ',0.675,0.400,1,SEGM)                                500.   
      CALL UGLINE(' ',0.675,0.150,1,SEGM)                                501.   
      CALL DPOINT(0.675,0.250,SEGM)                                      502.   
      CALL DPOINT(0.775,0.400,SEGM)                                      503.   
      CALL DPOINT(0.825,0.150,SEGM)                                      504.   
      CALL DPOINT(0.925,0.300,SEGM)                                      505.   
      CALL UGLINE(' ',0.675,0.250,0,SEGM)                                506.   
      CALL UGLINE(' ',0.925,0.300,1,SEGM)                                507.   
      CALL UGLINE(' ',0.775,0.400,0,SEGM)                                508.   
      CALL UGLINE(' ',0.825,0.150,1,SEGM)                                509.   
C    DRAW THE LABELS.                                                    510.   
      CALL UGXTXT('SIZE=0.02,CENTER',0.350,0.080,                        511.   
     X  'ASYMMETRIC SOLUTIONS',                                          512.   
     X  ' LLLLLLLLL  LLLLLLLL',SEGM)                                     513.   
      CALL UGXTXT('SIZE=0.02,CENTER',0.800,0.100,'SYMMETRIC',            514.   
     X                                           ' LLLLLLLL',SEGM)       515.   
      CALL UGXTXT('SIZE=0.02,CENTER',0.800,0.060,'SOLUTION',             516.   
     X                                           ' LLLLLLL',SEGM)        517.   
      CALL UGWRIT(' ',0,SEGM)                                            518.   
C                                                                        519.   
C  PRODUCE FIGURE SHOWING MESH SURFACE HEIGHT FUNCTION.                  520.   
      CALL UGFONT('DUPLEX')                                              521.   
      CALL PCINIT(P791,16.5*FACT,1,SEGM,NSEG)                            522.   
C    DRAW THE FIRST PICTURE.                                             523.   
      CALL UGLINE(' ',0.100,0.150,0,SEGM)                                524.   
      CALL UGLINE(' ',0.100,0.300,1,SEGM)                                525.   
      CALL UGLINE(' ',0.175,0.275,1,SEGM)                                526.   
      CALL UGLINE(' ',0.250,0.350,1,SEGM)                                527.   
      CALL UGLINE(' ',0.325,0.225,1,SEGM)                                528.   
      CALL UGLINE(' ',0.400,0.200,1,SEGM)                                529.   
      CALL UGLINE(' ',0.400,0.150,1,SEGM)                                530.   
      CALL DPOINT(0.400,0.200,SEGM)                                      531.   
      CALL DPOINT(0.325,0.225,SEGM)                                      532.   
      CALL DPOINT(0.250,0.350,SEGM)                                      533.   
      CALL DPOINT(0.175,0.275,SEGM)                                      534.   
      CALL DPOINT(0.100,0.300,SEGM)                                      535.   
      CALL UGXTXT('SIZE=0.02,CENTER',0.250,0.100,                        536.   
     X  'HEIGHT FUNCTION',                                               537.   
     X  ' LLLLL  LLLLLLL',SEGM)                                          538.   
      CALL UGXTXT('SIZE=0.02,CENTER',0.250,0.060,                        539.   
     X  'AFTER FIRST LINE',                                              540.   
     X  'LLLLL  LLLL  LLL',SEGM)                                         541.   
C    DRAW THE SECOND PICTURE.                                            542.   
      CALL UGLINE(' ',0.525,0.150,0,SEGM)                                543.   
      CALL UGLINE(' ',0.525,0.300,1,SEGM)                                544.   
      CALL UGLINE(' ',0.600,0.275,1,SEGM)                                545.   
      CALL UGLINE(' ',0.600,0.350,1,SEGM)                                546.   
      CALL UGLINE(' ',0.656,0.331,1,SEGM)                                547.   
      CALL UGLINE(' ',0.675,0.350,1,SEGM)                                548.   
      CALL UGLINE(' ',0.694,0.319,1,SEGM)                                549.   
      CALL UGLINE(' ',0.900,0.250,1,SEGM)                                550.   
      CALL UGLINE(' ',0.900,0.150,1,SEGM)                                551.   
      CALL DPOINT(0.900,0.250,SEGM)                                      552.   
      CALL DPOINT(0.825,0.275,SEGM)                                      553.   
      CALL DPOINT(0.750,0.300,SEGM)                                      554.   
      CALL DPOINT(0.600,0.350,SEGM)                                      555.   
      CALL DPOINT(0.825,0.200,SEGM)                                      556.   
      CALL DPOINT(0.750,0.225,SEGM)                                      557.   
      CALL DPOINT(0.675,0.350,SEGM)                                      558.   
      CALL DPOINT(0.600,0.275,SEGM)                                      559.   
      CALL DPOINT(0.525,0.300,SEGM)                                      560.   
      CALL DLNSEG('VDIM',0.600,0.275,0.675,0.350,0.600,0.609,SEGM)       561.   
      CALL DLNSEG('VDIM',0.600,0.275,0.675,0.350,0.618,0.628,SEGM)       562.   
      CALL DLNSEG('VDIM',0.600,0.275,0.675,0.350,0.638,0.647,SEGM)       563.   
      CALL DLNSEG('VDIM',0.675,0.350,0.750,0.225,0.694,0.700,SEGM)       564.   
      CALL DLNSEG('VDIM',0.675,0.350,0.750,0.225,0.706,0.713,SEGM)       565.   
      CALL DLNSEG('VDIM',0.675,0.350,0.750,0.225,0.719,0.725,SEGM)       566.   
      CALL DLNSEG('VDIM',0.675,0.350,0.750,0.225,0.731,0.738,SEGM)       567.   
      CALL DLNSEG('VDIM',0.675,0.350,0.750,0.225,0.744,0.750,SEGM)       568.   
      CALL DLNSEG('VDIM',0.750,0.225,0.825,0.200,0.763,0.775,SEGM)       569.   
      CALL DLNSEG('VDIM',0.750,0.225,0.825,0.200,0.788,0.800,SEGM)       570.   
      CALL DLNSEG('VDIM',0.750,0.225,0.825,0.200,0.813,0.825,SEGM)       571.   
      CALL UGLINE('VDIM',0.825,0.188,0,SEGM)                             572.   
      CALL UGLINE('VDIM',0.825,0.175,1,SEGM)                             573.   
      CALL UGLINE('VDIM',0.825,0.163,0,SEGM)                             574.   
      CALL UGLINE('VDIM',0.825,0.150,1,SEGM)                             575.   
      CALL UGXTXT('SIZE=0.02,CENTER',0.712,0.100,                        576.   
     X  'HEIGHT FUNCTION',                                               577.   
     X  ' LLLLL  LLLLLLL',SEGM)                                          578.   
      CALL UGXTXT('SIZE=0.02,CENTER',0.712,0.060,                        579.   
     X  'AFTER SECOND LINE',                                             580.   
     X  'LLLLL  LLLLL  LLL',SEGM)                                        581.   
      CALL UGWRIT(' ',0,SEGM)                                            582.   
C                                                                        583.   
C  PRODUCE FIGURE SHOWING MESH SURFACE PROBLEM.                          584.   
      CALL UGFONT('DUPLEX')                                              585.   
      CALL PCINIT(P792,16.5*FACT,1,SEGM,NSEG)                            586.   
      DO 102 INT1=1,4                                                    587.   
        IF (INT1.EQ.1) THEN                                              588.   
          FLT1=0.275                                                     589.   
          FLT2=0.500                                                     590.   
        ELSE IF (INT1.EQ.2) THEN                                         591.   
          FLT1=0.725                                                     592.   
          FLT2=0.500                                                     593.   
        ELSE IF (INT1.EQ.3) THEN                                         594.   
          FLT1=0.275                                                     595.   
          FLT2=0.200                                                     596.   
        ELSE                                                             597.   
          FLT1=0.725                                                     598.   
          FLT2=0.200                                                     599.   
        END IF                                                           600.   
C    DRAW THE POINTS.                                                    601.   
        DO 101 INT2=1,24                                                 602.   
          CALL DPOINT(FLT1+X792(INT2),FLT2+Y792(INT2),SEGM)              603.   
  101   CONTINUE                                                         604.   
C    DRAW THE X LINES.                                                   605.   
        IF (INT1.NE.2) THEN                                              606.   
          CALL UGLINE(' ',FLT1-0.200,FLT2,0,SEGM)                        607.   
          CALL UGLINE(' ',FLT1,FLT2-0.100,1,SEGM)                        608.   
          CALL UGLINE(' ',FLT1+0.050,FLT2-0.075,0,SEGM)                  609.   
          CALL UGLINE(' ',FLT1-0.150,FLT2+0.025,1,SEGM)                  610.   
          CALL UGLINE(' ',FLT1-0.100,FLT2+0.050,0,SEGM)                  611.   
          CALL UGLINE(' ',FLT1-0.050,FLT2+0.025,1,SEGM)                  612.   
          CALL UGLINE(' ',FLT1,FLT2+0.075,1,SEGM)                        613.   
          CALL UGLINE(' ',FLT1+0.050,FLT2-0.025,1,SEGM)                  614.   
          CALL UGLINE(' ',FLT1+0.100,FLT2-0.050,1,SEGM)                  615.   
          CALL UGLINE(' ',FLT1+0.150,FLT2-0.025,0,SEGM)                  616.   
          CALL UGLINE(' ',FLT1+0.050,FLT2+0.025,1,SEGM)                  617.   
          IF (INT1.NE.4)                                                 618.   
     X      CALL UGLINE(' ',FLT1+0.017,FLT2+0.042,1,SEGM)                619.   
          CALL UGLINE(' ',FLT1-0.017,FLT2+0.058,0,SEGM)                  620.   
          CALL UGLINE(' ',FLT1-0.050,FLT2+0.075,1,SEGM)                  621.   
          CALL UGLINE(' ',FLT1,FLT2+0.100,0,SEGM)                        622.   
          CALL UGLINE(' ',FLT1+0.200,FLT2,1,SEGM)                        623.   
        END IF                                                           624.   
C    DRAW THE Y LINES.                                                   625.   
        IF (INT1.NE.1) THEN                                              626.   
          CALL UGLINE(' ',FLT1+0.200,FLT2,0,SEGM)                        627.   
          CALL UGLINE(' ',FLT1,FLT2-0.100,1,SEGM)                        628.   
          CALL UGLINE(' ',FLT1-0.050,FLT2-0.075,0,SEGM)                  629.   
          CALL UGLINE(' ',FLT1+0.150,FLT2+0.025,1,SEGM)                  630.   
          CALL UGLINE(' ',FLT1+0.100,FLT2+0.050,0,SEGM)                  631.   
          CALL UGLINE(' ',FLT1+0.050,FLT2+0.025,1,SEGM)                  632.   
          CALL UGLINE(' ',FLT1,FLT2+0.075,1,SEGM)                        633.   
          CALL UGLINE(' ',FLT1-0.050,FLT2-0.025,1,SEGM)                  634.   
          CALL UGLINE(' ',FLT1-0.100,FLT2-0.050,1,SEGM)                  635.   
          CALL UGLINE(' ',FLT1-0.150,FLT2-0.025,0,SEGM)                  636.   
          CALL UGLINE(' ',FLT1-0.050,FLT2+0.025,1,SEGM)                  637.   
          IF (INT1.NE.4)                                                 638.   
     X      CALL UGLINE(' ',FLT1-0.017,FLT2+0.042,1,SEGM)                639.   
          CALL UGLINE(' ',FLT1+0.017,FLT2+0.058,0,SEGM)                  640.   
          CALL UGLINE(' ',FLT1+0.050,FLT2+0.075,1,SEGM)                  641.   
          CALL UGLINE(' ',FLT1,FLT2+0.100,0,SEGM)                        642.   
          CALL UGLINE(' ',FLT1-0.200,FLT2,1,SEGM)                        643.   
        END IF                                                           644.   
  102 CONTINUE                                                           645.   
C    DRAW THE LABELS.                                                    646.   
      CALL UGXTXT('SIZE=0.02,CENTER',0.275,0.375,                        647.   
     X  'LINES OF CONSTANT X',                                           648.   
     X  ' LLLL LL  LLLLLLL  ',SEGM)                                      649.   
      CALL UGXTXT('SIZE=0.02,CENTER',0.725,0.375,                        650.   
     X  'LINES OF CONSTANT Y',                                           651.   
     X  ' LLLL LL  LLLLLLL  ',SEGM)                                      652.   
      CALL UGXTXT('SIZE=0.02,CENTER',0.275,0.075,                        653.   
     X  'X-Y SOLUTIONS COMBINED',                                        654.   
     X  '     LLLLLLLL  LLLLLLL',SEGM)                                   655.   
      CALL UGXTXT('SIZE=0.02,CENTER',0.725,0.075,                        656.   
     X  'CORRECT SOLUTION',                                              657.   
     X  ' LLLLLL  LLLLLLL',SEGM)                                         658.   
      CALL UGWRIT(' ',0,SEGM)                                            659.   
C                                                                        660.   
C  PRODUCE FIGURE SHOWING WORKSTATION PROJECTION SCHEME.                 661.   
      CALL UGFONT('DUPLEX')                                              662.   
      CALL PCINIT(P9X1,16.5*FACT,1,SEGM,NSEG)                            663.   
C    DRAW THE SCREEN.                                                    664.   
      CALL UGLINE(' ',0.500,0.175,0,SEGM)                                665.   
      CALL UGLINE(' ',0.800,0.288,1,SEGM)                                666.   
      CALL UGLINE(' ',0.800,0.725,1,SEGM)                                667.   
      CALL UGLINE(' ',0.500,0.650,1,SEGM)                                668.   
      CALL UGLINE('VBRIGHT',0.500,0.650,0,SEGM)                          669.   
      CALL UGLINE('VBRIGHT',0.500,0.175,1,SEGM)                          670.   
C    DRAW THE SCREEN AXES.                                               671.   
      CALL UGLINE('VDIM',0.800,0.513,0,SEGM)                             672.   
      CALL UGLINE('VDIM',0.675,0.477,1,SEGM)                             673.   
      CALL UGLINE('VBRIGHT',0.675,0.477,0,SEGM)                          674.   
      CALL UGLINE('VBRIGHT',0.400,0.400,1,SEGM)                          675.   
      CALL DARROW('VBRIGHT',0.400,0.400,0.675,0.477,SEGM)                676.   
      CALL UGXTXT('SIZE=0.02',0.400,0.375,'V1','  ',SEGM)                677.   
      CALL UGLINE('VDIM',0.675,0.241,0,SEGM)                             678.   
      CALL UGLINE('VDIM',0.675,0.477,1,SEGM)                             679.   
      CALL UGLINE('VBRIGHT',0.675,0.477,0,SEGM)                          680.   
      CALL UGLINE('VBRIGHT',0.675,0.800,1,SEGM)                          681.   
      CALL DARROW('VBRIGHT',0.675,0.800,0.675,0.477,SEGM)                682.   
      CALL UGXTXT('SIZE=0.02',0.705,0.800,'V2','  ',SEGM)                683.   
C    DRAW THE SCREEN LABELS.                                             684.   
      CALL UGLINE('VDIM',0.675,0.625,0,SEGM)                             685.   
      CALL UGLINE('VDIM',0.575,0.600,1,SEGM)                             686.   
      CALL UGXTXT('SIZE=0.02',0.625,0.592,'T','L',SEGM)                  687.   
      CALL UGLINE('VDIM',0.575,0.600,0,SEGM)                             688.   
      CALL UGLINE('VDIM',0.575,0.449,1,SEGM)                             689.   
      CALL UGXTXT('SIZE=0.02',0.595,0.540,'U','L',SEGM)                  690.   
      CALL DPOINT(0.575,0.600,SEGM)                                      691.   
      CALL UGXTXT('SIZE=0.02',0.575,0.625,'Q',' ',SEGM)                  692.   
C    DRAW THE PROJECTION LINE.                                           693.   
      CALL DLNSEG('VDIM',0.100,0.550,0.575,0.600,0.100,0.500,SEGM)       694.   
      CALL DLNSEG('VDIM',0.100,0.550,0.575,0.600,0.510,0.520,SEGM)       695.   
      CALL DLNSEG('VDIM',0.100,0.550,0.575,0.600,0.530,0.540,SEGM)       696.   
      CALL DLNSEG('VDIM',0.100,0.550,0.575,0.600,0.550,0.560,SEGM)       697.   
      CALL DLNSEG('VDIM',0.100,0.550,0.575,0.600,0.570,0.575,SEGM)       698.   
      CALL DPOINT(0.338,0.575,SEGM)                                      699.   
      CALL UGXTXT('SIZE=0.02',0.338,0.600,'P',' ',SEGM)                  700.   
C    DRAW THE EYE COMPLEX.                                               701.   
      CALL DPOINT(0.100,0.550,SEGM)                                      702.   
      CALL UGXTXT('SIZE=0.02',0.100,0.575,'E',' ',SEGM)                  703.   
      CALL DLNSEG('VDIM',0.100,0.550,0.675,0.475,0.100,0.500,SEGM)       704.   
      CALL DLNSEG('VDIM',0.100,0.550,0.675,0.475,0.510,0.520,SEGM)       705.   
      CALL DLNSEG('VDIM',0.100,0.550,0.675,0.475,0.530,0.540,SEGM)       706.   
      CALL DLNSEG('VDIM',0.100,0.550,0.675,0.475,0.550,0.560,SEGM)       707.   
      CALL DLNSEG('VDIM',0.100,0.550,0.675,0.475,0.570,0.580,SEGM)       708.   
      CALL DLNSEG('VDIM',0.100,0.550,0.675,0.475,0.590,0.600,SEGM)       709.   
      CALL DLNSEG('VDIM',0.100,0.550,0.675,0.475,0.610,0.620,SEGM)       710.   
      CALL DLNSEG('VDIM',0.100,0.550,0.675,0.475,0.630,0.640,SEGM)       711.   
      CALL DLNSEG('VDIM',0.100,0.550,0.675,0.475,0.650,0.660,SEGM)       712.   
      CALL DLNSEG('VDIM',0.100,0.550,0.675,0.475,0.670,0.675,SEGM)       713.   
      CALL DPOINT(0.675,0.477,SEGM)                                      714.   
      CALL DLNSEG('VBRIGHT',0.100,0.550,0.675,0.475,0.675,0.900,SEGM)    715.   
      CALL DARROW('VBRIGHT',0.900,0.446,0.100,0.550,SEGM)                716.   
      CALL UGXTXT('SIZE=0.02',0.890,0.420,'V3','  ',SEGM)                717.   
      CALL UGXTXT('SIZE=0.02',0.690,0.460,'O',' ',SEGM)                  718.   
C    DRAW X-Y-Z TRIHEDRAL.                                               719.   
      CALL UGLINE('VBRIGHT',0.230,0.270,0,SEGM)                          720.   
      CALL UGLINE('VBRIGHT',0.100,0.200,1,SEGM)                          721.   
      CALL DARROW('VBRIGHT',0.100,0.200,0.230,0.270,SEGM)                722.   
      CALL UGXTXT('SIZE=0.02',0.100,0.225,'X',' ',SEGM)                  723.   
      CALL UGLINE('VBRIGHT',0.230,0.270,0,SEGM)                          724.   
      CALL UGLINE('VBRIGHT',0.400,0.270,1,SEGM)                          725.   
      CALL DARROW('VBRIGHT',0.400,0.270,0.230,0.270,SEGM)                726.   
      CALL UGXTXT('SIZE=0.02',0.400,0.290,'Y',' ',SEGM)                  727.   
      CALL UGLINE('VBRIGHT',0.230,0.270,0,SEGM)                          728.   
      CALL UGLINE('VBRIGHT',0.230,0.460,1,SEGM)                          729.   
      CALL DARROW('VBRIGHT',0.230,0.460,0.230,0.270,SEGM)                730.   
      CALL UGXTXT('SIZE=0.02',0.210,0.460,'Z',' ',SEGM)                  731.   
      CALL UGWRIT(' ',0,SEGM)                                            732.   
C                                                                        733.   
C  TERMINATE THE PROGRAM.                                                734.   
      CALL UGCLOS(' ')                                                   735.   
      STOP                                                               736.   
C                                                                        737.   
      END                                                                738.   
      SUBROUTINE DLN751(XCRD,YCRD,FLAG,DELX,DELY)                        739.   
C                                                                        740.   
C *********************************************************************  741.   
C *                                                                   *  742.   
C *  SUBROUTINE TO PROCESS THE POINTS IN THE FIGURE ILLUSTRATING THE  *  743.   
C *  DATA FOR THE SMOOTH CURVE INTERPOLATOR.                          *  744.   
C *                                                                   *  745.   
C *  THE CALLING SEQUENCE IS:                                         *  746.   
C *    CALL DLN751(XCRD,YCRD,FLAG,DELX,DELY)                          *  747.   
C *                                                                   *  748.   
C *  THE PARAMETERS IN THE CALLING SEQUENCE ARE:                      *  749.   
C *    XCRD  THE X COORDINATE OF THE LINE END POINT.                  *  750.   
C *    YCRD  THE Y COORDINATE OF THE LINE END POINT.                  *  751.   
C *    FLAG  THE BLANKING BIT (0 MEANS FIRST POINT AND 1 MEANS        *  752.   
C *          SUCCEEDING POINT).                                       *  753.   
C *    DELX  THE X COMPONENT OF THE TANGENT VECTOR.                   *  754.   
C *    DELY  THE Y COMPONENT OF THE TANGENT VECTOR.                   *  755.   
C *                                                                   *  756.   
C *********************************************************************  757.   
C                                                                        758.   
      REAL          XCRD,YCRD                                            759.   
      INTEGER       FLAG                                                 760.   
      REAL          DELX,DELY                                            761.   
C                                                                        762.   
C  COMMON BLOCK FOR THE GRAPHIC SEGMENT.                                 763.   
      SAVE          /FGGCBK/                                             764.   
C  NUMBER OF WORDS IN THE GRAPHIC SEGMENT.                               765.   
      INTEGER       NSEG                                                 766.   
      PARAMETER     (NSEG=1024)                                          767.   
C  THE DECLARATION OF THE COMMON BLOCK.                                  768.   
      COMMON        /FGGCBK/                                             769.   
     X              SEGM                                                 770.   
C  THE GRAPHIC SEGMENT.                                                  771.   
      INTEGER*4     SEGM(NSEG)                                           772.   
C                                                                        773.   
      SAVE          /FGCBKX/                                             774.   
      COMMON        /FGCBKX/                                             775.   
     X              A751                                                 776.   
      REAL          A751(2,5)                                            777.   
C                                                                        778.   
      IF (FLAG.EQ.0) THEN                                                779.   
        A751(1,1)=DELX                                                   780.   
        A751(2,1)=DELY                                                   781.   
        A751(1,3)=XCRD                                                   782.   
        A751(2,3)=YCRD                                                   783.   
      ELSE                                                               784.   
        A751(1,2)=DELX                                                   785.   
        A751(2,2)=DELY                                                   786.   
        IF (YCRD.GE.A751(2,3)) THEN                                      787.   
          A751(1,3)=XCRD                                                 788.   
          A751(2,3)=YCRD                                                 789.   
        END IF                                                           790.   
      END IF                                                             791.   
      CALL UGLINE(' ',XCRD,YCRD,FLAG,SEGM)                               792.   
      RETURN                                                             793.   
C                                                                        794.   
      END                                                                795.   
      SUBROUTINE PCINIT(PLIM,XSIZ,BFLG,SEGM,NSEG)                        796.   
C                                                                        797.   
C *********************************************************************  798.   
C *                                                                   *  799.   
C *  SUBROUTINE TO BEGIN A NEW PICTURE, INITIALIZE THE DRAWING        *  800.   
C *  SPACE, AND (OPTIONALLY) DRAW A BORDER.                           *  801.   
C *                                                                   *  802.   
C *  THE CALLING SEQUENCE IS:                                         *  803.   
C *    CALL PCINIT(PLIM,XSIZ,BFLG,SEGM,NSEG)                          *  804.   
C *                                                                   *  805.   
C *  THE PARAMETERS IN THE CALLING SEQUENCE ARE:                      *  806.   
C *    PLIM  THE PROGRAMMER LIMITS FOR THE PICTURE.                   *  807.   
C *    XSIZ  THE ACTUAL WIDTH OF THE PICTURE IN CENTIMETERS.          *  808.   
C *    BFLG  THE BORDER FLAG (0 MEANS NO BORDER AND 1 MEANS DRAW THE  *  809.   
C *          BORDER).                                                 *  810.   
C *    SEGM  THE GRAPHIC SEGMENT.                                     *  811.   
C *    NSEG  THE DIMENSION OF THE GRAPHIC SEGMENT.                    *  812.   
C *                                                                   *  813.   
C *********************************************************************  814.   
C                                                                        815.   
      REAL          PLIM(2,2)                                            816.   
      REAL          XSIZ                                                 817.   
      INTEGER       BFLG                                                 818.   
      INTEGER*4     SEGM(*)                                              819.   
      INTEGER       NSEG                                                 820.   
C                                                                        821.   
      REAL          DSPC(2,2)                                            822.   
      REAL          YSIZ,XRTO,YRTO                                       823.   
      CHARACTER*1   STRG                                                 824.   
      INTEGER       IARY(1)                                              825.   
      REAL          XARY(2)                                              826.   
C                                                                        827.   
C  START A NEW PICTURE.                                                  828.   
      CALL UGPICT('CLEAR',0)                                             829.   
C                                                                        830.   
C  OBTAIN A DRAWING SPACE WITH THE CORRECT ASPECT RATIO AND SIZE.        831.   
      CALL UGDSPC('PUT',1.0,1.0,0.0)                                     832.   
      CALL UGINFO('DSPCSIZE',STRG,IARY,XARY)                             833.   
      YSIZ=XSIZ*(PLIM(2,2)-PLIM(2,1))/(PLIM(1,2)-PLIM(1,1))              834.   
      XRTO=XSIZ/XARY(1)                                                  835.   
      YRTO=YSIZ/XARY(2)                                                  836.   
      IF (MAX(XRTO,YRTO).GT.1.0) THEN                                    837.   
        IF (XRTO.GT.YRTO) THEN                                           838.   
          YRTO=YRTO/XRTO                                                 839.   
          XRTO=1.0                                                       840.   
        ELSE                                                             841.   
          XRTO=XRTO/YRTO                                                 842.   
          YRTO=1.0                                                       843.   
        END IF                                                           844.   
      END IF                                                             845.   
      DSPC(1,1)=0.5*(1.0-XRTO)                                           846.   
      DSPC(1,2)=1.0-DSPC(1,1)                                            847.   
      DSPC(2,1)=0.5*(1.0-YRTO)                                           848.   
      DSPC(2,2)=1.0-DSPC(2,1)                                            849.   
      CALL UGWDOW('PUT',DSPC,PLIM)                                       850.   
C                                                                        851.   
C  CLEAR THE GRAPHIC SEGMENT AND DRAW A BORDER IF NECESSARY.             852.   
      CALL UGINIT('CLEAR',SEGM,NSEG)                                     853.   
      IF (BFLG.NE.0) THEN                                                854.   
        CALL UGLINE('VBRIGHT',PLIM(1,1)+0.0001,                          855.   
     X                        PLIM(2,1)+0.0001,0,SEGM)                   856.   
        CALL UGLINE('VBRIGHT',PLIM(1,2)-0.0001,                          857.   
     X                        PLIM(2,1)+0.0001,1,SEGM)                   858.   
        CALL UGLINE('VBRIGHT',PLIM(1,2)-0.0001,                          859.   
     X                        PLIM(2,2)-0.0001,1,SEGM)                   860.   
        CALL UGLINE('VBRIGHT',PLIM(1,1)+0.0001,                          861.   
     X                        PLIM(2,2)-0.0001,1,SEGM)                   862.   
        CALL UGLINE('VBRIGHT',PLIM(1,1)+0.0001,                          863.   
     X                        PLIM(2,1)+0.0001,1,SEGM)                   864.   
      END IF                                                             865.   
C                                                                        866.   
C  RETURN TO CALLING ROUTINE.                                            867.   
      RETURN                                                             868.   
C                                                                        869.   
      END                                                                870.   
      SUBROUTINE DLNSEG(INTN,XCD1,YCD1,XCD2,YCD2,XCLO,XCHI,SEGM)         871.   
C                                                                        872.   
C *********************************************************************  873.   
C *                                                                   *  874.   
C *  SUBROUTINE TO DRAW ONE SEGMENT OF A LONGER LINE.                 *  875.   
C *                                                                   *  876.   
C *  THE CALLING SEQUENCE IS:                                         *  877.   
C *    CALL DLNSEG(INTN,XCD1,YCD1,XCD2,YCD2,XCLO,XCHI,SEGM)           *  878.   
C *                                                                   *  879.   
C *  THE PARAMETERS IN THE CALLING SEQUENCE ARE:                      *  880.   
C *    INTN  THE INTENSITY LEVEL OF THE LINE.                         *  881.   
C *    XCD1  THE X COORDINATE OF ONE END OF THE LINE.                 *  882.   
C *    YCD1  THE Y COORDINATE OF ONE END OF THE LINE.                 *  883.   
C *    XCD2  THE X COORDINATE OF THE OTHER END OF THE LINE.           *  884.   
C *    YCD2  THE Y COORDINATE OF THE OTHER END OF THE LINE.           *  885.   
C *    XCLO  THE X COORDINATE OF ONE END OF THE SEGMENT.              *  886.   
C *    XCHI  THE X COORDINATE OF THE OTHER END OF THE SEGMENT.        *  887.   
C *    SEGM  THE GRAPHIC SEGMENT.                                     *  888.   
C *                                                                   *  889.   
C *********************************************************************  890.   
C                                                                        891.   
      CHARACTER*(*) INTN                                                 892.   
      REAL          XCD1,YCD1,XCD2,YCD2,XCLO,XCHI                        893.   
      INTEGER*4     SEGM(*)                                              894.   
C                                                                        895.   
      CALL UGLINE(INTN,                                                  896.   
     X  XCLO,((YCD2-YCD1)*(XCLO-XCD1)/(XCD2-XCD1))+YCD1,0,SEGM)          897.   
      CALL UGLINE(INTN,                                                  898.   
     X  XCHI,((YCD2-YCD1)*(XCHI-XCD1)/(XCD2-XCD1))+YCD1,1,SEGM)          899.   
      RETURN                                                             900.   
C                                                                        901.   
      END                                                                902.   
      SUBROUTINE DPOINT(XCRD,YCRD,SEGM)                                  903.   
C                                                                        904.   
C *********************************************************************  905.   
C *                                                                   *  906.   
C *  SUBROUTINE TO DRAW A LARGE POINT.                                *  907.   
C *                                                                   *  908.   
C *  THE CALLING SEQUENCE IS:                                         *  909.   
C *    CALL DPOINT(XCRD,YCRD,SEGM)                                    *  910.   
C *                                                                   *  911.   
C *  THE PARAMETERS IN THE CALLING SEQUENCE ARE:                      *  912.   
C *    XCRD  THE X COORDINATE OF THE POINT.                           *  913.   
C *    YCRD  THE Y COORDINATE OF THE POINT.                           *  914.   
C *    SEGM  THE GRAPHIC SEGMENT.                                     *  915.   
C *                                                                   *  916.   
C *********************************************************************  917.   
C                                                                        918.   
      REAL          XCRD,YCRD                                            919.   
      INTEGER*4     SEGM(*)                                              920.   
C                                                                        921.   
      CALL UGMARK('MARK=9,SIZE=0.010',XCRD,YCRD,SEGM)                    922.   
      CALL UGMARK('MARK=9,SIZE=0.005',XCRD,YCRD,SEGM)                    923.   
      RETURN                                                             924.   
C                                                                        925.   
      END                                                                926.   
      SUBROUTINE DARROW(INTN,XCD1,YCD1,XCD2,YCD2,SEGM)                   927.   
C                                                                        928.   
C *********************************************************************  929.   
C *                                                                   *  930.   
C *  SUBROUTINE TO DRAW AN ARROW HEAD.                                *  931.   
C *                                                                   *  932.   
C *  THE CALLING SEQUENCE IS:                                         *  933.   
C *    CALL DARROW(INTN,XCD1,YCD1,XCD2,YCD2,SEGM)                     *  934.   
C *                                                                   *  935.   
C *  THE PARAMETERS IN THE CALLING SEQUENCE ARE:                      *  936.   
C *    INTN  THE INTENSITY LEVEL OF THE ARROW.                        *  937.   
C *    XCD1  THE X COORDINATE OF THE HEAD OF THE ARROW.               *  938.   
C *    YCD1  THE Y COORDINATE OF THE HEAD OF THE ARROW.               *  939.   
C *    XCD2  THE X COORDINATE OF THE OTHER END OF THE ARROW.          *  940.   
C *    YCD2  THE Y COORDINATE OF THE OTHER END OF THE ARROW.          *  941.   
C *    SEGM  THE GRAPHIC SEGMENT.                                     *  942.   
C *                                                                   *  943.   
C *********************************************************************  944.   
C                                                                        945.   
      CHARACTER*(*) INTN                                                 946.   
      REAL          XCD1,YCD1,XCD2,YCD2                                  947.   
      INTEGER*4     SEGM(*)                                              948.   
C                                                                        949.   
      REAL          ARGU                                                 950.   
C                                                                        951.   
      ARGU=ATAN2(YCD2-YCD1,XCD2-XCD1)                                    952.   
      CALL UGLINE(INTN,XCD1,YCD1,0,SEGM)                                 953.   
      CALL UGLINE(INTN,                                                  954.   
     X  XCD1+0.02*COS(ARGU+0.25),YCD1+0.02*SIN(ARGU+0.25),1,SEGM)        955.   
      CALL UGLINE(INTN,                                                  956.   
     X  XCD1+0.02*COS(ARGU-0.25),YCD1+0.02*SIN(ARGU-0.25),1,SEGM)        957.   
      CALL UGLINE(INTN,XCD1,YCD1,1,SEGM)                                 958.   
      RETURN                                                             959.   
C                                                                        960.   
      END                                                                961.   
      SUBROUTINE UGXERR(LEVL,SNAM,INDX)                                  962.   
C                                                                        963.   
C *********************************************************************  964.   
C *                                                                   *  965.   
C *  SUBROUTINE TO PROCESS GRAPHIC SEGMENT OVERFLOWS.                 *  966.   
C *                                                                   *  967.   
C *  THE CALLING SEQUENCE IS:                                         *  968.   
C *    CALL UGXERR(LEVL,SNAM,INDX)                                    *  969.   
C *                                                                   *  970.   
C *  THE PARAMETERS IN THE CALLING SEQUENCE ARE:                      *  971.   
C *    LEVL  THE LEVEL OF THE ERROR.                                  *  972.   
C *    SNAM  THE NAME OF THE SUBROUTINE DETECTING THE ERROR.          *  973.   
C *    INDX  THE INDEX OF THE ERROR.                                  *  974.   
C *                                                                   *  975.   
C *********************************************************************  976.   
C                                                                        977.   
      INTEGER       LEVL                                                 978.   
      CHARACTER*8   SNAM                                                 979.   
      INTEGER       INDX                                                 980.   
C                                                                        981.   
C  COMMON BLOCK FOR THE GRAPHIC SEGMENT.                                 982.   
      SAVE          /FGGCBK/                                             983.   
C  NUMBER OF WORDS IN THE GRAPHIC SEGMENT.                               984.   
      INTEGER       NSEG                                                 985.   
      PARAMETER     (NSEG=1024)                                          986.   
C  THE DECLARATION OF THE COMMON BLOCK.                                  987.   
      COMMON        /FGGCBK/                                             988.   
     X              SEGM                                                 989.   
C  THE GRAPHIC SEGMENT.                                                  990.   
      INTEGER*4     SEGM(NSEG)                                           991.   
C                                                                        992.   
C  CLEAR THE GRAPHIC SEGMENT IF IT HAS OVERFLOWED.                       993.   
      IF (INDX.EQ.11) THEN                                               994.   
        CALL UGWRIT(' ',0,SEGM)                                          995.   
        CALL UGINIT('CONTINUE',SEGM,NSEG)                                996.   
        LEVL=0                                                           997.   
      END IF                                                             998.   
C                                                                        999.   
C  RETURN TO CALLING ROUTINE.                                           1000.   
      RETURN                                                            1001.   
C                                                                       1002.   
      END                                                               1003.   
