      PROGRAM       PGMDOC                                                 1.   
C                                                                          2.   
C *******************  THE UNIFIED GRAPHICS SYSTEM  *******************    3.   
C *                                                                   *    4.   
C *  THIS PROGRAM WILL GENERATE THE FIGURES FOR THE UNIFIED GRAPHICS  *    5.   
C *  SYSTEM PROGRAMMING MANUAL.                                       *    6.   
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
C  DECLARATIONS FOR FIGURE SHOWING TWO-DIMENSIONAL TRANSFORMATIONS.       29.   
      REAL          P251(2,2)                                             30.   
      REAL          X251(7),Y251(7)                                       31.   
      REAL          V251(2,2),W251(2,2)                                   32.   
      REAL          S251(2,2)                                             33.   
C                                                                         34.   
C  DECLARATIONS FOR FIGURE SHOWING THREE-DIMENSIONAL TRANSFORMATIONS.     35.   
      REAL          P252(2,2)                                             36.   
C                                                                         37.   
C  DECLARATIONS FOR FIGURE SHOWING SUBROUTINE HIERARCHY.                  38.   
      REAL          P253(2,2)                                             39.   
C                                                                         40.   
C  DECLARATIONS FOR FIGURE SHOWING SIMPLEX CHARACTER SET.                 41.   
      REAL          P261(2,2)                                             42.   
C                                                                         43.   
C  DECLARATIONS FOR FIGURE SHOWING DUPLEX CHARACTER SET.                  44.   
      REAL          P262(2,2)                                             45.   
C                                                                         46.   
C  DECLARATIONS FOR FIGURE SHOWING ASTROID/ELLIPSE EXAMPLE.               47.   
      REAL          P511(2,2)                                             48.   
      REAL          V511(2,2),W511(2,2)                                   49.   
      REAL          S511(2,2),T511(2,2)                                   50.   
C                                                                         51.   
      REAL          FLT1,FLT2,FLT3,FLT4,FLT5,FLT6                         52.   
      INTEGER       INT1                                                  53.   
C                                                                         54.   
      DATA          P251/ 0.000, 0.000, 1.000, 0.500/                     55.   
      DATA          X251/ 0.100, 0.100, 0.175, 0.175, 0.250,              56.   
     X                    0.250, 0.325/                                   57.   
      DATA          Y251/ 0.125, 0.250, 0.125, 0.250, 0.125,              58.   
     X                    0.250, 0.125/                                   59.   
      DATA          V251/ 0.600, 0.100, 0.850, 0.375/                     60.   
      DATA          W251/ 0.125, 0.150, 0.400, 0.325/                     61.   
      DATA          S251/ 0.225, 0.200, 0.350, 0.275/                     62.   
      DATA          P252/ 0.000, 0.000, 1.000, 0.625/                     63.   
      DATA          P253/ 0.000, 0.000, 1.000, 0.350/                     64.   
      DATA          P261/ 0.000, 0.000,34.000,25.000/                     65.   
      DATA          P262/ 0.000, 0.000,34.000,25.000/                     66.   
      DATA          P511/ 0.000, 0.000,14.000,10.000/                     67.   
      DATA          V511/ 4.500, 0.500,13.500, 9.500/                     68.   
      DATA          W511/-1.000,-1.000, 1.000, 1.000/                     69.   
      DATA          S511/-0.800, 0.300, 0.100, 0.600/                     70.   
      DATA          T511/ 0.000,-0.600, 0.600,-0.300/                     71.   
C                                                                         72.   
C  INITIALIZE THE PROGRAM.                                                73.   
C     CALL UGOPEN('IMGN300,GENIL',99)                                     74.   
      CALL UGOPEN('POSTSCR,GENIL',99)                                     74.   
C                                                                         75.   
C  PRODUCE FIGURE SHOWING TWO-DIMENSIONAL TRANSFORMATIONS.                76.   
      CALL UGFONT('DUPLEX')                                               77.   
      CALL PCINIT(P251,16.5*FACT,1,SEGM,NSEG)                             78.   
C    DRAW THE WORLD COORDINATE SYSTEM.                                    79.   
      CALL UGXTXT('SIZE=0.02',0.062,0.450,                                80.   
     X  'WORLD COORDINATE SYSTEM',                                        81.   
     X  ' LLLL  LLLLLLLLL  LLLLL',SEGM)                                   82.   
      CALL UGLINE(' ',0.050,0.050,0,SEGM)                                 83.   
      CALL UGLINE(' ',0.450,0.050,1,SEGM)                                 84.   
      CALL UGLINE(' ',0.450,0.425,1,SEGM)                                 85.   
      CALL UGLINE(' ',0.050,0.425,1,SEGM)                                 86.   
      CALL UGLINE(' ',0.050,0.050,1,SEGM)                                 87.   
C    DRAW THE DRAWING SPACE.                                              88.   
      CALL UGXTXT('SIZE=0.02',0.562,0.450,                                89.   
     X  'DRAWING SPACE',                                                  90.   
     X  ' LLLLLL  LLLL',SEGM)                                             91.   
      CALL UGLINE(' ',0.550,0.050,0,SEGM)                                 92.   
      CALL UGLINE(' ',0.950,0.050,1,SEGM)                                 93.   
      CALL UGLINE(' ',0.950,0.425,1,SEGM)                                 94.   
      CALL UGLINE(' ',0.550,0.425,1,SEGM)                                 95.   
      CALL UGLINE(' ',0.550,0.050,1,SEGM)                                 96.   
C    DRAW THE WINDOW.                                                     97.   
      CALL UGXTXT('SIZE=0.02',0.137,0.350,                                98.   
     X  'WINDOW',                                                         99.   
     X  ' LLLLL',SEGM)                                                   100.   
      CALL UGLINE(' ',W251(1,1),W251(2,1),0,SEGM)                        101.   
      CALL UGLINE(' ',W251(1,2),W251(2,1),1,SEGM)                        102.   
      CALL UGLINE(' ',W251(1,2),W251(2,2),1,SEGM)                        103.   
      CALL UGLINE(' ',W251(1,1),W251(2,2),1,SEGM)                        104.   
      CALL UGLINE(' ',W251(1,1),W251(2,1),1,SEGM)                        105.   
C    DRAW THE SHIELD.                                                    106.   
      CALL UGXTXT('SIZE=0.02',0.237,0.300,                               107.   
     X  'SHIELD',                                                        108.   
     X  ' LLLLL',SEGM)                                                   109.   
      CALL UGLINE(' ',S251(1,1),S251(2,1),0,SEGM)                        110.   
      CALL UGLINE(' ',S251(1,2),S251(2,1),1,SEGM)                        111.   
      CALL UGLINE(' ',S251(1,2),S251(2,2),1,SEGM)                        112.   
      CALL UGLINE(' ',S251(1,1),S251(2,2),1,SEGM)                        113.   
      CALL UGLINE(' ',S251(1,1),S251(2,1),1,SEGM)                        114.   
C    DRAW THE VIEW PORT.                                                 115.   
      CALL UGXTXT('SIZE=0.02',0.612,0.400,                               116.   
     X  'VIEW PORT',                                                     117.   
     X  ' LLL  LLL',SEGM)                                                118.   
      CALL UGLINE(' ',V251(1,1),V251(2,1),0,SEGM)                        119.   
      CALL UGLINE(' ',V251(1,2),V251(2,1),1,SEGM)                        120.   
      CALL UGLINE(' ',V251(1,2),V251(2,2),1,SEGM)                        121.   
      CALL UGLINE(' ',V251(1,1),V251(2,2),1,SEGM)                        122.   
      CALL UGLINE(' ',V251(1,1),V251(2,1),1,SEGM)                        123.   
C    DRAW THE CONNECTOR LINES.                                           124.   
      CALL UGLINE('VDIM',W251(1,1),W251(2,2),0,SEGM)                     125.   
      CALL UGLINE('VDIM',V251(1,1),V251(2,2),1,SEGM)                     126.   
      CALL UGLINE('VDIM',W251(1,2),W251(2,2),0,SEGM)                     127.   
      CALL UGLINE('VDIM',V251(1,2),V251(2,2),1,SEGM)                     128.   
      CALL UGLINE('VDIM',W251(1,2),W251(2,1),0,SEGM)                     129.   
      CALL UGLINE('VDIM',V251(1,2),V251(2,1),1,SEGM)                     130.   
      CALL UGLINE('VDIM',W251(1,1),W251(2,1),0,SEGM)                     131.   
      CALL UGLINE('VDIM',V251(1,1),V251(2,1),1,SEGM)                     132.   
C    DRAW THE RAW FIGURE.                                                133.   
      CALL UGPLIN('VBRIGHT',X251,Y251,7,1,1,SEGM)                        134.   
      CALL UGWRIT(' ',0,SEGM)                                            135.   
C    DRAW THE PROCESSED FIGURE.                                          136.   
      CALL UGWDOW('PUT,WINDOW',V251,W251)                                137.   
      CALL UGSHLD('PUT',S251)                                            138.   
      CALL UGINIT('CLEAR',SEGM,NSEG)                                     139.   
      CALL UGPLIN('VBRIGHT',X251,Y251,7,1,1,SEGM)                        140.   
      CALL UGWRIT(' ',0,SEGM)                                            141.   
C                                                                        142.   
C  PRODUCE FIGURE SHOWING THREE-DIMENSIONAL TRANSFORMATIONS.             143.   
      CALL UGFONT('DUPLEX')                                              144.   
      CALL PCINIT(P252,16.5*FACT,1,SEGM,NSEG)                            145.   
C    DRAW THE WORLD VOLUME.                                              146.   
      CALL UGXTXT('SIZE=0.02',0.162,0.575,                               147.   
     X  'WORLD VOLUME',                                                  148.   
     X  ' LLLL  LLLLL',SEGM)                                             149.   
      CALL UGLINE(' ',0.050,0.450,0,SEGM)                                150.   
      CALL UGLINE(' ',0.050,0.050,1,SEGM)                                151.   
      CALL UGLINE(' ',0.450,0.050,1,SEGM)                                152.   
      CALL UGLINE(' ',0.450,0.450,1,SEGM)                                153.   
      CALL UGLINE(' ',0.050,0.450,1,SEGM)                                154.   
      CALL UGLINE(' ',0.150,0.550,1,SEGM)                                155.   
      CALL UGLINE(' ',0.550,0.550,1,SEGM)                                156.   
      CALL UGLINE(' ',0.550,0.150,1,SEGM)                                157.   
      CALL UGLINE(' ',0.450,0.050,1,SEGM)                                158.   
      CALL UGLINE(' ',0.450,0.450,0,SEGM)                                159.   
      CALL UGLINE(' ',0.550,0.550,1,SEGM)                                160.   
C    DRAW THE OBJECT VOLUME.                                             161.   
      CALL UGXTXT('SIZE=0.02',0.275,0.380,                               162.   
     X  'OBJECT',                                                        163.   
     X  ' LLLLL',SEGM)                                                   164.   
      CALL UGXTXT('SIZE=0.02',0.275,0.350,                               165.   
     X  'VOLUME',                                                        166.   
     X  ' LLLLL',SEGM)                                                   167.   
      CALL UGLINE(' ',0.250,0.300,0,SEGM)                                168.   
      CALL UGLINE(' ',0.250,0.100,1,SEGM)                                169.   
      CALL UGLINE(' ',0.350,0.100,1,SEGM)                                170.   
      CALL UGLINE(' ',0.350,0.300,1,SEGM)                                171.   
      CALL UGLINE(' ',0.250,0.300,1,SEGM)                                172.   
      CALL UGLINE(' ',0.275,0.325,1,SEGM)                                173.   
      CALL UGLINE(' ',0.375,0.325,1,SEGM)                                174.   
      CALL UGLINE(' ',0.375,0.125,1,SEGM)                                175.   
      CALL UGLINE(' ',0.350,0.100,1,SEGM)                                176.   
      CALL UGLINE(' ',0.350,0.300,0,SEGM)                                177.   
      CALL UGLINE(' ',0.375,0.325,1,SEGM)                                178.   
C    DRAW THE EYE POINT.                                                 179.   
      CALL UGXTXT('SIZE=0.02,RIGHT',0.187,0.412,                         180.   
     X  'EYE POINT',                                                     181.   
     X  ' LL  LLLL',SEGM)                                                182.   
      CALL DPOINT(0.212,0.412,SEGM)                                      183.   
      CALL UGLINE('VDIM',0.212,0.412,1,SEGM)                             184.   
      CALL UGLINE('VDIM',0.312,0.212,1,SEGM)                             185.   
C    DRAW THE CENTER OF OBJECT.                                          186.   
      CALL UGXTXT('SIZE=0.02',0.100,0.292,                               187.   
     X  'CENTER OF',                                                     188.   
     X  ' LLLLL LL',SEGM)                                                189.   
      CALL UGXTXT('SIZE=0.02',0.100,0.262,                               190.   
     X  'OBJECT',                                                        191.   
     X  ' LLLLL',SEGM)                                                   192.   
      CALL UGXTXT('SIZE=0.02',0.100,0.232,                               193.   
     X  'VOLUME',                                                        194.   
     X  ' LLLLL',SEGM)                                                   195.   
      CALL DARROW('VDIM',0.300,0.215,0.187,0.262,SEGM)                   196.   
      CALL UGLINE('VDIM',0.300,0.215,0,SEGM)                             197.   
      CALL UGLINE('VDIM',0.187,0.262,1,SEGM)                             198.   
      CALL DPOINT(0.312,0.212,SEGM)                                      199.   
C    DRAW THE UPWARD DIRECTION.                                          200.   
      CALL UGXTXT('SIZE=0.02,CENTER',0.350,0.500,                        201.   
     X  'UP DIRECTION',                                                  202.   
     X  ' L  LLLLLLLL',SEGM)                                             203.   
      CALL DARROW('VDIM',0.400,0.262,0.362,0.475,SEGM)                   204.   
      CALL UGLINE('VDIM',0.400,0.262,0,SEGM)                             205.   
      CALL UGLINE('VDIM',0.362,0.475,1,SEGM)                             206.   
      CALL DARROW('VBRIGHT',0.462,0.287,0.312,0.212,SEGM)                207.   
      CALL UGLINE('VBRIGHT',0.462,0.287,0,SEGM)                          208.   
      CALL UGLINE('VBRIGHT',0.312,0.212,1,SEGM)                          209.   
C    DRAW THE THREE-DIMENSIONAL WINDOW.                                  210.   
      CALL UGXTXT('SIZE=0.02',0.100,0.175,                               211.   
     X  '3-D',                                                           212.   
     X  '   ',SEGM)                                                      213.   
      CALL UGXTXT('SIZE=0.02',0.100,0.145,                               214.   
     X  'WINDOW',                                                        215.   
     X  ' LLLLL',SEGM)                                                   216.   
      CALL DARROW('VDIM',0.212,0.175,0.162,0.175,SEGM)                   217.   
      CALL UGLINE('VDIM',0.212,0.175,0,SEGM)                             218.   
      CALL UGLINE('VDIM',0.162,0.175,1,SEGM)                             219.   
      CALL UGLINE(' ',0.375,0.275,0,SEGM)                                220.   
      CALL UGLINE(' ',0.425,0.300,1,SEGM)                                221.   
      CALL UGLINE(' ',0.400,0.225,1,SEGM)                                222.   
      CALL UGLINE('VBRIGHT',0.400,0.225,0,SEGM)                          223.   
      CALL UGLINE('VBRIGHT',0.200,0.125,1,SEGM)                          224.   
      CALL UGLINE(' ',0.200,0.125,0,SEGM)                                225.   
      CALL UGLINE(' ',0.225,0.200,1,SEGM)                                226.   
      CALL UGLINE(' ',0.250,0.212,1,SEGM)                                227.   
C    DRAW THE DRAWING SPACE.                                             228.   
      CALL UGXTXT('SIZE=0.02',0.612,0.475,                               229.   
     X  'DRAWING SPACE',                                                 230.   
     X  ' LLLLLL  LLLL',SEGM)                                            231.   
      CALL UGLINE(' ',0.600,0.200,0,SEGM)                                232.   
      CALL UGLINE(' ',0.950,0.200,1,SEGM)                                233.   
      CALL UGLINE(' ',0.950,0.450,1,SEGM)                                234.   
      CALL UGLINE(' ',0.600,0.450,1,SEGM)                                235.   
      CALL UGLINE(' ',0.600,0.200,1,SEGM)                                236.   
C    DRAW THE THREE-DIMENSIONAL VIEW PORT.                               237.   
      CALL UGXTXT('SIZE=0.02',0.662,0.425,                               238.   
     X  '3-D VIEW PORT',                                                 239.   
     X  '     LLL  LLL',SEGM)                                            240.   
      CALL UGLINE(' ',0.650,0.250,0,SEGM)                                241.   
      CALL UGLINE(' ',0.900,0.250,1,SEGM)                                242.   
      CALL UGLINE(' ',0.900,0.400,1,SEGM)                                243.   
      CALL UGLINE(' ',0.650,0.400,1,SEGM)                                244.   
      CALL UGLINE(' ',0.650,0.250,1,SEGM)                                245.   
      CALL UGLINE('VDIM',0.700,0.250,0,SEGM)                             246.   
      CALL UGLINE('VDIM',0.700,0.400,1,SEGM)                             247.   
      CALL UGLINE('VDIM',0.850,0.250,0,SEGM)                             248.   
      CALL UGLINE('VDIM',0.850,0.400,1,SEGM)                             249.   
C    DRAW THE CONNECTOR LINES.                                           250.   
      CALL UGLINE('VDIM',0.425,0.300,0,SEGM)                             251.   
      CALL UGLINE('VDIM',0.700,0.400,1,SEGM)                             252.   
      CALL UGLINE('VDIM',0.400,0.225,0,SEGM)                             253.   
      CALL UGLINE('VDIM',0.850,0.400,1,SEGM)                             254.   
      CALL UGLINE('VDIM',0.225,0.200,0,SEGM)                             255.   
      CALL UGLINE('VDIM',0.250,0.203,1,SEGM)                             256.   
      CALL UGLINE('VDIM',0.383,0.217,0,SEGM)                             257.   
      CALL UGLINE('VDIM',0.700,0.250,1,SEGM)                             258.   
      CALL UGLINE('VDIM',0.200,0.125,0,SEGM)                             259.   
      CALL UGLINE('VDIM',0.850,0.250,1,SEGM)                             260.   
      CALL UGWRIT(' ',0,SEGM)                                            261.   
C                                                                        262.   
C  PRODUCE FIGURE SHOWING SUBROUTINE HIERARCHY.                          263.   
      CALL UGFONT('DUPLEX')                                              264.   
      CALL PCINIT(P253,16.5*FACT,1,SEGM,NSEG)                            265.   
C    DRAW THE SUBROUTINE NAMES.                                          266.   
      CALL UGXTXT('SIZE=0.02,CENTER',0.325,0.275,                        267.   
     X  'UGDSPC015*1',                                                   268.   
     X  '      ZVV Z',SEGM)                                              269.   
      CALL UGXTXT('SIZE=0.02,CENTER',0.175,0.175,                        270.   
     X  'UGWDOW','      ',SEGM)                                          271.   
      CALL UGXTXT('SIZE=0.02,CENTER',0.175,0.075,                        272.   
     X  'UGSHLD','      ',SEGM)                                          273.   
      CALL UGXTXT('SIZE=0.02,CENTER',0.475,0.175,                        274.   
     X  'UG3WRD015*1',                                                   275.   
     X  '      ZVV Z',SEGM)                                              276.   
      CALL UGXTXT('SIZE=0.02,CENTER',0.475,0.075,                        277.   
     X  'UG3TRN','      ',SEGM)                                          278.   
C    DRAW THE OTHER LABELS.                                              279.   
      CALL UGXTXT('SIZE=0.02',0.675,0.275,                               280.   
     X  'NEW PICTURE',                                                   281.   
     X  ' LL  LLLLLL',SEGM)                                              282.   
      CALL UGXTXT('SIZE=0.02',0.650,0.165,                               283.   
     X  '*',' ',SEGM)                                                    284.   
      CALL UGXTXT('SIZE=0.02',0.675,0.165,                               285.   
     X  'CANNOT BE CALLED',                                              286.   
     X  ' LLLLL LL LLLLLL',SEGM)                                         287.   
      CALL UGXTXT('SIZE=0.02',0.675,0.135,                               288.   
     X  'BETWEEN CALLS TO',                                              289.   
     X  'LLLLLLL LLLLL LL',SEGM)                                         290.   
      CALL UGXTXT('SIZE=0.02',0.675,0.105,                               291.   
     X  'UGWRIT WITHIN A',                                               292.   
     X  '       LLLLLL L',SEGM)                                          293.   
      CALL UGXTXT('SIZE=0.02',0.675,0.075,                               294.   
     X  'PICTURE.',                                                      295.   
     X  'LLLLLLL ',SEGM)                                                 296.   
C    DRAW THE ARROWS.                                                    297.   
      CALL DARROW(' ',0.187,0.197,0.300,0.252,SEGM)                      298.   
      CALL UGLINE(' ',0.187,0.197,0,SEGM)                                299.   
      CALL UGLINE(' ',0.300,0.252,1,SEGM)                                300.   
      CALL DARROW(' ',0.175,0.097,0.175,0.152,SEGM)                      301.   
      CALL UGLINE(' ',0.175,0.097,0,SEGM)                                302.   
      CALL UGLINE(' ',0.175,0.152,1,SEGM)                                303.   
      CALL DARROW(' ',0.462,0.197,0.350,0.252,SEGM)                      304.   
      CALL UGLINE(' ',0.462,0.197,0,SEGM)                                305.   
      CALL UGLINE(' ',0.350,0.252,1,SEGM)                                306.   
      CALL DARROW(' ',0.475,0.097,0.475,0.152,SEGM)                      307.   
      CALL UGLINE(' ',0.475,0.097,0,SEGM)                                308.   
      CALL UGLINE(' ',0.475,0.152,1,SEGM)                                309.   
      CALL DARROW(' ',0.212,0.197,0.652,0.275,SEGM)                      310.   
      CALL UGLINE(' ',0.212,0.197,0,SEGM)                                311.   
      CALL UGLINE(' ',0.652,0.275,1,SEGM)                                312.   
      CALL DARROW(' ',0.500,0.097,0.652,0.262,SEGM)                      313.   
      CALL UGLINE(' ',0.500,0.097,0,SEGM)                                314.   
      CALL UGLINE(' ',0.652,0.262,1,SEGM)                                315.   
      CALL UGWRIT(' ',0,SEGM)                                            316.   
C                                                                        317.   
C  PRODUCE FIGURE SHOWING SIMPLEX/DUPLEX CHARACTER SET.                  318.   
      DO 101 INT1=1,2                                                    319.   
        IF (INT1.EQ.1) THEN                                              320.   
          CALL UGFONT('SIMPLEX')                                         321.   
          CALL PCINIT(P261,16.5*FACT,1,SEGM,NSEG)                        322.   
        ELSE                                                             323.   
          CALL UGFONT('DUPLEX')                                          324.   
          CALL PCINIT(P262,16.5*FACT,1,SEGM,NSEG)                        325.   
        END IF                                                           326.   
C    DRAW THE ROMAN ALPHABET.                                            327.   
        CALL UGXTXT('SIZE=1.0',1.50,23.50,                               328.   
     X    'ABCDEFGHIJKLMNOPQRSTUVWXYZ',                                  329.   
     X    '                          ',SEGM)                             330.   
        CALL UGXTXT('SIZE=1.0',1.50,21.50,                               331.   
     X    'ABCDEFGHIJKLMNOPQRSTUVWXYZ',                                  332.   
     X    'LLLLLLLLLLLLLLLLLLLLLLLLLL',SEGM)                             333.   
C    DRAW THE GREEK ALPHABET.                                            334.   
        CALL UGXTXT('SIZE=1.0',1.50,19.50,                               335.   
     X    'ABGDEZHQIKLMNXOPRSTUFCYW',                                    336.   
     X    'FFFFFFFFFFFFFFFFFFFFFFFF',SEGM)                               337.   
        CALL UGXTXT('SIZE=1.0',1.50,17.50,                               338.   
     X    'ABGDEZHQIKLMNXOPRSTUFCYW',                                    339.   
     X    'GGGGGGGGGGGGGGGGGGGGGGGG',SEGM)                               340.   
C    DRAW THE CYRILLIC ALPHABET.                                         341.   
        CALL UGXTXT('SIZE=1.0',1.50,15.50,                               342.   
     X    'ABVGDEXZI1KLMNOPRSTUFHC234QY56WJ',                            343.   
     X    'BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB',SEGM)                       344.   
        CALL UGXTXT('SIZE=1.0',1.50,13.50,                               345.   
     X    'ABVGDEXZI1KLMNOPRSTUFHC234QY56WJ',                            346.   
     X    'CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC',SEGM)                       347.   
C    DRAW THE NUMERALS.                                                  348.   
        CALL UGXTXT('SIZE=1.0',1.50,11.50,                               349.   
     X    '0123456789',                                                  350.   
     X    '          ',SEGM)                                             351.   
C    DRAW THE BASIC SPECIAL CHARACTERS.                                  352.   
        CALL UGXTXT('SIZE=1.0',14.50,11.50,                              353.   
     X    '+-*/=.,()',                                                   354.   
     X    '         ',SEGM)                                              355.   
C    DRAW THE PUNCTUATION SPECIAL CHARACTERS.                            356.   
        CALL UGXTXT('SIZE=1.0',1.50,9.50,                                357.   
     X    '.,EUIAQPDF',                                                  358.   
     X    'PPPPPPPPPP',SEGM)                                             359.   
C    DRAW THE ADDITIONAL SPECIAL CHARACTERS.                             360.   
        CALL UGXTXT('SIZE=1.0',14.50,9.50,                               361.   
     X    'DC+PA0VWUN/()LRBEXT',                                         362.   
     X    'SSSSSSSSSSSSSSSSSSS',SEGM)                                    363.   
C    DRAW THE MATHEMATICAL SPECIAL CHARACTERS.                           364.   
        CALL UGXTXT('SIZE=1.0',1.50,7.50,                                365.   
     X    '.X/P*+-LGMHN=ACSRT2DIJYZ()BE0',                               366.   
     X    'MMMMMMMMMMMMMMMMMMMMMMMMMMMMM',SEGM)                          367.   
C    DRAW THE SET THEORETIC SPECIAL CHARACTERS.                          368.   
        CALL UGXTXT('SIZE=1.0',1.50,5.50,                                369.   
     X    'EAMNIULGKF',                                                  370.   
     X    'TTTTTTTTTT',SEGM)                                             371.   
C    DRAW THE ARROWS AND POINTERS.                                       372.   
        CALL UGXTXT('SIZE=1.0',14.50,5.50,                               373.   
     X    'UDLRB',                                                       374.   
     X    'WWWWW',SEGM)                                                  375.   
C    DRAW THE PHYSICS SPECIAL CHARACTERS.                                376.   
        CALL UGXTXT('SIZE=1.0',22.50,5.50,                               377.   
     X    'HL',                                                          378.   
     X    'KK',SEGM)                                                     379.   
C    DRAW THE ASTRONOMICAL SPECIAL CHARACTERS.                           380.   
        CALL UGXTXT('SIZE=1.0',1.50,3.50,                                381.   
     X    'HMVEWJSUNPOC*XYKQT0123456789AB',                              382.   
     X    'AAAAAAAAAAAAAAAAAAAAAAAAAAAAAA',SEGM)                         383.   
C    DRAW THE PLOTTING SYMBOLS.                                          384.   
        CALL UGXTXT('SIZE=1.0',1.50,1.50,                                385.   
     X    '0123456789',                                                  386.   
     X    'OOOOOOOOOO',SEGM)                                             387.   
C    DRAW THE SPECIAL DRAWING SYMBOLS.                                   388.   
        CALL UGXTXT('SIZE=1.0',14.50,1.50,                               389.   
     X    'UO',                                                          390.   
     X    'DD',SEGM)                                                     391.   
C    DRAW THE INVALID CHARACTER SYMBOL.                                  392.   
        CALL UGXTXT('SIZE=1.0',32.50,1.50,                               393.   
     X    '$',                                                           394.   
     X    '$',SEGM)                                                      395.   
        CALL UGWRIT(' ',0,SEGM)                                          396.   
  101 CONTINUE                                                           397.   
C                                                                        398.   
C  PRODUCE FIGURE SHOWING ASTROID/ELLIPSE EXAMPLE.                       399.   
      CALL UGFONT('DUPLEX')                                              400.   
      CALL PCINIT(P511,16.5*FACT,1,SEGM,NSEG)                            401.   
C    DRAW THE TITLES.                                                    402.   
      CALL UGXTXT('CENTER,SIZE=0.5',2.0,8.0,                             403.   
     X  'THE',' LL',SEGM)                                                404.   
      CALL UGXTXT('CENTER,SIZE=0.5',2.0,7.0,                             405.   
     X  'ASTROID',' LLLLLL',SEGM)                                        406.   
      CALL UGXTXT('CENTER,SIZE=0.5',2.0,6.0,                             407.   
     X  'AS AN','LL LL',SEGM)                                            408.   
      CALL UGXTXT('CENTER,SIZE=0.5',2.0,5.0,                             409.   
     X  'ENVELOPE',' LLLLLLL',SEGM)                                      410.   
      CALL UGXTXT('CENTER,SIZE=0.5',2.0,4.0,                             411.   
     X  'OF','LL',SEGM)                                                  412.   
      CALL UGXTXT('CENTER,SIZE=0.5',2.0,3.0,                             413.   
     X  'ELLIPSES',' LLLLLLL',SEGM)                                      414.   
      CALL UGLINE(' ',V511(1,1),V511(2,1),0,SEGM)                        415.   
      CALL UGLINE(' ',V511(1,2),V511(2,1),1,SEGM)                        416.   
      CALL UGLINE(' ',V511(1,2),V511(2,2),1,SEGM)                        417.   
      CALL UGLINE(' ',V511(1,1),V511(2,2),1,SEGM)                        418.   
      CALL UGLINE(' ',V511(1,1),V511(2,1),1,SEGM)                        419.   
      CALL UGWRIT(' ',0,SEGM)                                            420.   
C    ESTABLISH A WINDOW.                                                 421.   
      CALL UGWDOW('PUT,WINDOW',V511,W511)                                422.   
C    DRAW THE SHIELDS.                                                   423.   
      CALL UGINIT('CLEAR',SEGM,NSEG)                                     424.   
      CALL UGLINE(' ',S511(1,1),S511(2,1),0,SEGM)                        425.   
      CALL UGLINE(' ',S511(1,2),S511(2,1),1,SEGM)                        426.   
      CALL UGLINE(' ',S511(1,2),S511(2,2),1,SEGM)                        427.   
      CALL UGLINE(' ',S511(1,1),S511(2,2),1,SEGM)                        428.   
      CALL UGLINE(' ',S511(1,1),S511(2,1),1,SEGM)                        429.   
      CALL UGXTXT('SIZE=0.05',-0.7,0.5,                                  430.   
     X  'THE ELLIPSES.',                                                 431.   
     X  ' LL  LLLLLLLP',SEGM)                                            432.   
      CALL UGXTXT('SIZE=0.05',-0.7,0.4,                                  433.   
     X  'X223/C223+Y223/(1-C)223=1',                                     434.   
     X  ' X X  X X  X X      X X  ',SEGM)                                435.   
      CALL UGLINE(' ',T511(1,1),T511(2,1),0,SEGM)                        436.   
      CALL UGLINE(' ',T511(1,2),T511(2,1),1,SEGM)                        437.   
      CALL UGLINE(' ',T511(1,2),T511(2,2),1,SEGM)                        438.   
      CALL UGLINE(' ',T511(1,1),T511(2,2),1,SEGM)                        439.   
      CALL UGLINE(' ',T511(1,1),T511(2,1),1,SEGM)                        440.   
      CALL UGXTXT('SIZE=0.05',0.1,-0.4,                                  441.   
     X  'THE ASTROID.',                                                  442.   
     X  ' LL  LLLLLLP',SEGM)                                             443.   
      CALL UGXTXT('SIZE=0.05',0.1,-0.5,                                  444.   
     X  'X22/33+Y22/33=1',                                               445.   
     X  ' X   X  X   X  ',SEGM)                                          446.   
      CALL UGWRIT(' ',0,SEGM)                                            447.   
C    ESTABLISH THE SHIELDS.                                              448.   
      CALL UGSHLD('PUT,SHIELD=1',S511)                                   449.   
      CALL UGSHLD('PUT,SHIELD=2',T511)                                   450.   
C    DRAW THE CURVES.                                                    451.   
      CALL UGINIT('CLEAR',SEGM,NSEG)                                     452.   
      DO 202 FLT1=0.0,0.901,0.1                                          453.   
        INT1=0                                                           454.   
        DO 201 FLT2=0.0,360.0,5.0                                        455.   
          FLT3=COS(FLT2/57.2957795)                                      456.   
          FLT4=SIN(FLT2/57.2957795)                                      457.   
          IF (FLT1.EQ.0.0) THEN                                          458.   
            FLT5=FLT3**3                                                 459.   
            FLT6=FLT4**3                                                 460.   
          ELSE                                                           461.   
            FLT5=FLT1*FLT3                                               462.   
            FLT6=(1.0-FLT1)*FLT4                                         463.   
          END IF                                                         464.   
          CALL UGLINE(' ',FLT5,FLT6,INT1,SEGM)                           465.   
          INT1=1                                                         466.   
  201   CONTINUE                                                         467.   
  202 CONTINUE                                                           468.   
      CALL UGWRIT(' ',0,SEGM)                                            469.   
C                                                                        470.   
C  TERMINATE THE PROGRAM.                                                471.   
      CALL UGCLOS(' ')                                                   472.   
      STOP                                                               473.   
C                                                                        474.   
      END                                                                475.   
      SUBROUTINE PCINIT(PLIM,XSIZ,BFLG,SEGM,NSEG)                        476.   
C                                                                        477.   
C *********************************************************************  478.   
C *                                                                   *  479.   
C *  SUBROUTINE TO BEGIN A NEW PICTURE, INITIALIZE THE DRAWING        *  480.   
C *  SPACE, AND (OPTIONALLY) DRAW A BORDER.                           *  481.   
C *                                                                   *  482.   
C *  THE CALLING SEQUENCE IS:                                         *  483.   
C *    CALL PCINIT(PLIM,XSIZ,BFLG,SEGM,NSEG)                          *  484.   
C *                                                                   *  485.   
C *  THE PARAMETERS IN THE CALLING SEQUENCE ARE:                      *  486.   
C *    PLIM  THE PROGRAMMER LIMITS FOR THE PICTURE.                   *  487.   
C *    XSIZ  THE ACTUAL WIDTH OF THE PICTURE IN CENTIMETERS.          *  488.   
C *    BFLG  THE BORDER FLAG (0 MEANS NO BORDER AND 1 MEANS DRAW THE  *  489.   
C *          BORDER).                                                 *  490.   
C *    SEGM  THE GRAPHIC SEGMENT.                                     *  491.   
C *    NSEG  THE DIMENSION OF THE GRAPHIC SEGMENT.                    *  492.   
C *                                                                   *  493.   
C *********************************************************************  494.   
C                                                                        495.   
      REAL          PLIM(2,2)                                            496.   
      REAL          XSIZ                                                 497.   
      INTEGER       BFLG                                                 498.   
      INTEGER*4     SEGM(*)                                              499.   
      INTEGER       NSEG                                                 500.   
C                                                                        501.   
      REAL          DSPC(2,2)                                            502.   
      REAL          YSIZ,XRTO,YRTO                                       503.   
      CHARACTER*1   STRG                                                 504.   
      INTEGER       IARY(1)                                              505.   
      REAL          XARY(2)                                              506.   
C                                                                        507.   
C  START A NEW PICTURE.                                                  508.   
      CALL UGPICT('CLEAR',0)                                             509.   
C                                                                        510.   
C  OBTAIN A DRAWING SPACE WITH THE CORRECT ASPECT RATIO AND SIZE.        511.   
      CALL UGDSPC('PUT',1.0,1.0,0.0)                                     512.   
      CALL UGINFO('DSPCSIZE',STRG,IARY,XARY)                             513.   
      YSIZ=XSIZ*(PLIM(2,2)-PLIM(2,1))/(PLIM(1,2)-PLIM(1,1))              514.   
      XRTO=XSIZ/XARY(1)                                                  515.   
      YRTO=YSIZ/XARY(2)                                                  516.   
      IF (MAX(XRTO,YRTO).GT.1.0) THEN                                    517.   
        IF (XRTO.GT.YRTO) THEN                                           518.   
          YRTO=YRTO/XRTO                                                 519.   
          XRTO=1.0                                                       520.   
        ELSE                                                             521.   
          XRTO=XRTO/YRTO                                                 522.   
          YRTO=1.0                                                       523.   
        END IF                                                           524.   
      END IF                                                             525.   
      DSPC(1,1)=0.5*(1.0-XRTO)                                           526.   
      DSPC(1,2)=1.0-DSPC(1,1)                                            527.   
      DSPC(2,1)=0.5*(1.0-YRTO)                                           528.   
      DSPC(2,2)=1.0-DSPC(2,1)                                            529.   
      CALL UGWDOW('PUT',DSPC,PLIM)                                       530.   
C                                                                        531.   
C  CLEAR THE GRAPHIC SEGMENT AND DRAW A BORDER IF NECESSARY.             532.   
      CALL UGINIT('CLEAR',SEGM,NSEG)                                     533.   
      IF (BFLG.NE.0) THEN                                                534.   
        CALL UGLINE('VBRIGHT',PLIM(1,1)+0.0001,                          535.   
     X                        PLIM(2,1)+0.0001,0,SEGM)                   536.   
        CALL UGLINE('VBRIGHT',PLIM(1,2)-0.0001,                          537.   
     X                        PLIM(2,1)+0.0001,1,SEGM)                   538.   
        CALL UGLINE('VBRIGHT',PLIM(1,2)-0.0001,                          539.   
     X                        PLIM(2,2)-0.0001,1,SEGM)                   540.   
        CALL UGLINE('VBRIGHT',PLIM(1,1)+0.0001,                          541.   
     X                        PLIM(2,2)-0.0001,1,SEGM)                   542.   
        CALL UGLINE('VBRIGHT',PLIM(1,1)+0.0001,                          543.   
     X                        PLIM(2,1)+0.0001,1,SEGM)                   544.   
      END IF                                                             545.   
C                                                                        546.   
C  RETURN TO CALLING ROUTINE.                                            547.   
      RETURN                                                             548.   
C                                                                        549.   
      END                                                                550.   
      SUBROUTINE DPOINT(XCRD,YCRD,SEGM)                                  551.   
C                                                                        552.   
C *********************************************************************  553.   
C *                                                                   *  554.   
C *  SUBROUTINE TO DRAW A LARGE POINT.                                *  555.   
C *                                                                   *  556.   
C *  THE CALLING SEQUENCE IS:                                         *  557.   
C *    CALL DPOINT(XCRD,YCRD,SEGM)                                    *  558.   
C *                                                                   *  559.   
C *  THE PARAMETERS IN THE CALLING SEQUENCE ARE:                      *  560.   
C *    XCRD  THE X COORDINATE OF THE POINT.                           *  561.   
C *    YCRD  THE Y COORDINATE OF THE POINT.                           *  562.   
C *    SEGM  THE GRAPHIC SEGMENT.                                     *  563.   
C *                                                                   *  564.   
C *********************************************************************  565.   
C                                                                        566.   
      REAL          XCRD,YCRD                                            567.   
      INTEGER*4     SEGM(*)                                              568.   
C                                                                        569.   
      CALL UGMARK('MARK=9,SIZE=0.010',XCRD,YCRD,SEGM)                    570.   
      CALL UGMARK('MARK=9,SIZE=0.005',XCRD,YCRD,SEGM)                    571.   
      RETURN                                                             572.   
C                                                                        573.   
      END                                                                574.   
      SUBROUTINE DARROW(INTN,XCD1,YCD1,XCD2,YCD2,SEGM)                   575.   
C                                                                        576.   
C *********************************************************************  577.   
C *                                                                   *  578.   
C *  SUBROUTINE TO DRAW AN ARROW HEAD.                                *  579.   
C *                                                                   *  580.   
C *  THE CALLING SEQUENCE IS:                                         *  581.   
C *    CALL DARROW(INTN,XCD1,YCD1,XCD2,YCD2,SEGM)                     *  582.   
C *                                                                   *  583.   
C *  THE PARAMETERS IN THE CALLING SEQUENCE ARE:                      *  584.   
C *    INTN  THE INTENSITY LEVEL OF THE ARROW.                        *  585.   
C *    XCD1  THE X COORDINATE OF THE HEAD OF THE ARROW.               *  586.   
C *    YCD1  THE Y COORDINATE OF THE HEAD OF THE ARROW.               *  587.   
C *    XCD2  THE X COORDINATE OF THE OTHER END OF THE ARROW.          *  588.   
C *    YCD2  THE Y COORDINATE OF THE OTHER END OF THE ARROW.          *  589.   
C *    SEGM  THE GRAPHIC SEGMENT.                                     *  590.   
C *                                                                   *  591.   
C *********************************************************************  592.   
C                                                                        593.   
      CHARACTER*(*) INTN                                                 594.   
      REAL          XCD1,YCD1,XCD2,YCD2                                  595.   
      INTEGER*4     SEGM(*)                                              596.   
C                                                                        597.   
      REAL          ARGU                                                 598.   
C                                                                        599.   
      ARGU=ATAN2(YCD2-YCD1,XCD2-XCD1)                                    600.   
      CALL UGLINE(INTN,XCD1,YCD1,0,SEGM)                                 601.   
      CALL UGLINE(INTN,                                                  602.   
     X  XCD1+0.02*COS(ARGU+0.25),YCD1+0.02*SIN(ARGU+0.25),1,SEGM)        603.   
      CALL UGLINE(INTN,                                                  604.   
     X  XCD1+0.02*COS(ARGU-0.25),YCD1+0.02*SIN(ARGU-0.25),1,SEGM)        605.   
      CALL UGLINE(INTN,XCD1,YCD1,1,SEGM)                                 606.   
      RETURN                                                             607.   
C                                                                        608.   
      END                                                                609.   
      SUBROUTINE UGXERR(LEVL,SNAM,INDX)                                  610.   
C                                                                        611.   
C *********************************************************************  612.   
C *                                                                   *  613.   
C *  SUBROUTINE TO PROCESS GRAPHIC SEGMENT OVERFLOWS.                 *  614.   
C *                                                                   *  615.   
C *  THE CALLING SEQUENCE IS:                                         *  616.   
C *    CALL UGXERR(LEVL,SNAM,INDX)                                    *  617.   
C *                                                                   *  618.   
C *  THE PARAMETERS IN THE CALLING SEQUENCE ARE:                      *  619.   
C *    LEVL  THE LEVEL OF THE ERROR.                                  *  620.   
C *    SNAM  THE NAME OF THE SUBROUTINE DETECTING THE ERROR.          *  621.   
C *    INDX  THE INDEX OF THE ERROR.                                  *  622.   
C *                                                                   *  623.   
C *********************************************************************  624.   
C                                                                        625.   
      INTEGER       LEVL                                                 626.   
      CHARACTER*8   SNAM                                                 627.   
      INTEGER       INDX                                                 628.   
C                                                                        629.   
C  COMMON BLOCK FOR THE GRAPHIC SEGMENT.                                 630.   
      SAVE          /FGGCBK/                                             631.   
C  NUMBER OF WORDS IN THE GRAPHIC SEGMENT.                               632.   
      INTEGER       NSEG                                                 633.   
      PARAMETER     (NSEG=1024)                                          634.   
C  THE DECLARATION OF THE COMMON BLOCK.                                  635.   
      COMMON        /FGGCBK/                                             636.   
     X              SEGM                                                 637.   
C  THE GRAPHIC SEGMENT.                                                  638.   
      INTEGER*4     SEGM(NSEG)                                           639.   
C                                                                        640.   
C  CLEAR THE GRAPHIC SEGMENT IF IT HAS OVERFLOWED.                       641.   
      IF (INDX.EQ.11) THEN                                               642.   
        CALL UGWRIT(' ',0,SEGM)                                          643.   
        CALL UGINIT('CONTINUE',SEGM,NSEG)                                644.   
        LEVL=0                                                           645.   
      END IF                                                             646.   
C                                                                        647.   
C  RETURN TO CALLING ROUTINE.                                            648.   
      RETURN                                                             649.   
C                                                                        650.   
      END                                                                651.   
