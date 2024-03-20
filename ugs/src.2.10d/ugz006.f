      SUBROUTINE UGZ006 ( IADR , NARG , IARG , K1,K2,K3 )

*******************  THE UNIFIED GRAPHICS SYSTEM  *******************
*     SUBROUTINE TO EXECUTE A SUBROUTINE WITH POINTER ARGUMENTS     *
*                                                                   *
*  THIS SUBROUTINE MAY BE USED TO EXECUTE A SUBROUTINE WHEN THE     *
*  ADDRESS OF THE SUBROUTINE AND/OR THE ADDRESSES OF THE            *
*  SUBROUTINE'S ARGUMENTS ARE KNOWN.  THIS VERSION OF THE           *
*  SUBROUTINE IS CALLED BY THE DEVICE-INDEPENDENT MODULES.          *
*                                                                   *
*  THE CALLING SEQUENCE IS:                                         *
*    CALL UGZ006(IADR,NARG,IARG,ARG1,ARG2,...)                      *
*                                                                   *
*  THE PARAMETERS IN THE CALLING SEQUENCE ARE:                      *
*    IADR  THE INDEX of the DRIVER TO BE CALLED.        THIS        *
*          ADDRESS MUST HAVE BEEN DEVELOPED BY SUBROUTINE UGZ002    *
*          OR UGZ004.                                               *
*    NARG,IARG unused                                               *
*    K1    FIRST ACTUAL ARGUMENT.                                   *
*    K2    SECOND ACTUAL ARGUMENT.                                  *
*    K3    SECOND ACTUAL ARGUMENT.                                  *
*                                                                   *
*                          ROBERT C. BEACH                          *
*                    COMPUTATION RESEARCH GROUP                     *
*                STANFORD LINEAR ACCELERATOR CENTER                 *
*                                                                   *
*********************************************************************

      INTEGER       IADR,NARG,IARG,K1,K3
      CHARACTER*(*) K2

      GOTO  (  1,  2,  3,  4,  5,  6,  7,  8,  9, 10,
     +        11, 12, 13, 14, 15, 16, 17, 18, 19, 20,
     +        21, 22, 23, 24, 25, 26, 27, 28, 29, 30,
     +        31, 32, 33, 34, 35, 36, 37, 38, 39, 40,
     +        41, 42, 43, 44, 45 ) IADR

      WRITE (*,* ) ' UGS called unknown driver index ',IADR
      STOP

    1 CONTINUE
         CALL UGCW01(K1,K2,K3)
         RETURN
    2 CONTINUE
         CALL UGGS01(K1,K2,K3)
         RETURN
    3 CONTINUE
         CALL UGGD01(K1,K2,K3)
         RETURN
    4 CONTINUE
         CALL UGGI01(K1,K2,K3)
         RETURN
    5 CONTINUE
         CALL UGVS01(K1,K2,K3)
         RETURN
    6 CONTINUE
         CALL UGVI01(K1,K2,K3)
         RETURN
    7 CONTINUE
         CALL UGGR01(K1,K2,K3)
         RETURN
    8 CONTINUE
         CALL UGIN01(K1,K2,K3)
         RETURN
    9 CONTINUE
         CALL UGIX01(K1,K2,K3)
         RETURN
   10 CONTINUE
         CALL UGMT01(K1,K2,K3)
         RETURN
   11 CONTINUE
         CALL UGPR01(K1,K2,K3)
         RETURN
   12 CONTINUE
         CALL UGPX01(K1,K2,K3)
         RETURN
   13 CONTINUE
         CALL UGSA01(K1,K2,K3)
         RETURN
   14 CONTINUE
         CALL UGSB01(K1,K2,K3)
         RETURN
   15 CONTINUE
         CALL UGSC01(K1,K2,K3)
         RETURN
   16 CONTINUE
         CALL UGSD01(K1,K2,K3)
         RETURN
   17 CONTINUE
         CALL UGSE01(K1,K2,K3)
         RETURN
   18 CONTINUE
         CALL UGSX01(K1,K2,K3)
         RETURN
   19 CONTINUE
         CALL UGTA01(K1,K2,K3)
         RETURN
   20 CONTINUE
         CALL UGTS01(K1,K2,K3)
         RETURN
   21 CONTINUE
         CALL UGTD01(K1,K2,K3)
         RETURN
   22 CONTINUE
         CALL UGTX01(K1,K2,K3)
         RETURN
   23 CONTINUE
         CALL UGXA01(K1,K2,K3)
         RETURN
   24 CONTINUE
         CALL UGXB01(K1,K2,K3)
         RETURN
   25 CONTINUE
         CALL UGXC01(K1,K2,K3)
         RETURN
   26 CONTINUE
         CALL UGUS01(K1,K2,K3)
         RETURN
   27 CONTINUE
         CALL UGUD01(K1,K2,K3)
         RETURN
   28 CONTINUE
         CALL UGUX01(K1,K2,K3)
         RETURN
   29 CONTINUE
         CALL UGWA01(K1,K2,K3)
         RETURN
   30 CONTINUE
         CALL UGWB01(K1,K2,K3)
         RETURN
   31 CONTINUE
         CALL UGWC01(K1,K2,K3)
         RETURN
   32 CONTINUE
         CALL UGWD01(K1,K2,K3)
         RETURN
   33 CONTINUE
         CALL UGWE01(K1,K2,K3)
         RETURN
   34 CONTINUE
         CALL UGWZ01(K1,K2,K3)
         RETURN
   35 CONTINUE
         CALL UGVF01(K1,K2,K3)
         RETURN
   36 CONTINUE
         CALL UGXS01(K1,K2,K3)
         RETURN
   37 CONTINUE
         CALL UGXI01(K1,K2,K3)
         RETURN
   38 CONTINUE
         CALL UGPU01(K1,K2,K3)
         RETURN
   39 CONTINUE
         CALL UGPL01(K1,K2,K3)
         RETURN
   40 CONTINUE
         CALL UGPS01(K1,K2,K3)
         RETURN
   41 CONTINUE
         CALL UGPM01(K1,K2,K3)
         RETURN
   42 CONTINUE
         CALL UGPI01(K1,K2,K3)
         RETURN
   43 CONTINUE
         CALL UGZZ01(K1,K2,K3)
         RETURN
   44 CONTINUE
         CALL UGQM01(K1,K2,K3)
         RETURN
   45 CONTINUE
         CALL UGEP01(K1,K2,K3)
         RETURN

      END
