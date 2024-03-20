      SUBROUTINE UGZ002(IACT,NAME,INDX)
C
C *******************  THE UNIFIED GRAPHICS SYSTEM  *******************
C *         SUBROUTINE TO ACTIVATE/DE-ACTIVATE A NAMED MODULE         *
C *                                                                   *
C *  THIS SUBROUTINE MAY BE USED TO ACTIVATE OR DE-ACTIVATE A NAMED   *
C *  MODULE.  WHEN A MODULE IS ACTIVATED, THE ADDRESS OF THE MODULE   *
C *  IS RETURNED.  IF THE MODULE CANNOT BE FOUND, A ZERO IS           *
C *  RETURNED.  WHEN A MODULE IS DE-ACTIVATED, BOTH ITS NAME AND ITS  *
C *  ADDRESS SHOULD BE GIVEN.                                         *
C *                                                                   *
C *  THE CALLING SEQUENCE IS:                                         *
C *    CALL UGZ002(IACT,NAME,INDX)                                    *
C *                                                                   *
C *  THE PARAMETERS IN THE CALLING SEQUENCE ARE:                      *
C *    IACT  THE OPERATION INDICATOR (0 MEANS ACTIVATE AND 1 MEANS    *
C *          DE-ACTIVATE).                                            *
C *    NAME  THE NAME OF THE MODULE TO BE ACTIVATED.  THIS ARGUMENT   *
C *          MUST BE A CHARACTER STRING OF EIGHT CHARACTERS.          *
C *    INDX  THE INDEX OF THE MODULE.                                 *
C *                                                                   *
C *                          ROBERT C. BEACH                          *
C *                    COMPUTATION RESEARCH GROUP                     *
C *                STANFORD LINEAR ACCELERATOR CENTER                 *
C *                                                                   *
C *********************************************************************
C
      INTEGER       IACT
      CHARACTER*8   NAME
      INTEGER       INDX

C  Number of modules that may be activated.
      PARAMETER     (NMOD=45)

      SAVE          MODN
      CHARACTER*8   MODN(NMOD)

      INTEGER       INT1

      DATA          MODN/'UGCW01  ',
     +                   'UGGS01  ',
     +                   'UGGD01  ',
     +                   'UGGI01  ',
     +                   'UGVS01  ',
     +                   'UGVI01  ',
     +                   'UGGR01  ',
     +                   'UGIN01  ',
     +                   'UGIX01  ',
     +                   'UGMT01  ',
     +                   'UGPR01  ',
     +                   'UGPX01  ',
     +                   'UGSA01  ',
     +                   'UGSB01  ',
     +                   'UGSC01  ',
     +                   'UGSD01  ',
     +                   'UGSE01  ',
     +                   'UGSX01  ',
     +                   'UGTA01  ',
     +                   'UGTS01  ',
     +                   'UGTD01  ',
     +                   'UGTX01  ',
     +                   'UGXA01  ',
     +                   'UGXB01  ',
     +                   'UGXC01  ',
     +                   'UGUS01  ',
     +                   'UGUD01  ',
     +                   'UGUX01  ',
     +                   'UGWA01  ',
     +                   'UGWB01  ',
     +                   'UGWC01  ',
     +                   'UGWD01  ',
     +                   'UGWE01  ',
     +                   'UGWZ01  ',
     +                   'UGVF01  ',
     +                   'UGXS01  ',
     +                   'UGXI01  ',
     +                   'UGPU01  ',
     +                   'UGPL01  ',
     +                   'UGPS01  ',
     +                   'UGPM01  ',
     +                   'UGPI01  ',
     +                   'UGZZ01  ',
     +                   'UGQM01  ',
     +                   'UGEP01  '/


C  IF THIS IS AN ACTIVATION REQUEST, OBTAIN THE ADDRESS.
      INDX=0
      IF (IACT.EQ.0) THEN
         DO 101 INT1=1,NMOD
            IF (NAME.EQ.MODN(INT1)) THEN
               INDX = INT1
               RETURN
            ENDIF
  101    CONTINUE
      ENDIF
      RETURN
      END
