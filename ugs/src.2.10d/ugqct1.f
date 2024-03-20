      SUBROUTINE UGQCT1(IVR1,DVR1,IVR2,DVR2,CVAL,IVAL,FLAG)
C
C *******************  THE UNIFIED GRAPHICS SYSTEM  *******************
C *                  COMPUTE A POINT ON THE CONTOUR                   *
C *                                                                   *
C *  THIS SUBROUTINE IS USED BY UGQCTR TO COMPUTE A POINT ON THE      *
C *  CURRENT CONTOUR CURVE.                                           *
C *                                                                   *
C *  THE CALLING SEQUENCE IS:                                         *
C *    CALL UGQCT1(IVR1,DVR1,IVR2,DVR2,CVAL,IVAL,FLAG)                *
C *                                                                   *
C *  THE PARAMETERS IN THE CALLING SEQUENCE ARE:                      *
C *    IVR1  THE INDEPENDENT VARIABLE OF THE FIRST POINT.             *
C *    DVR1  THE DEPENDENT VARIABLE OF THE FIRST POINT.               *
C *    IVR2  THE INDEPENDENT VARIABLE OF THE SECOND POINT.            *
C *    DVR2  THE DEPENDENT VARIABLE OF THE SECOND POINT.              *
C *    CVAL  THE CURRENT CONTOUR VALUE.                               *
C *    IVAL  THE COMPUTED VALUE OF THE INDEPENDENT VARIABLE.          *
C *    FLAG  A FLAG TO INDICATE IF A POINT WAS FOUND.                 *
C *            0 MEANS NO POINT WAS FOUND, AND                        *
C *            1 MEANS A POINT WAS FOUND.                             *
C *                                                                   *
C *                          ROBERT C. BEACH                          *
C *                    COMPUTATION RESEARCH GROUP                     *
C *                STANFORD LINEAR ACCELERATOR CENTER                 *
C *                                                                   *
C *********************************************************************
C
      REAL          IVR1,DVR1,IVR2,DVR2,CVAL,IVAL
      INTEGER       FLAG
C
C  COMPUTE THE INTERSECTION POINT.
      IF ((CVAL.GT.MIN(DVR1,DVR2)).AND.
     X    (CVAL.LE.MAX(DVR1,DVR2))) THEN
        IVAL=IVR1+((CVAL-DVR1)*(IVR2-IVR1)/(DVR2-DVR1))
        IVAL=MAX(IVAL,MIN(IVR1,IVR2))
        IVAL=MIN(IVAL,MAX(IVR1,IVR2))
        FLAG=1
      ELSE
        FLAG=0
      END IF
C
C  RETURN TO CALLING SUBROUTINE.
      RETURN
C
      END
