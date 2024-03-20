	SUBROUTINE	UGGKSW(DDIN,DDST,DDEX)
C
C *******************  THE UNIFIED GRAPHICS SYSTEM  *******************
C *                  DEVICE-DEPENDENT MODULE FOR GKS                  *
C *		Dummy module - causes UGSYS to exit                   *
C *                                                                   *
C *  THIS SUBROUTINE IS A DEVICE-DEPENDENT MODULE FOR A NON-          *
C *  INTERACTIVE GRAPHIC DISPLAY DEVICE.                              *
C *                                                                   *
C *  THE CALLING SEQUENCE IS:                                         *
C *    CALL UGSI01(DDIN,DDST,DDEX)                                    *
C *                                                                   *
C *  THE PARAMETERS IN THE CALLING SEQUENCE ARE:                      *
C *    DDIN  AN INTEGER INPUT ARRAY.                                  *
C *    DDST  A CHARACTER STRING FOR INPUT AND OUTPUT.                 *
C *    DDEX  AN INTEGER OUTPUT ARRAY.                                 *
C *                                                                   *
C *                                                                   *
C *********************************************************************
C
	INTEGER		DDIN(*)
	CHARACTER*(*)	DDST
	INTEGER		DDEX(*)

	DDEX(1)=-1

	END
