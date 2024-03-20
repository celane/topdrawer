      PROGRAM TRY  ! Test UGS 1.5 calls as used in TD

*   19931228  KREYMER@FNAL.GOV

      INTEGER    LSEG
      PARAMETER (LSEG = 10000 )
      INTEGER*4  SEG  ( LSEG )

      INTEGER*4  XTEXT (1000) , YTEXT (1000) , BTEXT (1000)

      CHARACTER*32 STRING

      CALL UGINIT ( 'CLEAR' , SEG , LSEG )

      CALL UGOPEN ( ',,lksjdlij, XWINDOW ,=3 ' , 1 )
*      CALL UGOPEN ( 'GENIL,POSTSCR , DDNAME=TRY.PS ' , 1 )
      CALL UGSLCT ( ' ' , 1 )

      CALL UGFONT ( 'SIMPLEX' )
*      CALL UGFONT ( 'DUPLEX' )
      CALL UGCTOL (
     +  ' ' ,	      ! OPTION
*     +   'SIZE=.02',  ! option
     +	 0.4 ,	      ! X
     +   0.5 ,	      ! Y
     +   'TEXT',      ! TEXT
     +   '    ',      ! SEC TEXT
     +   1000,	      ! NSIZE
     +   XTEXT,	      ! X ARRAY
     +   YTEXT,	      ! Y ARRAY
     +   NCOORD,      ! NUM COORDS
     +   BTEXT	      ! BLANKING
     +   )

      if (1.eq.2) goto 66666

      CALL UGLINE ( ' ' , 0.1 , 0.1 , 0 , SEG )
      CALL UGLINE ( 'RED' , 0.9 , 0.9 , 1 , SEG )
      CALL UGLINE ( ' ' , 0.1 , 0.9 , 0 , SEG )
      CALL UGLINE ( 'GREEN' , 0.9 , 0.1 , 1 , SEG )

      CALL UGPLIN ( ' ' , XTEXT,YTEXT,NCOORD , BTEXT,-NCOORD, SEG )
      CALL UGLINE ( ' ' , 0.35 , 0.4925 , 0 , SEG )
      CALL UGLINE ( ' ' , 0.38 , 0.4925 , 1 , SEG )
      CALL UGLINE ( ' ' , 0.35 , 0.5075 , 0 , SEG )
      CALL UGLINE ( ' ' , 0.38 , 0.5075 , 1 , SEG )

      CALL UGCTOL(' SIZE = .1 ',.65,.5,
     +   'TEXT','    ',1000,XTEXT,YTEXT,NCOORD,BTEXT)
      CALL UGPLIN ( ' ' , XTEXT,YTEXT,NCOORD , BTEXT,-NCOORD, SEG )
      CALL UGLINE ( ' ' , 0.57 , 0.45 , 0 , SEG )
      CALL UGLINE ( ' ' , 0.60 , 0.45 , 1 , SEG )
      CALL UGLINE ( ' ' , 0.57 , 0.55 , 0 , SEG )
      CALL UGLINE ( ' ' , 0.60 , 0.55 , 1 , SEG )

      CALL UGWRIT ( ' ' , 0 , SEG )

*     IF ( 1.EQ.2 ) THEN
      CALL UGENAB ( 'LOCATOR' )
      CALL UGECTL ( 'LOCATOR' , STRING,I,X,Y)
      WRITE (*,*) I,X,Y
      CALL UGECTL ( 'LOCATOR' , STRING,I,X,Y)
      WRITE (*,*) I,X,Y
      CALL UGECTL ( 'LOCATOR' , STRING,I,X,Y)
      WRITE (*,*) I,X,Y
*     ENDIF

      CALL UGINIT ( 'CLEAR' , SEG , LSEG )

*     WRITE (*,*) ' Paws : '
*     READ  (*,*)

66666 continue

      CALL UGCLOS ( ' ' )

      STOP
      END
