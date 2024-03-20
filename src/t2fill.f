C        MORTRAN 2.79 (BRACKETED KEYWORD MACROS OF 09/28/81)          
      SUBROUTINE T2_FILL(INFOIN,CARDIN)
      IMPLICIT NONE
      INTEGER INFOIN(10)
      CHARACTER*(*) CARDIN
      CHARACTER*1 CXYZ(3)
      COMMON /T2FLGC/ FLAGS(200), LTRAP,LHANDL,LSYERR,LSCREV
      LOGICAL FLAGS, LTRAP,LHANDL,LSYERR,LSCREV(3)
      INTEGER RELFLAG 
      PARAMETER (RELFLAG=151)
      INTEGER IWINLEV 
      INTEGER NWINLEV , MAX_FILL 
      PARAMETER (NWINLEV =4)
      PARAMETER (MAX_FILL=4)
      REAL GRDSYM, GRDSIZ, DATDAT(2,8), H2STAT(12), TITLIN(4), TITFA
     *C(4), TITLOC(4), TITMAR(4), TITLMX(4), TIKFAC, TITINX, TITX1, TIT
     *Y1, TITX, TITY, TITZ, TITSIZ, TITCON(5), REVLEV, REDUCE(5), XFR
     *M12(12), XFRM13(12), XFRM14(12), XFRM23(12), XFRM24(12), XFRM34(6
     *), XVARSX(20), CIRSIZ(2,3), ASIZE, AFLARE, SMSZDF, ORAXES(3), EY
     *EDIR(3), VUEDIR(3), VUECEN(3), VERTCL(3), EYESEP, SCRD, SCRZ, EYE
     *DIS, EYEPNT(3), XYPART(2, 2), BARSIZ(3), BARBRK(3), TIKSIZ(3), XY
     *ZBAS(3), TTHETA, TPHI, FRELBL(4), PATRN(20,30), SCLPRM(10,3), WI
     *NDOW(4), SCREEN(2), SYMBOL, NOSYMB, FACTXY(19), LBLSIZ, SYMSIZ, X
     *YZLIM(3,2), WORLD(3), RADRMX, RADRMN, RADAMN, RADANG, RADX0, RADY
     *0, XUNUSD(6), AXANG(3), REDFAC, DSCAL, PATSZ, MARGIN(4), PSCR(2
     *), OREAL, OCPU, CSCRD, DATPTS, ERRPTS, DATXMN, DATXMX, DATYMN, DA
     *TYMX, DATZMN, DATZMX, DATSUM, ERRSUM, DATAVE, ERRAVE, DATCEN, ERR
     *CEN, DATSTD, ERRSTD, WINREL(2,2,NWINLEV),WINABS(2,2,NWINLEV),WINL
     *IM(2,2), FMARKER(3,8), SYDIR(3), GRDIR(3), EXYZLIM(3,2), DSLOPE(2
     *,2)
      REAL PLOT_EXTENT(2,2),FILL_ANGLE(MAX_FILL),FILL_WIDTH(MAX_FILL)
      INTEGER FILL_INDEX, FILL_TEX(MAX_FILL), N_LINES(6)
      EQUIVALENCE (DATDAT, DATPTS)
      INTEGER MAXREP
      PARAMETER (MAXREP=5)
      INTEGER IPATRN, NPATRN(30), NXYLIM(3,2), NPLOTS, NCSETS, NCCOL, N
     *CROW, IYEAR, IMONTH, IDAY, ISIGFG, ITXPRI, ITXSEC, ITITDT(3), L
     *ENCRD, IPROP, IHTEX, IBLINK, IERASE, ITCNTR, IUNUSD, NXYZ1(3), 
     *NXYZ2(3), LBLFMT(3), LBLCHR(3), NDIMNS(3), IBLKTP, IDIMNS, NMES
     *H0, NMESH1, NMESH2, MESH1, MESH2, MESH3, MESH2D, MESHN(3), NMESHN(
     *2), NFIELD, IFIELD(19), NINCR, NPOINT, IVARBL(19), IVPTR(19), I_
     *VORDER(19,2), LINEAR(4), NONLIN(4), MXECHO, LINES, MAXLNS, USRK
     *BD, USRSCR, PLTFIL, LINWID, LINCOL, LINTEX, ICPOIN(3), IREPCT(MA
     *XREP), LEVREP, GRDTYP, GRDTEX, OUTTEX, IAXTEX, TITEX, LETSET, LAB
     *TEX, TICTEX, SHADOWTYP,SHADOWTEX, NXYZDEF1(10), NXYZDEF2(10), IF
     *ILE_CASE ,MAX_SUBST
      INTEGER MAX_CYCLE
      PARAMETER (MAX_CYCLE=20)
      INTEGER N_CYCLE, ITX_CYCLE(MAX_CYCLE)
      REAL SYM_CYCLE(MAX_CYCLE)
      EQUIVALENCE (NMESH1,NMESHN)
      EQUIVALENCE (MESH1,MESHN)
      INTEGER IBUFFR(400)
      REAL BUFFER(400)
      INTEGER NCHSAVE 
      PARAMETER (NCHSAVE=2*512) 
      INTEGER*4 ICHSAVE(NCHSAVE)
      INTEGER*4 IVRPTR(15)
      CHARACTER*512 OUTSTR,C_FILE, C_NAME, C_SELECT
      CHARACTER C_TIT_ESCAPE*1,C_TIT_SUBSTITUTE*2
      INTEGER N_FILE, N_NAME, N_SELECT
      CHARACTER*8 PXNAME
      CHARACTER*64 INPFMT
      LOGICAL LTDATA, OUTSID(4)
      COMMON /T2COM/ PXNAME, REDUCE, TITX, TITY ,XFRM12, XFRM13, XFRM1
     *4, XFRM23 ,XFRM24, XFRM34 ,XVARSX ,TITSIZ, GRDSYM, GRDSIZ, CIRS
     *IZ, ASIZE, AFLARE ,SMSZDF, ORAXES, EYEDIR, VUEDIR, VUECEN ,VERTC
     *L, EYESEP, SCRD, SCRZ, EYEDIS, EYEPNT ,XYPART, BARSIZ, TIKSIZ, XY
     *ZBAS, TTHETA ,TPHI, FRELBL, IPATRN, NPATRN, PATRN ,SCLPRM, WINDO
     *W, SCREEN, SYMBOL, NOSYMB ,FACTXY, LBLSIZ, SYMSIZ, XYZLIM, WORLD 
     *,RADRMX, RADRMN, RADAMN, RADANG, RADX0, RADY0, XUNUSD ,USRKBD, U
     *SRSCR, PLTFIL ,ITITDT, LENCRD ,INPFMT, GRDTYP, LINWID, LINCOL, L
     *INTEX, TITCON, ITCNTR ,IUNUSD, NXYZ1, NXYZ2 ,LBLCHR, NDIMNS, IBL
     *KTP, IDIMNS ,NMESH1, NMESH2, MESH1, MESH2, MESH3, NFIELD ,IFIELD
     *, NINCR ,IVARBL, IVRPTR ,LINEAR, NONLIN ,NPOINT, LETSET ,MXECH
     *O, LINES, MAXLNS ,AXANG, MESH2D, REDFAC, GRDTEX, OUTTEX, IAXTEX, 
     *TITEX, DSCAL ,OUTSTR, IPROP, IHTEX, IBLINK, IERASE, LABTEX, TICTE
     *X, NXYLIM, PATSZ ,MARGIN, PSCR, ITXPRI, ITXSEC, OREAL, OCPU, CSCR
     *D, OUTSID ,TITLIN, TITINX, TITZ, TITX1, TITY1, LTDATA, NPLOTS, RE
     *VLEV ,LBLFMT, IYEAR, IMONTH, IDAY, ISIGFG ,DATPTS, ERRPTS, DATXM
     *N, DATXMX, DATYMN, DATYMX, DATZMN, DATZMX ,DATSUM, ERRSUM, DATAVE
     *, ERRAVE, DATCEN, ERRCEN, DATSTD, ERRSTD ,NCSETS, NCCOL, NCROW, F
     *MARKER, H2STAT, TITFAC, TITLOC, TITMAR ,TIKFAC, IWINLEV, WINREL, 
     *WINABS, WINLIM ,ICPOIN, LEVREP, IREPCT, SYDIR, GRDIR, NMESH0, EXY
     *ZLIM, IVPTR ,TITLMX, NXYZDEF1, NXYZDEF2, IFILE_CASE, N_CYCLE, ITX
     *_CYCLE ,SYM_CYCLE, PLOT_EXTENT, FILL_ANGLE, FILL_WIDTH, FILL_TEX 
     *,FILL_INDEX ,N_LINES ,DSLOPE ,MAX_SUBST, BARBRK ,I_VORDER,SHADOW
     *TYP,SHADOWTEX
      COMMON /T2SCRT/ BUFFER, IBUFFR, N_FILE,N_NAME,N_SELECT, ICHSAVE
      COMMON /T2_CHAR/ C_TIT_ESCAPE,C_TIT_SUBSTITUTE
      COMMON /T2_SCRT/ C_FILE, C_NAME, C_SELECT
      INTEGER JOUFIL, ERRFIL, OUTFIL, DBGFIL, INPFIL, NINMAX
      PARAMETER (NINMAX=20)
      INTEGER NINP(NINMAX)
      COMMON /T2TRBK/NINP, JOUFIL, ERRFIL, OUTFIL, DBGFIL, INPFIL
      CHARACTER*512 STRNG, STJOU
      LOGICAL LTOKEN
      REAL FLOTNG
      INTEGER INTERP, KEYORD, NSTRNG, MAXSTR, NSTJOU, LSTJOU, NTOKEN
      INTEGER*4 INTEG
      COMMON /TOKENC/ INTERP, INTEG, FLOTNG, KEYORD, NSTRNG, MAXSTR, ST
     *RNG, NSTJOU, LSTJOU, STJOU, LTOKEN, NTOKEN
      INTEGER LISTPT,NPTMAX,NDSETS
      REAL DATBUF(10248)
      COMMON /T2XPNT/ LISTPT,NPTMAX,NDSETS,DATBUF
      LOGICAL LAPPEN, LCONF, LMONITOR, LCHECK, LLOG
      INTEGER N1,N2,N3,N4,NS1,NS2
      REAL TLIM(3,2)
      COMMON /T2SELECT/N1 ,N2 ,N3 ,N4 ,NS1 ,NS2 ,TLIM ,LMONITOR ,LAPPEN
     * ,LCONF ,LLOG ,LCHECK
      REAL HNONE
      INTEGER NSETD 
      PARAMETER (NSETD=19+9)
      INTEGER ISETD(NSETD),NINCR0,NINCR1,NINCR2,IBTYPE
      INTEGER I1,I2,I3,I4,I5,I6,I7,IM,NP,NL,IENDAT,IBGDAT
      INTEGER I_NDX,I_NDY,I_NDZ
      INTEGER N_SYMBOL 
      INTEGER N_XDATA 
      INTEGER N_DXDATA 
      INTEGER N_NDXDATA 
      INTEGER N_YDATA 
      INTEGER N_DYDATA 
      INTEGER N_NDYDATA 
      INTEGER N_ZDATA 
      INTEGER N_DZDATA 
      INTEGER N_NDZDATA 
      INTEGER N_UDATA 
      INTEGER N_DUDATA 
      INTEGER N_NDUDATA 
      INTEGER N_VDATA 
      INTEGER N_DVDATA 
      INTEGER N_NDVDATA 
      INTEGER N_WDATA 
      INTEGER N_DWDATA 
      INTEGER N_NDWDATA 
      INTEGER NCMAX 
      INTEGER NCSIZE 
      INTEGER NBTYPE 
      INTEGER NINDAT 
      INTEGER NSDATA 
      INTEGER NXDATA 
      INTEGER NDXDATA 
      INTEGER NNDXDATA 
      INTEGER NYDATA 
      INTEGER NDYDATA 
      INTEGER NNDYDATA 
      INTEGER NZDATA 
      INTEGER NDZDATA 
      INTEGER NNDZDATA 
      INTEGER NPDATA 
      INTEGER NLDATA 
      INTEGER NMDATA 
      INTEGER NBGDAT 
      INTEGER NENDAT 
      PARAMETER (N_SYMBOL=1) 
      PARAMETER (N_XDATA=2) 
      PARAMETER (N_DXDATA=3) 
      PARAMETER (N_NDXDATA=4) 
      PARAMETER (N_YDATA=5) 
      PARAMETER (N_DYDATA=6) 
      PARAMETER (N_NDYDATA=7) 
      PARAMETER (N_ZDATA=8) 
      PARAMETER (N_DZDATA=9) 
      PARAMETER (N_NDZDATA=10) 
      PARAMETER (N_UDATA=11) 
      PARAMETER (N_DUDATA=12) 
      PARAMETER (N_NDUDATA=13) 
      PARAMETER (N_VDATA=14) 
      PARAMETER (N_DVDATA=15) 
      PARAMETER (N_NDVDATA=16) 
      PARAMETER (N_WDATA=17) 
      PARAMETER (N_DWDATA=18) 
      PARAMETER (N_NDWDATA=19) 
      PARAMETER (NCMAX=6) 
      PARAMETER (NCSIZE=3) 
      PARAMETER (NBTYPE=1) 
      PARAMETER (NINDAT=2) 
      PARAMETER (NSDATA=5) 
      PARAMETER (NXDATA=NSDATA+1) 
      PARAMETER (NDXDATA=NXDATA+1) 
      PARAMETER (NNDXDATA=NXDATA+2) 
      PARAMETER (NYDATA=NXDATA+NCSIZE) 
      PARAMETER (NDYDATA=NYDATA+1) 
      PARAMETER (NNDYDATA=NYDATA+2) 
      PARAMETER (NZDATA=NYDATA+NCSIZE) 
      PARAMETER (NDZDATA=NZDATA+1) 
      PARAMETER (NNDZDATA=NZDATA+2) 
      PARAMETER (NPDATA=NSDATA+19) 
      PARAMETER (NLDATA=NPDATA+1) 
      PARAMETER (NMDATA=NLDATA+1) 
      PARAMETER (NBGDAT=NMDATA+1) 
      PARAMETER (NENDAT=NBGDAT+1) 
      EQUIVALENCE (IBTYPE,ISETD(NBTYPE))
      EQUIVALENCE (NINCR0,ISETD(NINDAT))
      EQUIVALENCE (NINCR1,ISETD(NINDAT+1))
      EQUIVALENCE (NINCR2,ISETD(NINDAT+2))
      EQUIVALENCE (I7,ISETD(NSDATA))
      EQUIVALENCE (I1,ISETD(NXDATA))
      EQUIVALENCE (I4,ISETD(NDXDATA))
      EQUIVALENCE (I2,ISETD(NYDATA))
      EQUIVALENCE (I5,ISETD(NDYDATA))
      EQUIVALENCE (I3,ISETD(NZDATA))
      EQUIVALENCE (I6,ISETD(NDZDATA))
      EQUIVALENCE (I_NDX,ISETD(NNDXDATA))
      EQUIVALENCE (I_NDY,ISETD(NNDYDATA))
      EQUIVALENCE (I_NDZ,ISETD(NNDZDATA))
      EQUIVALENCE (NP,ISETD(NPDATA))
      EQUIVALENCE (NL,ISETD(NLDATA))
      EQUIVALENCE (IM,ISETD(NMDATA))
      EQUIVALENCE (IBGDAT,ISETD(NBGDAT))
      EQUIVALENCE (IENDAT,ISETD(NENDAT))
      LOGICAL ZEXIST, LTEST
      INTEGER JFILL
      INTEGER I,J,K,L,M,N
      INTEGER NLIM,ISET,JSET,J1,J2,J3,J4
      INTEGER JPOINTS,JSETS
      INTEGER IBY,LEVEL,NLEVEL,ILEVEL, IADD
      REAL ADD
      INTEGER ISETC(NSETD)
      INTEGER ISTEP 
      PARAMETER (ISTEP=2) 
      INTEGER IXP, IDXP, IYP, IDYP, IZP, IDZP, IXP1, IYP1, IZP1, ISZ
      REAL XP, DXP, YP, DYP, XP1, XP2, YP1, YP2, ZP1, ZP2, T, T1, T2, T3
     *, YMIN, YMAX
      DATA CXYZ/'X','Y','Z'/
      DATA HNONE /-666.666/
      JPOINTS=0
      JSETS=0
      N_NAME=6
      C_NAME='Fill %'
      IBY=0
      LEVEL=0
      IADD=1
      ADD=1.0
      GOTO 10011
10040 CONTINUE
      IADD=1
      ADD=1.0
      GOTO 10051
10080 CONTINUE
      IADD=2
      GOTO 10052
10060 GOTO ( 10080,10055),KEYORD
10090 CONTINUE
      IADD=1
      ADD=FLOTNG
      GOTO 10052
10051 CALL TOKEN(INFOIN,CARDIN,'Z,;')
      GOTO (10055,10060,10090,10090,10055,10055,10055,10060,10055),INTER
     *P
10055 CALL T2ERR(INFOIN,CARDIN,' ',5)
      GOTO 10052
10052 CONTINUE
      GOTO 10011
10100 CONTINUE
      LAPPEN=LTOKEN
      GOTO 10011
10110 CONTINUE
      LEVEL=INTEG
      GOTO 10011
10120 CONTINUE
      CALL T2XYZC(INFOIN,CARDIN,TLIM,2,NLIM)
      GOTO 10011
10130 CONTINUE
      CALL T2ROWS(INFOIN,CARDIN,N3,N4)
      GOTO 10011
10140 CONTINUE
      LLOG=LTOKEN
      GOTO 10011
10150 CONTINUE
      LMONITOR=LTOKEN
      GOTO 10011
10160 CONTINUE
      N_NAME=MAX(1,NSTRNG)
      C_NAME=STRNG(:N_NAME)
      GOTO 10011
10170 CONTINUE
      CALL T2PNTS(INFOIN,CARDIN,N1,N2)
      GOTO 10011
10180 CONTINUE
      CALL T2NSET(INFOIN,CARDIN,NS1,NS2)
      GOTO 10011
10190 CONTINUE
      IBY=INTEG
      CALL T2GDSET(IBY,1, 2 147 483 647  ,1, 2 147 483 647,ISETC,HNONE,'
     **')
      IF (ISETC(NBTYPE) .ne. 1) THEN
      CALL T2ERR(INFOIN,CARDIN,'*** ERROR *** Data set is mesh',4)
      RETURN
      ENDIF
      GOTO 10011
10200 CONTINUE
      C_SELECT=STRNG
      N_SELECT=MAX(1,NSTRNG)
      GOTO 10011
10011 CALL TOKEN(INFOIN,CARDIN,'ADD,APPEND:Y,LEVEL:0:9999:0,LIMITED,LINE
     *S,ROWS,LOG:Y,MONITOR:Y,NAME:S,POINTS,COLUMNS,SETS,BY:D,SELECT:S,;'
     *)
      GOTO (10012,10017,10015,10015,10015,10015,10011,10017, 10015),INTE
     *RP
10015 CALL T2ERR(INFOIN,CARDIN,' ',5)
      GOTO 10011
10017 GOTO ( 10040,10100,10110,10120,10130,10130,10140,10150,10160,10170
     *,10170,10180,10190,10200,10015),KEYORD
10012 CONTINUE
      IF (LTRAP) RETURN
      IF (IBY .eq. 0) THEN
      CALL T2ERR(INFOIN,CARDIN,'*** ERROR *** No FILL BY specified',4)
      ELSEIF ( IADD .eq. 2 .and. ISETC(NDZDATA) .eq. 1) THEN
      CALL T2ERR(INFOIN,CARDIN,'*** ERROR *** No Z coordinate if BY data
     * set',4)
      ENDIF
      IF (FLAGS(23)) RETURN
      YMIN= 1.0E35
      YMAX=-1.0E35
      DO 10211 J=0,ISETC(NPDATA)-1
      YP=DATBUF(ISETC(NINDAT+1)*J+ISETC(NCSIZE*(MESH2-1)+NXDATA))
      YMIN=MIN(YMIN,YP)
      YMAX=MAX(YMAX,YP)
10211 CONTINUE
10212 CONTINUE
      DO 10221 ISET=NS1,NS2
      CALL T2GDSET(ISET,N1,N2,N3,N4,ISETD,TLIM,C_SELECT(:N_SELECT))
      IF (MESH3 .ne. 3) THEN
      WRITE(OUTSTR,*)'*** ERROR *** Mesh dependent variable must be Z', 
     *' Set=',ISET
      CALL T2ERR(INFOIN,CARDIN,OUTSTR,-4)
      RETURN
      ENDIF
      IF(NP .lt. N1)GOTO 10221
      IF(ISET .eq. IBY)GOTO 10221
      IF(ISETD(NZDATA) .eq. 1)GOTO 10221
      IF (LAPPEN) THEN
      J1=1
      J2= 2 147 483 647
      J3=1
      J4= 2 147 483 647
      CALL T2SCOP(INFOIN,CARDIN,ISET,N1,N2,N3,N4  ,JSET,J1,J2,J3,J4,C_NA
     *ME(:N_NAME))
      CALL T2GDSET(ISET,J1,J2,J3,J4,ISETD,HNONE,'*')
      ELSE
      JSET=ISET
      J1=N1
      J2=N2
      J3=N3
      J4=N4
      ENDIF
      IF (IBTYPE .eq. 2) THEN
      NINCR1=NINCR1*NINCR0
      NINCR2=0
      ELSE
      NINCR1=NINCR0
      NINCR2=NINCR0
      NL=NP-J1+J3
      NP=J1
      ENDIF
      DYP=0
      DXP=0
      NLEVEL=1
      IF (ISETD(NDYDATA) .ne. 1) NLEVEL=MAX(1,LEVEL)
      IF (LEVEL .eq. 0) ISETD(NDXDATA)=1
      DO 10231 I=0,NL-J3
      IF (LTRAP) GOTO 10240
      YP=DATBUF(ISETD(NYDATA))
      IF (NLEVEL .gt. 1) THEN
      DYP=ABS(DATBUF(ISETD(NYDATA)+1))
      YP=YP-DYP
      DYP=2*DYP/(NLEVEL)
      YP=YP+DYP/2
      ENDIF
      DO 10251 ILEVEL=1,MAX(1,NLEVEL)
      ISZ=0
      IF (YP .ge. YMIN .and. YP .le. YMAX) THEN
      IXP1=ISETC(NCSIZE*(MESH1-1)+NXDATA)
      IYP1=ISETC(NCSIZE*(MESH2-1)+NXDATA)
      XP1=DATBUF(ISETC(NINDAT)*(ISETC(NPDATA)-1)+IXP1)
      YP1=DATBUF(ISETC(NINDAT)*(ISETC(NPDATA)-1)+IYP1)
      IF (IADD .eq. 2) THEN
      IZP1=ISETC(NCSIZE*(MESH3-1)+NXDATA)
      YP1=DATBUF(ISETC(NINDAT)*(ISETC(NPDATA)-1)+IZP1)
      ENDIF
      DO 10261 J=0,ISETC(NPDATA)-1
      XP2=DATBUF(IXP1)
      YP2=DATBUF(IYP1)
      IF (IADD .eq. 2) ZP2=DATBUF(IZP1)
      IF( YP .GE. MIN(YP1,YP2) .AND. YP .LT. MAX(YP1,YP2)) THEN
      T=XP1+(XP2-XP1)*(YP-YP1)/(YP2-YP1)
      ISZ=ISZ+1
      BUFFER(ISZ)=T
      IF (IADD .eq. 2) THEN
      ADD=DATBUF(IZP1)
      ENDIF
      ISZ=ISZ+1
      BUFFER(ISZ)=ADD
      IF(ISZ .GT. 400-ISTEP)GOTO 10262
      ENDIF
      IXP1=IXP1+ISETC(NINDAT)
      IYP1=IYP1+ISETC(NINDAT)
      IZP1=IYP1+ISETC(NINDAT)
      XP1=XP2
      YP1=YP2
      ZP1=ZP2
10261 CONTINUE
10262 CONTINUE
      ISZ=ISZ-ISTEP+1
      IF (ISZ .gt. ISTEP) THEN
      DO 10271 J=1,ISZ,ISTEP
      DO 10281 K=J,ISZ,ISTEP
      IF (BUFFER(K) .lt. BUFFER(J)) THEN
      DO 10291 L=0,ISTEP-1
      T=BUFFER(K+L)
      BUFFER(K+L)=BUFFER(J+L)
      BUFFER(J+L)=T
10291 CONTINUE
10292 CONTINUE
      ENDIF
10281 CONTINUE
10282 CONTINUE
10271 CONTINUE
10272 CONTINUE
      IXP=ISETD(NXDATA)
      IZP=ISETD(NZDATA)
      JFILL=0
      DO 10301 J=0,NP-J1
      IF (LTRAP) GOTO 10240
      XP=DATBUF(IXP)
      IF (ISETD(NDXDATA) .ne. 1) THEN
      DXP=ABS(DATBUF(IXP+1))
      ENDIF
      IF (XP+DXP .ge. BUFFER(1) .and. XP-DXP .le. BUFFER(ISZ)) THEN
      DO 10311 K=1,ISZ,2*ISTEP
      IF(XP+DXP .lt. BUFFER(K))GOTO 10311
      IF(XP-DXP .gt. BUFFER(K+ISTEP))GOTO 10311
      IF(BUFFER(K) .eq. BUFFER(K+ISTEP))GOTO 10311
      JFILL=1
      IF (DXP .eq. 0) THEN
      T= BUFFER(K+1) + (BUFFER(K+ISTEP+1)-BUFFER(K+1))  *(XP-BUFFER(K))/
     *(BUFFER(K+ISTEP)-BUFFER(K))
      DATBUF(IZP)=DATBUF(IZP)+T/NLEVEL
      GOTO 10312
      ELSE
      T2=MIN(BUFFER(K+ISTEP),XP+DXP)
      T1=MAX(BUFFER(K),XP-DXP)
      T3=( T2 - T1 )/DXP
      T =T3*  (BUFFER(K+1) + (BUFFER(K+ISTEP+1)-BUFFER(K+1))  *((T2+T1)/
     *2-BUFFER(K))  /(BUFFER(K+ISTEP)-BUFFER(K)))
      DATBUF(IZP)=DATBUF(IZP)+T/NLEVEL
      ENDIF
10311 CONTINUE
10312 CONTINUE
      ENDIF
      JPOINTS=JPOINTS+JFILL
      IXP=IXP+NINCR0
      IZP=IZP+NINCR0
10301 CONTINUE
10302 CONTINUE
      ENDIF
      ENDIF
      YP=YP+DYP
10251 CONTINUE
10252 CONTINUE
      ISETD(NXDATA)=ISETD(NXDATA)+NINCR2
      ISETD(NYDATA)=ISETD(NYDATA)+NINCR1
      ISETD(NZDATA)=ISETD(NZDATA)+NINCR1
10231 CONTINUE
10232 CONTINUE
      IF (LMONITOR) THEN
      CALL TXVOID
      IF (JSETS .gt. 1) CALL T2WAIT('Fill:',LTEST)
      CALL TXNEXT
      CALL T2REST
      NXYLIM(3,1)=NDSETS
      NXYLIM(3,2)=NDSETS
      DO 10321 I=1,2
      DO 10331 J=1,3
      EXYZLIM(J,I)=TLIM(J,I)
10331 CONTINUE
10332 CONTINUE
10321 CONTINUE
10322 CONTINUE
      CALL T2GDSET(JSET,J1,J2,J3,J4,ISETD,TLIM,'*')
      NXYLIM(1,1)=J1
      NXYLIM(1,2)=J2
      NXYLIM(2,1)=J3
      NXYLIM(2,2)=J4
      NXYLIM(3,1)=JSET
      NXYLIM(3,2)=JSET
      DO 10341 I=1,2
      DO 10351 J=1,3
      EXYZLIM(J,I)=TLIM(J,I)
10351 CONTINUE
10352 CONTINUE
10341 CONTINUE
10342 CONTINUE
      IF (IBTYPE .eq. 1) THEN
      CALL T2JOIN(DATBUF(I1),DATBUF(I2),DATBUF(I3), 0,1,NP-N1+1,NINCR0)
      ELSE
      CALL T23JIN(DATBUF(IM),NINCR0,NINCR1,NINCR2,0,0, J1,NP-J1+1,J3,NL-
     *J3+1)
      ENDIF
      CALL T2_PLOT_TITLE(JSET,.false.)
      CALL TXVOID
      ENDIF
      JSETS=JSETS+1
10221 CONTINUE
10222 CONTINUE
10240 CONTINUE
      IF (LTRAP .OR. LLOG .or. JPOINTS .le. 0) WRITE(6,*) '  FILL ',JPOI
     *NTS,' points ', JSETS,' Sets by ',IBY
      END
