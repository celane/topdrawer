C        MORTRAN 2.79 (BRACKETED KEYWORD MACROS OF 09/28/81)          
      SUBROUTINE T2_SAVE_DATA(INFOIN,CARDIN)
      IMPLICIT NONE
      INTEGER INFOIN(10)
      CHARACTER *(*) CARDIN
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
      LOGICAL LAPPEN, LCONF, LMONITOR, LCHECK, LLOG
      INTEGER N1,N2,N3,N4,NS1,NS2
      REAL TLIM(3,2)
      COMMON /T2SELECT/N1 ,N2 ,N3 ,N4 ,NS1 ,NS2 ,TLIM ,LMONITOR ,LAPPEN
     * ,LCONF ,LLOG ,LCHECK
      INTEGER LISTPT,NPTMAX,NDSETS
      REAL DATBUF(10248)
      COMMON /T2XPNT/ LISTPT,NPTMAX,NDSETS,DATBUF
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
      REAL HNONE
      LOGICAL LOPEN
      INTEGER I,J,K,ILINE,M1,M2,ISET,IPTS,JPTS
      INTEGER NLIM,JLIM,ISTAT, T2BTRIM
      REAL T2MSHX,T2MSHY
      REAL T,DLIM(3,2),XFAC,YFAC,ZFAC,DVEC(19)
      CHARACTER*3 C_STATUS
      CHARACTER*10 C_ACCESS
      DATA HNONE /-666.666/
      C_FILE='tdsave.tdb'
      N_FILE=10
      C_NAME='TOPDRAWER data'
      GOTO 10011
10040 CONTINUE
      LAPPEN=LTOKEN
      GOTO 10011
10050 CONTINUE
      CALL T2NSET(INFOIN,CARDIN,NS1,NS2)
      GOTO 10011
10060 CONTINUE
      CALL T2PNTS(INFOIN,CARDIN,N1,N2)
      GOTO 10011
10070 CONTINUE
      LLOG=LTOKEN
      GOTO 10011
10080 CONTINUE
      CALL T2ROWS(INFOIN,CARDIN,N3,N4)
      GOTO 10011
10090 CONTINUE
      C_SELECT=STRNG
      N_SELECT=MAX(1,NSTRNG)
      GOTO 10011
10100 CONTINUE
      CALL T2BNDA(INFOIN,CARDIN,TLIM)
      GOTO 10011
10110 CONTINUE
      CALL T2XYZC(INFOIN,CARDIN,TLIM,2,NLIM)
      GOTO 10011
10120 CONTINUE
      C_FILE=STRNG
      N_FILE=NSTRNG
      CALL T2_SET_DEFAULT_FILE(c_file,N_file,'tdsave.tdb')
      GOTO 10011
10130 CONTINUE
      IF (NSTRNG.gt.0) C_NAME=STRNG(1:NSTRNG)
      GOTO 10011
10011 CALL TOKEN(INFOIN,CARDIN,'APPEND:Y,SETS,POINTS,COLUMNS,LOG:Y,LINES
     *,ROWS,SELECT:S,SLICES,LIMITED,FILE:F,OUTPUT:F,NAME:T,;')
      GOTO (10012,10017,10015,10015,10015,10015,10011,10017, 10015),INTE
     *RP
10015 CALL T2ERR(INFOIN,CARDIN,' ',5)
      GOTO 10011
10017 GOTO ( 10040,10050,10060,10060,10070,10080,10080,10090,10100,10110
     *,10120,10120,10130,10015),KEYORD
10012 CONTINUE
      CALL T2DTLM(TLIM,N1,N2,N3,N4,NS1,NS2,XFAC,YFAC,ZFAC,DLIM,2,'*')
      IF (DLIM(1,1).gt.DLIM(1,2)) THEN
      CALL T2ERR(INFOIN,CARDIN,'*** ERROR *** No data selected',4)
      RETURN
      ENDIF
      IF (LAPPEN) THEN
      C_STATUS='OLD'
      C_ACCESS='APPEND'
      ELSE
      C_STATUS='NEW'
      C_ACCESS='SEQUENTIAL'
      ENDIF
      OPEN(ERR=10140 ,UNIT=1  ,FILE=C_FILE(1:N_FILE)  ,STATUS=C_STATUS ,
     *ACCESS=C_ACCESS  ,IOSTAT=ISTAT ,FORM='UNFORMATTED')
      CALL T2_CONCEAL_PASSWORD(C_FILE,N_FILE)
      IF (.not. LAPPEN) THEN
      N_NAME=80
      WRITE(1,ERR=10150)N_NAME,C_NAME(:N_NAME)
      ENDIF
      J=0
      JPTS=0
      DO 10161 ISET=NS1,NS2
      IPTS=0
      CALL T2GDSET(ISET,N1,N2,N3,N4,ISETD,TLIM,C_SELECT(:N_SELECT))
      IF(NP .lt. N1)GOTO 10161
      CALL T2_GET_SET_NAME(IBGDAT,C_NAME)
      N_NAME=T2BTRIM(C_NAME)
      IF (C_NAME(:N_NAME) .ne. ' ') THEN
      WRITE(1,ERR=10150)-1,0
      WRITE(1,ERR=10150)N_NAME,C_NAME(:N_NAME)
      ENDIF
      J=0
      IF (IBTYPE.eq.2) THEN
      M2=NL-N3+2
      M1=NP-N1+2
      IF(M2.le.1 .or. M1.le.1)GOTO 10161
      WRITE(1,ERR=10150)IBTYPE,J
      IF (NINCR0.eq.2) M1=-M1
      WRITE(1,ERR=10150)M1,M2
      M1=ABS(M1)-1
      M2=M2-1
      ILINE=1
      WRITE(1,ERR=10150)ILINE,(DATBUF(IM+J),J=0,NINCR0-1)  ,(DATBUF(I1+J
     *),J=0,M1*NINCR0-1)
      DO 10171 I=1,M2
      ILINE=ILINE+1
      WRITE(1,ERR=10150)ILINE,(DATBUF(I2+J),J=0,NINCR0-1)  ,(DATBUF(I3+J
     *),J=0,M1*NINCR0-1)
      I3=I3+NINCR1*NINCR0
      I2=I2+NINCR1*NINCR0
10171 CONTINUE
10172 CONTINUE
      ILINE=ILINE+1
      DO 10181 I=1,NINCR0
      BUFFER(I)= T2MSHX(DATBUF(IM),NINCR0,NINCR1,NINCR2,N1)
      BUFFER(I+NINCR0)= T2MSHX(DATBUF(IM),NINCR0,NINCR1,NINCR2,NP+1)
      BUFFER(I+2*NINCR0)=T2MSHY(DATBUF(IM),NINCR0,NINCR1,NINCR2,N3)
      BUFFER(I+3*NINCR0)=T2MSHY(DATBUF(IM),NINCR0,NINCR1,NINCR2,NL+1)
10181 CONTINUE
10182 CONTINUE
      WRITE(1,ERR=10150) ILINE,(BUFFER(J),J=1,4*NINCR0)
      IPTS=(M1)*(M2)
      ELSEIF (IBTYPE.eq.1) THEN
      WRITE(1,ERR=10150)IBTYPE,J
      WRITE(1,ERR=10150)NINCR,(I_VORDER(IVARBL(J),2),J=1,NINCR)
      JLIM=0
      IF (TLIM(1,1).ne.HNONE) THEN
      JLIM=2
      IF (DATBUF(I3).ne.HNONE) JLIM=3
      DO 10191 J=1,19
      DVEC(J)=0
10191 CONTINUE
10192 CONTINUE
      I1=IM
      ENDIF
      DO 10201 I=N1,NP
      IF (JLIM.gt.0) THEN
      DO 10211 J=1,NINCR
      DVEC(IVARBL(J))=DATBUF(I1)
      I1=I1+1
10211 CONTINUE
10212 CONTINUE
      DO 10221 J=1,JLIM
      IF (DVEC(J*2).lt.TLIM(J,1) .or. DVEC(J*2).gt.TLIM(J,2)) GOTO 10230
     *
10221 CONTINUE
10222 CONTINUE
      ENDIF
      WRITE(1,ERR=10150)(DATBUF(J),J=IM,IM+NINCR-1)
      IM=IM+NINCR
      IPTS=IPTS+1
10230 CONTINUE
10201 CONTINUE
10202 CONTINUE
      WRITE(1,ERR=10150)(HNONE,J=1,NINCR)
      ENDIF
      JPTS=JPTS+IPTS
      IF (LLOG) WRITE(6,*)'   ',IPTS, ' Points written from set',ISET,' 
     *Name=', '"'//C_NAME(:N_NAME)//'"'
10161 CONTINUE
10162 CONTINUE
      IF (JPTS.le.0) THEN
      CALL T2ERR(0,' ',('*** WARNING *** No data saved'),3)
      ELSEIF(LLOG)THEN
      IF (LAPPEN) THEN
      WRITE(6,*)'Appended to:',C_FILE(:N_FILE)
      ELSE
      WRITE(6,*)'to:',C_FILE(:N_FILE)
      ENDIF
      ENDIF
      GOTO 10240
10150 CONTINUE
      CALL T2_TYPE_ERROR(INFOIN,CARDIN,' ',ISTAT)
10240 CONTINUE
      CLOSE(UNIT=1,ERR=10250)
10250 CONTINUE
      RETURN
10140 CONTINUE
      CALL T2_TYPE_ERROR(INFOIN,CARDIN,C_FILE(:N_FILE),ISTAT)
      END
      SUBROUTINE T2_RESTORE_DATA(INFOIN,CARDIN)
      IMPLICIT NONE
      INTEGER INFOIN(10)
      CHARACTER *(*) CARDIN
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
      LOGICAL LAPPEN, LCONF, LMONITOR, LCHECK, LLOG
      INTEGER N1,N2,N3,N4,NS1,NS2
      REAL TLIM(3,2)
      COMMON /T2SELECT/N1 ,N2 ,N3 ,N4 ,NS1 ,NS2 ,TLIM ,LMONITOR ,LAPPEN
     * ,LCONF ,LLOG ,LCHECK
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
      REAL HNONE
      INTEGER LISTPT,NPTMAX,NDSETS
      REAL DATBUF(10248)
      COMMON /T2XPNT/ LISTPT,NPTMAX,NDSETS,DATBUF
      LOGICAL LGET, T2_MATCHC
      CHARACTER*8 C_TYPE(2)
      INTEGER T2_CONFIRM
      INTEGER I,J,K,IV(19),JV(19),IPTR,IPTS,JPTS
      INTEGER N,NV,ISTAT,ISETS,NSETS,T2BTRIM
      REAL VAL1,VAL2,VAL3
      EQUIVALENCE (IPTR,IVRPTR(9))
      CHARACTER*3 CXYZ
      INTEGER IAND,IOR
      LOGICAL T2_VIRT,DMMY
      DATA HNONE /-666.666/
      DATA VAL3/0.0/
      DATA CXYZ/'XYZ'/
      DATA C_TYPE/' XY data',' Mesh'/
      C_FILE='tdsave.tdb'
      N_FILE=10
      NS2=9999
      GOTO 10261
10290 CONTINUE
      LCONF=LTOKEN
      GOTO 10261
10300 CONTINUE
      N_SELECT=MAX(1,NSTRNG)
      C_SELECT=STRNG(:N_SELECT)
      GOTO 10261
10310 CONTINUE
      VAL1=1
      VAL2=9999.
      CALL T2FRTO(INFOIN,CARDIN,VAL1,VAL2,VAL3)
      NS1=VAL1
      NS2=VAL2
      VAL3=NS2
      GOTO 10261
10320 CONTINUE
      LAPPEN=LTOKEN
      GOTO 10261
10330 CONTINUE
      LLOG=LTOKEN
      GOTO 10261
10340 CONTINUE
      C_FILE=STRNG(1:NSTRNG)
      N_FILE=NSTRNG
      CALL T2_SET_DEFAULT_FILE(c_file,N_file,'tdsave.tdb')
      GOTO 10261
10261 CALL TOKEN(INFOIN,CARDIN,'CONFIRM:Y,SELECT:S,SETS,APPEND:Y,LOG:Y,F
     *ILE:F,INPUT:F,;')
      GOTO (10262,10267,10265,10265,10265,10265,10261,10267, 10265),INTE
     *RP
10265 CALL T2ERR(INFOIN,CARDIN,' ',5)
      GOTO 10261
10267 GOTO ( 10290,10300,10310,10320,10330,10340,10340,10265),KEYORD
10262 CONTINUE
      OPEN(ERR=10140 ,UNIT=1  ,FILE=C_FILE(1:N_FILE)  ,STATUS='OLD',   I
     *OSTAT=ISTAT ,FORM='UNFORMATTED')
      CALL T2_CONCEAL_PASSWORD(C_FILE,N_FILE)
      READ(1,END=10240,ERR=10150)N_NAME,C_NAME(:80)
      IF (N_NAME.ne.80) GOTO 10150
      N_NAME=T2BTRIM(C_NAME(:N_NAME))
      LLOG=LLOG .or. LCONF
      IF (llog) THEN
      write(6,*)'  Name=','"'//c_name(:n_name)//'"'
      WRITE(6,*)'    File=',C_FILE(1:N_FILE)
      ENDIF
      IF (.not.LAPPEN) CALL T2PNTR
      JPTS=0
      N_NAME=1
      C_NAME(:N_NAME)=' '
      NSETS=1
10351 CONTINUE
      READ(1,END=10240,ERR=10150)IBTYPE,J
      IF (J.ne.0) GOTO 10150
      IPTS=0
      ISETS=1
      LGET = T2_MATCHC(C_NAME(:N_NAME),C_SELECT(:N_SELECT),FLAGS(130)) .
     *and.  NSETS .ge. NS1 .and. NSETS .le. NS2
      IF (LCONF .and. IBTYPE .gt. 0) THEN
      WRITE(6,*)' Set=',NSETS,' Name="',C_NAME(:N_NAME),'"', C_TYPE(IBTY
     *PE)
      GOTO (10360,10370,10380,10390) T2_CONFIRM('Get')
10380 CONTINUE
      GOTO 10352
10390 CONTINUE
      LCONF=.false.
10360 CONTINUE
      LGET=.true.
      GOTO 10400
10370 CONTINUE
      LGET=.false.
10400 CONTINUE
      ENDIF
      IF (IBTYPE.eq.2) THEN
      NSETS=NSETS+1
      READ(1,END=10150,ERR=10150)NINCR1,NINCR2
      IF (LGET) THEN
      NINCR0=1
      IF (NINCR1.lt.0) THEN
      NINCR1=ABS(NINCR1)
      NINCR0=2
      ENDIF
      IF (REVLEV.lt.3) CALL T2PNTR
      CALL T2NEWM(NINCR0,NINCR1,NINCR2,C_NAME(:N_NAME))
      IF (FLAGS(23)) GOTO 10240
      IPTR=IVRPTR(8)
      DO 10411 I=1,NINCR2
      READ(1,END=10150,ERR=10150)K, (DATBUF(J),J=IPTR,IPTR+NINCR1*NINCR0
     *-1)
      IF (K.ne.I) GOTO 10150
      IPTR=IPTR+NINCR0*NINCR1
10411 CONTINUE
10412 CONTINUE
      READ(1,END=10150,ERR=10150) K,(DATBUF(J),J=IPTR,IPTR+4*NINCR0-1)
      IF (K.ne.I) GOTO 10150
      CALL T23SMS(0)
      IPTS=(NINCR1-1)*(NINCR2-1)
      ELSE
      DO 10421 I=1,NINCR2+1
      READ(1,END=10150,ERR=10150)K
      IF (K.ne.I) GOTO 10150
10421 CONTINUE
10422 CONTINUE
      ENDIF
      ELSEIF (IBTYPE.eq.1) THEN
      NSETS=NSETS+1
      N=1
      READ (1,END=10150,ERR=10150) NV,(IV(J),J=1,MIN(MAX(NV,1),19))
      IF (NV.lt.3.or.NV.gt.19) GOTO 10150
      DO 10431 J=1,NV
      IF (IV(J) .gt. 19) GOTO 10150
10431 CONTINUE
10432 CONTINUE
      I=0
      DO 10441 J=1,NINCR
      I=IOR(I,2**IVARBL(J))
10441 CONTINUE
10442 CONTINUE
      DO 10451 J=1,NV
      IF(IV(J) .gt. 19 .or. IV(J).lt.1) GOTO 10150
      IV(J)=I_VORDER(IV(J),1)
      I=IOR(I,2**IV(J))
10451 CONTINUE
10452 CONTINUE
      JV(1)=0
      DO 10461 J=1,19
      IF (IAND(I,2**J).ne.0) THEN
      JV(1)=JV(1)+1
      JV(JV(1)+1)=J
      ENDIF
10461 CONTINUE
10462 CONTINUE
      IF (LGET) CALL T2SETS(JV)
      IF (LGET) CALL T2NEWS(0,C_NAME(:N_NAME))
      IF (FLAGS(23)) GOTO 10240
      CALL T2GDSET(NDSETS,1,1,1,1,ISETD,HNONE,'*')
10471 CONTINUE
      READ(1,END=10480,ERR=10150) (BUFFER(IV(I)),I=1,NV)
      IF(BUFFER(1) .eq. HNONE)GOTO 10472
      IF (LGET) THEN
      IF (BUFFER(1).ge.1.0E35) THEN
      CALL T2NEWS(0,' ')
      CALL T2GDSET(NDSETS,1,1,1,1,ISETD,HNONE,'*')
      ELSE
      IF ((NPTMAX-IVRPTR(12)+1)/NINCR.le.0) THEN
C     CALL T2_VIRT(500*NINCR)
      DMMY=T2_VIRT(500*NINCR)	! for unix-fortran
      CALL T2GDSET(NDSETS,IPTS+1,IPTS,1,1,ISETD,HNONE,'*')
      IF ((NPTMAX-IVRPTR(12)+1)/NINCR.le.0) THEN
      CALL T2ERR(INFOIN,CARDIN,'*** WARNING *** Storage too small',4)
      GOTO 10240
      ENDIF
      ENDIF
      DO 10491 I=1,NINCR
      DATBUF(IM)=BUFFER(IVARBL(I))
      IM=IM+1
10491 CONTINUE
10492 CONTINUE
      DATBUF(IBGDAT+2)=DATBUF(IBGDAT+2)+NINCR
      IVRPTR(12)=IM
      NPOINT=NPOINT+1
      IPTS=IPTS+1
      ENDIF
      ENDIF
      GOTO 10471
10472 CONTINUE
      ELSEIF (IBTYPE .eq. -1) THEN
      READ(1,ERR=10150)N_NAME,C_NAME(:N_NAME)
      GOTO 10351
      ELSE
      GOTO 10150
      ENDIF
10480 CONTINUE
      IF (LGET) THEN
      JPTS=JPTS+IPTS
      FLAGS(28)=.false.
      IF (LLOG) THEN
      IF (IBTYPE.eq.1) THEN
      WRITE(OUTSTR,*)  '  Restore data ',IPTS,' points ',ISETS,' sets'
      ELSE
      WRITE(OUTSTR,*)  '  Restore Mesh ',CXYZ(MESH1:MESH1),NINCR1-1, ' b
     *y ',CXYZ(MESH2:MESH2),NINCR2-1,' Set=',ndsets
      ENDIF
      CALL T2WRSQ(OUTSTR,.true.,6,3)
      WRITE(6,*)'    Name="',C_NAME(:N_NAME),'"'
      ENDIF
      ENDIF
      N_NAME=1
      C_NAME(:N_NAME)=' '
      GOTO 10351
10352 CONTINUE
      IF (JPTS.le.0) THEN
      WRITE(OUTSTR,*)'*** WARNING *** No data restored out of',N,' sets 
     *available'
      CALL T2ERR(INFOIN,CARDIN,OUTSTR,-4)
      ELSEIF (LLOG) THEN
      WRITE(6,*) 'From:',C_FILE(:N_FILE)
      ENDIF
      GOTO 10240
10150 CONTINUE
      CALL T2_TYPE_ERROR(INFOIN,CARDIN,' ',ISTAT)
10240 CONTINUE
      CLOSE(UNIT=1,ERR=10250)
10250 CONTINUE
      RETURN
10140 CONTINUE
      CALL T2_TYPE_ERROR(INFOIN,CARDIN,C_FILE(:N_FILE),ISTAT)
      END
