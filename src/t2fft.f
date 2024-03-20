C        MORTRAN 2.79 (BRACKETED KEYWORD MACROS OF 09/28/81)          
      SUBROUTINE T2FFT(INFOIN,CARDIN)
      IMPLICIT NONE
      INTEGER INFOIN(10)
      CHARACTER*(*) CARDIN
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
      INTEGER LISTPT,NPTMAX,NDSETS
      REAL DATBUF(10248)
      COMMON /T2XPNT/ LISTPT,NPTMAX,NDSETS,DATBUF
      REAL HNONE
      LOGICAL LINVER ,LHIS ,CFT ,LPOLAR ,LFIRST ,LTEST
      INTEGER T2BTRIM
      INTEGER IPTS(2),ISET,JSET
      INTEGER IREAL,IIMAG
      INTEGER I,J,K,L,M
      REAL T,X0(2),DX(2)
      LOGICAL TDSETS,DMMY
      DATA HNONE /-666.666/
      LPOLAR=.false.
      LINVER=.false.
      N_NAME=-7
      C_NAME='Fft   %'
      GOTO 10011
10040 CONTINUE
      LAPPEN=LTOKEN
      GOTO 10011
10050 CONTINUE
      LCHECK=LTOKEN
      GOTO 10011
10060 CONTINUE
      LINVER=LTOKEN
      GOTO 10011
10070 CONTINUE
      LLOG=LTOKEN
      GOTO 10011
10080 CONTINUE
      LMONITOR=LTOKEN
      GOTO 10011
10090 CONTINUE
      N_NAME=MAX(1,NSTRNG)
      C_NAME=STRNG(:N_NAME)
      GOTO 10011
10100 CONTINUE
      CALL T2NSET(INFOIN,CARDIN,NS1,NS2)
      GOTO 10011
10110 CONTINUE
      C_SELECT=STRNG
      N_SELECT=MAX(1,NSTRNG)
      GOTO 10011
10120 CONTINUE
      LPOLAR=LTOKEN
      GOTO 10011
10011 CALL TOKEN(INFOIN,CARDIN,'APPEND:Y,CHECK:Y,INVERT:Y,LOG:Y,MONITOR:
     *Y,NAME:S,SETS,SELECT:S,POLAR:Y,;')
      GOTO (10012,10017,10015,10015,10015,10015,10011,10017, 10015),INTE
     *RP
10015 CALL T2ERR(INFOIN,CARDIN,' ',5)
      GOTO 10011
10017 GOTO ( 10040,10050,10060,10070,10080,10090,10100,10110,10120,10015
     *),KEYORD
10012 CONTINUE
      IF (FLAGS(23)) RETURN
      IPTS(1)=1
      IPTS(2)=1
      DO 10131 ISET=NS1,NS2
      CALL T2GDSET(ISET,N1,N2,N3,N4,ISETD,HNONE,C_SELECT(:N_SELECT))
      IF (IBTYPE.eq.2) I3=I6
      IF (DATBUF(I3).eq.HNONE .and. DATBUF(I5).eq.HNONE) THEN
      CALL T2ERR(INFOIN,CARDIN,'*** ERROR *** Missing Z or DY coordinate
     *',4)
      RETURN
      ENDIF
      DO 10141 I=1,IBTYPE
      IF (LCHECK) THEN
      CALL T2FHIS(ISET,I,N1,N2,N3,N4,LHIS,IPTS,X0,DX)
      IF (.not. LHIS) THEN
      CALL T2ERR(0,' ','*** ERROR *** Data set not hist',3)
      RETURN
      ENDIF
      ENDIF
10141 CONTINUE
10142 CONTINUE
10131 CONTINUE
10132 CONTINUE
      LFIRST=.true.
      DO 10151 ISET=NS1,NS2
      CALL T2GDSET(ISET,N1,N2,N3,N4,ISETD,HNONE,C_SELECT(:N_SELECT))
      DO 10161 I=1,IBTYPE
      IPTS(I)=1
      CALL T2FHIS(ISET,I,N1,N2,N3,N4,LHIS,IPTS(I),X0(I),DX(I))
10161 CONTINUE
10162 CONTINUE
      JSET=ISET
      IF (LMONITOR) THEN
      CALL TXVOID
      IF (.not. LFIRST) CALL T2WAIT('FFT:',LTEST)
      CALL TXNEXT
      CALL T2REST
C     CALL TDSETS('WINDOW 1 OF 2')
      DMMY=TDSETS('WINDOW 1 OF 2')	! for unix-fortran
      NXYLIM(3,1)=ISET
      NXYLIM(3,2)=ISET
      IF (IBTYPE .eq. 1) THEN
      CALL T2HIST(DATBUF(I1),DATBUF(I2),DATBUF(I3), DATBUF(I4),DATBUF(I5
     *),DATBUF(I6), 0,0,NP-N1+1,NINCR0, HNONE,HNONE,HNONE)
      ELSE
      CALL T23JIN(DATBUF(IM),NMESH0,NMESH1,NMESH2,0,0, 1,NP,1,NL)
      ENDIF
      CALL T2_PLOT_TITLE(ISET,.false.)
      CALL TXVOID
      ENDIF
      LFIRST=.false.
      IF (LAPPEN) THEN
      IF (LINVER .and. N_NAME .lt. 0) C_NAME(4:5)='-1'
      IF (IBTYPE .EQ. 1) THEN
      CALL T2NEWS(IPTS,C_NAME(:ABS(N_NAME)))
      J=IVRPTR(8)
      ELSE
      CALL T2NEWM(NINCR0,NP+1,NL+1,C_NAME(:ABS(N_NAME)))
      J=IVRPTR(2)-NINCR0
      ENDIF
      J=IVRPTR(8)
      IF (FLAGS(23)) RETURN
      CALL T2GDSET(ISET,N1,N2,N3,N4,ISETD,HNONE,C_SELECT(:N_SELECT))
      DO 10171 I=IM,IENDAT
      DATBUF(J)=DATBUF(I)
      J=J+1
10171 CONTINUE
10172 CONTINUE
      JSET=NDSETS
      CALL T2GDSET(JSET,N1,N2,N3,N4,ISETD,HNONE,'*')
      ENDIF
      J=NINCR0
      IF (LINVER)J=-NINCR0
      IF (IBTYPE.eq.1) THEN
      IF (DATBUF(I3).eq.HNONE) I3=I5
      IREAL=I2
      IIMAG=I3
      IF (LPOLAR.and.LINVER) THEN
      CALL T2POLCV(DATBUF(I2),DATBUF(I3), DATBUF(I2),DATBUF(I3),-NP,NINC
     *R0,.false.,radang)
      ENDIF
      FLAGS(23)=.not. CFT(DATBUF(I2),DATBUF(I3),NP,NP,NP,J)
      ELSE
      IREAL=I3
      IIMAG=I6
      NINCR1=NINCR1*NINCR0
      K=I3
      L=I3
      DO 10181 I=1,NL
      DO 10191 M=1,NINCR0*NP
      DATBUF(K)=DATBUF(L)
      K=K+1
      L=L+1
10191 CONTINUE
10192 CONTINUE
      L=L+NINCR0
10181 CONTINUE
10182 CONTINUE
      FLAGS(23)=.not. CFT(DATBUF(I3),DATBUF(I6),NP*NL,NP,NP,J)
      IF (FLAGS(23)) GOTO 10200
      FLAGS(23)=.not. CFT(DATBUF(I3),DATBUF(I6),NP*NL,NL,NP*NL,J)
      ENDIF
      IF (FLAGS(23)) GOTO 10200
      IF (.not. LINVER) THEN
      T=NP*IPTS(2)
      J=IREAL
      DO 10211 I=1,NP*IPTS(2)
      DATBUF(J)=DATBUF(J)/T
      J=J+NINCR0
10211 CONTINUE
10212 CONTINUE
      J=IIMAG
      DO 10221 I=1,NP*IPTS(2)
      DATBUF(J)=DATBUF(J)/T
      J=J+NINCR0
10221 CONTINUE
10222 CONTINUE
      ENDIF
      IF (IBTYPE.eq.2) THEN
      DO 10231 I=1,NL-1
      L=L-NINCR0
      DO 10241 M=1,NINCR0*NP
      K=K-1
      L=L-1
      DATBUF(L)=DATBUF(K)
10241 CONTINUE
10242 CONTINUE
10231 CONTINUE
10232 CONTINUE
      ELSEIF (LPOLAR.and. .not. LINVER) THEN
      CALL T2POLCV(DATBUF(I3),DATBUF(I2), DATBUF(I3),DATBUF(I2),IPTS,NIN
     *CR0,.false.,radang)
      ENDIF
      DO 10251 I=1,IBTYPE
      DX(I)=1/(IPTS(I)*DX(I))
10251 CONTINUE
10252 CONTINUE
      J=I1
      DO 10261 I=1,NP
      DATBUF(J)=(I-1)*DX(1)
      J=J+NINCR0
10261 CONTINUE
10262 CONTINUE
      IF (DATBUF(I4).ne.HNONE) THEN
      J=I4
      DO 10271 I=1,NP
      DATBUF(J)=DX(1)/2
      J=J+NINCR0
10271 CONTINUE
10272 CONTINUE
      ENDIF
      IF (IBTYPE.eq.2) THEN
      J=I2
      DO 10281 I=1,IPTS(2)
      DATBUF(J)=(I-1)*DX(2)
      J=J+NINCR1
10281 CONTINUE
10282 CONTINUE
      DO 10291 I=1,2*NINCR0
      DATBUF(J)=0
      J=J+1
10291 CONTINUE
10292 CONTINUE
      IF (DATBUF(I5).ne.HNONE) THEN
      J=I5
      DO 10301 I=1,IPTS(2)
      DATBUF(J)=DX(2)/2
      J=J+NINCR1
10301 CONTINUE
10302 CONTINUE
      ENDIF
      ENDIF
      IF (LMONITOR) THEN
C     CALL TDSETS('WINDOW 2 OF 2')
      DMMY=TDSETS('WINDOW 2 OF 2')	! for unix-fortran
      NXYLIM(3,1)=JSET
      NXYLIM(3,2)=JSET
      IF (IBTYPE .eq. 1) THEN
      CALL T2HIST(DATBUF(I1),DATBUF(I2),DATBUF(I3), DATBUF(I4),DATBUF(I5
     *),DATBUF(I6), 0,0,NP-N1+1,NINCR0, HNONE,HNONE,HNONE)
      ELSE
      CALL T23JIN(DATBUF(IM),NMESH0,NMESH1,NMESH2,0,0, 1,NP,1,NL)
      ENDIF
      CALL T2_PLOT_TITLE(JSET,.false.)
      CALL TXVOID
      ENDIF
      DATDAT(1,1)=HNONE
      IF (LLOG) then
      WRITE(OUTSTR,'(4(A,I4))')  '   Fourier transform of',ISET, ' to ',
     *Jset
      IF (LINVER)OUTSTR(T2BTRIM(OUTSTR)+2:)='Invert'
      IF (LPOLAR)OUTSTR(T2BTRIM(OUTSTR)+2:)='Polar'
      CALL T2WRSQ(OUTSTR,.true.,6,3)
      ENDIF
10151 CONTINUE
10152 CONTINUE
      IF (LFIRST) WRITE(6,*) '  NO data has been Fourier transformed'
      RETURN
10200 CONTINUE
      CALL T2ERR(0,' ','*** ERROR *** Failure to Fourier transform (boun
     *ds exceeded)',3)
      END
      SUBROUTINE T2TRNS(INFOIN,CARDIN)
      IMPLICIT NONE
      INTEGER INFOIN(10)
      CHARACTER*(*) CARDIN
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
      INTEGER LISTPT,NPTMAX,NDSETS
      REAL DATBUF(10248)
      COMMON /T2XPNT/ LISTPT,NPTMAX,NDSETS,DATBUF
      REAL HNONE
      CHARACTER*10 COPT(3)
      LOGICAL LLOG,LAPPEN,LERR
      INTEGER NS1,NS2,N1,N2,N3,N4,IFRTO,IOP(2),ISET,JSET,I,J
      DATA HNONE /-666.666/
      DATA COPT/'Cartesian','Polar','Spherical'/
      LLOG=FLAGS(95)
      LAPPEN=.false.
      LERR=.false.
      NS1=1
      NS2=NDSETS
      N_SELECT=1
      C_SELECT='*'
      N1=1
      N2= 2 147 483 647
      N3=1
      N4= 2 147 483 647
      IFRTO=0
      IOP(1)=1
      IOP(2)=1
      GOTO 10311
10340 CONTINUE
      LAPPEN=LTOKEN
      GOTO 10311
10350 CONTINUE
      LLOG=LTOKEN
      GOTO 10311
10360 CONTINUE
      CALL T2NSET(INFOIN,CARDIN,NS1,NS2)
      GOTO 10311
10370 CONTINUE
      C_SELECT=STRNG
      N_SELECT=MAX(1,NSTRNG)
      GOTO 10311
10380 CONTINUE
      IFRTO=INTEG
      GOTO 10311
10390 CONTINUE
      IF (IFRTO.eq.0) THEN
      CALL T2ERR(INFOIN,CARDIN,'*** ERROR *** Missing FROM/TO',4)
      RETURN
      ENDIF
      IOP(IFRTO)=INTEG
      IFRTO=0
      GOTO 10311
10311 CALL TOKEN(INFOIN,CARDIN,'APPEND:Y,LOG:Y,SETS,SELECT:S,FROM:1,TO:2
     *,CARTESIAN:1,POLAR:2,SPHERICAL:3,;')
      GOTO (10312,10317,10315,10315,10315,10315,10311,10317, 10315),INTE
     *RP
10315 CALL T2ERR(INFOIN,CARDIN,' ',5)
      GOTO 10311
10317 GOTO ( 10340,10350,10360,10370,10380,10380,10390,10390,10390,10315
     *),KEYORD
10312 CONTINUE
      IF (FLAGS(23)) RETURN
      DO 10401 ISET=NS1,NS2
      JSET=ISET
      CALL T2GDSET(ISET,N1,N2,N3,N4,ISETD,HNONE,C_SELECT(:N_SELECT))
      IF(IBTYPE.eq.2)GOTO 10401
      IF ((IOP(1).ge.3 .or. IOP(2) .ge.3) .and. (DATBUF(I3).eq.HNONE) ) 
     *THEN
      CALL T2ERR(0,' ','*** ERROR *** No Z coordinate',3)
      RETURN
      ENDIF
      IF (LAPPEN) THEN
      CALL T2NEWS(NP,'%')
      IF (FLAGS(23)) RETURN
      J=IVRPTR(8)
      CALL T2GDSET(ISET,N1,N2,N3,N4,ISETD,HNONE,C_SELECT(:N_SELECT))
      DO 10411 I=1,NP*NINCR0
      DATBUF(J)=DATBUF(I7)
      J=J+1
      I7=I7+1
10411 CONTINUE
10412 CONTINUE
      JSET=NDSETS
      CALL T2GDSET(JSET,N1,N2,N3,N4,ISETD,HNONE,'*')
      ENDIF
      DO 10421 IFRTO=1,2
      NP=-NP
      LERR=DATBUF(I4).ne.HNONE .and. DATBUF(I5).ne.HNONE .and.  (IOP(IFR
     *TO) .lt. 3 .or. DATBUF(I6).ne.HNONE)
      GOTO (10430,10440,10450) IOP(IFRTO)
      GOTO 10430
10440 CONTINUE
      CALL T2POLCV(DATBUF(I1),DATBUF(I2), DATBUF(I1),DATBUF(I2), NP,NINC
     *R0,LERR,radang)
      GOTO 10430
10450 CONTINUE
      CALL T2SPHCV(DATBUF(I1),DATBUF(I2),DATBUF(I3), DATBUF(I1),DATBUF(I
     *2),DATBUF(I3), NP,NINCR0,LERR,radang)
10430 CONTINUE
10421 CONTINUE
10422 CONTINUE
      DATDAT(1,1)=HNONE
      IF (LLOG) then
      WRITE(OUTSTR,'(4(2A,I4))')  '   Transform data set ',COPT(IOP(1)),
     *ISET, ' to ',COPT(IOP(2)),Jset
      CALL T2WRSQ(OUTSTR,.true.,6,3)
      ENDIF
10401 CONTINUE
10402 CONTINUE
      END
      SUBROUTINE T2POLCV(X,Y,X1,Y1,N,INC,LERR,RAD)
      IMPLICIT NONE
      LOGICAL LERR
      INTEGER N,INC,I
      REAL X(INC,*),Y(INC,*),X1(INC,*),Y1(INC,*),RAD,T,DT,DT1
      IF (N.gt.0) THEN
      DO 10461 I=1,N
      T=SQRT(X(1,I)**2+Y(1,I)**2)
      IF (T.ne.0) THEN
      IF (LERR) THEN
      DT1=SQRT((X(2,I)*X(1,I))**2+  (Y(2,I)*Y(1,I))**2)/T
      X1(2,I)=SQRT((X(2,I)*Y(1,I))**2+  (Y(2,I)*Y(1,I))**2)/(RAD*T**2)
      Y1(2,I)=DT1
      ENDIF
      X1(1,I)=ATAN2(Y(1,I),X(1,I))/RAD
      Y1(1,I)=T
      ENDIF
10461 CONTINUE
10462 CONTINUE
      ELSE
      DO 10471 I=1,-N
      T=Y(1,I)
      Y1(1,I)=T*SIN(RAD*X(1,I))
      X1(1,I)=T*COS(RAD*X(1,I))
      IF (T .ne. 0 .and. LERR) THEN
      DT=X(2,I)*T/360.
      DT1=SQRT((Y(2,I)*X(1,I))**2+  (DT*Y(1,I))**2)/T
      Y1(2,I)=SQRT((Y(2,I)*Y(1,I))**2+  (DT*X(1,I))**2)/T
      X1(2,I)=DT1
      ENDIF
10471 CONTINUE
10472 CONTINUE
      ENDIF
      END
      SUBROUTINE T2SPHCV(X,Y,Z,X1,Y1,Z1,N,INC,LERR,RAD)
      IMPLICIT NONE
      LOGICAL LERR
      INTEGER N,INC,I
      REAL X(INC,*),Y(INC,*),Z(INC,*),RAD,T1,T2,T3,DT1,DT2,DT3,T
      REAL X1(INC,*),Y1(INC,*),Z1(INC,*)
      IF (N.gt.0) THEN
      DO 10481 I=1,N
      DT1=0
      DT2=0
      DT3=0
      T3=SQRT(X(1,I)**2+Y(1,I)**2+Z(1,I)**2)
      T2=SQRT(X(1,I)**2+Y(1,I)**2)
      T1=T2
      IF (T3.ne.0) THEN
      IF (LERR) THEN
      IF (T1.eq.0) T1=1
      DT3=SQRT((X(2,I)*X(1,I))**2+  (Y(2,I)*Y(1,I))**2+  (Z(2,I)*Z(1,I))
     ***2)/T3
      DT2=SQRT((X(2,I)*(X(1,I)/T1)*(Z(1,I)))**2+  (Y(2,I)*(Y(1,I)/T1)*(Z
     *(1,I)))**2+  (Z(2,I)*(T1))**2)/T3
      DT1=SQRT((X(2,I)*Y(2,I))**2+  (Y(2,I)*X(2,I))**2)/T1
      X1(2,I)=DT1/(RAD*T1)
      Z1(2,I)=DT2/(RAD*T1)
      Y1(2,I)=DT3
      ENDIF
      IF (T2 .ne. 0) X1(1,I)=ATAN2(Y(1,I),X(1,I))/RAD
      Z1(1,I)=ATAN2(T2,Z(1,I))/RAD
      Y1(1,I)=T3
      ENDIF
10481 CONTINUE
10482 CONTINUE
      ELSE
      DO 10491 I=1,-N
      T1=Y(1,I)*COS(RAD*X(1,I))*SIN(RAD*Z(1,I))
      T2=Y(1,I)*SIN(RAD*X(1,I))*SIN(RAD*Z(1,I))
      T3=Y(1,I)*COS(RAD*Z(1,I))
      IF (Y(1,I) .ne. 0 .and. LERR) THEN
      T=SQRT(T1**2+T2**2)
      X(2,I)=X(2,I)*T/360
      Z(2,I)=Z(2,I)*Y(1,I)/360
      DT3=SQRT((Y(2,I)*T3/Y(1,I))**2+  (Z(2,I)*SIN(RAD*Z(1,I)))**2)
      DT1=SQRT((X(2,I)*COS(RAD*X(1,I)))**2+  (Z(2,I)*COS(RAD*Z(1,I))*SIN
     *(RAD*X(1,I)))**2+  (Y(2,I)*(T1/Y(1,I)))**2)
      DT2=SQRT((X(2,I)*(T3/Y(1,I))*COS(RAD*X(1,I)))**2+  (Z(2,I)*COS(RAD
     **Z(1,I))*COS(RAD*X(1,I)))**2+  (Y(2,I)*(T2/Y(1,I)))**2)
      X1(2,I)=DT1
      Y1(2,I)=DT2
      Z1(2,I)=DT3
      ENDIF
      X1(1,I)=T1
      Y1(1,I)=T2
      Z1(1,I)=T3
10491 CONTINUE
10492 CONTINUE
      ENDIF
      END
      LOGICAL FUNCTION CFT(A,B,NTOT,N,NSPAN,ISN)                                
C                                                                               
C     LIBRARY-ROUTINE                                                           
C                                                                               
C                                                06/AUG/1980                    
C                                                C.J. KOST SIN                  
C     Modifiedby J. Clement to be a logical function 12-Jul-1989                   
C     Originally a CERN library routine                                             
C                                                                               
C    reqd. routines - NONE                                                      
C                                                                               
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC          
C                                                                               
C      MULTIVARIATE COMPLEX FOURIER TRANSFORM, COMPUTED IN PLACE                
C      USING MIXED-RADIX FAST FOURIER TRANSFORM ALGORITHM.                      
C      BY R. C. SINGLETON, STANFORD RESEARCH INSTITUTE, OCT. 1968               
C      ARRAYS A AND B ORIGINALLY HOLD THE REAL AND IMAGINARY                    
C      COMPONENTS OF THE DATA, AND RETURN THE REAL AND                          
C      IMAGINARY COMPONENTS OF THE RESULTING FOURIER COEFFICIENTS.              
C      MULTIVARIATE DATA IS INDEXED ACCORDING TO THE FORTRAN                    
C      ARRAY ELEMENT SUCCESSOR FUNCTION, WITHOUT LIMIT                          
C      ON THE NUMBER OF IMPLIED MULTIPLE SUBSCRIPTS.                            
C      THE SUBROUTINE IS CALLED ONCE FOR EACH VARIATE.                          
C      THE CALLS FOR A MULTIVARIATE TRANSFORM MAY BE IN ANY ORDER.              
C      NTOT IS THE TOTAL NUMBER OF COMPLEX DATA VALUES.                         
C      N IS THE DIMENSION OF THE CURRENT VARIABLE.                              
C      NSPAN/N IS THE SPACING OF CONSUCUTIVE DATA VALUES                        
C      WHILE INDEXING THE CURRENT VARIABLE.                                     
C      THE SIGN OF ISN DETERMINES THE SIGN OF THE COMPLEX                       
C      EXPONENTIAL, AND THE MAGNITUDE OF ISN IS NORMALLY ONE.                   
C                                                                               
C      FOR A SINGLE-VARIATE TRANSFORM,                                          
C      NTOT = N = NSPAN = (NUMBER OF COMPLEX DATA VALUES), F.G.                 
C      CALL CFT(A,B,N,N,N,1)                                                    
C                                                                               
C      A TRI-VARIATE TRANSFORM WITH A(N1,N2,N3), B(N1,N2,N3)                    
C      IS COMPUTED BY                                                           
C      CALL CFT(A,B,N1*N2*N3,N1,N1,1)                                           
C      CALL CFT(A,B,N1*N2*N3,N2,N1*N2,1)                                        
C      CALL CFT(A,B,N1*N2*N3,N3,N1*N2*N3,1)                                     
C                                                                               
C      THE DATA MAY ALTERNATIVELY BE STORED IN A SINGLE COMPLEX                 
C      ARRAY A, THEN THE MAGNITUDE OF ISN CHANGED TO TWO TO                     
C      GIVE THE CORRECT INDEXING INCREMENT AND THE SECOND PARAMETER             
C      USED TO PASS THE INITIAL ADDRESS FOR THE SEQUENCE OF                     
C      IMAGINARY VALUES, E.G.                                                   
C                                                                               
C         REAL S(2)                                                             
C         EQUIVALENCE (A,S)                                                     
C         ....                                                                  
C         ....                                                                  
C         CALL CFT(A,S(2),NTOT,N,NSPAN,2)                                       
C                                                                               
C      Arrays AT(MAXF), CK(MAXF), BT(MAXF), SK(MAXF), and NP(MAXP)              
C      ARE USED FOR TEMPORARY STORAGE. IF THE AVAILABLE STORAGE                 
C      IS INSUFFICIENT, T2CFT=.false.                                           
C      MAXF MUST BE .GE. THE MAXIMUM PRIME FACTOR OF N.                         
C      MAXP MUST BE .GT. THE NUMBER OF PRIME FACTORS OF N.                      
C      IN ADDITION, IF THE SQUARE-FREE PORTION K of N HAS TWO OR                
C      MORE PRIME FACTORS, THEN MAXP MUST BE .GE. K-1.                          
C      ARRAY STORAGE IN NFAC FOR A MAXIMUM OF 11 FACTORS OF N.                  
C      IF N HAS MORE THAN ONE SQUARE-FREE FACTOR, THE PRODUCT OF THE            
C      SQUARE-FREE FACTORS MUST BE .LE. 210                                     
C                                                                               
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC        
      DIMENSION A(1),B(1)                                                       
      DIMENSION NFAC(11),NP(209)                                                
C      ARRAY STORAGE FOR MAXIMUM PRIME FACTOR OF 23                             
      DIMENSION AT(23),CK(23),BT(23),SK(23)                                     
      EQUIVALENCE (I,II)                                                        
C      THE FOLLOWING TWO CONSTANTS SHOULD AGREE WITH THE ARRAY DIMENSIONS       
      CFT=.true.                                                                
      MAXF=23                                                                   
      MAXP=209                                                                  
      IF(N .LT. 2) GOTO 998                                                     
      INC=ISN                                                                   
C      THE FOLLOWING CONSTANTS ARE RAD = 2.*PI , S72 = SIN(0.4*PI) ,            
C      C72 = COS(0.4*PI) AND S120 = SQRT(0.75)                                  
      RAD = 6.2831853071796                                                     
      S72 = 0.95105651629515                                                    
      C72 = 0.30901699437495                                                    
      S120 = 0.86602540378444                                                   
      IF(ISN .GE. 0) GO TO 10                                                   
      S72=-S72                                                                  
      S120=-S120                                                                
      RAD=-RAD                                                                  
      INC=-INC                                                                  
10      NT=INC*NTOT                                                             
      KS=INC*NSPAN                                                              
      KSPAN=KS                                                                  
      NN=NT-INC                                                                 
      JC=KS/N                                                                   
      RADF=RAD*FLOAT(JC)*0.5                                                    
      I=0                                                                       
      JF=0                                                                      
C      DETERMINE THE FACTORS OF N                                               
      M=0                                                                       
      K=N                                                                       
      GO TO 20                                                                  
15      M=M+1                                                                   
      NFAC(M)=4                                                                 
      K=K/16                                                                    
20      IF(K-(K/16)*16 .EQ. 0) GO TO 15                                         
      J=3                                                                       
      JJ=9                                                                      
      GO TO 30                                                                  
25      M=M+1                                                                   
      NFAC(M)=J                                                                 
      K=K/JJ                                                                    
30      IF(MOD(K,JJ) .EQ. 0) GO TO 25                                           
      J=J+2                                                                     
      JJ=J**2                                                                   
      IF(JJ .LE. K) GO TO 30                                                    
      IF(K .GT. 4) GO TO 40                                                     
      KT=M                                                                      
      NFAC(M+1)=K                                                               
      IF(K .NE. 1) M=M+1                                                        
      GO TO 80                                                                  
40      IF(K-(K/4)*4 .NE. 0) GO TO 50                                           
      M=M+1                                                                     
      NFAC(M)=2                                                                 
      K=K/4                                                                     
50      KT=M                                                                    
      J=2                                                                       
60      IF(MOD(K,J) .NE. 0) GO TO 70                                            
      M=M+1                                                                     
      NFAC(M)=J                                                                 
      K=K/J                                                                     
70      J=((J+1)/2)*2+1                                                         
      IF(J .LE. K) GO TO 60                                                     
80      IF(KT .EQ. 0) GO TO 100                                                 
      J=KT                                                                      
90      M=M+1                                                                   
      NFAC(M)=NFAC(J)                                                           
      J=J-1                                                                     
      IF(J .NE. 0) GO TO 90                                                     
C      COMPUTE FOURIER TRANSFORM                                                
100      SD=RADF/FLOAT(KSPAN)                                                   
      CD=2.0*SIN(SD)**2                                                         
      SD=SIN(SD+SD)                                                             
      KK=1                                                                      
      I=I+1                                                                     
      IF(NFAC(I) .NE. 2) GO TO 400                                              
C      TRANSFORM FOR FACTOR OF 2 (INCLUDING ROTATION FACTOR)                    
      KSPAN=KSPAN/2                                                             
      K1=KSPAN+2                                                                
210      K2=KK+KSPAN                                                            
      AK=A(K2)                                                                  
      BK=B(K2)                                                                  
      A(K2)=A(KK)-AK                                                            
      B(K2)=B(KK)-BK                                                            
      A(KK)=A(KK)+AK                                                            
      B(KK)=B(KK)+BK                                                            
      KK=K2+KSPAN                                                               
      IF(KK .LE. NN) GO TO 210                                                  
      KK=KK-NN                                                                  
      IF(KK .LE. JC) GO TO 210                                                  
      IF(KK .GT. KSPAN) GO TO 800                                               
220      C1=1.0-CD                                                              
      S1=SD                                                                     
230      K2=KK+KSPAN                                                            
      AK=A(KK)-A(K2)                                                            
      BK=B(KK)-B(K2)                                                            
      A(KK)=A(KK)+A(K2)                                                         
      B(KK)=B(KK)+B(K2)                                                         
      A(K2)=C1*AK-S1*BK                                                         
      B(K2)=S1*AK+C1*BK                                                         
      KK=K2+KSPAN                                                               
      IF(KK .LT. NT) GO TO 230                                                  
      K2=KK-NT                                                                  
      C1=-C1                                                                    
      KK=K1-K2                                                                  
      IF(KK .GT. K2) GO TO 230                                                  
      AK=C1-(CD*C1+SD*S1)                                                       
      S1=(SD*C1-CD*S1)+S1                                                       
C      THE FOLLOWING THREE STATEMENTS COMPENSATE FOR TRUNCATION                 
C      ERROR. IF ROUNDED ARITHMETIC IS USED, THEY MAY BE DELETED.               
C      C1=0.5/(AK**2+S1**2)+0.5                                                 
C      S1=C1*S1                                                                 
C      C1=C1*AK                                                                 
C      NEXT STATEMENT SHOULD BE DELETED IF NON-ROUNDED ARITHMETIC IS USED       
      C1=AK                                                                     
      KK=KK+JC                                                                  
      IF(KK .LT. K2) GO TO 230                                                  
      K1=K1+INC+INC                                                             
      KK=(K1-KSPAN)/2+JC                                                        
      IF(KK .LE. JC+JC) GO TO 220                                               
      GO TO 100                                                                 
C      TRANSFORM FOR FACTOR OF 3 (OPTIONAL CODE)                                
320      K1=KK+KSPAN                                                            
      K2=K1+KSPAN                                                               
      AK=A(KK)                                                                  
      BK=B(KK)                                                                  
      AJ=A(K1)+A(K2)                                                            
      BJ=B(K1)+B(K2)                                                            
      A(KK)=AK+AJ                                                               
      B(KK)=BK+BJ                                                               
      AK=-0.5*AJ+AK                                                             
      BK=-0.5*BJ+BK                                                             
      AJ=(A(K1)-A(K2))*S120                                                     
      BJ=(B(K1)-B(K2))*S120                                                     
      A(K1)=AK-BJ                                                               
      B(K1)=BK+AJ                                                               
      A(K2)=AK+BJ                                                               
      B(K2)=BK-AJ                                                               
      KK=K2+KSPAN                                                               
      IF(KK .LT. NN) GO TO 320                                                  
      KK=KK-NN                                                                  
      IF(KK .LE. KSPAN) GO TO 320                                               
      GO TO 700                                                                 
C      TRANSFORM FOR FACTOR OF 4                                                
400      IF(NFAC(I) .NE. 4) GO TO 600                                           
      KSPNN=KSPAN                                                               
      KSPAN=KSPAN/4                                                             
410      C1=1.0                                                                 
      S1=0                                                                      
420      K1=KK+KSPAN                                                            
      K2=K1+KSPAN                                                               
      K3=K2+KSPAN                                                               
      AKP=A(KK)+A(K2)                                                           
      AKM=A(KK)-A(K2)                                                           
      AJP=A(K1)+A(K3)                                                           
      AJM=A(K1)-A(K3)                                                           
      A(KK)=AKP+AJP                                                             
      AJP=AKP-AJP                                                               
      BKP=B(KK)+B(K2)                                                           
      BKM=B(KK)-B(K2)                                                           
      BJP=B(K1)+B(K3)                                                           
      BJM=B(K1)-B(K3)                                                           
      B(KK)=BKP+BJP                                                             
      BJP=BKP-BJP                                                               
      IF(ISN .LT. 0) GO TO 450                                                  
      AKP=AKM-BJM                                                               
      AKM=AKM+BJM                                                               
      BKP=BKM+AJM                                                               
      BKM=BKM-AJM                                                               
      IF(S1 .EQ. 0.0) GO TO 460                                                 
430      A(K1)=AKP*C1-BKP*S1                                                    
      B(K1)=AKP*S1+BKP*C1                                                       
      A(K2)=AJP*C2-BJP*S2                                                       
      B(K2)=AJP*S2+BJP*C2                                                       
      A(K3)=AKM*C3-BKM*S3                                                       
      B(K3)=AKM*S3+BKM*C3                                                       
      KK=K3+KSPAN                                                               
      IF(KK .LE. NT) GO TO 420                                                  
440      C2=C1-(CD*C1+SD*S1)                                                    
      S1=(SD*C1-CD*S1)+S1                                                       
C      THE FOLLOWING THREE STATEMENTS COMPENSATE FOR TRUNCATION                 
C      ERROR. IF ROUNDED ARITHMETIC IS USED, THEY MAY BE DELETED.               
C      C1=0.5/(C2**2+S1**2)+0.5                                                 
C      S1=C1*S1                                                                 
C      C1=C1*C2                                                                 
C      NEXT STATEMENT SHOULD BE DELETED IF NON-ROUNDED ARITHMETIC IS USED       
      C1=C2                                                                     
      C2=C1**2-S1**2                                                            
      S2=2.0*C1*S1                                                              
      C3=C2*C1-S2*S1                                                            
      S3=C2*S1+S2*C1                                                            
      KK=KK-NT+JC                                                               
      IF(KK .LE. KSPAN) GO TO 420                                               
      KK=KK-KSPAN+INC                                                           
      IF(KK .LE. JC) GO TO 410                                                  
      IF(KSPAN .EQ. JC) GO TO 800                                               
      GO TO 100                                                                 
450      AKP=AKM+BJM                                                            
      AKM=AKM-BJM                                                               
      BKP=BKM-AJM                                                               
      BKM=BKM+AJM                                                               
      IF(S1 .NE. 0.0) GO TO 430                                                 
460      A(K1)=AKP                                                              
      B(K1)=BKP                                                                 
      A(K2)=AJP                                                                 
      B(K2)=BJP                                                                 
      A(K3)=AKM                                                                 
      B(K3)=BKM                                                                 
      KK=K3+KSPAN                                                               
      IF(KK .LE. NT) GO TO 420                                                  
      GO TO 440                                                                 
C      TRANSFORM FOR FACTOR OF 5 (OPTIONAL CODE)                                
510      C2=C72**2-S72**2                                                       
      S2=2.0*C72*S72                                                            
520      K1=KK+KSPAN                                                            
      K2=K1+KSPAN                                                               
      K3=K2+KSPAN                                                               
      K4=K3+KSPAN                                                               
      AKP=A(K1)+A(K4)                                                           
      AKM=A(K1)-A(K4)                                                           
      BKP=B(K1)+B(K4)                                                           
      BKM=B(K1)-B(K4)                                                           
      AJP=A(K2)+A(K3)                                                           
      AJM=A(K2)-A(K3)                                                           
      BJP=B(K2)+B(K3)                                                           
      BJM=B(K2)-B(K3)                                                           
      AA=A(KK)                                                                  
      BB=B(KK)                                                                  
      A(KK)=AA+AKP+AJP                                                          
      B(KK)=BB+BKP+BJP                                                          
      AK=AKP*C72+AJP*C2+AA                                                      
      BK=BKP*C72+BJP*C2+BB                                                      
      AJ=AKM*S72+AJM*S2                                                         
      BJ=BKM*S72+BJM*S2                                                         
      A(K1)=AK-BJ                                                               
      A(K4)=AK+BJ                                                               
      B(K1)=BK+AJ                                                               
      B(K4)=BK-AJ                                                               
      AK=AKP*C2+AJP*C72+AA                                                      
      BK=BKP*C2+BJP*C72+BB                                                      
      AJ=AKM*S2-AJM*S72                                                         
      BJ=BKM*S2-BJM*S72                                                         
      A(K2)=AK-BJ                                                               
      A(K3)=AK+BJ                                                               
      B(K2)=BK+AJ                                                               
      B(K3)=BK-AJ                                                               
      KK=K4+KSPAN                                                               
      IF(KK .LT. NN) GO TO 520                                                  
      KK=KK-NN                                                                  
      IF(KK .LE. KSPAN) GO TO 520                                               
      GO TO 700                                                                 
C      TRANSFORM FOR ODD FACTORS                                                
600      K=NFAC(I)                                                              
      KSPNN=KSPAN                                                               
      KSPAN=KSPAN/K                                                             
      IF(K .EQ. 3) GO TO 320                                                    
      IF(K .EQ. 5) GO TO 510                                                    
      IF(K .EQ. JF) GO TO 640                                                   
      JF=K                                                                      
      S1=RAD/FLOAT(K)                                                           
      C1=COS(S1)                                                                
      S1=SIN(S1)                                                                
      IF(JF .GT. MAXF) GO TO 998                                                
      CK(JF)=1.0                                                                
      SK(JF)=0.0                                                                
      J=1                                                                       
630      CK(J)=CK(K)*C1+SK(K)*S1                                                
      SK(J)=CK(K)*S1-SK(K)*C1                                                   
      K=K-1                                                                     
      CK(K)=CK(J)                                                               
      SK(K)=-SK(J)                                                              
      J=J+1                                                                     
      IF(J .LT. K) GO TO 630                                                    
640      K1=KK                                                                  
      K2=KK+KSPNN                                                               
      AA=A(KK)                                                                  
      BB=B(KK)                                                                  
      AK=AA                                                                     
      BK=BB                                                                     
      J=1                                                                       
      K1=K1+KSPAN                                                               
650      K2=K2-KSPAN                                                            
      J=J+1                                                                     
      AT(J)=A(K1)+A(K2)                                                         
      AK=AT(J)+AK                                                               
      BT(J)=B(K1)+B(K2)                                                         
      BK=BT(J)+BK                                                               
      J=J+1                                                                     
      AT(J)=A(K1)-A(K2)                                                         
      BT(J)=B(K1)-B(K2)                                                         
      K1=K1+KSPAN                                                               
      IF(K1 .LT. K2) GO TO 650                                                  
      A(KK)=AK                                                                  
      B(KK)=BK                                                                  
      K1=KK                                                                     
      K2=KK+KSPNN                                                               
      J=1                                                                       
660      K1=K1+KSPAN                                                            
      K2=K2-KSPAN                                                               
      JJ=J                                                                      
      AK=AA                                                                     
      BK=BB                                                                     
      AJ=0.0                                                                    
      BJ=0.0                                                                    
      K=1                                                                       
670      K=K+1                                                                  
      AK=AT(K)*CK(JJ)+AK                                                        
      BK=BT(K)*CK(JJ)+BK                                                        
      K=K+1                                                                     
      AJ=AT(K)*SK(JJ)+AJ                                                        
      BJ=BT(K)*SK(JJ)+BJ                                                        
      JJ=JJ+J                                                                   
      IF(JJ .GT. JF) JJ=JJ-JF                                                   
      IF(K .LT. JF) GO TO 670                                                   
      K=JF-J                                                                    
      A(K1)=AK-BJ                                                               
      B(K1)=BK+AJ                                                               
      A(K2)=AK+BJ                                                               
      B(K2)=BK-AJ                                                               
      J=J+1                                                                     
      IF(J .LT. K) GO TO 660                                                    
      KK=KK+KSPNN                                                               
      IF(KK .LE. NN) GO TO 640                                                  
      KK=KK-NN                                                                  
      IF(KK .LE. KSPAN) GO TO 640                                               
C      MULTIPLY BY ROTATION FACTOR (EXCEPT FOR FACTORS OF 2 AND 4)              
700      IF(I .EQ. M) GO TO 800                                                 
      KK=JC+1                                                                   
710      C2=1.0-CD                                                              
      S1=SD                                                                     
720      C1=C2                                                                  
      S2=S1                                                                     
      KK=KK+KSPAN                                                               
730      AK=A(KK)                                                               
      A(KK)=C2*AK-S2*B(KK)                                                      
      B(KK)=S2*AK+C2*B(KK)                                                      
      KK=KK+KSPNN                                                               
      IF(KK .LE. NT) GO TO 730                                                  
      AK=S1*S2                                                                  
      S2=S1*C2+C1*S2                                                            
      C2=C1*C2-AK                                                               
      KK=KK-NT+KSPAN                                                            
      IF(KK .LE. KSPNN) GO TO 730                                               
      C2=C1-(CD*C1+SD*S1)                                                       
      S1=S1+(SD*C1-CD*S1)                                                       
C      THE FOLLOWING THREE STATEMENTS COMPENSATE FOR TRUNCATION                 
C      ERROR. IF ROUNDED ARITHMETIC IS USED, THEY MAY                           
C      BE DELETED.                                                              
C      C1=0.5/(C2**2+S1**2)+0.5                                                 
C      S1=C1*S1                                                                 
C      C2=C1*C2                                                                 
      KK=KK-KSPNN+JC                                                            
      IF(KK .LE. KSPAN) GO TO 720                                               
      KK=KK-KSPAN+JC+INC                                                        
      IF(KK .LE. JC+JC) GO TO 710                                               
      GO TO 100                                                                 
C      PERMUTE THE RESULTS TO NORMAL ORDER---DONE IN TWO STAGES                 
C      PERMUTATION FOR SQUARE FACTORS OF N                                      
800      NP(1)=KS                                                               
      IF(KT .EQ. 0) GO TO 890                                                   
      K=KT+KT+1                                                                 
      IF(M .LT. K) K=K-1                                                        
      J=1                                                                       
      NP(K+1)=JC                                                                
810      NP(J+1)=NP(J)/NFAC(J)                                                  
      NP(K)=NP(K+1)*NFAC(J)                                                     
      J=J+1                                                                     
      K=K-1                                                                     
      IF(J .LT. K) GO TO 810                                                    
      K3=NP(K+1)                                                                
      KSPAN=NP(2)                                                               
      KK=JC+1                                                                   
      K2=KSPAN+1                                                                
      J=1                                                                       
      IF(N .NE. NTOT) GO TO 850                                                 
C      PERMUTATION FOR SINGLE-VARIATE TRANSFORM (OPTIONAL CODE)                 
820      AK=A(KK)                                                               
      A(KK)=A(K2)                                                               
      A(K2)=AK                                                                  
      BK=B(KK)                                                                  
      B(KK)=B(K2)                                                               
      B(K2)=BK                                                                  
      KK=KK+INC                                                                 
      K2=KSPAN+K2                                                               
      IF(K2 .LT. KS) GO TO 820                                                  
830      K2=K2-NP(J)                                                            
      J=J+1                                                                     
      K2=NP(J+1)+K2                                                             
      IF(K2 .GT. NP(J)) GO TO 830                                               
      J=1                                                                       
840      IF(KK .LT. K2) GO TO 820                                               
      KK=KK+INC                                                                 
      K2=KSPAN+K2                                                               
      IF(K2 .LT. KS) GO TO 840                                                  
      IF(KK .LT. KS) GO TO 830                                                  
      JC=K3                                                                     
      GO TO 890                                                                 
C      PERMUTATION FOR MULTIVARIATE TRANSFORM                                   
850      K=KK+JC                                                                
860      AK=A(KK)                                                               
      A(KK)=A(K2)                                                               
      A(K2)=AK                                                                  
      BK=B(KK)                                                                  
      B(KK)=B(K2)                                                               
      B(K2)=BK                                                                  
      KK=KK+INC                                                                 
      K2=K2+INC                                                                 
      IF(KK .LT. K) GO TO 860                                                   
      KK=KK+KS-JC                                                               
      K2=K2+KS-JC                                                               
      IF(KK .LT. NT) GO TO 850                                                  
      K2=K2-NT+KSPAN                                                            
      KK=KK-NT+JC                                                               
      IF(K2 .LT. KS) GO TO 850                                                  
870      K2=K2-NP(J)                                                            
      J=J+1                                                                     
      K2=NP(J+1)+K2                                                             
      IF(K2 .GT. NP(J)) GO TO 870                                               
      J=1                                                                       
880      IF(KK .LT. K2) GO TO 850                                               
      KK=KK+JC                                                                  
      K2=KSPAN+K2                                                               
      IF(K2 .LT. KS) GO TO 880                                                  
      IF(KK .LT. KS) GO TO 870                                                  
      JC=K3                                                                     
890      IF(2*KT+1 .GE. M) RETURN                                               
      KSPNN=NP(KT+1)                                                            
C      PERMUTATION FOR SQUARE-FREE FACTORS OF N                                 
      J=M-KT                                                                    
      NFAC(J+1)=1                                                               
900      NFAC(J)=NFAC(J)*NFAC(J+1)                                              
      J=J-1                                                                     
      IF(J .NE. KT) GO TO 900                                                   
      KT=KT+1                                                                   
      NN=NFAC(KT)-1                                                             
      IF(NN .GT. MAXP) GO TO 998                                                
      JJ=0                                                                      
      J=0                                                                       
      GO TO 906                                                                 
902      JJ=JJ-K2                                                               
      K2=KK                                                                     
      K=K+1                                                                     
      KK=NFAC(K)                                                                
904      JJ=KK+JJ                                                               
      IF(JJ .GE. K2) GO TO 902                                                  
      NP(J)=JJ                                                                  
906      K2=NFAC(KT)                                                            
      K=KT+1                                                                    
      KK=NFAC(K)                                                                
      J=J+1                                                                     
      IF(J .LE. NN) GO TO 904                                                   
C      DETERMINE THE PERMUTATION CYCLES OF LENGTH GREATER THAN 1                
      J=0                                                                       
      GO TO 914                                                                 
910      K=KK                                                                   
      KK=NP(K)                                                                  
      NP(K)=-KK                                                                 
      IF(KK .NE. J) GO TO 910                                                   
      K3=KK                                                                     
914      J=J+1                                                                  
      KK=NP(J)                                                                  
      IF(KK .LT. 0) GO TO 914                                                   
      IF(KK .NE. J) GO TO 910                                                   
      NP(J)=-J                                                                  
      IF(J .NE. NN) GO TO 914                                                   
      MAXF=INC*MAXF                                                             
C      REORDER A AND B, FOLLOWING THE PERMUTATION CYCLES                        
      GO TO 950                                                                 
924      J=J-1                                                                  
      IF(NP(J) .LT. 0) GO TO 924                                                
      JJ=JC                                                                     
926      KSPAN=JJ                                                               
      IF(JJ .GT. MAXF) KSPAN=MAXF                                               
      JJ=JJ-KSPAN                                                               
      K=NP(J)                                                                   
      KK=JC*K+II+JJ                                                             
      K1=KK+KSPAN                                                               
      K2=0                                                                      
928      K2=K2+1                                                                
      AT(K2)=A(K1)                                                              
      BT(K2)=B(K1)                                                              
      K1=K1-INC                                                                 
      IF(K1 .NE. KK) GO TO 928                                                  
932      K1=KK+KSPAN                                                            
      K2=K1-JC*(K+NP(K))                                                        
      K=-NP(K)                                                                  
936      A(K1)=A(K2)                                                            
      B(K1)=B(K2)                                                               
      K1=K1-INC                                                                 
      K2=K2-INC                                                                 
      IF(K1 .NE. KK) GO TO 936                                                  
      KK=K2                                                                     
      IF(K .NE. J) GO TO 932                                                    
      K1=KK+KSPAN                                                               
      K2=0                                                                      
940      K2=K2+1                                                                
      A(K1)=AT(K2)                                                              
      B(K1)=BT(K2)                                                              
      K1=K1-INC                                                                 
      IF(K1 .NE. KK) GO TO 940                                                  
      IF(JJ .NE. 0) GO TO 926                                                   
      IF(J .NE. 1) GO TO 924                                                    
950      J=K3+1                                                                 
      NT=NT-KSPNN                                                               
      II=NT-INC+1                                                               
      IF(NT .GE. 0) GO TO 924                                                   
      RETURN                                                                    
C      ERROR FINISH, INSUFFICIENT ARRAY STORAGE                                 
998   CFT=.false.                                                               
C999      FORMAT(44H0ARRAY BOUNDS EXCEEDED WITHIN SUBROUTINE CFT)               
      END                                                                       
