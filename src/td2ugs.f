      SUBROUTINE TD2UGS(TEXTP,TEXTS)
      CHARACTER*(*) TEXTP,TEXTS,DEFCAS*1,DEFCS0*1
      LOGICAL OLDTDR
      DATA OLDTDR/.TRUE./,DEFCAS/' '/
      DO I=1,MIN(LEN(TEXTP),LEN(TEXTS))
        IF (TEXTP(I:I).NE.' '.AND.TEXTS(I:I).EQ.' ') TEXTS(I:I)=DEFCAS
      ENDDO
      IF (.NOT.OLDTDR) RETURN
      NTEXT=LEN(TEXTP)
      if (NTEXT.gt.LEN(TEXTS)) NTEXT=LEN(TEXTS)
      do n = 1, ntext
         if (texts(n:n).eq.'H') then
            texts(n:n)='F'
c        else if (texts(n:n).eq.'P') then
c           texts(n:n)='O'
c        else if (texts(n:n).eq.'A') then
c           texts(n:n)='W'

c
c     b+ e+ b- e-  -> super/sub scripts
c
         else if (texts(n:n) .eq. '+') then
            texts(n:n) = 'X'
            if (textp(n:n) .eq. 'b' .or. textp(n:n) .eq. 'B') then
               textp(n:n) = '2'
            else if (textp(n:n) .eq. 'e' .or. textp(n:n) .eq. 'E') then
               textp(n:n) = '3'
            end if
         else if (texts(n:n) .eq. '-') then
            texts(n:n) = 'X'
            if (textp(n:n) .eq. 'b' .or. textp(n:n) .eq. 'B') then
               textp(n:n) = '0'
            else if (textp(n:n) .eq. 'e' .or. textp(n:n) .eq. 'E') then
               textp(n:n) = '1'
            end if
c
c     p0 .. p9  -> plotting symbols
c
         else if (texts(n:n) .eq. 'P') then
            if (textp(n:n) .ge. '0' .and. textp(n:n) .le. '9') then
               texts(n:n) = 'O'
            end if
c        else if (texts(n:n).eq.'X') then
c          texts(n:n)='T'
c          if (textp(n:n).eq.'L') then
c            textp(n:n)='K'
c          else if (textp(n:n).eq.'R') then
c            textp(n:n)='F'
c          else if (textp(n:n).eq.'<') then
c            textp(n:n)='L'
c          else if (textp(n:n).eq.'>') then
c            textp(n:n)='G'
c          end if
c        else if (texts(n:n).eq.'S') then
c          if (textp(n:n).eq.'A') then
c            textp(n:n)='L'
c          else if (textp(n:n).eq.'D') then
c            textp(n:n)='Z'
c            texts(n:n)='M'
c          else if (textp(n:n).eq.'E') then
c            textp(n:n)='A'
c            texts(n:n)='M'
c          else if (textp(n:n).eq.'F') then
c            texts(n:n)='P'
c          else if (textp(n:n).eq.'G') then
c            textp(n:n)='D'
c            texts(n:n)='P'
c          else if (textp(n:n).eq.'H') then
c            texts(n:n)='K'
c          else if (textp(n:n).eq.'I') then
c            texts(n:n)='M'
c          else if (textp(n:n).eq.'J') then
c            texts(n:n)='M'
c          else if (textp(n:n).eq.'L') then
c            textp(n:n)='B'
c          else if (textp(n:n).eq.'N') then
c            textp(n:n)='P'
c            texts(n:n)='P'
c          else if (textp(n:n).eq.'O') then
c            textp(n:n)='D'
c            texts(n:n)='M'
c          else if (textp(n:n).eq.'P') then
c            textp(n:n)='Y'
c            texts(n:n)='M'
c          else if (textp(n:n).eq.'Q') then
c            textp(n:n)='R'
c            texts(n:n)='M'
c          else if (textp(n:n).eq.'R') then
c            textp(n:n)='E'
c          else if (textp(n:n).eq.'S') then
c            textp(n:n)='+'
c            texts(n:n)='M'
c         else if (textp(n:n).eq.'U') then
c            textp(n:n)='U'
c            texts(n:n)='D'
c         else if (textp(n:n).eq.'V') then
c            textp(n:n)='O'
c            texts(n:n)='D'
c         else if (textp(n:n).eq.'W') then
c            textp(n:n)='L'
c            texts(n:n)='K'
c         else if (textp(n:n).eq.'X') then
c            texts(n:n)='M'
c         else if (textp(n:n).eq.'Z') then
c            textp(n:n)='R'
c          else if (textp(n:n).eq.'0') then
c            texts(n:n)='M'
c          else if (textp(n:n).eq.'2') then
c            texts(n:n)='M'
c          else if (textp(n:n).eq.':') then
c            textp(n:n)='/'
c            texts(n:n)='M'
c          else if (textp(n:n).eq.'+') then
c            textp(n:n)='P'
c            texts(n:n)='M'
c          else if (textp(n:n).eq.'*') then
c            texts(n:n)='M'
c          else if (textp(n:n).eq.'<') then
c            textp(n:n)='M'
c            texts(n:n)='M'
c          else if (textp(n:n).eq.'>') then
c            textp(n:n)='H'
c            texts(n:n)='M'
c          else if (textp(n:n).eq.'?') then
c            textp(n:n)='I'
c            texts(n:n)='P'
c          end if
c        else if (texts(n:n).eq.'C') then
c          if (textp(n:n).eq.'0') then
c            textp(n:n)='2'
c            texts(n:n)='X'
c          else if (textp(n:n).eq.'1') then
c            textp(n:n)='3'
c            texts(n:n)='X'
c          else if (textp(n:n).eq.'2') then
c            textp(n:n)='0'
c            texts(n:n)='X'
c          else if (textp(n:n).eq.'3') then
c            textp(n:n)='1'
c            texts(n:n)='X'
c          else if (textp(n:n).eq.'4') then
c            textp(n:n)='0'
c            texts(n:n)='Z'
c         else if (textp(n:n).eq.'5') then
c            textp(n:n)='1'
c            texts(n:n)='Z'
c         else if (textp(n:n).eq.'6') then
c            textp(n:n)='2'
c            texts(n:n)='Z'
c         else if (textp(n:n).eq.'7') then
c            textp(n:n)='3'
c            texts(n:n)='Z'
c         else if (textp(n:n).eq.'8') then
c            textp(n:n)='4'
c            texts(n:n)='Z'
c         else if (textp(n:n).eq.'9') then
c            textp(n:n)='5'
c            texts(n:n)='Z'
c          else if (textp(n:n).eq.'E') then
c            textp(n:n)='0'
c            texts(n:n)='Y'
c          else if (textp(n:n).eq.'F') then
c            textp(n:n)='1'
c            texts(n:n)='Y'
c          end if
        end if
      end do
      RETURN
      ENTRY TD0UGS
      OLDTDR=.FALSE.
      RETURN
      ENTRY SET_DEFAULT_CASE(DEFCS0)
      DEFCAS=DEFCS0
      RETURN
      ENTRY GET_DEFAULT_CASE(DEFCS0)
      DEFCS0=DEFCAS
      RETURN
      END
