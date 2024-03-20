      parameter (maxkey=20)
      character key(maxkey)*20,sep(maxkey)*1
      character line*255,flname*255,format*80
      data key/maxkey*' '/,sep/maxkey*' '/

      if (iargc().ne.0) then
        call getarg(1,flname)
        open(5,file=flname,status='old',err=91000)
      endif
      write(6,'(1H?)')
      do while(.true.)
        read(5,'(A)',end=80000) line
        call uscape(line)
        if (line(1:1).eq.' ') then
          do i=2,length(line)
            if (index(' _',line(i:i)).eq.0) goto 20000
          enddo
          do i=2,length(line)
            if (line(i:i).eq.'_') line(i:i)='~'
          enddo
20000     write(6,'(A)') line(2:length(line))
        else if (index('0123456789',line(1:1)).ne.0) then
          read(line,*,err=92000) n
          if (n.gt.maxkey) goto 92000
          i=1
          do while (i.lt.len(line).and.
     -              index('0123456789',line(i:i)).ne.0)
            i=i+1
          enddo
          do while (i.lt.len(line).and.line(i:i).eq.' ')
            i=i+1
          enddo
          call lwcase(line(i:),key(n))
c         do m=2,n
          do m=2,2
            sep(m-1)=' '
            sep(m  )='?'
            write(format,'(1H(,I2,2HA))') 2*(n-m+1)
            write(6,format) (sep(i),key(i)(:length(key(i))),i=m,n)
          enddo
          sep(n)=' '
        else
          write(0,*) '*** Unrecognized Input: ',line(:10),'...'
        endif
      enddo
80000 stop
91000 write(0,*) '*** File open error: ',flname(:length(flname))
      stop
92000 write(0,*) '*** Illigal key: ',line(:10),'...'
      stop
      end

      subroutine lwcase(s1,s2)
      character s1*(*),s2*(*)
      do i=1,len(s2)
        if (i.le.len(s1)) then
          j=ichar(s1(i:i))
          if (j.ge.ichar('A').and.j.le.ichar('Z')) j=j+32
          s2(i:i)=char(j)
        else
          s2(i:i)=' '
        endif
      enddo
      return
      end

      subroutine uscape(s)
      character s*(*),t*255
      logical   under,bold
      under=.false.
      bold =.false.
      i=0
      j=0
      do while(i.lt.len(s))
         i=i+1
         if (s(i:i).eq.'') then
            if     (s(i+1:i+3).eq.'[4m') then
               under=.true.
            elseif (s(i+1:i+3).eq.'[1m'.or.s(i+1:i+3).eq.'[5m') then
               bold =.true.
            elseif (s(i+1:i+3).eq.'[0m') then
               under=.false.
               bold =.false.
            else
               write(0,*) 'Unknown escape sequence "',s(i+1:i+3),'".'
            endif
            i=i+3
         else
            j=j+1
            if (under) then
               t(j:j+1)='_'
               j=j+2
            endif
            if (bold) then
               t(j:j+1)=s(i:i)//''
               j=j+2
            endif
            t(j:j)=s(i:i)
         endif
      enddo
      s=t(:j)
      return
      end

      FUNCTION LENGTH(S)
      CHARACTER S*(*)
      LENGTH=LEN(S)
   10 IF (LENGTH.LE.0) RETURN
      IF (LGT(S(LENGTH:LENGTH),' ')) RETURN
      LENGTH=LENGTH-1
      GOTO 10
      END
