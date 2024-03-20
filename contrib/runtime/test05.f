      program test05
      implicit none
c
c test for put in function of topdrawer
c
      integer*4 max_pts
      parameter (max_pts=4096)
      real*4 x(max_pts),y(max_pts),dx(max_pts),dy(max_pts)
      integer*4 npts
      logical dummy
      logical tdtext,tdtext_narg
      logical tdcrcl,tdcrcl_narg
      logical ltemp/.false./
      real*4 xl(2)/0.,10./,yl(2)/0.,10./
      integer*4 iline,rlen

      integer*4 ipts
      real*4 xstep
      integer*4 idum
      real*4 xyz(2),xyz1(2),xyz2(3),size(3)
      logical lf1,lf2

c
c scratched
c
      integer*4 i,j
      real*4 s
      character*80 line
      call tdsets('dev xw')
      call tdsets('font duplex')

      call tdsets('size 10 10')
      call tdsets('window x 2 8 y 2 8')

      write(line,'(A,2F10.4,A,2F10.4)') 'limit x ',xl(1),xl(2),
     &                                  ' y ',yl(1),yl(2)
      iline=rlen(line,len(line))
      call tdsets(line(1:iline))

c      call tdsets('label all off')
c      call tdsets('ticks all off')
c      call tdsets('outline all off')

      call tdsets('grid red dot on')
c      call tdshow('all')


      dummy=tdtext_narg(6)
      dummy=tdtext('top','Test5 Topdrawer RTL',
     &                   'IIIII IIIIIIIII III',5.0,8.5,0.0)

      call tdflsh
      call tdplot_narg(1)
      call tdplot(0)

      call tdsets('texture sol')

      size(1) = 1.0
      size(2) = 1.0
      size(3) = 0.0

      dummy= tdcrcl_narg(4)

      do while(.true.)
         call t2curs(idum,xyz,xyz1,xyz2,lf1,lf2)
         write(0,*) xyz(1),xyz(2)
         write(0,*) xyz1(1),xyz1(2)
         write(0,*) xyz2(1),xyz2(2)
         dummy=tdcrcl(xyz2, size,3,3)
      enddo


      CALL T2WAIT('Paused:',ltemp)
      write(0,*) 'Plot Done'
      call tdend
      stop
      end

      integer*4 function rlen(line,length)
      character*(*) line
      integer*4 length

      do i = length,1,-1
         if( line(i:i) .ne. ' ' .and. line(i:i) .ne. char(0) )then
            rlen = i
            goto 09000
         endif
      enddo
      rlen = 0
09000 continue
      return
      end
