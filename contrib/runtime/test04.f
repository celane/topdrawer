      program test04
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
c      real*8 xtmp(2),ytmp(2)
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

      call tdsets('label all off')
      call tdsets('ticks all off')
      call tdsets('outline all off')

      call tdsets('texture sol')
c      call tdsets('grid red dot on')
c      call tdshow('all')
      
      dummy=tdtext_narg(6)
      dummy=tdtext('top','Test4 Topdrawer RTL',
     &                   'IIIII IIIIIIIII III',5.0,8.5,0.0)

      call tdsets('texture sol')


      npts=120
      xstep = 0.01
      do i = 1,npts
         x(i) = 5.0+xstep*float(i)
         y(i) = 5.0
      enddo

      call tdjoin_narg(3)
      call tdsets('color white')
      call tdjoin(npts,x,y)
      ipts = 2
      do while(.true.)
c      do j = 1,1000000
         call get_next(xstep,xl,yl,x(npts),y(npts),x(npts+1),y(npts+1))
         call tdsets('color white')
         call tdjoin(ipts,x(npts),y(npts))
         call tdsets('color black')
         call tdjoin(ipts,x(1),y(1))
         do i = 1,npts
            x(i) = x(i+1)
            y(i) = y(i+1)
         enddo
      enddo


      CALL T2WAIT('Paused:',ltemp)
      write(0,*) 'Plot Done'
      call tdend
      stop
      end

      subroutine get_next(xstep,xl,yl,x0,y0,x1,y1)
      real*4 xstep
      real*4 xl(2),yl(2)
      real*4 x0,y0,x1,y1
      real*4 theta/0.0/,ran
      integer*4 iseed/1664501/
      real*4 theta_old
      save theta_old

c      theta = 2.*atan(1.)*4.*ran(iseed)
      theta = theta_old+0.1*(ran(iseed)*2.-1.)

      x1 = x0 + xstep*cos(theta)
      y1 = y0 + xstep*sin(theta)
c      write(0,*) x1,y1,xstep,theta

      if(x1 .ge. xl(2)) then
         x1 = xl(2) - (x1 - xl(2))
         theta = atan(1.)*4-theta
      elseif (x1 .le. xl(1)) then
         x1 = xl(1) - (x1 - xl(1))
         theta = atan(1.)*4-theta
      endif
      if(y1 .ge. yl(2)) then
         y1 = yl(2) - (y1 - yl(2))
         theta = -theta
      elseif (y1 .le. yl(1)) then
         y1 = yl(1) - (y1 - yl(1))
         theta = -theta
      endif
      theta_old = theta
      return
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
