      program test02
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

      real*4 xyz(3),size(3)
c
c scratched
c
      integer*4 i
      real*4 s
      call tdsets('dev xw')
      call tdsets('font duplex')

      call tdsets('size 10 10')
      call tdsets('window x 2 8 y 2 8')
      call tdsets('limit x 0 10 y 0 10')

c      call tdsets('label all off')
c      call tdsets('ticks all off')
c      call tdsets('outline all off')

      call tdsets('texture sol')
      call tdsets('grid red dot on')
c      call tdshow('all')
      
      dummy=tdtext_narg(6)
      dummy=tdtext('top','Test2 Topdrawer RTL',
     &                   'IIIII IIIIIIIII III',5.0,8.5,0.0)

      call tdsets('texture sol')

      xyz(1) = 5.0
      xyz(2) = 5.0
      xyz(3) = 0.0

      dummy=tdcrcl_narg(4)
      do s = 1.,5.,0.5
         size(1) = s
         size(2) = s
         size(3) = 0.
         dummy=tdcrcl(xyz,size,3,3)
      enddo
      call tdflsh
      CALL T2WAIT('Paused:',ltemp)
      call tdnewp

      npts = 10
      do i = 1,npts
         x(i) = float(i)
         y(i) = float(i)
         dx(i) = 0.
         dy(i) = sqrt(float(i))
      enddo

      call tdsets('size 10 10')
      call tdsets('ticks size 0.05')
      call tdsets('window x 2 9.2 y 2 8')
      call tdsets('limit x -1 11 y 0 10')
      call tdsets('bar size 0.0')

      dummy=tdtext_narg(6)
      dummy=tdtext('top','Test2 Topdrawer RTL cont.',
     &                   'IIIII IIIIIIIII III      ',5.6,8.5,0.)


      call tdhist_narg(3)
      call tdhist(npts,x,y)

      call tdplot_narg(5)
      call tdplot(npts,x,y,dx,dy)
c      call tdjoin(npts,x,y,dx,dy,1,4*64+1*512)
c      call spawn('ls')

      CALL T2WAIT('Paused:',ltemp)

      write(0,*) 'Plot Done'
      call tdend
      stop
      end
