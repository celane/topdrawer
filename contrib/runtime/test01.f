      program test01
c
c test for put in function of topdrawer
c
      real*4 x(100),y(100)
      integer*4 npts/5/
      logical dummy
      logical tdtext,tdtext_narg
      logical ltemp/.false./

c dummy
      real*4 dx(100),dy(100)

      call tdsets('dev xw')
      call tdsets('font duplex')

      call tdsets('size 10 10')
      call tdsets('window x 2 8 y 2 8')
      call tdsets('limit x 0 25 y 0 25')
c      call tdsets('texture sol')
c      call tdsets('grid red dot on')
c      call tdshow('all')
      
      

      dummy=tdtext_narg(6)
      dummy=tdtext('top','Test Topdrawer RTL',
     &                   'IIII IIIIIIIII III',5.0,8.5,0.0)

      do i = 1,npts
         x(i) = float(i)
         y(i) = float(i)**2
         dx(i) = 0.
         dy(i) = 0.
      enddo
      
      call tdsets('texture sol')

      call tdjoin_narg(7)
c      call tdjoin(npts,x,y)
      call tdjoin(npts,x,y,dx,dy,1,4*64+1*512)

      do i = 1,npts
         x(i) = float(i+5)
         y(i) = float(i)**2
         dx(i) = 0.
         dy(i) = 0.
      enddo

      call tdjoin_narg(3)
      call tdjoin(npts,x,y)
c      call tdjoin(npts,x,y,dx,dy,1,4*64+1*512)

c      call spawn('ls')

      CALL T2WAIT('Paused:',ltemp)
      write(0,*) 'Plot Done'
      call tdend
      stop
      end
