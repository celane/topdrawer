      program test03
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
      integer*4 i,j
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
      dummy=tdtext('top','Test3 Topdrawer RTL',
     &                   'IIIII IIIIIIIII III',5.0,8.5,0.0)

      call tdsets('texture sol')

      xyz(1) = 5.0
      xyz(2) = 5.0
      xyz(3) = 0.0

      dummy=tdcrcl_narg(4)
      do j = 1,100
         do s = 1.,12.,0.5
            size(1) = s
            size(2) = s
            size(3) = 0.
            call tdsets('color white')
            dummy=tdcrcl(xyz,size,3,3)
            call tdflsh
            do i = 1,300
c     wait     
               write(0,*) 'wait'
            enddo
            call tdsets('color black')
            dummy=tdcrcl(xyz,size,3,3)
         enddo
         do s = 12.,1.,-0.5
            size(1) = s
            size(2) = s
            size(3) = 0.
            call tdsets('color white')
            dummy=tdcrcl(xyz,size,3,3)
            call tdflsh
            
            do i = 1,300
c     wait
               write(0,*) 'wait'
            enddo
            call tdsets('color black')
            dummy=tdcrcl(xyz,size,3,3)
         enddo
      enddo


      CALL T2WAIT('Paused:',ltemp)
      write(0,*) 'Plot Done'
      call tdend
      stop
      end
