      subroutine ugxsym(x0,y0,exsz,exdz,angle,p,s,segm,*)
      real      x0,y0,x,y,angle,sn,cs
      character p*(*),s*(*)
      integer*4 segm(*)
      parameter (maxpnt=100)
      real      xv(maxpnt),yv(maxpnt)

      x=x0
      y=y0
      IF (EXSZ.GT.0.0) THEN
        d0=EXSZ
      ELSE IF (EXDZ.GT.0.0) THEN
        d0=-EXDZ
      ELSE IF (EXSZ.LT.0.0) THEN
        d0=-EXSZ
      ELSE IF (EXDZ.LT.0.0) THEN
        d0= EXDZ
      ELSE
        d0= 0.015
      END IF
      d0=2.5*d0
      sn=sin(angle/57.2957795)
      cs=cos(angle/57.2957795)
      do 30000 i=1,len(p)
        if (s(i:i).eq.'O') then
          m=index('ABCDEFGHIJKLXY',p(i:i))
          goto (10100,10100,10200,10200,10300,10300,
     -          10400,10400,10500,10500,10600,10600,
     -          18100,18200) m
          goto  30000
c  A, B = Circle
10100     d=d0*1.2
          n=31
          r=3.14159265*2./(n-1)
          do j=1,n
            xv(j)=+d*cos(r*j)
            yv(j)=+d*sin(r*j)
          enddo
          goto 20000
c  C, D = Square
10200     d=d0
          xv(1)=-d
          yv(1)=-d
          xv(2)=+d
          yv(2)=-d
          xv(3)=+d
          yv(3)=+d
          xv(4)=-d
          yv(4)=+d
          xv(5)=-d
          yv(5)=-d
          n=5
          goto 20000
c  E, F = Rohm
10300     d=d0*1.4142
          xv(1)=-d
          yv(1)= 0
          xv(2)= 0
          yv(2)=-d
          xv(3)=+d
          yv(3)= 0
          xv(4)= 0
          yv(4)=+d
          xv(5)=-d
          yv(5)= 0
          n=5
          goto 20000
c  G, H = Triangle
10400     d=d0*1.51967
          xv(1)=-d
          yv(1)=+d/1.73205
          xv(2)= 0
          yv(2)=-d*1.15470
          xv(3)=+d
          yv(3)=+d/1.73205
          xv(4)=-d
          yv(4)=+d/1.73205
          n=4
          goto 20000
c  I, J = Inv.Triangle
10500     d=d0*1.51967
          xv(1)=-d
          yv(1)=-d/1.73205
          xv(2)=+d
          yv(2)=-d/1.73205
          xv(3)= 0
          yv(3)=+d*1.15470
          xv(4)=-d
          yv(4)=-d/1.73205
          n=4
          goto 20000
c  K, L = Star
10600     d=d0*1.8842
          r=3.14159265/5.
          do j=1,12,2
            xv(j  )=d*sin(r* j   )
            yv(j  )=d*cos(r* j   )
            xv(j+1)=d*sin(r*(j+1))*0.384
            yv(j+1)=d*cos(r*(j+1))*0.384
          enddo
          n=11
          goto 20000
c  X    = \\
18100     d=d0
          xv(1)=-d*3
          yv(1)=+d*2
          xv(2)=+d
          yv(2)=-d*2
          xv(3)=+d*3
          yv(3)=-d*2
          xv(4)=-d
          yv(4)=+d*2
          goto 18800
c  Y    = \\
18200     d=d0
          xv(1)=-d*2
          yv(1)=+d*3
          xv(2)=+d*2
          yv(2)=-d
          xv(3)=+d*2
          yv(3)=-d*3
          xv(4)=-d*2
          yv(4)=+d
18800     do j=1,4
             temp =xv(j)*cs-yv(j)*sn
             yv(j)=xv(j)*sn+yv(j)*cs +y
             xv(j)=             temp +x
          enddo
          call ugpfil('BLACK',xv,yv,4,segm)
          call ugplin('SOLID',xv(1),yv(1),2,1,1,segm)
          call ugplin('SOLID',xv(3),yv(3),2,1,1,segm)
          goto 28000
20000     do j=1,n
             temp =xv(j)*cs-yv(j)*sn
             yv(j)=xv(j)*sn+yv(j)*cs +y
             xv(j)=             temp +x
          enddo
          if (mod(m,2).eq.1) then
            call ugpfil('SOLID',xv,yv,n,segm)
          else
            call ugpfil('BLACK',xv,yv,n,segm)
            call ugplin('SOLID',xv,yv,n,1,1,segm)
          endif
28000     p(i:i)=' '
          s(i:i)=' '
        endif
30000 x=x+3.15*abs(d0)
      if (p.ne.' ') return 1
      return
      end
