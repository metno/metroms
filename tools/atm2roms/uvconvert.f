      subroutine uvconvert(npos,xr,yr,u,v,
     +                     igtypa,ga,igtypr,gr,udef,ierror)
c
c****************************************************************
c
c     uvconvert - turn vector (velocity) components between grids
c
c  purpose:
c
c     turn vector (velocity) components between different grids;
c     spherical, spherical rotated, polar stereographic or mercator.
c     (you may use xyconvert to convert the positions first.)
c
c  input/output parameters:
c
c     npos   - no. of positions (in xr,yr,u,v)
c     xr     - input x position in the output grid
c     yr     - input y position in the output grid
c     u      - input/output vector (velocity) component in x-direction
c     v      - input/output vector (velocity) component in y-direction
c     igtypa - input  grid type
c     ga     - input  grid description
c     igtypr - output grid type
c     gr     - output grid description
c     ierror - output error status, 0=no error
c
c  description of g = ga and gr (for igtype = igtypa and igtypr):
c
c  for spherical (rotated) grid (igtype=2,3):
c
c     g(1) - western boundary (degrees)
c     g(2) - southern boundary (degrees)
c     g(3) - longitude increment (degrees)
c     g(4) - latitude increment (degrees)
c     g(5) - xcen: longitude position of rotated equator (degrees)
c     g(6) - ycen: latitude  position of rotated equator (degrees)
c            (lamda,theta)=(xcen,ycen) at (lamda',theta')=(0,0),
c            where (lamda,theta) are usual spherical coord. and
c            (lamda',theta') are rotated spherical coord.
c            xcen = ycen = 0 for usual spherical coord.
c
c     (note: if the coordinates are geographic longitude(x),latitude(y)
c            set igtype=2 and g(1:6) = 1.,1.,1.,1.,0.,0. )
c
c  for polar stereographic grid (igtype=1,4):
c
c     g(1) - x-position of north pole
c     g(2) - y-position of north pole
c     g(3) - number of grid distances between pole and equator
c     g(4) - rotation angle of the grid (degrees)
c     g(5) - projection latitude (degrees)
c            (60 degrees north for igtype=1)
c     g(6) - 0. (not used)
c
c  for mercator (unrotated) grid, igtype=5:
c
c     g(1) - western boundary (longitude for x=1) (degrees)
c     g(2) - southern boundary (latitude for y=1) (degrees)
c     g(3) - x (longitude) increment (km)
c     g(4) - y (latitude)  increment (km)
c     g(5) - reference (construction) latitude (degrees)
c     g(6) - 0.  (not used)
c
c  externals:
c
c     pol2sph - polar sterographic <-> spherical coordinates
c     sph2rot - spherical <-> spherical rotated coordinates
c     mer2sph - mercator (unrotated) <-> spherical coordinates
c
c  history:
c
c     j.e. haugen/dnmi      nov -94 ... grv2grv
c     a.   foss   dnmi   02.02.1995 ... no size limits
c     a.   foss   dnmi   25.08.1995 ... uvconvert
c     a.   foss   dnmi   15.05.1996 ... mercator (unrotated)
c
c****************************************************************
c
      implicit none
c
      integer npos, igtypa, igtypr, ierror
      real xr(npos), yr(npos), u(npos), v(npos),
     +     ga(6), gr(6), udef
c
      integer nmax
c
      parameter ( nmax = 1000 )
c
      real x1(nmax), y1(nmax), x2(nmax), y2(nmax),
     +     x3(nmax), y3(nmax)
c
      real zpir18,
     +     xca, yca,
     +     xwr, ysr, dxr, dyr, xcr, ycr,
     +     fia,
     +     xpr, ypr, anr, fir, fpr,
     +     xwmr, ysmr, dxmr, dymr, ycmr
      real zsyca, zcyca, zsycr, zcycr,
     +     zsxsph, zcxsph, zsysph, zcysph, zxmxc, zsxmxc,
     +     zcxmxc, zsxrot, zcxrot, zsyrot, zcyrot,
     +     zb1, zb2, zb3, zb4, za1, za2, za3, za4,
     +     zsfia, zcfia, zsfir, zcfir,
     +     zsfi, zcfi, zsyc, zcyc,
     +     zv1, zv2, zv3, zv4, ua, va
      integer j, j0, j1, j2, nxy, irota, irotr
c
      ierror = 0
c
      if (igtypa.eq.5 .and. igtypr.eq.5) return
c
      if ((igtypa.eq.2 .or. igtypa.eq.3) .and.
     +    (igtypr.eq.2 .or. igtypr.eq.3)) then
        if (ga(5).eq.gr(5) .and. ga(6).eq.gr(6)) then
ccc..this may be needed in some unknown cases
ccc        if (ga(3)*gr(3).lt.0.) then
ccc           do j = 1,npos
ccc              u(j) = -u(j)
ccc           enddo
ccc        endif
ccc        if (ga(4)*gr(4).lt.0.) then
ccc           do j = 1,npos
ccc              v(j) = -v(j)
ccc           enddo
ccc        endif
           return
        end if
      end if
c
      if ((igtypa.eq.2 .or. igtypa.eq.3) .and. igtypr.eq.5 .and.
     +	                     ga(5).eq.0. .and. ga(6).eq.0.) return
c
      if ((igtypr.eq.2 .or. igtypr.eq.3) .and. igtypa.eq.5 .and.
     +	                     gr(5).eq.0. .and. gr(6).eq.0.) return
c
      zpir18 = 2.0*asin(1.0)/180.
c
      if ((igtypa.eq.2 .or. igtypa.eq.3 .or. igtypa.eq.5) .and.
     +    (igtypr.eq.2 .or. igtypr.eq.3 .or. igtypr.eq.5)) then
c
c..sph/mer->sph/mer
c
      irota=0
      irotr=0
c
      if (igtypa.eq.2 .or. igtypa.eq.3) then
         xca = ga(5)*zpir18
         yca = ga(6)*zpir18
	 if (xca.ne.0. .or. yca.ne.0.) irota=1
      elseif (igtypa.eq.5) then
         xca = 0.
         yca = 0.
      end if
c
      if (igtypr.eq.2 .or. igtypr.eq.3) then
         xwr = gr(1)*zpir18
         ysr = gr(2)*zpir18
         dxr = gr(3)*zpir18
         dyr = gr(4)*zpir18
         xcr = gr(5)*zpir18
         ycr = gr(6)*zpir18
	 if (xcr.ne.0. .or. ycr.ne.0.) irotr=1
      elseif (igtypr.eq.5) then
         xwmr = gr(1)*zpir18
         ysmr = gr(2)*zpir18
         dxmr = gr(3)*1000.
         dymr = gr(4)*1000.
         ycmr = gr(5)*zpir18
         xcr  = 0.
         ycr  = 0.
      end if
c
      do j1 = 1,npos,nmax
c
      j0 = j1 - 1
      j2 = min(j0+nmax,npos)
      nxy = j2 - j0
c
      if (igtypr.eq.2 .or. igtypr.eq.3) then
c
         do j = 1,nxy
            x2(j) = xwr + (xr(j0+j)-1.)*dxr
            y2(j) = ysr + (yr(j0+j)-1.)*dyr
         enddo
c
         if(irotr.eq.1) then
            do j = 1,nxy
               x1(j) = x2(j)
               y1(j) = y2(j)
            enddo
            call sph2rot(-1,nxy,x2(1),y2(1),xcr,ycr,ierror)
            if(ierror.ne.0) return
         end if
c
      elseif (igtypr.eq.5) then
c
         do j = 1,nxy
            x2(j) = xr(j0+j)
            y2(j) = yr(j0+j)
         enddo
c
	 call mer2sph(+1,nxy,x2(1),y2(1),xwmr,ysmr,dxmr,dymr,ycmr,ierror)
         if(ierror.ne.0) return
c
      end if
c
      if (irota.eq.1) then
         do j = 1,nxy
            x3(j) = x2(j)
            y3(j) = y2(j)
         enddo
         call sph2rot(+1,nxy,x3(1),y3(1),xca,yca,ierror)
         if(ierror.ne.0) return
      end if
c
      if (irota.eq.1 .and. irotr.eq.1) then
c
      zsyca = sin(yca)
      zcyca = cos(yca)
      zsycr = sin(ycr)
      zcycr = cos(ycr)
c
      do j = 1,nxy
        if (u(j0+j).ne.udef .and. v(j0+j).ne.udef) then
          zsxsph = sin(x2(j))
          zcxsph = cos(x2(j))
          zsysph = sin(y2(j))
          zcysph = cos(y2(j))
          zxmxc  = x2(j) - xca
          zsxmxc = sin(zxmxc)
          zcxmxc = cos(zxmxc)
          zsxrot = sin(x3(j))
          zcxrot = cos(x3(j))
          zsyrot = sin(y3(j))
          zcyrot = cos(y3(j))
          za1 = zcxmxc*zcxrot + zcyca*zsxmxc*zsxrot
          za2 = zcyca*zsxmxc*zcxrot*zsyrot + zsyca*zsxmxc*zcyrot -
     +          zcxmxc*zsxrot*zsyrot
          za3 =-zsyca*zsxrot/zcysph
          za4 = (zcyca*zcyrot - zsyca*zcxrot*zsyrot)/zcysph
          zxmxc  = x2(j) - xcr
          zsxmxc = sin(zxmxc)
          zcxmxc = cos(zxmxc)
          zsxrot = sin(x1(j))
          zcxrot = cos(x1(j))
          zsyrot = sin(y1(j))
          zcyrot = cos(y1(j))
          zb1 = zcycr*zsxmxc*zsxrot + zcxmxc*zcxrot
          zb2 = zcycr*zcxmxc*zsysph*zsxrot - zsycr*zcysph*zsxrot -
     +          zsxmxc*zsysph*zcxrot
          zb3 = zsycr*zsxmxc/zcyrot
          zb4 = (zsycr*zcxmxc*zsysph + zcycr*zcysph)/zcyrot
          zv1 = zb1*za1 + zb2*za3
          zv2 = zb1*za2 + zb2*za4
          zv3 = zb3*za1 + zb4*za3
          zv4 = zb3*za2 + zb4*za4
          ua = u(j0+j)
          va = v(j0+j)
          u(j0+j) = zv1*ua + zv2*va
          v(j0+j) = zv3*ua + zv4*va
        else
          u(j0+j) = udef
          v(j0+j) = udef
        endif
      enddo
c
      elseif (irota.eq.1) then
c
      zsyca = sin(yca)
      zcyca = cos(yca)
c
      do j = 1,nxy
        if (u(j0+j).ne.udef .and. v(j0+j).ne.udef) then
          zsxsph = sin(x2(j))
          zcxsph = cos(x2(j))
          zsysph = sin(y2(j))
          zcysph = cos(y2(j))
          zxmxc  = x2(j) - xca
          zsxmxc = sin(zxmxc)
          zcxmxc = cos(zxmxc)
          zsxrot = sin(x3(j))
          zcxrot = cos(x3(j))
          zsyrot = sin(y3(j))
          zcyrot = cos(y3(j))
          za1 = zcxmxc*zcxrot + zcyca*zsxmxc*zsxrot
          za2 = zcyca*zsxmxc*zcxrot*zsyrot + zsyca*zsxmxc*zcyrot -
     +          zcxmxc*zsxrot*zsyrot
          za3 =-zsyca*zsxrot/zcysph
          za4 = (zcyca*zcyrot - zsyca*zcxrot*zsyrot)/zcysph
          ua = u(j0+j)
          va = v(j0+j)
          u(j0+j) = za1*ua + za2*va
          v(j0+j) = za3*ua + za4*va
        else
          u(j0+j) = udef
          v(j0+j) = udef
        endif
      enddo
c
      elseif (irotr.eq.1) then
c
      zsycr = sin(ycr)
      zcycr = cos(ycr)
c
      do j = 1,nxy
        if (u(j0+j).ne.udef .and. v(j0+j).ne.udef) then
          zsxsph = sin(x2(j))
          zcxsph = cos(x2(j))
          zsysph = sin(y2(j))
          zcysph = cos(y2(j))
          zxmxc  = x2(j) - xcr
          zsxmxc = sin(zxmxc)
          zcxmxc = cos(zxmxc)
          zsxrot = sin(x1(j))
          zcxrot = cos(x1(j))
          zsyrot = sin(y1(j))
          zcyrot = cos(y1(j))
          zb1 = zcycr*zsxmxc*zsxrot + zcxmxc*zcxrot
          zb2 = zcycr*zcxmxc*zsysph*zsxrot - zsycr*zcysph*zsxrot -
     +          zsxmxc*zsysph*zcxrot
          zb3 = zsycr*zsxmxc/zcyrot
          zb4 = (zsycr*zcxmxc*zsysph + zcycr*zcysph)/zcyrot
          ua = u(j0+j)
          va = v(j0+j)
          u(j0+j) = zb1*ua + zb2*va
          v(j0+j) = zb3*ua + zb4*va
        else
          u(j0+j) = udef
          v(j0+j) = udef
        endif
      enddo
c
      endif
c
      enddo
c
      elseif ((igtypa.eq.1 .or. igtypa.eq.4) .and.
     +        (igtypr.eq.1 .or. igtypr.eq.4)) then
c
c..pol->pol
c
      if (ga(4).eq.gr(4)) return
c
      fia = ga(4)*zpir18
      fir = gr(4)*zpir18
c
      zsfia = sin(fia)
      zcfia = cos(fia)
      zsfir = sin(fir)
      zcfir = cos(fir)
      zv1 = zsfir*zsfia + zcfir*zcfia
      zv2 = zsfir*zcfia - zcfir*zsfia
      zv3 = zcfir*zsfia - zsfir*zcfia
      zv4 = zcfir*zcfia + zsfir*zsfia
c
      do j = 1,npos
         if (u(j).ne.udef .and. v(j).ne.udef) then
            ua = u(j)
            va = v(j)
            u(j) = zv1*ua + zv2*va
            v(j) = zv3*ua + zv4*va
         else
            u(j) = udef
            v(j) = udef
         endif
      enddo
c
      elseif ((igtypa.eq.2 .or. igtypa.eq.3 .or. igtypa.eq.5) .and.
     +        (igtypr.eq.1 .or. igtypr.eq.4)) then
c
c..sph/mer->pol
c
      irota=0
c
      if (igtypa.eq.2 .or. igtypa.eq.3) then
        xca = ga(5)*zpir18
        yca = ga(6)*zpir18
	if (xca.ne.0. .or. yca.ne.0.) irota=1
      elseif (igtypa.eq.5) then
        xca = 0.
        yca = 0.
      end if
c
      xpr = gr(1)
      ypr = gr(2)
      anr = gr(3)
      fir = gr(4)*zpir18
      fpr = gr(5)*zpir18
c
      do j1 = 1,npos,nmax
c
      j0 = j1 - 1
      j2 = min(j0+nmax,npos)
      nxy = j2 - j0
c
      do j = 1,nxy
         x2(j) = xr(j0+j)
         y2(j) = yr(j0+j)
      enddo
c
      call pol2sph(+1,nxy,x2(1),y2(1),fpr,xpr,ypr,anr,fir,ierror)
      if(ierror.ne.0) return
c
      if(irota.eq.1) then
c
      do j = 1,nxy
         x3(j) = x2(j)
         y3(j) = y2(j)
      enddo
      call sph2rot(+1,nxy,x3(1),y3(1),xca,yca,ierror)
      if(ierror.ne.0) return
c
      zsfi = sin(fir)
      zcfi = cos(fir)
      zsyc = sin(yca)
      zcyc = cos(yca)
c
      do j = 1,nxy
        if (u(j0+j).ne.udef .and. v(j0+j).ne.udef) then
          zsxsph = sin(x2(j))
          zcxsph = cos(x2(j))
          zsysph = sin(y2(j))
          zcysph = cos(y2(j))
          zxmxc  = x2(j) - xca
          zsxmxc = sin(zxmxc)
          zcxmxc = cos(zxmxc)
          zsxrot = sin(x3(j))
          zcxrot = cos(x3(j))
          zsyrot = sin(y3(j))
          zcyrot = cos(y3(j))
          za1 = zcxmxc*zcxrot + zcyc*zsxmxc*zsxrot
          za2 = zcyc*zsxmxc*zcxrot*zsyrot + zsyc*zsxmxc*zcyrot -
     +          zcxmxc*zsxrot*zsyrot
          za3 =-zsyc*zsxrot/zcysph
          za4 = (zcyc*zcyrot - zsyc*zcxrot*zsyrot)/zcysph
          zb1 = -zsxsph*za1 - zcxsph*za3
          zb2 = -zsxsph*za2 - zcxsph*za4
          zb3 =  zcxsph*za1 - zsxsph*za3
          zb4 =  zcxsph*za2 - zsxsph*za4
          zv1 = -zsfi*zb1 + zcfi*zb3
          zv2 = -zsfi*zb2 + zcfi*zb4
          zv3 = -zcfi*zb1 - zsfi*zb3
          zv4 = -zcfi*zb2 - zsfi*zb4
          ua = u(j0+j)
          va = v(j0+j)
          u(j0+j) = zv1*ua + zv2*va
          v(j0+j) = zv3*ua + zv4*va
        else
          u(j0+j) = udef
          v(j0+j) = udef
        endif
      enddo
c
      else
c
      zsfi = sin(fir)
      zcfi = cos(fir)
c
      do j = 1,nxy
        if (u(j0+j).ne.udef .and. v(j0+j).ne.udef) then
          zsxsph = sin(x2(j))
          zcxsph = cos(x2(j))
          zsysph = sin(y2(j))
          zcysph = cos(y2(j))
          zv1 = zsfi*zsxsph + zcfi*zcxsph
          zv2 = zsfi*zcxsph - zcfi*zsxsph
          zv3 = zcfi*zsxsph - zsfi*zcxsph
          zv4 = zcfi*zcxsph + zsfi*zsxsph
          ua = u(j0+j)
          va = v(j0+j)
          u(j0+j) = zv1*ua + zv2*va
          v(j0+j) = zv3*ua + zv4*va
        else
          u(j0+j) = udef
          v(j0+j) = udef
        endif
      enddo
c
      end if
c
      enddo
c
      elseif ((igtypa.eq.1 .or. igtypa.eq.4) .and.
     +        (igtypr.eq.2 .or. igtypr.eq.3 .or. igtypr.eq.5)) then
c
c..pol->sph/mer
c
      fia = ga(4)*zpir18
c
      irotr=0
c
      if (igtypr.eq.2 .or. igtypr.eq.3) then
         xwr = gr(1)*zpir18
         ysr = gr(2)*zpir18
         dxr = gr(3)*zpir18
         dyr = gr(4)*zpir18
         xcr = gr(5)*zpir18
         ycr = gr(6)*zpir18
	 if (xcr.ne.0. .or. ycr.ne.0.) irotr=1
      elseif (igtypr.eq.5) then
         xwmr = gr(1)*zpir18
         ysmr = gr(2)*zpir18
         dxmr = gr(3)*1000.
         dymr = gr(4)*1000.
         ycmr = gr(5)*zpir18
         xcr  = 0.
         ycr  = 0.
      end if
c
      do j1 = 1,npos,nmax
c
      j0 = j1 - 1
      j2 = min(j0+nmax,npos)
      nxy = j2 - j0
c
      if (igtypr.eq.2 .or. igtypr.eq.3) then
c
         do j = 1,nxy
            x2(j) = xwr + (xr(j0+j)-1.)*dxr
            y2(j) = ysr + (yr(j0+j)-1.)*dyr
         enddo
c
         if(irotr.eq.1) then
            do j = 1,nxy
               x1(j) = x2(j)
               y1(j) = y2(j)
            enddo
            call sph2rot(-1,nxy,x2(1),y2(1),xcr,ycr,ierror)
            if(ierror.ne.0) return
         end if
c
      elseif (igtypr.eq.5) then
c
         do j = 1,nxy
            x2(j) = xr(j0+j)
            y2(j) = yr(j0+j)
         enddo
c
	 call mer2sph(+1,nxy,x2(1),y2(1),xwmr,ysmr,dxmr,dymr,ycmr,ierror)
         if(ierror.ne.0) return
c
      end if
c
      if (irotr.eq.1) then
c
      zsfi = sin(fia)
      zcfi = cos(fia)
      zsyc = sin(ycr)
      zcyc = cos(ycr)
c
      do j = 1,nxy
        if (u(j0+j).ne.udef .and. v(j0+j).ne.udef) then
          zsxsph = sin(x2(j))
          zcxsph = cos(x2(j))
          zsysph = sin(y2(j))
          zcysph = cos(y2(j))
          zxmxc  = x2(j) - xcr
          zsxmxc = sin(zxmxc)
          zcxmxc = cos(zxmxc)
          zsxrot = sin(x1(j))
          zcxrot = cos(x1(j))
          zsyrot = sin(y1(j))
          zcyrot = cos(y1(j))
          za1 = zsxsph*zsfi + zcxsph*zcfi
          za2 = zsxsph*zcfi - zcxsph*zsfi
          za3 = zcxsph*zsfi - zsxsph*zcfi
          za4 = zcxsph*zcfi + zsxsph*zsfi
          zb1 = zcyc*zsxmxc*zsxrot + zcxmxc*zcxrot
          zb2 = zcyc*zcxmxc*zsysph*zsxrot - zsyc*zcysph*zsxrot -
     +          zsxmxc*zsysph*zcxrot
          zb3 = zsyc*zsxmxc/zcyrot
          zb4 = (zsyc*zcxmxc*zsysph + zcyc*zcysph)/zcyrot
          zv1 = zb1*za1 + zb2*za3
          zv2 = zb1*za2 + zb2*za4
          zv3 = zb3*za1 + zb4*za3
          zv4 = zb3*za2 + zb4*za4
          ua = u(j0+j)
          va = v(j0+j)
          u(j0+j) = zv1*ua + zv2*va
          v(j0+j) = zv3*ua + zv4*va
        else
          u(j0+j) = udef
          v(j0+j) = udef
        endif
      enddo
c
      else
c
      zsfi = sin(fia)
      zcfi = cos(fia)
c
      do j = 1,nxy
        if (u(j0+j).ne.udef .and. v(j0+j).ne.udef) then
          zsxsph = sin(x2(j))
          zcxsph = cos(x2(j))
          zsysph = sin(y2(j))
          zcysph = cos(y2(j))
          za1 = zsxsph*zsfi + zcxsph*zcfi
          za2 = zsxsph*zcfi - zcxsph*zsfi
          za3 = zcxsph*zsfi - zsxsph*zcfi
          za4 = zcxsph*zcfi + zsxsph*zsfi
          ua = u(j0+j)
          va = v(j0+j)
          u(j0+j) = za1*ua + za2*va
          v(j0+j) = za3*ua + za4*va
        else
          u(j0+j) = udef
          v(j0+j) = udef
        endif
      enddo
c
      endif
c
      enddo
c
      else
c
      ierror = 1
c
      endif
c
      return
      end
