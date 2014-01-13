      subroutine sph2rot(icall,n,x,y,xcen,ycen,ierror)
c
c  conversion between spherical (x=xsph,y=ysph) and spherical rotated
c  (x=xrot,y=yrot) coordinates. (xcen,ycen) is the position of the
c  rotated equator/greenwich in terms of (longitude,latitude).
c  all values are given in radians.
c
      integer icall, n, ierror
      real    x(n), y(n), xcen, ycen
c
      ierror = 0
c
      zsycen = sin(ycen)
      zcycen = cos(ycen)
c
      if (icall.eq.1) then
c
c  compute spherical rotated coordinates as function of
c  spherical coordinates
c
      do j = 1,n
         xsph = x(j)
         ysph = y(j)
         zxmxc  = xsph - xcen
         zsxmxc = sin(zxmxc)
         zcxmxc = cos(zxmxc)
         zsysph = sin(ysph)
         zcysph = cos(ysph)
         zsyrot = zcycen*zsysph - zsycen*zcysph*zcxmxc
         zsyrot = max(zsyrot,-1.0)
         zsyrot = min(zsyrot,+1.0)
         yrot   = asin(zsyrot)
         zcyrot = cos(yrot)
         zcxrot = (zcycen*zcysph*zcxmxc +
     +             zsycen*zsysph)/zcyrot
         zcxrot = max(zcxrot,-1.0)
         zcxrot = min(zcxrot,+1.0)
         zsxrot = zcysph*zsxmxc/zcyrot
         xrot   = acos(zcxrot)
         if (zsxrot.lt.0.0) xrot = -xrot
         x(j) = xrot
         y(j) = yrot
      enddo
c
      elseif (icall.eq.-1) then
c
c  compute spherical coordinates as function of
c  spherical rotated coordinates
c
      do j = 1,n
         xrot = x(j)
         yrot = y(j)
         zsxrot = sin(xrot)
         zcxrot = cos(xrot)
         zsyrot = sin(yrot)
         zcyrot = cos(yrot)
         zsysph = zcycen*zsyrot + zsycen*zcyrot*zcxrot
         zsysph = max(zsysph,-1.0)
         zsysph = min(zsysph,+1.0)
         ysph   = asin(zsysph)
         zcysph = cos(ysph)
         zcxmxc = (zcycen*zcyrot*zcxrot -
     +             zsycen*zsyrot)/zcysph
         zcxmxc = max(zcxmxc,-1.0)
         zcxmxc = min(zcxmxc,+1.0)
         zsxmxc = zcyrot*zsxrot/zcysph
         zxmxc  = acos(zcxmxc)
         if (zsxmxc.lt.0.0) zxmxc = -zxmxc
         xsph = zxmxc + xcen
         x(j) = xsph
         y(j) = ysph
      enddo
c
      else
c
      ierror=1
c
      endif
c
      return
      end
