      subroutine pol2sph(icall,n,x,y,fpol,xp,yp,an,fi,ierror)
c
c  conversion between polar stereographic and spherical
c  coordinates. polarstereographic coordinates are given in real
c  numbers, where (xpol,ypol)=(1.0,1.0) in lower left corner of
c  the polarsterographic grid. spherical coordinates are given
c  in radians and fi is the angle of rotation in radians.
c
c----------------------------------------------------------------------
c  DNMI/FoU  xx.xx.1995  Jan Erik Haugen ... Hirlam code (?)
c  DNMI/FoU  xx.05.1995  Anstein Foss ...... DNMI library version
c  DNMI/FoU  xx.06.1995  Anstein Foss ...... simpler (old) code
c  DNMI/FoU  23.05.1997  Anstein Foss ...... southern hemisphere
c----------------------------------------------------------------------
c
      integer icall, n, ierror
      real    x(n), y(n), fpol, xp, yp, an, fi
c
ccc   data za/6371220.0/
      data za/-1.0/
c
      ierror = 0
c
      if(za.le.0.) call earthr(za)
c
c-----------------------------------------------------------
      zpi    = 2.0 * asin(1.0)
      ztwopi = 4.0 * asin(1.0)
      zns = 1.
      zfi = fi
      if (fpol.lt.0.) then
	 zns = -1.
	 zfi = -fi
      endif
c-----------------------------------------------------------
      zpihal = asin(1.0)
c-----------------------------------------------------------
c     zcons1 = za*(1.0 + sin(fpol))
c     zcons2 = 1.0/zcons1
c     zdel   = zcons1/an
c     zrdel  = an/zcons1
c-----------------------------------------------------------
c
      if (icall.eq.1) then
c
c  compute spherical coordinates as function of
c  polar sterographic coordinates
c
      do j = 1,n
         xpol = x(j)
         ypol = y(j)
c-----------------------------------------------------------
c        zdx = zdel*(xpol - xp)
c        zdy = zdel*(yp - ypol)
c        zr  = sqrt( zdx*zdx + zdy*zdy )
c        zrx = zr
c        if (zrx.eq.0.0) zr = zdel
c        zrr = 1.0/zr
c        zsa = zdx*zrr
c        zca = zdy*zrr
c        zsa = max( zsa,-1.0 )
c        zsa = min( zsa,+1.0 )
c        zca = max( zca,-1.0 )
c        zca = min( zca,+1.0 )
c        zas = asin( zsa )
c        zac = acos( zca )
c        zxmfi = zac
c        if (zas.lt.0.0) zxmfi = -zac
c        xsph = zxmfi + fi
c        ysph = zpihal - 2.0*atan( zr*zcons2 )
c        if (zrx.eq.0.0) xsph = 0.0
c        if (zrx.eq.0.0) ysph = zpihal
c-----------------------------------------------------------
         zdx=xpol-xp
         zdy=yp-ypol
         zr =sqrt(zdx*zdx+zdy*zdy)
         ysph=zpihal-2.0*atan(zr/an)
         xsph=0.
         if(zr.gt.1.e-10) xsph=zfi+atan2(zdx,zdy)
         if(xsph.le.-zpi) xsph=xsph+ztwopi
         if(xsph.gt.+zpi) xsph=xsph-ztwopi
c-----------------------------------------------------------
         x(j) = xsph * zns
         y(j) = ysph * zns
      enddo
c
      elseif (icall.eq.-1) then
c
c  compute polar stereographic coordinates as function of
c  spherical coordinates
c
c-----------------------------------------------------------
      alfa=sin(zpihal+zfi)
      beta=cos(zpihal+zfi)
c-----------------------------------------------------------
c
      do j = 1,n
         xsph = x(j) * zns
         ysph = y(j) * zns
c-----------------------------------------------------------
c        zr = zcons1*tan( 0.5*(zpihal - ysph) )
c        xpol = xp + zr*sin(xsph - fi)*zrdel
c        ypol = yp - zr*cos(xsph - fi)*zrdel
c-----------------------------------------------------------
         zr=an*cos(ysph)/(1.+sin(ysph))
         xr=+zr*sin(xsph)
         yr=-zr*cos(xsph)
         xpol=xr*alfa-yr*beta+xp
         ypol=yr*alfa+xr*beta+yp
c-----------------------------------------------------------
         x(j) = xpol
         y(j) = ypol
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
