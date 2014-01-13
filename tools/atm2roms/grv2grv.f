      subroutine grv2grv (icall,igtypa,ga,igtypr,gr,
     +                    ur,vr,nxr,nyr,vturn,udef,ierror)
c
c****************************************************************
c
c     grv2grv - turn velocity components between grids
c
c  purpose:
c
c     turn velocity components between different grids; spherical,
c     spherical rotated, polar stereographic or mercator.
c
c  input/output parameters:
c
c     icall  - switch for preparations/turning
c              = 1: preparations
c              = 2: preparations and turning
c              = 3: turning
c     igtypa - input grid type
c     ga     - input grid parameters
c     igtypr - output grid type
c     gr     - output grid parameters
c     ur     - input/output velocity component in x-direction
c     vr     - input/output velocity component in y-direction
c     nxr    - x-dimension 
c     nyr    - y-dimension
c     vturn  - preparation matrix, vturn(4,nx*ny)
c     udef   - value in 'undefined' points
c
c  description of g = ga and gr (for igtype = igtypa and igtypr):
c
c  for spherical (rotated) grid, igtype=2,3:
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
c  for polar stereographic grid, igtype=1,4:
c
c     g(1) - x-position of north pole
c     g(2) - y-position of north pole
c     g(3) - number of grid distances between pole and equator
c     g(4) - rotation angle of the grid (degrees)
c     g(5) - projection latitude (degrees)
c	     (usually 60 degrees north)
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
c     uvconvert - here used to compute constants for the turning
c
c  history:
c
c     j.e. haugen/dnmi      nov -94
c     a.   foss   dnmi   02.02.1995 ... no size limits
c     a.   foss   dnmi   04.06.1996 ... new call, using uvconvert,...
c
c****************************************************************
c
      implicit none
c
      integer icall, igtypa, igtypr, nxr, nyr, ierror
      real vturn(4,nxr*nyr), ur(nxr*nyr), vr(nxr*nyr),
     +     ga(6), gr(6), udef
c
      integer nmax
c
      parameter ( nmax = 1000 )
c
      real x1(nmax), y1(nmax), u1(nmax), v1(nmax)
c
      real ua, va
      integer j, j0, j1, j2, nxyr, ix, iy
c
c  preparations:
c
      if (icall.le.2) then
c
	do j1 = 1,nxr*nyr,nmax
c
	j0 = j1 - 1
	j2 = min(j0+nmax,nxr*nyr)
	nxyr = j2 - j0
c
	do j = 1,nxyr
	   iy = (j0+j+nxr-1)/nxr
	   ix = j0+j - (iy-1)*nxr
           x1(j) = float(ix)
           y1(j) = float(iy)
	enddo
c
	do j = 1,nxyr
           u1(j) = 1.
           v1(j) = 0.
	enddo
	call uvconvert(nxyr,x1(1),y1(1),u1(1),v1(1),
     +                 igtypa,ga,igtypr,gr,udef,ierror)
	if(ierror.ne.0) then
	  write(6,*) '**GRV2GRV** UVCONVERT ERROR: ',ierror
	  return
	end if
	do j = 1,nxyr
           vturn(1,j0+j) = u1(j)
           vturn(3,j0+j) = v1(j)
	enddo
c
	do j = 1,nxyr
           u1(j) = 0.
           v1(j) = 1.
	enddo
	call uvconvert(nxyr,x1(1),y1(1),u1(1),v1(1),
     +                 igtypa,ga,igtypr,gr,udef,ierror)
	if(ierror.ne.0) then
	  write(6,*) '**GRV2GRV** UVCONVERT ERROR: ',ierror
	  return
	end if
	do j = 1,nxyr
           vturn(2,j0+j) = u1(j)
           vturn(4,j0+j) = v1(j)
	enddo
c
	enddo
c
      end if
c
c  turning
c
      if (icall.ge.2) then
c
      do j = 1,nxr*nyr
         if (ur(j).ne.udef .and. vr(j).ne.udef) then
	    ua = ur(j)
	    va = vr(j)
            ur(j) = vturn(1,j)*ua + vturn(2,j)*va
            vr(j) = vturn(3,j)*ua + vturn(4,j)*va
         else
            ur(j) = udef
            vr(j) = udef
         endif
      enddo
c
      endif
c
      return
      end
