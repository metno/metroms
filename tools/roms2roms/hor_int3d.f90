subroutine hor_int3d(array_in,Mpi,Lpi,xp,yp,ylon,dx,lon_rhoo,lat_rhoo,Mpo,Lpo,N,array_out,undef,rmaski,rmasko,tx,critx,cor,mxs)
! ---------------------------------------------------------------------
! Find output grid point locations on input grid
! Get interpolation indices for bilinear interpolation

!!BAA Compute ipos, jpos based on lon_rhoo and lat_rhoo


implicit none
  integer :: i,j,k,Mpo,Lpo,margin,Lpi,Mpi,N
  real    :: alpha, phinull, rearth, r, lambda, phi
  REAL, DIMENSION(:,:), allocatable :: ipos
  REAL, DIMENSION(:,:), allocatable :: jpos
  real    :: xp,yp,DTOR,ylon
  REAL, DIMENSION(:,:), allocatable :: lon_rhoo
  REAL, DIMENSION(:,:), allocatable :: lat_rhoo
  real  :: scr(Lpi,Mpi)
  real, intent(in)   :: rmaski(Lpi,Mpi),rmasko(Lpo,Mpo)
  
  real, intent(in)  :: tx,critx,cor
  integer, intent(in)   :: mxs
  real  :: array_in(Lpi,Mpi,N)
  real, intent(out)  :: array_out(Lpo,Mpo,N)
  
  real   :: rimin,rimax,rjmin,rjmax
  integer   :: i1,i2,j1,j2
  integer   :: ia,ja
  real      :: rx,ry,rxm,rym
  real      :: dx,undef,pi
  
  integer   :: nvalue

  REAL                 :: work(Lpi,Mpi)
  REAL                 :: error(Lpi,Mpi)

!! Verdier for input grid
!! Må deklareres og sette inn verdier
pi = ATAN(1.)*4.
DTOR = pi/180.
alpha = ylon * DTOR
phinull = 60.0 * DTOR
!rearth = 6378.137   ! earth radius [km] (equatorial)
rearth = 6371.0   ! earth radius [km] (average)

DO j = 1, Mpo
  DO i = 1, Lpo
    phi = lon_rhoo(i,j) * DTOR
    lambda = lat_rhoo(i,j) * DTOR
    r = rearth * cos(lambda) * (1 + sin(phinull)) / (1 + sin(lambda))
    ipos(i,j) = xp + r * sin(phi-alpha) / dx
    jpos(i,j) = yp - r * cos(phi-alpha) / dx
  ENDDO
ENDDO

! Find subarea in Large-Area model containing output grid
rimin = 1.e+23
rimax =-1.e+23
rjmin = 1.e+23
rjmax =-1.e+23
DO j=1,Mpo
DO i=1,Lpo
  rimin = MIN(rimin,ipos(i,j))
  rimax = MAX(rimax,ipos(i,j))
  rjmin = MIN(rjmin,jpos(i,j))
  rjmax = MAX(rjmax,jpos(i,j))
ENDDO
ENDDO
margin=10
i1 = MAX(FLOOR(rimin)-margin,1)
i2 = MIN(CEILING(rimax)+margin,Lpi)
j1 = MAX(FLOOR(rjmin)-margin,1)
j2 = MIN(CEILING(rjmax)+margin,Mpi)

print '(A,4I6,A,I4)', 'Need subarea from input grid (i1,i2,j1,j2)= ', i1, i2, j1, j2, ' where added rim is ', margin







! ! Fill in masked-out values 
   DO k=1,N
      scr = 0.
      scr = array_in(:,:,k)
      WHERE((rmaski<0.01).or.(scr>1e+30)) scr = undef
      CALL fill(Lpi,Mpi,i1,i2,j1,j2,scr,tx,critx,cor,mxs,work,error,nvalue)
      array_in(:,:,k) = scr
   ENDDO
!    DO k=1,Ni
!       scr = 0.
!       scr = array_in(:,:,k)
!       WHERE((rmaski<1).or.(rmasko<1).or.(scr>1e+30)) scr = undef
!       !WHERE(rmasko<1) scr = undef
!       !call creep_fill(scr,rmaski,1,Lp,1,Mp,mxs,1)  !'Max iterations in fill or creep_fill  : ',mxs
!       CALL fill(Lpi,Mpi,1,Lpi,1,Mpi,scr,tx,critx,cor,mxs,work,error,nvalue)
!       array_in(:,:,k) = scr
!    ENDDO






! Horizontal interpolation
   array_out = undef
   DO j=1,Mpo
   DO i=1,Lpo
      ia = INT(ipos(i,j))
      ja = INT(jpos(i,j))
      IF (ia < 1 .OR. ia > Lpi .OR. ja < 1 .OR. ja > Mpi) CYCLE
      rx = (ipos(i,j)-ia)
      ry = (jpos(i,j)-ja)
      rxm = 1.0-rx
      rym = 1.0-ry
      DO k=1,N
         array_out(i,j,k) = rxm*rym*array_in(ia,ja,k)+rx*rym*array_in(ia+1,ja,k)&
         +rx*ry*array_in(ia+1,ja+1,k)+rxm*ry*array_in(ia,ja+1,k)
      ENDDO
   ENDDO
   ENDDO






end subroutine hor_int3d