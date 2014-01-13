program atm2roms
  !      
  ! =======================================================================
  ! -----------------------------------------------------------------------
  ! -----------------------------------------------------------------------
  ! 	Program that calculates and writes rainrate from rain,
  ! 	dewpoint from temp and RH, and rotate winds from feltfile so u- and
  ! 	v-vector are referred to north instead of the rotated grid axis.
  !	Output FELTfile is ready for ROMS after convertion to NetCDF via FIMEX.
  !
  ! 	Compile: gfortran -o atm2roms atm2roms.f90 grv2grv.f mer2sph.f pol2sph.f sph2rot.f uvconvert.f -L/fou/hav/felles/lib/libmi_pgi/lib/libmi_pgi.a -lmi 
! gfortran -o atm2roms atm2roms.f90 grv2grv.f mer2sph.f pol2sph.f sph2rot.f uvconvert.f earthr.f -L/usr/lib/libmi.a -lmi 
  !
  !	03.05.2011: Nils MK - nilsmk@met.no
  ! -----------------------------------------------------------------------
  IMPLICIT NONE


  !	Declaration of Variables
  INTEGER grid,len,NLAT,NLON,PROD,ntimes,length
  integer              igtypein,igtypeot,icall,undef
  real                 gridin(6),gridot(6),rainfac
  parameter    (undef= -32767)


  CHARACTER*120 infile
  CHARACTER*120 outfile

  !	felt variables:
  INTEGER      maxsiz, ldata,ihour,ierror
  INTEGER      vlevel,it,starttime,dt_neg,dt_pos,dt
  PARAMETER    (vlevel=1000)
  INTEGER*2    in(1:16)
  INTEGER*2, dimension(:), allocatable :: idata

  REAL, DIMENSION(:,:), ALLOCATABLE :: rr_field,TDair,phy
  REAL, DIMENSION(:,:), ALLOCATABLE :: Tair,cloud,Pair
  REAL, DIMENSION(:,:), ALLOCATABLE :: rain,rainrate
  REAL, DIMENSION(:,:), ALLOCATABLE :: rain_old,Qair
  REAL, DIMENSION(:,:), ALLOCATABLE :: uwind,vwind
  REAL, DIMENSION(:,:), ALLOCATABLE :: vturn
  REAL  a, b, temp      


  !	Read the input file name and the output
  read(5,'(a)') infile
  read(5,'(a)') outfile
  read(5,*) grid,starttime,dt_neg,dt_pos,len,NLAT,NLON,PROD
  read(5,*) igtypein,gridin(1),gridin(2),gridin(3),gridin(4),gridin(5),gridin(6)
  read(5,*) igtypeot,gridot(1),gridot(2),gridot(3),gridot(4),gridot(5),gridot(6)
  read(5,*) rainfac
  infile=trim(infile)
  outfile=trim(outfile)
  write(*,*) 'in: ', infile   
  write(*,*) 'out: ',outfile

  ntimes=len
  maxsiz=NLON*NLAT
  ldata=20+maxsiz+50
  
  allocate(vturn(4,NLAT*NLON))
  allocate(rr_field(NLON,NLAT))
  allocate(TDair(NLON,NLAT))
  allocate(phy(NLON,NLAT))
  allocate(Tair(NLON,NLAT))
  allocate(Pair(NLON,NLAT))
  allocate(Qair(NLON,NLAT))
  allocate(cloud(NLON,NLAT))
  allocate(rain(NLON,NLAT))
  allocate(rainrate(NLON,NLAT))
  allocate(rain_old(NLON,NLAT))
  allocate(uwind(NLON,NLAT))
  allocate(vwind(NLON,NLAT))
  allocate(idata(1:ldata))

  !c     felt variables:
!   in(1) = PROD
!   in(2) = grid
!   in(3) = -32767
!   in(4) = -32767
!   in(5) = -32767
!   in(9) = -32767
!   in(11)= -32767
!   in(13)= vlevel
!   in(14)= 0

  it=0
  icall=2  !Switch for å rotere grid


  a=17.271
  b=237.7
  !All defs done!!


  !C     setter rainrate lik null
  rainrate=0.0
  rain_old=0.0


  if(starttime.lt.0) then
    dt=dt_neg
  else
    dt=dt_pos
  end if
  !c     Loop over time steps
  it=starttime
  DO while (it.lt.(ntimes-1))            
     in(10)=it           !each r  
     write(*,*) dt
     write(*,*) it
    if(it.lt.0) then
      dt=dt_neg
    else
      dt=dt_pos
    end if
! !
!      IF(it.gt.0) THEN
!         in(9)=2
!      else
! 	in(9)=3
!      END IF
  in(1) = PROD
  in(2) = grid
  in(3) = -32767
  in(4) = -32767
  in(5) = -32767
  in(9) = -32767
  in(11)= -32767
  in(13)= vlevel
  in(14)= 0
!  in(15)= -32767
     !=================================================
     !==== Read everything ============================
     !=================================================
     ! read u component of wind 
     in(12)=33              !u componet of the wind
     CALL mrfelt(0,infile,20,in,1,maxsiz,rr_field,1.,ldata,idata,ierror)
     IF (ierror.NE.0) STOP 'ierror .ne. 0 '  
     uwind=rr_field

     ! read v component of wind         
     in(12)=34              !v componet of the wind
     CALL mrfelt(0,infile,20,in,1,maxsiz,rr_field,1.,ldata,idata,ierror)        
     IF (ierror.NE.0) STOP 'ierror .ne. 0 '
     vwind=rr_field

     !Rotate winds to use true North as ref (not grid)
     call grv2grv(icall,igtypein,gridin,igtypeot,gridot,uwind,vwind,NLON,NLAT,vturn,undef,ierror)
     IF (ierror.NE.0) STOP 'ierror .ne. 0 '
     !-----------------------------
     !---Winds OK!, will be written later
     !-----------------------------

     !     read surface air pressure
     in(12)=58              !surface air pres
     CALL mrfelt(0,infile,20,in,1,maxsiz,Pair,1.,ldata,idata,ierror)
     !Pair=rr_field
     write (*,*) Pair(100,100)
     !C     write  MSLP (file previously opened by using nyfelt)
     idata(6)=58
     idata(8)=0
     CALL mwfelt(0,outfile,10,1,maxsiz,Pair,1.,ldata, idata,ierror)
     IF (ierror.NE.0) STOP 'ierror .ne. 0 '
     
     !     read surface air temperature
     in(12)=31              !surface air temp
     CALL mrfelt(0,infile,20,in,1,maxsiz,rr_field,1.,ldata,idata,ierror)
     Tair=rr_field-273.15


     !     read cload cover
     in(12)=25              !cloud cover
     CALL mrfelt(0,infile,20,in,1,maxsiz,rr_field,1.,ldata,idata,ierror)
     cloud=rr_field*.01  !scaling

     !     read total precipitation and calculate rain rate
     in(12)=17              !total precipitation
     CALL mrfelt(0,infile,20,in,1,maxsiz,rr_field,1.,ldata,idata,ierror)
     IF (ierror.NE.0) then
        !rr_field=0.0
        rr_field=rain_old
     end if
     rainrate=(rr_field-rain_old)*rainfac     !mm/h -> kg/m2/s
     rain_old=rr_field
     where (rainrate<0.0)
        rainrate=0.0
     end where
        

     !C     read Qair
     in(12)=32              !Qair
     CALL mrfelt(0,infile,20,in,1,maxsiz,rr_field,1.,ldata,idata,ierror)
     phy=((a*Tair)/(b+Tair))+log(rr_field/100)
     TDair=(b*phy)/(a-phy)
     call spec_hum(TDair, Tair, Pair, Qair)

     !=================================================
     !==== Write everything ===========================
     !=================================================
     ! write  u componet of the wind (file previously opened by using nyfelt)
     idata(6)=33
     idata(8)=0
     CALL mwfelt(0,outfile,10,1,maxsiz,uwind,1.,ldata, idata,ierror)
     IF (ierror.NE.0) STOP 'ierror .ne. 0 '

     ! write  v componet of the wind
     idata(6)=34
     idata(8)=0
     CALL mwfelt(0,outfile,10,1,maxsiz,vwind,1.,ldata, idata,ierror)
     IF (ierror.NE.0) STOP 'ierror .ne. 0 '  


     !C     write  surface air temp (file previously opened by using nyfelt)
     idata(6)=31
     idata(8)=0
     CALL mwfelt(0,outfile,10,1,maxsiz,Tair,1.,ldata, idata,ierror)
     IF (ierror.NE.0) STOP 'ierror .ne. 0 '

     !C     write air temp (file previously opened by using nyfelt)
     idata(6)=25
     idata(8)=0
     CALL mwfelt(0,outfile,10,1,maxsiz,cloud,1.,ldata, idata,ierror)
     IF (ierror.NE.0) STOP 'ierror .ne. 0 '

     !C     write rain rate (file previously opened by using nyfelt)
     idata(6)=339
     idata(8)=0
     idata(20)=-7   !presisjon
     CALL mwfelt(0,outfile,10,1,maxsiz,rainrate,1.,ldata, idata,ierror)
     IF (ierror.NE.0) STOP 'ierror .ne. 0 '

     !C     write  Qair
     idata(6)=32
     idata(8)=0
     idata(20)=-9   !presisjon
     CALL mwfelt(0,outfile,10,1,maxsiz,Qair,1.,ldata, idata,ierror)
     IF (ierror.NE.0) STOP 'ierror .ne. 0 '  

     !Tømmer arrays
     vwind=0.0
     uwind=0.0
     Pair=0.0
     Tair=0.0
     TDair=0.0
     cloud=0.0
     rainrate=0.0
     Qair=0.0
     rr_field=0.0
     
     it=it+dt
  end do

contains

  subroutine spec_hum(TD, TA, P, Q)
  ! This subroutine is taken from the atmseq2roms-program

    real, dimension(NLON, NLAT), intent(in) :: TD   ! Dew point temperature
    real, dimension(NLON, NLAT), intent(in) :: TA   ! Atmospheric temperature
    real, dimension(NLON, NLAT), intent(in) :: P    ! Atmospheric pressure
    real, dimension(NLON, NLAT), intent(out) :: Q

    real, parameter :: Aw = 7.5
    real, parameter :: Ai = 9.5
    real, parameter :: Bw = 237.3
    real, parameter :: Bi = 265.5

    real, dimension(NLON, NLAT) :: es, e, cff, e_sat, qw


    es = 6.1078 * 10**((TA*Aw)/(TA+Bw))
    where(TA < 0) 
       es = 6.1078 * 10**((TA*Ai)/(TA+Bi))
    end where

    e = 6.1078 * 10**((TD*Aw)/(TD+Bw))

    Q = e/es  ! fraction

    where (Q < 0.0) Q = 0.0
    where (Q > 1.0) Q = 1.0

    cff = (0.7859 + 0.03477*TA)/(1.0 + 0.00412*TA)
    e_sat = 10.**cff  ! hPa
    qw = 0.622*e_sat/(P - 0.378*e_sat)
    Q = (Q*qw/(1.-qw*(1.-Q)))/1000  ! g/kg
    !print *, "Specific humidity calculated from dew point temperature ", maxval(Q)

  end subroutine spec_hum

end program atm2roms
! C--------------------------------------------------------------------
! C     
! C======================================================================

