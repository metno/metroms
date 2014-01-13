program convert_rr
! C     
! C=======================================================================
! C-----------------------------------------------------------------------
! C     23456789 123456789 123456789 123456789 123456789 123456789 123456789 12
! C     1         2         3         4         5         6         7
! C-----------------------------------------------------------------------
! C     Program that calculates and writes rainrate from rain
! C     and dewpoint from temp and RH
! C     
! C-----------------------------------------------------------------------
      IMPLICIT NONE


!c     Declaration of Variables
      INTEGER grid,len,NLAT,NLON,PROD,ntimes,length
      PARAMETER (grid=2115,len=66,NLAT=580,NLON=315,PROD=88,ntimes=len)

      
      CHARACTER*50 infile
      CHARACTER*50 outfile
      CHARACTER*1  space
      CHARACTER*1  code


!c     felt variables:
      INTEGER      maxsiz, ldata,ihour,ierror
      INTEGER      vlevel,it
      PARAMETER    (maxsiz=NLON*NLAT)
      PARAMETER    (ldata=20+maxsiz+50)
      PARAMETER    (vlevel=1000)
      INTEGER*2    in(1:16),idata(1:ldata)
      
      REAL         rr_field(NLON,NLAT),TDair(NLON,NLAT),phy(NLON,NLAT)
      REAL         Tair(NLON,NLAT),cloud(NLON,NLAT),Pair(NLON,NLAT)
      REAL         rain(NLON,NLAT),rainrate(NLON,NLAT)
      REAL         rain_old(NLON,NLAT),Qair(NLON,NLAT)
      REAL         a, b, temp      


!C     Read the input file name and the output
      CALL GETARG(1,infile)
      CALL GETARG(2,outfile)
      write(*,*) 'in: ', infile    
      write(*,*) 'out: ',outfile 



!c     felt variables:
      in(1) = PROD
      in(2) = grid
      in(3) = -32767
      in(4) = -32767
      in(5) = -32767
      in(9) = -32767
      in(11)= -32767
      in(13)= vlevel
      in(14)= 0

      it=0


      a=17.271
      b=237.7


!C     setter rainrate lik null
      rainrate=0.0
      rain_old=0.0
      

      
!c     Loop over time steps for a fix member
      in(9)=3



      DO it=0,ntimes,1            
         in(10)=it           !each r            
         IF(it.gt.0) THEN
            in(9)=2
         END IF
!C     read surface air pressure
!         write(*,*) 'reading MSLP' 
         in(12)=58              !surface air pres
         CALL mrfelt(0,infile,20,in,1,maxsiz,rr_field,1.,ldata,idata,ierror)
         Pair=rr_field
!C     read surface air temperature
!         write(*,*) 'Tair' 
         in(12)=31              !surface air temp
         CALL mrfelt(0,infile,20,in,1,maxsiz,rr_field,1.,ldata,idata,ierror)
         Tair=rr_field-273.15
         idata(6)=31
         idata(8)=0
!C     write  surface air temp (file previously opened by using nyfelt)
         CALL mwfelt(0,outfile,10,1,maxsiz,Tair,1.,ldata, idata,ierror)
         IF (ierror.NE.0) STOP 'ierror .ne. 0 '


!C     read cload cover
!         write(*,*) 'cloud' 
         in(12)=25              !cloud cover
         CALL mrfelt(0,infile,20,in,1,maxsiz,rr_field,1.,ldata,idata,ierror)
         cloud=rr_field*.01  !scaling
         idata(6)=25
         idata(8)=0
!C     write air temp (file previously opened by using nyfelt)
         CALL mwfelt(0,outfile,10,1,maxsiz,cloud,1.,ldata, idata,ierror)
         IF (ierror.NE.0) STOP 'ierror .ne. 0 '

!C     read total precipitation and calculate rain rate
!         write(*,*) 'rain' 
         in(12)=17              !total precipitation
         CALL mrfelt(0,infile,20,in,1,maxsiz,rr_field,1.,ldata,idata,ierror)
         rainrate=(rr_field-rain_old)*(27.78e-5)     !mm/h -> kg/m2/s
         idata(6)=339
         idata(8)=0
         idata(20)=-7   !presisjon
!C     write rain rate (file previously opened by using nyfelt)
         CALL mwfelt(0,outfile,10,1,maxsiz,rainrate,1.,ldata, idata,ierror)
         IF (ierror.NE.0) STOP 'ierror .ne. 0 '
         
!C     read total precipitation
         in(12)=17              !total precipitation
         CALL mrfelt(0,infile,20,in,1,maxsiz,rr_field,1.,ldata,idata,ierror)
         rain_old=rr_field
         
!C     read Qair
!         write(*,*) 'Qair' 
         in(12)=32              !Qair
         CALL mrfelt(0,infile,20,in,1,maxsiz,rr_field,1.,ldata,idata,ierror)
         phy=((a*Tair)/(b+Tair))+log(rr_field/100)
         TDair=(b*phy)/(a-phy)
         call spec_hum(TDair, Tair, Pair, Qair)
         !print *, maxval(Qair)
         
         idata(6)=32
         idata(8)=0
         idata(20)=-9   !presisjon
!C     write  Qair
         CALL mwfelt(0,outfile,10,1,maxsiz,Qair,1.,ldata, idata,ierror)
         IF (ierror.NE.0) STOP 'ierror .ne. 0 '  
         
         Pair=0.0
         Tair=0.0
         TDair=0.0
         cloud=0.0
         rainrate=0.0
         Qair=0.0
         rr_field=0.0
         
         
!         write(*,*) 'all done at time ' , in(10)
      end do

contains

subroutine spec_hum(TD, TA, P, Q)


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

end program convert_rr
! C--------------------------------------------------------------------
! C     
! C======================================================================
! C     to compile in UBUNTU
! C     gfortran -o convert_rr convert_rr.f  -L/fou/hav/felles/lib/libmi_pgi/lib/libmi_pgi.a -lmi 

