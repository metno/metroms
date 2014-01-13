program dew2spec
  ! nilsmk@met.no 1/11/2011
  ! gfortran -o dew2spec dew2spec.f90 -I/home/metno_op/roms/testdir/NETCDF_gfortran/include/ -L/home/metno_op/roms/testdir/NETCDF_gfortran/lib -lnetcdf !bore

  use netcdf
  
  implicit none
  REAL, DIMENSION(:,:), ALLOCATABLE :: Tair,Pair,TDair
  REAL, DIMENSION(:,:,:), ALLOCATABLE :: Qair
  character(len=99) :: infile
  character(len=80) :: x_dimname,y_dimname,time_dimname
  integer   :: X,Y,TIME,ncid,dim_x,dim_y,dim_time,k,length,posx,posy
  integer   :: status,TairVarId,QairVarId,PairVarId,varid
  real      :: scaleFactor,addOffset
  
  call getarg(1,infile)

  posx=50
  posy=50
  addOffset=0.0
  scaleFactor=1.0

  X = 0
  Y = 0
  TIME=0
  write(*,*) 'X, Y, TIME = ',X,Y,TIME

  status = nf90_open(trim(infile),nf90_write,ncid)
  status = nf90_inq_dimid(ncid,'x',dim_x)
  if (status /= nf90_noerr) status = nf90_inq_dimid(ncid,'rlon',dim_x)
  if (status /= nf90_noerr) status = nf90_inq_dimid(ncid,'longitude',dim_x)
  status = nf90_inq_dimid(ncid,'y',dim_y)
  if (status /= nf90_noerr) status = nf90_inq_dimid(ncid,'rlat',dim_y)
  if (status /= nf90_noerr) status = nf90_inq_dimid(ncid,'latitude',dim_y)
  status = nf90_Inquire_Dimension(ncid,dim_x,x_dimname,X)
  status = nf90_Inquire_Dimension(ncid,dim_y,y_dimname,Y)
  status = nf90_inq_dimid(ncid,'time',dim_time)
  status = nf90_Inquire_Dimension(ncid,dim_time,time_dimname,TIME)

  write(*,*) 'infile= ',infile
  write(*,*) 'X, Y, TIME = ',X,Y,TIME

  
  allocate(Tair(X,Y))
  allocate(TDair(X,Y))
  allocate(Pair(X,Y))
  allocate(Qair(X,Y,TIME))

  do k=1,TIME
     status = nf90_inq_varid(ncid,'Tair',TairVarId)
     if (status /= nf90_noerr) status = nf90_inq_varid(ncid,'t2m',TairVarId)
     status = nf90_get_var(ncid,TairVarId,Tair,start=(/ 1, 1, k/))
     status = nf90_get_att(ncid, TairVarId, "scale_factor", scaleFactor)
     if (status == nf90_noerr) status = nf90_get_att(ncid, TairVarId, "add_offset", addOffset)
     if (status == nf90_noerr) Tair=(Tair*scaleFactor)+addOffset
     
     status = nf90_inq_varid(ncid,'d2m',QairVarId)
     if (status /= nf90_noerr) status = nf90_inq_varid(ncid,'Qair',QairVarId)
     status = nf90_get_var(ncid,QairVarId,TDair,start=(/ 1, 1, k/))
     status = nf90_get_att(ncid, QairVarId, "scale_factor", scaleFactor)
     if (status == nf90_noerr) status = nf90_get_att(ncid, QairVarId, "add_offset", addOffset)
     if (status == nf90_noerr) TDair=(TDair*scaleFactor)+addOffset

     status = nf90_inq_varid(ncid,'Pair',PairVarId)
     if (status /= nf90_noerr) status = nf90_inq_varid(ncid,'msl',PairVarId)
     status = nf90_get_var(ncid,PairVarId,Pair,start=(/ 1, 1, k/))
     status = nf90_get_att(ncid, PairVarId, "scale_factor", scaleFactor)
     if (status == nf90_noerr) status = nf90_get_att(ncid, PairVarId, "add_offset", addOffset)
     if (status == nf90_noerr) Pair=(Pair*scaleFactor)+addOffset
     
     if (Pair(1,1) < 20000)  Pair=Pair/10  !output is hPa
     if (Pair(1,1) >= 20000) Pair=Pair/100  !output is hPa
  !if (Tair(1,1) >= ) Tair=(Tair/100)-273.15   !output is C
     if (Tair(1,1) > 100) Tair=Tair-273.15   !output is C
     if (TDair(1,1) > 100) TDair=TDair-273.15 !in C
     

     
     write(*,*) 'time= ', k
     
     write(*,*) Pair(posx,posy)
     write(*,*) Tair(posx,posy)
     write(*,*) TDair(posx,posy)
     call spec_hum(TDair(:,:), Tair(:,:), Pair(:,:), Qair(:,:,k))
     write(*,*) Qair(posx,posy,k)
     
  end do

  !status = nf90_put_var(ncid,QairVarId,Qair,start=(/1,1,1/))
  !status = nf90_close(ncid)

  status = nf90_redef(ncid)
  if (status /= nf90_noerr) write(*,*) "error1!",status
  status = nf90_def_var(ncid,"Qair", nf90_float, (/ dim_x, dim_y, dim_time /), varid)
  if (status /= nf90_noerr) write(*,*) "error2!",status
  status = nf90_enddef(ncid)
  if (status /= nf90_noerr) write(*,*) "error3!",status
  status = nf90_put_var(ncid,varid,Qair,start=(/1,1,1/))
  if (status /= nf90_noerr) write(*,*) "error4!",status
  status = nf90_close(ncid)
  if (status /= nf90_noerr) write(*,*) "error5!",status

  
contains
  subroutine spec_hum(TD, TA, P, Q)
    ! This subroutine is taken from the atmseq2roms-program
    
    real, dimension(X, Y), intent(in) :: TD   ! Dew point temperature
    real, dimension(X, Y), intent(in) :: TA   ! Atmospheric temperature
    real, dimension(X, Y), intent(in) :: P    ! Atmospheric pressure
    real, dimension(X, Y), intent(out) :: Q
    
    real, parameter :: Aw = 7.5
    real, parameter :: Ai = 9.5
    real, parameter :: Bw = 237.3
    real, parameter :: Bi = 265.5
    
    real, dimension(X, Y) :: es, e, cff, e_sat, qw
    
    
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
    Q = (Q*qw/(1.-qw*(1.-Q)))   !/1000  ! g/kg
    !print *, "Specific humidity calculated from dew point temperature ", maxval(Q)
  end subroutine spec_hum
  
end program dew2spec
