program check_tide
!ifort -O3 -o check_tide check_tide.f90  -lnetcdff -lnetcdf
!nilsmk 29.feb2012
  use netcdf
  
  implicit none
  REAL, DIMENSION(:,:,:), ALLOCATABLE :: zeta_detided
  character(len=99) :: infile
  character(len=80) :: x_dimname,y_dimname,time_dimname
  integer   :: X,Y,TIME,ncid,dim_x,dim_y,dim_time,k
  integer   :: statusi,statuso,zeta_detidedVarId
  
  call getarg(1,infile)

  X = 0
  Y = 0
  write(*,*) 'X, Y, TIME = ',X,Y,TIME

  statusi = nf90_open(trim(infile),nf90_nowrite,ncid)
  statusi = nf90_inq_dimid(ncid,'x',dim_x)
  if (statusi /= nf90_noerr) statusi = nf90_inq_dimid(ncid,'rlon',dim_x)
  if (statusi /= nf90_noerr) statusi = nf90_inq_dimid(ncid,'xi_rho',dim_x)
  statusi = nf90_inq_dimid(ncid,'y',dim_y)
  if (statusi /= nf90_noerr) statusi = nf90_inq_dimid(ncid,'rlat',dim_y)
  if (statusi /= nf90_noerr) statusi = nf90_inq_dimid(ncid,'eta_rho',dim_y)
  statusi = nf90_Inquire_Dimension(ncid,dim_x,x_dimname,X)
  statusi = nf90_Inquire_Dimension(ncid,dim_y,y_dimname,Y)
  statusi = nf90_inq_dimid(ncid,'time',dim_time)
  if (statusi /= nf90_noerr) statusi = nf90_inq_dimid(ncid,'ocean_time',dim_time)
  statusi = nf90_Inquire_Dimension(ncid,dim_time,time_dimname,TIME)

  write(*,*) 'infile= ',infile
  write(*,*) 'X, Y, TIME = ',X,Y,TIME

  
  allocate(zeta_detided(X,Y,TIME))
  statusi = nf90_inq_varid(ncid,'zeta_detided',zeta_detidedVarId)
  statusi = nf90_get_var(ncid,zeta_detidedVarId,zeta_detided)

! Check if any values are above or below a selected treshold [-10,10] meters
  print *, maxval(zeta_detided)
  where (zeta_detided > 1.0e35) zeta_detided=0.0
  print *, maxval(zeta_detided)
  
  if (maxval(zeta_detided)>10.0.or.minval(zeta_detided)<-10.0) then
     print*, "Error in detiding, PANIC!!!"
     OPEN (7, FILE = 'error.txt', ACCESS = 'APPEND',STATUS = 'NEW')
     WRITE(7,*) "Error in detiding, PANIC!!!"
     CLOSE(7)
  end if



  statuso = nf90_close(ncid)

end program check_tide
