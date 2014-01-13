program prec2int
  use netcdf
  
  implicit none
  REAL, DIMENSION(:,:), ALLOCATABLE :: temp,temp2
  REAL, DIMENSION(:,:,:), ALLOCATABLE :: rain
  character(len=99) :: infile
  character(len=80) :: x_dimname,y_dimname,time_dimname
  integer   :: X,Y,TIME,ncid,dim_x,dim_y,dim_time,k
  integer   :: statusi,statuso,rainVarId

  call getarg(1,infile)

  X = 0
  Y = 0
  statusi = nf90_open(trim(infile),nf90_write,ncid)
  statusi = nf90_inq_dimid(ncid,'x',dim_x)
  if (statusi /= nf90_noerr) statusi = nf90_inq_dimid(ncid,'rlon',dim_x)
  statusi = nf90_inq_dimid(ncid,'y',dim_y)
  if (statusi /= nf90_noerr) statusi = nf90_inq_dimid(ncid,'rlat',dim_y)
  statusi = nf90_Inquire_Dimension(ncid,dim_x,x_dimname,X)
  statusi = nf90_Inquire_Dimension(ncid,dim_y,y_dimname,Y)
  statusi = nf90_inq_dimid(ncid,'time',dim_time)
  statusi = nf90_Inquire_Dimension(ncid,dim_time,time_dimname,TIME)

  write(*,*) 'infile= ',infile
  write(*,*) 'X, Y, TIME = ',X,Y,TIME
  
  allocate(rain(X,Y,TIME))
  allocate(temp(X,Y))
  allocate(temp2(X,Y))
  temp=0.0
  temp2=0.0
  statusi = nf90_inq_varid(ncid,'rain',rainVarId)
  statusi = nf90_get_var(ncid,rainVarId,rain)

  do k=TIME,2,-1
     write(*,*) 'time= ', k
     write(*,*) rain(100,100,k)
     rain(:,:,k)=rain(:,:,k)-rain(:,:,(k-1))
     where( rain(:,:,k) < 0.0 ) rain(:,:,k)=0.0
     write(*,*) rain(100,100,k)

  end do

  statuso = nf90_put_var(ncid,rainVarId,rain,start=(/1,1,1/))
  statuso = nf90_close(ncid)

end program prec2int
