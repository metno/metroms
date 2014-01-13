!Program for rotating winds on NetCDF file
!Compile: gfortran -o rotate_winds rotate_winds.f90 grv2grv.f mer2sph.f pol2sph.f sph2rot.f uvconvert.f 
!On Ve: ifort -O3 -o rotate_winds rotate_winds_nc.f90 grv2grv.f mer2sph.f pol2sph.f sph2rot.f uvconvert.f earthr.f -lnetcdff -lnetcdf

program rotate_winds
  use netcdf

  implicit none
  integer              igtypein,igtypeot,ierror,icall,it,undef
  real                 gridin(6),gridot(6)
  parameter            (undef= -32767)
  CHARACTER(len=99)    infile
  character(len=80)    :: x_dimname,y_dimname,time_dimname
  integer              :: X,Y,TIME,ncid,dim_x,dim_y,dim_time
  integer              :: statusi,statuso,UwindVarId,VwindVarId
  !variables:
  real, dimension(:,:,:), allocatable :: uwind,vwind
  real, dimension(:,:), allocatable   :: vturn

  !--------------------------------------------------------------
  icall=2
  ! Definisjon av grid inn og ut (NorKyst-800 ut)
  igtypein=3
  gridin(1)=10.505
  gridin(2)=-6.895
  gridin(3)=0.036
  gridin(4)=0.036
  gridin(5)=-24
  gridin(6)=66.5
  igtypeot=5
  gridot(1)=gridin(1)
  gridot(2)=gridin(2)
  gridot(3)=4
  gridot(4)=4
  gridot(5)=66.5
  gridot(6)=0
  
  !All defs done!!

  call getarg(1,infile)
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
  allocate(uwind(X,Y,TIME))
  allocate(vwind(X,Y,TIME))
  allocate(vturn(4,X*Y))
  !read u and v
  statusi = nf90_inq_varid(ncid,'Uwind',UwindVarId)
  statusi = nf90_get_var(ncid,UwindVarId,uwind)
  statusi = nf90_inq_varid(ncid,'Vwind',VwindVarId)
  statusi = nf90_get_var(ncid,VwindVarId,vwind)

  do it=1,TIME
     call grv2grv(icall,igtypein,gridin,igtypeot,gridot,uwind(:,:,it),vwind(:,:,it),Y,X,vturn,undef,ierror)
     IF (ierror.NE.0) STOP 'ierror .ne. 0 '
  end do
  !write new u and v
  statuso = nf90_put_var(ncid,UwindVarId,uwind,start=(/1,1,1/))
  statuso = nf90_put_var(ncid,VwindVarId,vwind,start=(/1,1,1/))
  statuso = nf90_close(ncid)

end program rotate_winds
