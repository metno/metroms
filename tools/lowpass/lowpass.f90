program lowpass
  ! nilsmk@met.no 1/11/2011
! gfortran -o dew2spec dew2spec.f90 -I/home/metno_op/roms/testdir/NETCDF_gfortran/include/ -L/home/metno_op/roms/testdir/NETCDF_gfortran/lib -lnetcdf !bore

use netcdf

implicit none
REAL, DIMENSION(:,:,:), ALLOCATABLE :: zeta,zetamean
character(len=99) :: infile
character(len=80) :: x_dimname,y_dimname,time_dimname
integer   :: X,Y,TIME,ncid,dim_x,dim_y,dim_time,k,i,j
integer   :: statusi,statuso,zetaVarId,avgperiod

avgperiod=12

call getarg(1,infile)

X = 0
Y = 0
!write(*,*) 'X, Y, TIME = ',X,Y,TIME

statusi = nf90_open(trim(infile),nf90_write,ncid)
statusi = nf90_inq_dimid(ncid,'X',dim_x)
if (statusi /= nf90_noerr) statusi = nf90_inq_dimid(ncid,'rlon',dim_x)
if (statusi /= nf90_noerr) statusi = nf90_inq_dimid(ncid,'xi_rho',dim_x)
statusi = nf90_inq_dimid(ncid,'Y',dim_y)
if (statusi /= nf90_noerr) statusi = nf90_inq_dimid(ncid,'rlat',dim_y)
if (statusi /= nf90_noerr) statusi = nf90_inq_dimid(ncid,'eta_rho',dim_y)
statusi = nf90_Inquire_Dimension(ncid,dim_x,x_dimname,X)
statusi = nf90_Inquire_Dimension(ncid,dim_y,y_dimname,Y)
statusi = nf90_inq_dimid(ncid,'time',dim_time)
if (statusi /= nf90_noerr) statusi = nf90_inq_dimid(ncid,'ocean_time',dim_time)
statusi = nf90_Inquire_Dimension(ncid,dim_time,time_dimname,TIME)

write(*,*) 'infile= ',infile
write(*,*) 'X, Y, TIME = ',X,Y,TIME


allocate(zeta(X,Y,TIME))
allocate(zetamean(X,Y,TIME))
statusi = nf90_inq_varid(ncid,'zeta_detided',zetaVarId)
statusi = nf90_get_var(ncid,zetaVarId,zeta)


!write(*,*) zeta(400,400,:)
!write(*,*) '----------------------------'


do k=1,TIME
  !write(*,*) 'time= ', k
  do i=1,X
    do j=1,Y
      if (k.le.(avgperiod/2)) then
        zetamean(i,j,k)=sum(zeta(i,j,1:(k+(avgperiod/2))))/(k+(avgperiod/2))
      else
        if ((k+(avgperiod/2)).ge.TIME) then
          zetamean(i,j,k)=sum(zeta(i,j,(k-(avgperiod/2)):TIME))/(TIME-k+(avgperiod/2))
        else
          zetamean(i,j,k)=sum(zeta(i,j,(k-(avgperiod/2)):(k+(avgperiod/2))))/avgperiod
        end if
      end if
    end do
  end do
end do

!write(*,*) zeta(400,400,:)
!write(*,*) '----------------------------'
!write(*,*) zetamean(400,400,:)

statuso = nf90_put_var(ncid,zetaVarId,zetamean)
statuso = nf90_close(ncid)



end program lowpass
