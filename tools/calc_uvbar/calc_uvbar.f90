program calc_uvbar
! nilsmk@met.no
! ocean_clm.nc
! arctic-20km_grd.nc
! 30,0.1,6,2,1
 
  use netcdf
  implicit none

  real              :: Tcline,theta_b,theta_s
  integer           :: Vtrans,Vstretch,ncid,dim_x,dim_y,dim_time,dim_srho,statusi,statuso,varid
  integer           :: X,Y,srho,TIME,k,i,j
  character(len=99) :: infile,gridFile,x_dimname,y_dimname,time_dimname,srho_dimname
  
  real, dimension(:,:), allocatable     ::  zeta,ubar,vbar,h,hu,hv
  real, dimension(:,:,:), allocatable   ::  z_r,z_w,dz,dzu,dzv,u,v
  !real, dimension(:,:,:,:), allocatable ::  

  ! call getarg(1,infile)  !string
  ! call getarg(2,gridFile)!string
  ! call getarg(3,Tcline)  !real
  ! call getarg(4,theta_b) !real
  ! call getarg(5,theta_s) !real
  ! call getarg(6,Vtrans)  !int
  ! call getarg(7,Vstretch)!int
  read(5,'(a)') infile
  read(5,'(a)') gridFile
  read(5,*) Tcline,theta_b,theta_s,Vtrans,Vstretch

  !write(*,*) infile
  !write(*,*) gridFile
  
  statusi = nf90_open(trim(infile),nf90_nowrite,ncid)
  statusi = nf90_inq_dimid(ncid,'xi_rho',dim_x)
  statusi = nf90_inq_dimid(ncid,'eta_rho',dim_y)
  statusi = nf90_inq_dimid(ncid,'clim_time',dim_time)
  statusi = nf90_inq_dimid(ncid,'s_rho',dim_srho)
  statusi = nf90_Inquire_Dimension(ncid,dim_x,x_dimname,X)
  statusi = nf90_Inquire_Dimension(ncid,dim_y,y_dimname,Y)
  statusi = nf90_Inquire_Dimension(ncid,dim_time,time_dimname,TIME)
  statusi = nf90_Inquire_Dimension(ncid,dim_srho,srho_dimname,srho)
  statusi = nf90_close(ncid)

  write(*,*) 'X, Y, srho, TIME = ',X,Y,srho,TIME
  write(*,*) Tcline,theta_b,theta_s,Vtrans,Vstretch

  allocate(h(X,Y))
!   allocate(zeta(X,Y,TIME))
!   allocate(u(X-1,Y,srho,TIME))
!   allocate(v(X,Y-1,srho,TIME))
!   allocate(ubar(X-1,Y,TIME))
!   allocate(vbar(X,Y-1,TIME))
  allocate(zeta(X,Y))
  allocate(u(X-1,Y,srho))
  allocate(v(X,Y-1,srho))
  allocate(ubar(X-1,Y))
  allocate(vbar(X,Y-1))
  
  statusi = nf90_open(trim(gridFile),nf90_nowrite,ncid)
  statusi = nf90_inq_varid(ncid,'h',varid)
  statusi = nf90_get_var(ncid,varid,h)
  statusi = nf90_close(ncid)

  allocate(hu(X-1,Y))
  allocate(hv(X,Y-1))
  hu=(h(1:(X-1),:)+h(2:X,:))/2
  hv=(h(:,1:(Y-1))+h(:,2:Y))/2

  


  ! Vertical grid specification
  allocate(z_r(X,Y,srho))
  allocate(z_w(X,Y,(srho+1)))
  allocate(dz(X,Y,srho))
  allocate(dzu(X-1,Y,srho))
  allocate(dzv(X,Y-1,srho))

  do k=1,TIME
     statusi = nf90_open(trim(infile),nf90_write,ncid)
     statusi = nf90_inq_varid(ncid,'zeta',varid)
     statusi = nf90_get_var(ncid,varid,zeta,start=(/ 1, 1, k/))
     statusi = nf90_inq_varid(ncid,'u',varid)
     statusi = nf90_get_var(ncid,varid,u,start=(/ 1, 1, 1, k/))
     statusi = nf90_inq_varid(ncid,'v',varid)
     statusi = nf90_get_var(ncid,varid,v,start=(/ 1, 1, 1, k/))
     !Specify output vertical grid
     call spec_vert_grid(X,Y,srho,h,zeta(:,:),Tcline,theta_b,theta_s,Vtrans,Vstretch,z_r,z_w)

     dz=-(z_w(:,:,1:srho)-z_w(:,:,2:(srho+1)))
     dzu=(dz(1:(X-1),:,:)+dz(2:X,:,:))/2
     dzv=(dz(:,1:(Y-1),:)+dz(:,2:Y,:))/2
     !ubar(:,:)=sum(u(:,:,:)*dzu,3)/hu
     !vbar(:,:)=sum(v(:,:,:)*dzv,3)/hv
     !ubar=sum(u*dzu,3)/hu
     !vbar=sum(v*dzv,3)/hv
     do i=1,X-1
        do j=1,Y-1
           ubar(i,j)=sum(u(i,j,:)*dzu(i,j,:))/hu(i,j)
           vbar(i,j)=sum(v(i,j,:)*dzv(i,j,:))/hv(i,j)
           !write(*,*) 'i, j, k = ',i,j,k,ubar(i,j),vbar(i,j)
        end do
     end do
     
     statuso = nf90_inq_varid(ncid,'ubar',varid)
     statuso = nf90_put_var(ncid,varid,ubar,start=(/1,1,k/))
     statuso = nf90_inq_varid(ncid,'vbar',varid)
     statuso = nf90_put_var(ncid,varid,vbar,start=(/1,1,k/))
     statuso = nf90_sync(ncid)
     write(*,*) 'wrote time ',k
  end do

 
  statusi = nf90_close(ncid)

end program calc_uvbar
