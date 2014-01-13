subroutine addvar(varname,ncidin,ncidout)

!*****************************************************************
! This subroutine read variable data from ROMS output file,      !
! interpolates both in the vertical (from s- to z-levels),       !
! and if necessary in the horizontal (from u- or v-coordinates   !
! to rho-coordinates, so that all variables on output file are   !
! given on the same grid.                                        !
! Variables are also packed as short integers                    ! 
!*****************************************************************
use setup
use dimensions
use netcdf_atts
use netcdf

implicit none

character (len=*), intent(in) :: varname
integer, intent(in) :: ncidin,ncidout 

character (len=40) :: name
integer :: ndim, status, varidin, varidout,n,tmpvaridin
integer :: xtype,natts
real, dimension(:,:), allocatable ::mask, mask_var,F2
real, dimension(:,:,:), allocatable :: F3, F3U, F3V, Z,F3T,Z_tmp
real, dimension(:,:,:,:), allocatable :: F4, F4U, F4V, F4Z, F4T
integer, dimension(:,:,:), allocatable ::F3I
integer, dimension(:,:,:,:), allocatable ::F4I
integer, dimension(NF_MAX_DIMS) :: dimid
real, parameter :: undef=1.0e37
integer :: i,j, t,k,s, irec
real :: w1, w2
real, dimension(xi_rho,eta_rho) :: H
real , dimension(:), allocatable :: slev,cs
real :: hc, Vtrans
integer :: imax, jmax, itime

allocate(mask(xi_rho,eta_rho))

status=nf_inq_varid(ncidin,"mask_rho",varidin); call check_err(status)
status = nf_get_var_real(ncidin,varidin, mask); call check_err(status)

status = nf_inq_varid(ncidin,trim(varname),varidin); call check_err(status)
status = nf_inq_varndims(ncidin,varidin,ndim); call check_err(status)
status = nf_inq_vardimid(ncidin,varidin,dimid); call check_err(status)

! find the variable in list of possible output variables, in order use 
! information about how to handle them
do i=1,maxvar
  if (verify(trim(varname),trim(var_info(i)%rname)).eq.0) then
       print *, "Varname: ",trim(varname), " varinfo: ",trim(var_info(i)%rname)
    irec=i
    exit
  end if
end do


if (ndim.eq.3) then

  if (dimid(1).eq.xi_rhoID.and.dimid(2).eq.eta_rhoID) then

    allocate(F3(xi_rho,eta_rho,outtime), F3T(X,Y,outtime))

    status = nf_get_var_real(ncidin,varidin,F3); call check_err(status)
    ! Give landpoints undef value 
    do n=1, outtime
      where(mask(:,:) < 0.5)
        F3(:,:,n)=undef
      end where
    end do
    F3T(:,:,:)=F3(xstart:xend,ystart:yend,:)

    allocate(F3I(X,Y,outtime))
    ! Convert to integer
    F3I=real2int_2D(F3T,X,Y,outtime)

    status = nf_inq_varid(ncidout,trim(var_info(irec)%vname),varidout); call check_err(status)
    status = nf_put_var_int(ncidout,varidout,F3I); call check_err(status)

    deallocate(F3,F3T,F3I)
    print *, trim(varname), " written to file"

  elseif (dimid(1).eq.xi_uID.and.dimid(2).eq.eta_uID) then

    allocate(F3U(xi_u,eta_u,outtime), F3(xi_rho-2,eta_rho-2,outtime),mask_var(xi_u,eta_u), F3T(X,Y,outtime))

    status = nf_get_var_real(ncidin,varidin, F3U); call check_err(status)
    ! Interpolate to rho-points
    F3=u2rho_2D(F3U)
    print *, trim(varname), " interpolated from u-points to rho-points"

    F3T(:,:,:)=F3(xstart-1:xend-1,ystart-1:yend-1,:)
    allocate(F3I(X,Y,outtime))
    ! Convert to integer
    F3I=real2int_2D(F3T,X,Y,outtime)

    status = nf_inq_varid(ncidout,trim(var_info(irec)%vname),varidout); call check_err(status)
    status = nf_put_var_int(ncidout,varidout,F3I); call check_err(status)

    deallocate(F3,F3U,mask_var,F3T,F3I)
    print *, trim(varname), " written to file"

  elseif (dimid(1).eq.xi_vID.and.dimid(2).eq.eta_vID) then

    allocate(F3V(xi_v,eta_v,outtime), F3(xi_rho-2,eta_rho-2,outtime),mask_var(xi_v,eta_v), F3T(X,Y,outtime))

    status = nf_get_var_real(ncidin,varidin, F3V); call check_err(status)
    ! Interpolate to rho-points
    F3=v2rho_2D(F3V)
    print *, trim(varname), " interpolated from u-points to rho-points"

    F3T(:,:,:)=F3(xstart-1:xend-1,ystart-1:yend-1,:)
    allocate(F3I(X,Y,outtime))
    ! convert to integer
    F3I=real2int_2D(F3T,X,Y,outtime)

    status = nf_inq_varid(ncidout,trim(var_info(irec)%vname),varidout); call check_err(status)
    status = nf_put_var_int(ncidout,varidout,F3I); call check_err(status)

    deallocate(F3,F3V,mask_var,F3T,F3I)
    print *, trim(varname), " written to file"
  end if

elseif (ndim.eq.4) then

  if (dimid(1).eq.xi_rhoID.and.dimid(2).eq.eta_rhoID) then
    ! Handle the different vertical coordinates
    if (dimid(3).eq.s_rhoID) then
      S=s_rho
      allocate(slev(S),cs(S))
      status = nf_inq_varid(ncidin,"s_rho",varidin); call check_err(status)
      status = nf_get_var_real(ncidin,varidin, slev); call check_err(status)
      status = nf_inq_varid(ncidin,"Cs_r",varidin); call check_err(status)
      status = nf_get_var_real(ncidin,varidin, cs); call check_err(status)
    elseif (dimid(3).eq.s_wID) then
      S=s_w
      allocate(slev(S),cs(S))
      status = nf_inq_varid(ncidin,"s_w",varidin); call check_err(status)
      status = nf_get_var_real(ncidin,varidin, slev); call check_err(status)
      status = nf_inq_varid(ncidin,"Cs_w",varidin); call check_err(status)
      status = nf_get_var_real(ncidin,varidin, cs); call check_err(status)
    end if

    allocate(Z(xi_rho,eta_rho,S),F3(xi_rho,eta_rho,S),F4Z(xi_rho,eta_rho,nz,outtime),F4T(X,Y,nz,outtime))
    ! calculate actual depth in grid 
    Z=depths(slev,cs)
    deallocate(slev,cs)

    status = nf_inq_varid(ncidin,trim(varname),varidin); call check_err(status)
    imax=xi_rho; jmax=eta_rho
    ! Loop throug field, interpolate from s- to z-levels
    do t = 1, outtime
      status = nf_get_vara_real(ncidin,varidin, (/ 1, 1, 1, t /), (/ xi_rho, eta_rho, S, 1 /), F3); call check_err(status)  

      do k=1,nz

        F4Z(:,:,k,t) = s2z(F3(:,:,:), -zlev(k))
        if (k.eq.1) then
          where (mask.lt.0.5)
            F4Z(:,:,k,t)=undef
          end where
        else
          where (mask.lt.0.5 .or. H(:,:).lt.zlev(k))
            F4Z(:,:,k,t)=undef
          end where
        end if
        F4T(:,:,k,t)=F4Z(xstart:xend, ystart:yend,k,t)
      end do

    end do
    print *, trim(varname), " interpolated from s-levels to z-levels"

    allocate(F4I(X,Y,nz,outtime))
    ! convert to integer
    F4I=real2int_3D(F4T,X,Y,outtime)

    status = nf_inq_varid(ncidout,trim(var_info(irec)%vname),varidout); call check_err(status)
    status = nf_put_var_int(ncidout,varidout,F4I); call check_err(status)


    deallocate(F4Z,Z,F3,F4T,F4I)

    print *, trim(varname), " written to file"

  elseif (dimid(1).eq.xi_uID.and.dimid(2).eq.eta_uID) then

    tmpvaridin=varidin
    ! Handle different type of s-coordinates
    if (dimid(3).eq.s_rhoID) then
      S=s_rho
      allocate(slev(S),cs(S))
      status = nf_inq_varid(ncidin,"s_rho",varidin); call check_err(status)
      status = nf_get_var_real(ncidin,varidin, slev); call check_err(status)
      status = nf_inq_varid(ncidin,"Cs_r",varidin); call check_err(status)
      status = nf_get_var_real(ncidin,varidin, cs); call check_err(status)
    elseif (dimid(3).eq.s_wID) then
      S=s_w
      allocate(slev(S),cs(S))
      status = nf_inq_varid(ncidin,"s_w",varidin); call check_err(status)
      status = nf_get_var_real(ncidin,varidin, slev); call check_err(status)
      status = nf_inq_varid(ncidin,"Cs_w",varidin); call check_err(status)
      status = nf_get_var_real(ncidin,varidin, cs); call check_err(status)
    end if

    !allocate(Z_tmp(xi_rho,eta_rho,S),F4(xi_rho-2,eta_rho-2,S,outtime),F4U(xi_u,eta_u,S,outtime),mask_var(xi_u,eta_u))
    allocate(Z_tmp(xi_rho,eta_rho,S),F4(xi_rho-2,eta_rho-2,S,outtime),F3U(xi_u,eta_u,S),mask_var(xi_u,eta_u))
    ! Calculate actual depth of s-levels in every grid point
    Z_tmp=depths(slev,cs)
    allocate(Z(xi_rho-2,eta_rho-2,S))
    Z=Z_tmp(2:xi_rho-1,2:eta_rho-1,:)
    deallocate(slev,cs,Z_tmp)
    status = nf_inq_varid(ncidin,"mask_u",varidin); call check_err(status)
    status = nf_get_var_real(ncidin,varidin, mask_var); call check_err(status)

    do itime=1,outtime
      !print *,"t= ",itime
      status = nf90_get_var(ncidin,tmpvaridin,F3U,start=(/1,1,1,itime/)); call check_err(status)
      !status = nf_get_var_real(ncidin,tmpvaridin, F4U); call check_err(status)
      ! Interpolate to rho-coordinates
      do k=1,S
        F4(:,:,k,itime)=u2rho_2D_notime(F3U(:,:,k))
      end do
    end do

    !deallocate(F4U,mask_var)
    deallocate(F3U,mask_var)

    allocate(F3(xi_rho-2,eta_rho-2,S), F4Z(xi_rho-2,eta_rho-2,nz,outtime),F4T(X,Y,nz,outtime))
    ! Interpolate from s-levels to z-levels
    imax=xi_rho-2; jmax=eta_rho-2
    do t = 1, outtime

      F3 = F4(:,:,:,t)

      do k=1,nz
        F4Z(:,:,k,t) = s2z(F3(:,:,:), -zlev(k))
        if (k.eq.1) then
          where (mask(2:xi_rho-1,2:eta_rho-1).lt.0.5)
            F4Z(:,:,k,t)=undef
          end where
        else
          where (mask(2:xi_rho-1,2:eta_rho-1).lt.0.5 .or. H(2:xi_rho-1,2:eta_rho-1).lt.zlev(k))
            F4Z(:,:,k,t)=undef
          end where
        end if
        F4T(:,:,k,t)=F4Z(xstart-1:xend-1, ystart-1:yend-1,k,t)
      end do

    end do
    print *, trim(varname), " interpolated from s-levels to z-levels"

    allocate(F4I(X,Y,nz,outtime))
    ! Convert to integer
    F4I=real2int_3D(F4T,X,Y,outtime)

    status = nf_inq_varid(ncidout,trim(var_info(irec)%vname),varidout); call check_err(status)
    status = nf_put_var_int(ncidout,varidout,F4I); call check_err(status)


    deallocate(Z, F3,F4,F4T,F4Z,F4I)
    print *, trim(varname), " written to file"

  elseif (dimid(1).eq.xi_vID.and.dimid(2).eq.eta_vID) then
    tmpvaridin=varidin
    ! Handle the different types of vertical coordinate
    if (dimid(3).eq.s_rhoID) then
      S=s_rho
      allocate(slev(S),cs(S))
      status = nf_inq_varid(ncidin,"s_rho",varidin); call check_err(status)
      status = nf_get_var_real(ncidin,varidin, slev); call check_err(status)
      status = nf_inq_varid(ncidin,"Cs_r",varidin); call check_err(status)
      status = nf_get_var_real(ncidin,varidin, cs); call check_err(status)
    elseif (dimid(3).eq.s_wID) then
      S=s_w
      allocate(slev(S),cs(S))
      status = nf_inq_varid(ncidin,"s_w",varidin); call check_err(status)
      status = nf_get_var_real(ncidin,varidin, slev); call check_err(status)
      status = nf_inq_varid(ncidin,"Cs_w",varidin); call check_err(status)
      status = nf_get_var_real(ncidin,varidin, cs); call check_err(status)
    end if
    allocate(Z_tmp(xi_rho,eta_rho,S),F4(xi_rho-2,eta_rho-2,S,outtime),F3V(xi_v,eta_v,S),mask_var(xi_v,eta_v))
    ! Calculate actual depth of s-levels in every grid point
    Z_tmp=depths(slev,cs)
    allocate(Z(xi_rho-2,eta_rho-2,S))
    Z=Z_tmp(2:xi_rho-1,2:eta_rho-1,:)
    deallocate(slev,cs,Z_tmp)
    status = nf_inq_varid(ncidin,"mask_v",varidin); call check_err(status)
    status = nf_get_var_real(ncidin,varidin, mask_var); call check_err(status)

    ! interpolate to rho-points
    do itime=1,outtime
      !print *,"t= ",itime
      status = nf90_get_var(ncidin,tmpvaridin,F3V,start=(/1,1,1,itime/)); call check_err(status)
      do k=1,S
        !print *,"k= ",k
        F4(:,:,k,itime)=v2rho_2D_notime(F3V(:,:,k))
      end do
    end do

    deallocate(F3V,mask_var)
    allocate(F3(xi_rho-2,eta_rho-2,S), F4Z(xi_rho-2,eta_rho-2,nz,outtime),F4T(X,Y,nz,outtime))

    imax=xi_rho-2; jmax=eta_rho-2
    ! Interpolate from s-levels to z-levels
    do t = 1, outtime

      F3 = F4(:,:,:,t)

      do k=1,nz
        F4Z(:,:,k,t) = s2z(F3(:,:,:), -zlev(k))
        if (k.eq.1) then
          where (mask(2:xi_rho-1,2:eta_rho-1).lt.0.5)
            F4Z(:,:,k,t)=undef
          end where
        else
          where (mask(2:xi_rho-1,2:eta_rho-1).lt.0.5 .or. H(2:xi_rho-1,2:eta_rho-1).lt.zlev(k))
            F4Z(:,:,k,t)=undef
          end where
        end if
        F4T(:,:,k,t)=F4Z(xstart-1:xend-1, ystart-1:yend-1,k,t)
      end do

    end do

    print *, trim(varname), " interpolated from s-levels to z-levels"

    allocate(F4I(X,Y,nz,outtime))
    ! convert to integer
    F4I=real2int_3D(F4T,X,Y,outtime)

    status = nf_inq_varid(ncidout,trim(var_info(irec)%vname),varidout); call check_err(status)
    status = nf_put_var_int(ncidout,varidout,F4I); call check_err(status)

    deallocate(F4Z,Z, F3,F4, F4T,F4I )
    print *, trim(varname), " written to file"


  end if
end if

deallocate(mask)

contains
function u2rho_2D_notime(F) result(G)
! interpolates from u-grid to rho-grid
real, dimension(xi_u,eta_u) :: F
real, dimension(xi_rho-2,eta_rho-2) :: G

status = nf_inq_varid(ncidin,"mask_u",varidin); call check_err(status)
status = nf_get_var_real(ncidin,varidin, mask_var); call check_err(status)
do i=1,xi_rho-2
  do j=1,eta_rho-2
    if (mask_var(i+1,j+1).gt.0.5.and.mask_var(i+2,j+1).gt.0.5) then
      w1=0.5; w2=0.5
    elseif (mask_var(i+1,j+1).gt.0.5.and.mask_var(i+2,j+1).lt.0.5) then
      w1=1.0; w2=0.0;
    elseif (mask_var(i+1,j+1).lt.0.5.and.mask_var(i+2,j+1).gt.0.5) then
      w1=0.0; w2=1.0;
    else
      w1=0.0; w2=0.0;
    end if
    G(i,j) = w1 * F(i+1,j+1) + w2 * F(i+2,j+1)
  end do
end do


where(mask(2:xi_rho-1,2:eta_rho-1) < 0.5)
  G(:,:)=undef
end where
end function u2rho_2D_notime

function u2rho_2D(F) result(G)
! interpolates from u-grid to rho-grid
real, dimension(xi_u,eta_u,outtime) :: F
real, dimension(xi_rho-2,eta_rho-2,outtime) :: G

status = nf_inq_varid(ncidin,"mask_u",varidin); call check_err(status)
status = nf_get_var_real(ncidin,varidin, mask_var); call check_err(status)
do i=1,xi_rho-2
  do j=1,eta_rho-2
    if (mask_var(i+1,j+1).gt.0.5.and.mask_var(i+2,j+1).gt.0.5) then
      w1=0.5; w2=0.5
    elseif (mask_var(i+1,j+1).gt.0.5.and.mask_var(i+2,j+1).lt.0.5) then
      w1=1.0; w2=0.0;
    elseif (mask_var(i+1,j+1).lt.0.5.and.mask_var(i+2,j+1).gt.0.5) then
      w1=0.0; w2=1.0;
    else
      w1=0.0; w2=0.0;
    end if
    G(i,j,:) = w1 * F(i+1,j+1,:) + w2 * F(i+2,j+1,:)
  end do
end do


do n=1, outtime
  where(mask(2:xi_rho-1,2:eta_rho-1) < 0.5)
    G(:,:,n)=undef
  end where
end do
end function u2rho_2D

function v2rho_2D_notime(F) result(G)
! Interpolates from v-points to rho-points
real, dimension(xi_v,eta_v) :: F
real, dimension(xi_rho-2,eta_rho-2) :: G

!print *,eta_v,xi_v

status = nf_inq_varid(ncidin,"mask_v",varidin); call check_err(status)
status = nf_get_var_real(ncidin,varidin, mask_var); call check_err(status)
do i=1,xi_rho-2
  do j=1,eta_rho-2
    !print *,i,j
    if (mask_var(i+1,j+1).gt.0.5.and.mask_var(i+1,j+2).gt.0.5) then
      w1=0.5; w2=0.5
    elseif (mask_var(i+1,j+1).gt.0.5.and.mask_var(i+1,j+2).lt.0.5) then
      w1=1.0; w2=0.0;
    elseif (mask_var(i+1,j+1).lt.0.5.and.mask_var(i+1,j+2).gt.0.5) then
      w1=0.0; w2=1.0;
    else
      w1=0.0; w2=0.0;
    end if
    G(i,j) = w1 * F(i+1,j+1) + w2 * F(i+1,j+2)
  end do
end do

where(mask(2:xi_rho-1,2:eta_rho-1) < 0.5)
  G(:,:)=undef
end where
end function v2rho_2D_notime

function v2rho_2D(F) result(G)
! Interpolates from v-points to rho-points
real, dimension(xi_v,eta_v,outtime) :: F
real, dimension(xi_rho-2,eta_rho-2,outtime) :: G

status = nf_inq_varid(ncidin,"mask_v",varidin); call check_err(status)
status = nf_get_var_real(ncidin,varidin, mask_var); call check_err(status)
do i=1,xi_rho-2
  do j=1,eta_rho-2
    if (mask_var(i+1,j+1).gt.0.5.and.mask_var(i+1,j+2).gt.0.5) then
      w1=0.5; w2=0.5
    elseif (mask_var(i+1,j+1).gt.0.5.and.mask_var(i+1,j+2).lt.0.5) then
      w1=1.0; w2=0.0;
    elseif (mask_var(i+1,j+1).lt.0.5.and.mask_var(i+1,j+2).gt.0.5) then
      w1=0.0; w2=1.0;
    else
      w1=0.0; w2=0.0;
    end if
    G(i,j,:) = w1 * F(i+1,j+1,:) + w2 * F(i+1,j+2,:)
  end do
end do

do n=1, outtime
  where(mask(2:xi_rho-1,2:eta_rho-1) < 0.5)
    G(:,:,n)=undef
  end where
end do
end function v2rho_2D

function real2int_2D(F,ni,nj,tmax) result(G)
! convert from real to integer, as specified
! by var_info
integer, intent(in) :: ni,nj,tmax
real, dimension(ni,nj,tmax), intent(in) :: F
integer, dimension(ni,nj,tmax) :: G

where (F.ne.undef)
  G= floor((max(min(F,var_info(irec)%max_val),var_info(irec)%min_val)-var_info(irec)%add_offset) / var_info(irec)%scale_factor)
elsewhere
  G=var_info(irec)%fillvalue
end where

end function real2int_2D

function real2int_3D(F,ni,nj,tmax) result(G)
integer, intent(in) :: ni,nj,tmax
real, dimension(ni,nj,nz,tmax), intent(in) :: F
integer, dimension(ni,nj,nz,tmax) :: G

where (F.ne.undef)
  G= floor((max(min(F,var_info(irec)%max_val),var_info(irec)%min_val)-var_info(irec)%add_offset) / var_info(irec)%scale_factor)
elsewhere
  G=var_info(irec)%fillvalue
end where

end function real2int_3D

function depths(slev,cs) result(Z)
! calculate depth of s-levels in every grid point
real, dimension(S), intent(in) :: slev, cs
real, dimension(xi_rho,eta_rho,S) :: Z

status = nf_inq_varid(ncidin,"Vtransform",varidin); call check_err(status)
status = nf_get_var_real(ncidin,varidin, Vtrans); call check_err(status)
status = nf_inq_varid(ncidin,"hc",varidin); call check_err(status)
status = nf_get_var_real(ncidin,varidin, hc); call check_err(status)
status = nf_inq_varid(ncidin,"h",varidin); call check_err(status)
status = nf_get_var_real(ncidin,varidin, H); call check_err(status) 

status = nf_inq_varid(ncidout,"depth",varidout); call check_err(status)
status = nf_get_var_real(ncidout,varidout, zlev); call check_err(status)
print *,"Vtrans: ", Vtrans
if (int(Vtrans) == 1) then
  do k =1, S
    Z(:,:,k) = hc * slev(k) + (H-hc)*cs(k)
  end do
else if (int(Vtrans) == 2) then
  do k = 1, S
    Z(:,:,k)   = H * ((hc * slev(k) + H * cs(k)) / (H+hc) )
  end do
else
  print *, "Unknown Vtransform... PANIC!!!"
  stop
end if
end function depths

function s2z(F, z0) result(G)
! Estimates 3D field at level G by linear interpolation
real, dimension(imax,jmax,S), intent(in) :: F
real, intent(in) :: z0
real, dimension(imax, jmax) :: G
integer :: ks
real :: a 
!!$
do j = 1, jmax
  do i = 1, imax
    if (z0 < Z(i,j,1)) then
      G(i,j) = F(i,j,1)
    else if (z0 >= Z(i,j,S)) then
      G(i,j) = F(i,j,S)
    else ! Z(1) <= z0 < Z(N)
      do ks = 2, S
        if (z0 < Z(i,j,ks)) then ! Z(ks-1) <= z0 < Z(ks)
          a = (Z(i,j,ks)-z0) / (Z(i,j,ks) - Z(i,j,ks-1))
          G(i,j) = a*F(i,j,ks-1) + (1-a)*F(i,j,ks)
          exit
        endif ! (z0 < Z(i,j,ks)
      enddo ! ks
    endif ! (z0 < Z(i,j,1)
  enddo ! i
enddo ! j

end function s2z

end subroutine addvar
