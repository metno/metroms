subroutine vert_int(array_in,Mp,Lp,Ni,No,z_ri,array_out,z_ro,undef,maski,masko)
!Denne subroutine er uavhengig av Vtransform og Vstretching, kun z_ri og z_ro
!10.05.2011: Removed fill, this is now handled by FIMEX
  implicit none
  integer, intent(in) :: Mp,Lp,No,Ni,maski(Lp,Mp),masko(Lp,Mp)
  real   , intent(in) :: undef,z_ri(Lp,Mp,Ni),z_ro(Lp,Mp,No)
  real   , intent(out) :: array_out(Lp,Mp,No)
  integer	:: i,j,k,kT,nvalue
  real		:: rz1,rz2,scr(Lp,Mp),array_in_temp(Lp,Mp,Ni),array_in(Lp,Mp,Ni)
  REAL          :: work(Lp,Mp),z_ri_swap(Lp,Mp,Ni),z_rii(Lp,Mp,Ni)
  REAL          :: error(Lp,Mp)


  ! Test which direction the values for depth are
  if (z_ri(1,1,2).lt.z_ri(1,1,1)) then
     write (*,*) 'swapper dyp...'
     do k=1,Ni
        array_in_temp(:,:,k)=array_in(:,:,(Ni-k+1))
        z_ri_swap(:,:,k)=z_ri(:,:,(Ni-k+1))
     end do
     array_in=array_in_temp
     z_rii=z_ri_swap
  else
     write (*,*) 'no swap'
     z_rii=z_ri
  end if

  ! Vertical interpolation
  array_out = 0.0 !undef
  !where(z_ro==undef) array_out=0.0
  do j=1,Mp
     do i=1,Lp
	do k=1,No
    if(z_ro(i,j,k).le.z_rii(i,j,1).and.z_ro(i,j,k).ne.undef) then
       array_out(i,j,k) = array_in(i,j,1)
    elseif(z_ro(i,j,k).ge.z_rii(i,j,Ni).and.z_ro(i,j,k).ne.undef) then
       array_out(i,j,k) = array_in(i,j,Ni)
    else
       do kT=1,Ni
          if(z_ro(i,j,k).lt.z_rii(i,j,kT+1).and.z_ro(i,j,k).ge.z_rii(i,j,kT)) then
             rz2 = (z_ro(i,j,k)-z_rii(i,j,kT))/(z_rii(i,j,kT+1)-z_rii(i,j,kT))
             rz1 = 1.0-rz2 
             array_out(i,j,k) = (rz1*array_in(i,j,kT)) + (rz2*array_in(i,j,kT+1))
             exit
          end if
       end do
    end if
 end do
end do
end do

  
write (*,*) 'end vert_int'
end subroutine vert_int
