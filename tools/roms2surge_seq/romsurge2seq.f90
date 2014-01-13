program romsurge2seq

! --------------------------------------------------
! Program for converting ROMS output NetCDF files
! to met.no's sequential file format
!
! Bjørn Ådlandsvik, <bjorn@imr.no>
! Institute of Marine Research
! 04 March 2005
!
! Modified to also be able to have output in subarea
! by Jon Albretsen, met.no
!
! Also include the most common ice variables (see switch in sup-file)
! by Jon Albretsen, met.no
!
! Modified to be able to output specified time period
! by Nils Melsom Kristensen, met.no
!
! New version by Nils MK 22.11.2011:
! Only handle zeta (strom surge)
! -------------------------------------------------

use setup        ! Handle setup file
use ncutil       ! Utilities for NetCDF input 
use time_module  ! Handle time

implicit none

integer, parameter :: sunit = 51  ! Unit for seq. output file

integer :: nc          ! id for the ROMS file

integer :: L, M, N     ! dimensins in ROMS file

character(len=80) :: supfile

real, dimension(:,:),   allocatable :: H, mask_rho  ! Depth, land-mask
real :: hc  
real, dimension(:),     allocatable :: sc_r, cs_r
real, dimension(:,:,:), allocatable :: Z     ! Depth of s-level


real, dimension(:,:),   allocatable :: F2    ! 2D field from ROMS
real, dimension(:,:),   allocatable :: F2U   ! 2D field in U-points
real, dimension(:,:),   allocatable :: F2V   ! 2D field in V-points
real, dimension(:,:,:), allocatable :: F3    ! 3D field from ROMS
real, dimension(:,:,:), allocatable :: F3U   ! 3D field in U-points
real, dimension(:,:,:), allocatable :: F3V   ! 3D field in V-points

integer :: imin, jmin, imax, jmax   ! Dimensions for sequential file

integer(kind=2), dimension(:,:), allocatable :: ifelt
                             ! Scaled data for sequential file

integer :: status     ! Error status from NetCDF library calls

real :: scale         ! Scale factor for sequential storage

integer :: i, j, k    ! Space indices
integer :: ntimes     ! Number of time steps
integer :: tstep      ! Time index
character(len=7) :: roms_time_units
type(time_type)  :: roms_time_origin, roms_time
type(time_type)  :: seq_time_origin
integer, dimension(6) :: tvec

character(len=40) :: tstring

! ---------------------------------------------------------

! ------------------------------
! Read the setup file
! ------------------------------
call getarg(1,supfile)
supfile=trim(supfile)
print *, "supfile = ", supfile
call readsup(supfile)

print *, identi

! ------------------------------------
! Open ROMS file and allocate arrays
! ------------------------------------

print *, "romsfile = ", romsfile
status = nf_open(trim(romsfile), NF_NOWRITE, nc)
call check_err(status)

L = dimlen(nc, 'xi_rho')-1
M = dimlen(nc, 'eta_rho')-1

allocate(F2(0:L,0:M), F2U(1:L,0:M), F2V(0:L,1:M), F3(0:L,0:M,N), F3U(1:L,0:M,N), F3V(0:L,1:M,N))

! ------------------------------------
! ROMS s-coordinate 
! ------------------------------------

allocate(mask_rho(0:L,0:M))

call getvar(nc, "mask_rho", mask_rho)

! -------------------------------------------------------
! Output dimensions, array allocation and file opening
! -------------------------------------------------------
imin = 1
jmin = 1
imax = L-1
jmax = M-1

! Quality control
if ((imax /= identi(10)) .or. (jmax /= identi(11))) then
  write(*,*) "ROMS file size do not match identi from setup file"
  stop
end if

allocate(ifelt(imax,jmax))

open(unit=sunit, file=seqfile, action='write',         & 
     access='sequential', form='unformatted')

! -------------------------------------------
! Time handling
! -------------------------------------------

! Get ntimes, time_units, and time_origin from ROMS file
if ((t0 == 0 ) .and. (t1 == 0)) then
   ntimes = dimlen(nc, 'ocean_time')
else
   ntimes = t1 - t0 + 1
end if
call getatt(nc, "ocean_time", "units", tstring)
if (tstring(1:1) == "d") then
  roms_time_units = "days"
  tstring = tstring(12:)
else if (tstring(1:1) == "h") then
  roms_time_units = "hours"
  tstring = tstring(13:)
else if (tstring(1:1) == "s") then
  roms_time_units = "seconds"
  tstring = tstring(15:)
else
  print *, "***Illegal units attribute: ", tstring
  stop
end if
roms_time_origin = tstring(1:19)
print *, tstring(1:19)
call time2str(tstring(1:19), roms_time_origin)
print *, tstring(1:19)
print *, "roms_time_origin = ", roms_time_origin

tvec(1) = identi(12)
tvec(2) = identi(13) / 100
tvec(3) = mod(identi(13), 100_2)
tvec(4) = identi(14) / 100
tvec(5:6) = 0
seq_time_origin = tvec
print *, "seq_time_origin  = ", seq_time_origin

if (identi(9) /= 1) STOP "Error, program not coded for other grids than polar stereographic!"

if ( (i1 == 0) .AND. (i2 == 0) .AND. (j1 == 0) .AND. (j2 == 0) ) then
  i1 = imin
  i2 = imax
  j1 = jmin
  j2 = jmax
endif

!// Change identi(10:11) and identi(15:18) if output on subarea
if ( (i1 > 0) .AND. (i2 > 0) .AND. (j1 > 0) .AND. (j2 > 0) ) then
  if ( (i1 < i2) .AND. (i2 < imax) .AND. (j1 < j2) .AND. (j2 < jmax) ) then
    print *, "Output on sigma-levels on subarea:", i1, i2, j1, j2
    identi(10) = i2 - i1 + 1
    identi(11) = j2 - j1 + 1
    identi(15) = identi(15) - i1 + 1
    identi(16) = identi(16) - j1 + 1
    print *, "New identities on output file:"
    print *, "identi(10:11)=", identi(10:11)
    print *, "identi(15:18)=", identi(15:18)
  endif
endif



! ------------------------------------------
! Time loop
! ------------------------------------------

do tstep = 1, ntimes

  ! -------------------------
  ! Set time 
  ! -------------------------
  call get_time((t0+tstep), roms_time)
  !!!print *, tstep, timestr(roms_time)
  call time2str(tstring, roms_time)
  print *, tstep, tstring

  !identi(4) = nint(24*(roms_time - seq_time_origin))
  identi(4) = ceiling(24*(roms_time - seq_time_origin))
  print *, "identi(4) = ", identi(4)
  if (identi(4) > 0) then
    identi(3) = 2
  else
    identi(3) = 1
  end if

  ! -------------------------
  ! Sea surface elevation
  ! -------------------------
  call getvar2dtime(nc, 'zeta_detided', (t0+tstep), F2)

  identi(5)  = 8
  identi(6)  = 301
  identi(7)  = 0
  identi(20) = -3  
  scale = 10**(-identi(20))

  ifelt = nint(scale*F2(imin:imax,jmin:jmax), kind=2)
  ! Handle land points
  !print *, "mask_rho = ", mask_rho(200,400)
  where(mask_rho(imin:imax,jmin:jmax) < 0.5)
    ifelt = -32767
  end where

  write(sunit) identi
  write(sunit) ifelt(i1:i2,j1:j2)

  ! -------------------------
  ! ubar
  ! -------------------------
    call getvar2dtime(nc, 'ubar', (t0+tstep), F2U)
    F2(imin:imax,:) = 0.5*(F2U(imin:imax,:)+F2U(imin+1:imax+1,:))

    identi(5)  = 8
    identi(6)  = 302
    identi(7)  = 0
    identi(19) = 0
    identi(20) = -4
    scale = 10**(-identi(20))

    ifelt = nint(scale*F2(imin:imax,jmin:jmax), kind=2)
    where(mask_rho(imin:imax,jmin:jmax) < 0.5)
      ifelt = -32767
    end where

    write(sunit) identi
    write(sunit) ifelt(i1:i2,j1:j2)

  ! -------------------------
  ! vabr
  ! -------------------------
    call getvar2dtime(nc, 'vbar', (t0+tstep), F2V)
    F2(:,jmin:jmax) = 0.5*(F2V(:,jmin:jmax)+F2V(:,jmin+1:jmax+1))

    identi(5)  = 8
    identi(6)  = 303
    identi(7)  = 0
    identi(19) = 0
    identi(20) = -4
    scale = 10**(-identi(20))

    ifelt = nint(scale*F2(imin:imax,jmin:jmax), kind=2)
    where(mask_rho(imin:imax,jmin:jmax) < 0.5)
      ifelt = -32767
    end where

    write(sunit) identi
    write(sunit) ifelt(i1:i2,j1:j2)


end do
! ----------------------
! Final clean-up
! ----------------------
close(sunit)
status = nf_close(nc)
call check_err(status)

contains

    
  subroutine get_time(tstep, ocean_time)
    ! Return the time stamp of timestep=tstep in 
    ! a open ROMS file

    integer, intent(in) :: tstep
    type(time_type), intent(out) :: ocean_time
     
    real(kind=8) :: otime

    call getvartime(nc, "ocean_time", tstep, otime)
    print *, "otime = ", otime

    if (roms_time_units == "seconds") then
      otime = otime / (24*3600)
    else if (roms_time_units == "hours") then
      otime = otime / 24
    end if
    print *, "otime = ", otime
    ocean_time = roms_time_origin + otime

  end subroutine get_time

end program romsurge2seq
