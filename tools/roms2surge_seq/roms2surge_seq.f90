program roms2surge_seq
  ! nilsmk@met.no 22.11.11
  use netcdf
  use ncutil       ! Utilities for NetCDF input 
  use time_module  ! Handle time
  implicit none
  integer, parameter :: sunit = 51  ! Unit for seq. output file
  integer(kind=2), dimension(20) :: identi ! The identi to be used
  integer :: ntimes,Lp,Mp
  character(len=99) :: file3D,file2D,seqfile
  real, dimension(:,:), allocatable :: zeta2D,zeta3D,surge,ifelt
  integer, dimension(6) :: seq_time_origin
  character(len=40) :: tstring
  type(time_type)  :: roms_time
  real :: scale         ! Scale factor for sequential storage

  call getarg(1,file3D)
  call getarg(2,file2D)
  call getarg(3,seqfile)
  call getarg(4,(identi(12))) !yy
  call getarg(5,identi(13))  !moda
  call getarg(6,identi(14))  !hh
  call getarg(7,ntimes)
  seqfile=trim(seqfile)
  file3D=trim(file3D)
  file2D=trim(file2D)
  write(*,*) "3D-file:",file3D
  write(*,*) "2D-file:",file2D
  write(*,*) "seqfile:",seqfile
  
  call get_dims(file3D,Lp,Mp)
  write(*,*) 'Lp, Mp = ',Lp,Mp
  allocate(zeta2D(Lp,Mp))
  allocate(zeta3D(Lp,Mp))
  allocate(surge(Lp,Mp))
  allocate(ifelt(Lp,Mp))
  

  open(unit=sunit, file=seqfile, action='write',         & 
       access='sequential', form='unformatted')
  identi(1)  = 88
  identi(2)  = 948
  identi(3)  = 4
  identi(4)  = 0
  identi(5)  = 8
  identi(6)  = 301
  identi(7)  = 0
  identi(8)  = 0
  identi(9)  = Mp
  identi(10) = Lp
  identi(15) = 997
  identi(16) = 637
  identi(17) = -40
  identi(18) = 58
  identi(19) = 0
  identi(20) = -3  

  seq_time_origin(1) = identi(12)
  seq_time_origin(2) = identi(13) / 100
  seq_time_origin(3) = mod(identi(13), 100_2)
  seq_time_origin(4) = identi(14) / 100
  seq_time_origin(5:6) = 0
  print *, "seq_time_origin  = ", seq_time_origin
  do tstep = 1, ntimes
     ! -------------------------
     ! Set time 
     ! -------------------------
     call get_time(tstep, roms_time)
     call time2str(tstring, roms_time)
     print *, tstep, tstring
     !
     identi(4) = nint(24*(roms_time - seq_time_origin))
     print *, "identi(4) = ", identi(4)
     if (identi(4) > 0) then
        identi(3) = 2
     else
        identi(3) = 1
     end if
     scale = 10**(-identi(20))
     call get_zeta(file3D, Lp, Mp, tstep, zeta3D)
     call get_zeta(file2D, Lp, Mp, tstep, zeta2D)
     
     surge = zeta3D - zeta2D
     ifelt = nint(scale*surge, kind=2)
     write(sunit) identi
     write(sunit) ifelt
  end do ! tstep
  
  close(sunit)

contains
  subroutine get_zeta(filename, Lp, Mp, tstep, zeta)
    !!
    integer, intent(in) :: Lp,Mp,tstep
    real, intent(out) :: zeta(Lp, Mp)
    integer :: ncgrid
    status = nf90_open(trim(filename),nf90_nowrite,ncgrid)
    if(status /= nf90_NoErr) call handle_err(status)
    status = nf90_inq_varid(ncgrid,'zeta',id_zeta)
    if(status /= nf90_NoErr) call handle_err(status)
    status = nf90_get_var(ncgrid,ZetaVarIdi,zeta,start=(/ 1, 1, tstep/))
    if(status /= nf90_NoErr) call handle_err(statusi)
!  end subroutine get_zeta 
  !
  subroutine get_dims(filename, Lp, Mp)
    !!
    character(len=99), intent(in)  :: filename
    integer, intent(out)  :: Lp, Mp
    filenam=trim(filename)
    status = nf90_open(trim(filenam),nf90_nowrite,ncgrid)
    if(status /= nf90_NoErr) call handle_err(status)
    status = nf90_Inquire_Dimension(ncgrid,dim_xi_rho,xi_dimname,Lp)
    if(status /= nf90_NoErr) call handle_err(status)
    status = nf90_Inquire_Dimension(ncgrid,dim_eta_rho,eta_dimname,Mp)
    if(status /= nf90_NoErr) call handle_err(status)
  end subroutine get_dims
  !
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
  !
end program roms2surge_seq
