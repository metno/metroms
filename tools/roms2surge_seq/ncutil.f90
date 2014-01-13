module ncutil

!  private
 
  include "netcdf.inc"

  ! Generic interface for reading a variable
  interface getvar
    module procedure getvar0d, getvar1d, getvar2d, getvar3d
  end interface getvar

  ! Generic interface for reading a variable at a specific time step
  interface getvartime
    module procedure getvar0dtime, getdouble0dtime, getvar2dtime, getvar3dtime
  end interface getvartime

!  public check_err, dimlen, getvar, getvartime
!  public nf_open, nf_close
!  public NF_NOWRITE

  contains  

  !
  ! -----------------------------------------------
  !
  subroutine check_err(status)
  ! NetCDF file operation error handling
    integer, intent(in) :: status
    if (status .ne. NF_NOERR) then
      print *, "***NetCDF error, program terminating"
      print *, "NetCDF error message:"
      print *, "  ", nf_strerror(status)
      stop
    endif
  end subroutine check_err
  !
  ! ------------------------------------
  !
  function dimlen(ncid, dimname)
    ! Returns the length of a NetCDF dimension
    integer :: dimlen
    integer, intent(in) :: ncid
    character(len=*), intent(in) :: dimname
    integer :: dimid
    integer :: status

    status = nf_inq_dimid(ncid, dimname, dimid)
    if (status == NF_EBADDIM) then
      write(*,*) "***Wrong dimension name: ", dimname
    end if
    call check_err(status)
    status = nf_inq_dimlen(ncid, dimid, dimlen)
    call check_err(status)
  end function dimlen
  ! 
  ! --------------------------------------------
  ! 
  subroutine getvar0d(ncid, vname, value)
  ! Read a scalar value from the NetCDF file
    integer, intent(in) :: ncid
    character(len=*), intent(in) :: vname
    real, intent(out) :: value
    integer :: varid
    integer :: status

    status = nf_inq_varid(ncid, vname, varid)
    if (status == NF_ENOTVAR) then
      write(*,*) "***Wrong variable name: ", vname
    end if
    call check_err(status)
    status = nf_get_var_real(ncid, varid, value)
    call check_err(status)
  end subroutine getvar0d

  subroutine getvar1d(ncid, vname, field)
  ! Read a 1D field from the NetCDF file
    integer, intent(in) :: ncid
    character(len=*), intent(in) :: vname
    real, dimension(:), intent(out) :: field
    integer :: varid
    integer :: status

    status = nf_inq_varid(ncid, vname, varid)
    if (status == NF_ENOTVAR) then
      write(*,*) "***Wrong variable name: ", vname
    end if
    call check_err(status)
    status = nf_get_var_real(ncid, varid, field)
    call check_err(status)
  end subroutine getvar1d
  !
  ! -------------------------------------------------------
  !
  subroutine getvar2d(ncid, vname, field)
    ! Read a 2D field from the NetCDF file
    integer, intent(in) :: ncid
    character(len=*), intent(in) :: vname
    real, dimension(:,:), intent(out) :: field
    integer :: varid
    integer :: status

    status = nf_inq_varid(ncid, vname, varid)
    if (status == NF_ENOTVAR) then
      write(*,*) "***Wrong variable name: ", vname
    end if
    call check_err(status)
    status = nf_get_var_real(ncid, varid, field)
    call check_err(status)
  end subroutine getvar2d
  !
  ! -------------------------------------------------------
  !
  subroutine getvar3d(ncid, vname, field)
    ! Read a 3D field from the NetCDF file
    integer, intent(in) :: ncid
    character(len=*), intent(in) :: vname
    real, dimension(:,:,:), intent(out) :: field
    integer :: varid
    integer :: status

    status = nf_inq_varid(ncid, vname, varid)
    if (status == NF_ENOTVAR) then
      write(*,*) "***Wrong variable name: ", vname
    end if
    call check_err(status)
    status = nf_get_var_real(ncid, varid, field)
    call check_err(status)
  end subroutine getvar3d
  
!
! --------------------------------------------
!
  subroutine getvar0dtime(ncid, vname, timestep, field)
    integer, intent(in) :: ncid
    character(len=*), intent(in) :: vname
    integer, intent(in) :: timestep
    real, intent(out) :: field
    integer :: varid
    integer, dimension(1) :: start, count
    integer :: status

    start = (/ timestep /)
    count = (/ 1 /)

    status = nf_inq_varid(ncid, vname, varid)
    if (status == NF_ENOTVAR) then
      write(*,*) "***Wrong variable name: ", vname
    end if
    call check_err(status)
    status = nf_get_vara_real(ncid, varid, start, count, field)
    call check_err(status)
  end subroutine getvar0dtime

  subroutine getdouble0dtime(ncid, vname, timestep, field)
    integer, intent(in) :: ncid
    character(len=*), intent(in) :: vname
    integer, intent(in) :: timestep
    real(kind=8), intent(out) :: field
    integer :: varid
    integer, dimension(1) :: start, count
    integer :: status

    start = (/ timestep /)
    count = (/ 1 /)

    status = nf_inq_varid(ncid, vname, varid)
    if (status == NF_ENOTVAR) then
      write(*,*) "***Wrong variable name: ", vname
    end if
    call check_err(status)
    status = nf_get_vara_double(ncid, varid, start, count, field)
    call check_err(status)
  end subroutine getdouble0dtime

  subroutine getvar2dtime(ncid, vname, timestep, field)
    integer, intent(in) :: ncid
    character(len=*), intent(in) :: vname
    integer, intent(in) :: timestep
    real, dimension(:,:), intent(out) :: field
    integer :: varid
    integer, dimension(3) :: start, count
    integer :: status
   
    start = (/ 1, 1, timestep /)
    count(1:2) = shape(field)
    count(3) = 1

    status = nf_inq_varid(ncid, vname, varid)
    if (status == NF_ENOTVAR) then
      write(*,*) "***Wrong variable name: ", vname
    end if
    call check_err(status)
    status = nf_get_vara_real(ncid, varid, start, count, field)
    call check_err(status)
  end subroutine getvar2dtime


  subroutine getvar3dtime(ncid, vname, timestep, field)
    integer, intent(in) :: ncid
    character(len=*), intent(in) :: vname
    integer, intent(in) :: timestep
    real, dimension(:,:,:), intent(out) :: field
    integer :: varid
    integer, dimension(4) :: start, count
    integer :: status
   
    start = (/ 1, 1, 1, timestep /)
    count(1:3) = shape(field)
    count(4) = 1

    status = nf_inq_varid(ncid, vname, varid)
    if (status == NF_ENOTVAR) then
      write(*,*) "***Wrong variable name: ", vname
    end if
    call check_err(status)
    status = nf_get_vara_real(ncid, varid, start, count, field)
    call check_err(status)
  end subroutine getvar3dtime
  
  !
  ! --------------------------------------------------------
  !  
  subroutine getatt(ncid, varname, attname, attstr)
  ! Get a string attribute
    integer, intent(in) :: ncid
    character(len=*), intent(in) :: varname
    character(len=*), intent(in) :: attname
    character(len=*), intent(out) :: attstr

    integer :: varid 
    integer :: status

    status = nf_inq_varid(ncid, varname, varid)
    if (status == NF_ENOTVAR) then
      write(*,*) "***Wrong variable name: ", varname
    end if
    call check_err(status)
    
    status = nf_get_att_text(ncid, varid, attname, attstr)
    if (status == NF_ENOTATT) then
      write(*,*) "***Variable: ", varname, " has no attribute ", attname
    end if
    if (status == NF_EBADTYPE) then
      write(*,*) "***Attribute", attname, " to variable ", varname,  &
                     "is not text"
    end if
    call check_err(status)

  end subroutine getatt
    


end module ncutil



