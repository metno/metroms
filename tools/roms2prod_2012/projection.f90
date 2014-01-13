module projection

! ----------------------------------------------
! Program to read projection input file,       !
! store information to be used as attributes   !
! of projection variable on output netcdf file !
! ----------------------------------------------

  implicit none
  character(len=80) :: proj_name
  integer :: ntext, nreal
  character(len=100), dimension(:),allocatable :: txt_attname, real_attname
  character(len=100), dimension(:),allocatable :: txt_attvalue
  real, dimension(:), allocatable :: real_attvalue
  real :: sw_lon, sw_lat, dlon, dlat
  contains

  subroutine readproj(proj_file)
   character(len=*), intent(in) :: proj_file
    integer, parameter :: uproj = 20   ! File unit for the setup file
!!!    character(len=80) :: line         ! A line from the setup file
    character(len=132) :: line         ! A line from the setup file
    integer :: nline
    ! --------------------------
    ! Open the setup file
    ! --------------------------
    print *, "Projection as defined in ", trim(proj_file)
    open(unit=uproj, file=trim(proj_file), status='old', action='read')

    call readline(proj_name)

    call readline(line)
    ! Number of text attributes
    read(line,*) ntext

    allocate(txt_attname(ntext), txt_attvalue(ntext))
    ! Get name and value of text attributes
    do nline = 1, ntext
       call readline(txt_attname(nline))
       call readline(txt_attvalue(nline))
    end do

    call readline(line)
    ! Number of real attributes
    read(line,*) nreal

    allocate(real_attname(nreal), real_attvalue(nreal))
    ! Get name and value of real attributes
    do nline = 1, nreal
       call readline(real_attname(nline))
       call readline(line)
       read(line,*) real_attvalue(nline)
    end do

    ! Get x-position of south-west corner
    call readline(line)
    read(line,*) sw_lon
    ! Get grid resolution in x-direction
    call readline(line)
    read(line,*) dlon

    ! Get y-position of sout-west corner
    call readline(line)
    read(line,*) sw_lat
    ! Get grid resolution in y-direction
    call readline(line)
    read(line,*) dlat

    close(uproj) 
  
    
    contains

    ! -----------------------------
    subroutine readline(line)
    ! --------------------------------------
    !  Reads a line from a file, skipping
    !  comments and blank lines.
    !
    !  Comments starts with a character from
    !  COMCHAR and continues to the end of line.
    ! --------------------------------------
      ! -----------------
      ! Arguments 
      ! -----------------
      character(len=*), intent(out) :: line ! Line in text file

      ! --------------------------------
      ! Local constants and variables
      ! -------------------------------
      character(len=*), parameter :: COMCHAR = "*!#"
                         ! Comment starting characters
      integer :: ipos    ! Start position for comment

      ! ------------------------------
      ! Line scanning loop
      ! ------------------------------
      do
        
        ! Read a line
        read(unit=uproj, fmt="(A)") line
        ! Remove any comments
        ipos = scan(line, COMCHAR)
        if (ipos /= 0) then  
          line = line(:ipos-1)
        end if
        ! Exit loop if decommented line is not blank
        if (len_trim(line) /= 0) then  
          exit
        end if
      end do
    end subroutine readline

  end subroutine readproj 

end module projection
