module global_atts

! ---------------------------------------------------
! Global attribute module for roms2prod             !
! Reads global attributes from inputfile specified  !
! in roms2prod.sup                                  !
! ---------------------------------------------------

  implicit none
  integer :: gtext, greal
  character(len=100), dimension(:),allocatable :: txt_g_attname, real_g_attname
  character(len=100), dimension(:),allocatable :: txt_g_attvalue
  real, dimension(:), allocatable :: real_g_attvalue
  contains

  subroutine readglobal_atts(global_att_file)
   character(len=*), intent(in) :: global_att_file
    integer, parameter :: uatt = 20   ! File unit for the setup file
!!!    character(len=80) :: line         ! A line from the setup file
    character(len=132) :: line         ! A line from the setup file
    integer :: nline
    ! --------------------------
    ! Open the setup file
    ! --------------------------
    print *, "Global attributes from ", trim(global_att_file)
    open(unit=uatt, file=trim(global_att_file), status='old', action='read')


    call readline(line)
    ! Number of global text attributes
    read(line,*) gtext

    allocate(txt_g_attname(gtext), txt_g_attvalue(gtext))
    ! Read name and value of text attributes
    do nline = 1, gtext
       call readline(txt_g_attname(nline))
       call readline(txt_g_attvalue(nline))
    end do

    call readline(line)
    ! Number of global real attributes
    read(line,*) greal

    allocate(real_g_attname(greal), real_g_attvalue(greal))
    ! Read name and value of real attributes
    do nline = 1, greal
       call readline(real_g_attname(nline))
       call readline(line)
       read(line,*) real_g_attvalue(nline)
    end do

    close(uatt) 
  
    
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
        read(unit=uatt, fmt="(A)") line
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

  end subroutine readglobal_atts

end module global_atts
