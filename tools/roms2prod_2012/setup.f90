module setup

! ----------------------------------------------
! Setup module for roms2prod
! ---------------------------------------------

  implicit none

  character(len=80) :: infilename  ! Name of the ROMS file
  character(len=80) :: outfilename   ! Name of the sequential file

  integer ::nlev  ! Number of vertical z-levels in the ouput
  real, dimension(:), allocatable :: zlev  ! The vertical z-level values

  integer :: nvar ! Number of variables to include in output
  character (len=40), dimension(:), allocatable :: vars
  character (len=80) :: proj_file, global_att_file
  integer :: nline
  integer :: xstart, xend,ystart,yend, subarea
  contains

    subroutine readsup(supfile)

      character(len=*), intent(in) :: supfile

      integer, parameter :: usup = 17   ! File unit for the setup file
!!!    character(len=80) :: line         ! A line from the setup file
      character(len=132) :: line         ! A line from the setup file

      ! --------------------------
      ! Open the setup file
      ! --------------------------
      open(unit=usup, file=trim(supfile), status='old', action='read')

      ! ----------------------
      ! Get file names
      ! ----------------------
      call readln(infilename)
      call readln(outfilename)


      ! --------------------
      ! Get vertical levels
      ! --------------------
      call readln(line)
      read(line, *) nlev
      if (nlev == -1) then
        allocate(zlev(1))
      else
        allocate(zlev(nlev))
      end if

      call readln(line)
      read(line, *) zlev

      call readln(line)
      read(line,*) nvar

      allocate(vars(nvar))

      do nline=1,nvar
         call readln(line)
         read(line,*) vars(nline)
      end do


      ! ----------------------
      ! Get projection details
      !-----------------------
      call readln(proj_file)

      ! -------------------------------
      ! Get global attribute details
      !------------------------------
      call readln(global_att_file)

      ! ----------------------
      ! Get subarea details
      !-----------------------
      call readln(line)
      read(line,*) subarea
      if (subarea.eq.1) then
         call readln(line)
         read(line,*) xstart
         call readln(line)
         read(line,*) xend
         call readln(line)
         read(line,*) ystart
         call readln(line)
         read(line,*) yend
      end if
      close(usup) 


    contains

      ! -----------------------------
      subroutine readln(line)
        ! --------------------------------------
        !  Reads a line from a file, skipping
        !  comments and blank lines.
        !
        !  Comments starts with a character from
        !  COMCHAR and continues to the end of line.
        !
        !  Bjørn Ådlandsvik,
        !  IMR, October 1997
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
           read(unit=usup, fmt="(A)") line
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
      end subroutine readln

    end subroutine readsup

end module setup
