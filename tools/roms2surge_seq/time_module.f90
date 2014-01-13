module time_module

! ---------------------------------------------
! Time module for Fortran 90
! ---------------------------------------------
!
! This module inplements a derived type, time_type
! for handling date and time.
!
! Instances of time_type are ordered.
! The difference between to time instances is defined.
! A number can be added or subtracted from a time instance.
! The module offers easy conversion and extended assignment
! between various time representations.
!
! Extended assignment
! ===================
!
! 1) From date number = serial day number since 1968-05-23 00:00:00
!       (also called Modified Julian Day)
!    NOTE: Do not use single precision real numbers, 
!          as the resolution is not good enough.
!
! 2) From date vectors = integer vectors with form
!       (/ year, month, day, hours, minutes, seconds /)
! 
! 3) From time strings = strings of format "yyyy-mm-dd hh:mm:ss" 
!
! Examples
! --------
! For the examples, time is declared by
!    type(time_type) :: time1, time2, time3
! The three examples give the same time instance
! (sjekk med ==)
!
!    time1 = 12897.7264699074D0
!    time2 = (/ 2003, 9, 14, 17, 26, 07 /)  
!    time3 = "2003-09-14 17:26:07"                
!
!
! The inverse assignments are available if the receiving types are
! 1)  real(kind=r8)
! 2)  integer, dimension(6)          
! 3)  character(len=19)  
! [Test dette, hva skjer dersom typene ikke identiske ??
!  evt. øk fleksibiliteten og robustheten, gi eksempler]
!
! Ordering
! ========
!
! The standard comparison operators (==, /=, <, >, <=, =>)
! and their equivalents (.eq., ...) can be used to compare
! to time instances.
! Example:
!   type(time_type) :: time1, time2
!   time1 = (/ 2003, 9, 14, 17, 26, 07 /)  
!   time2 = "1990-01-01 00:00:00"
!   print *, (time1 > time2)  ! prints T for true
!
!       
! Arithmetic
! ==========
! [Skriv mer i stil som over]
! The operator - is overloaded to produce the time difference
!    time1 - time2 = time difference, unit = days, single precision real
!
! The operator + is overloaded to add a length of time 
! (unit = days, single presicion real) to the time
!
! The operators are connected by:
!    time1 + (time2-time1) == time1
!    
! Examples:
!   [lag et par enkle]
! 
! Functions
! =========
!   string = timestr(time) 
!      returns a representantion of time in format "yyyy-mm-dd hh:mm:ss"
!      Equivalent to the assignment string = time, 
!      but more useful for printing: write(*,*) timestr(time)
!           
!  [De under er ikke implementert ennå]
!   string = datestr(time)
!      returns a representantion of the date in format "yyyy-mm-dd"
!   string = clockstr(time)
!      returns a representantion of the time of day in format "hh:mm:ss.sss"
!   integer = weekday(time)
!      returns day number in week (monday = 1, ..., sunday = 7)
!   integer == weeknumber(time)
!      returns week number in year [hva er definisjonen ??]
!  
! -----------------------------------------------------------------
!  
! 14 September 2003
! Bjørn Ådlandsvik <bjorn@imr.no>
! Instute of Marine Research
! Bergen, Norway
! ------------------------------------------------------------------

  implicit none
 
  integer, parameter, private :: r4 = 4
  integer, parameter, private :: r8 = 8

!  real(kind=r8), parameter, private :: secday = 86400
  integer, parameter, private :: secday = 86400   

  
     ! Number of seconds per day = 24*3600
!  real, parameter, private :: isecday = 1.0/86400.0
!  finn bedre navn

  integer, parameter, private :: offset = 2440000
    ! Produces Modified Julian Day, zero on 1968-05-23 00:00:00

  ! time_type structure
  ! As in GOTM, use two integers
  type time_type
!    real(kind=r8) :: datenum
    integer :: day    ! Modified Julian Day
    integer :: sec    ! Second after midnight
  end type time_type

  ! Automatic conversion between time_type and time strings 
  interface assignment(=)
    module procedure str2time     ! time string -> time_type
    module procedure time2str     ! time_type   -> time string
  end interface

  ! Automatic conversion between time_type and time vectors
  interface assignment(=)
    module procedure vec2time     ! time vector -> time_type
    module procedure time2vec     ! time_type   -> time vector
  end interface

  ! Automatic conversion between time_type and serial date numbers
  interface assignment(=)
    module procedure double2time   ! real(r8)  -> time_type
    module procedure real2time     ! real(r4)  -> time_type
    module procedure int2time      ! int       -> time_type
    module procedure time2num      ! time_type -> real(r8)
  end interface

  ! ---------------
  ! Time arithmetic 
  ! =============== 
 
  ! Define generic add_days
  interface add_days
    module procedure add_days_real, add_days_double, add_days_int
  end interface


  ! Time difference, time1 - time2 -> real(kind=r4)
  interface operator (-)
    module procedure timediff
  end interface 

  ! Time addition:
  !   time + t -> time, 
  !     t = real(r4), real(r8) or default integer
  interface operator (+)
    module procedure add_days_real, add_days_double, add_days_int
  end interface

  ! Time subtraction:
  !   time - t -> time, 
  !     t = real(r4), real(r8) or default integer
  interface operator (-)
    module procedure sub_days_real, sub_days_double, sub_days_int
  end interface


  ! -------------------- 
  ! Comparison operators 
  ! ==================== 
  interface operator (==)
    module procedure eqtime
  end interface
  interface operator (/=)
    module procedure netime
  end interface
  interface operator (<)
    module procedure lttime
  end interface
  interface operator (<=)
    module procedure letime
  end interface
  interface operator (>)
    module procedure gttime
  end interface
  interface operator (>=)
    module procedure getime
  end interface

  contains


  !  
  ! ******************************************************  
  !
  subroutine JulianDay(y, m, d, jd)
  ! --------------------------------------------------------------
  ! Computes the Julian day, given a (proleptic) Gregorian date.
  ! Julian day zero = 24 November 4714 b.c. = -4713-11-24 G.
  ! Algorithm works for -4800-03-01 G onwards
  !
  ! Algorithm by Fliegel and van Flandern, 1968
  ! Communications of the ACM, vol. 11, p. 657
  ! See also: http://bcn.boulder.co.us/y2k/y2kbcalc.htm
  !
  ! Control values:
  ! -4713, 11, 24 =>       0
  !     1,  1,  1 => 1721426
  !  1900,  1,  1 => 2415021
  !  1968,  5, 23 => 2440000     ! Offset for modified Julian day
  !
  ! 13 September 2003
  ! Bjørn Ådlandsvik, <bjorn@imr.no>
  ! Institute of Marine Research
  ! -------------------------------------------------------------
    integer, intent(in)  :: y    ! year
    integer, intent(in)  :: m    ! month
    integer, intent(in)  :: d    ! day
    integer, intent(out) :: jd   ! Julian Day

    jd = ( 1461 * ( y + 4800 + ( m - 14 ) / 12 ) ) / 4 +            &
         ( 367 * ( m - 2 - 12 * ( ( m - 14 ) / 12 ) ) ) / 12 -      &
         ( 3 * ( ( y + 4900 + ( m - 14 ) / 12 ) / 100 ) ) / 4 +     &
           d - 32075

  end subroutine JulianDay

  !
  ! -------------------------------------------------------------
  ! 
  subroutine GregorianDate(jd, y, m, d)
  ! 
  ! Computes the Gregorian date, given a Julian day number
  ! Julian day zero = 24 November 4714 b.c. = -4713-11-24 G.
  ! Algorithm works for -4900-03-01 G onwards
  ! Inverse to julday
  !
  ! Algorithm by Fliegel and van Flandern, 1968
  ! Communications of the ACM, vol. 11, p. 657
  ! See also: http://bcn.boulder.co.us/y2k/y2kbcalc.htm
  !
  ! 13 September 2003
  ! Bjørn Ådlandsvik, <bjorn@imr.no>
  ! Institute of Marine Research
  ! -------------------------------------------------------------

    ! --- Arguments ---
    integer, intent(in)  :: jd   ! Julian day number
    integer, intent(out) :: y    ! Year
    integer, intent(out) :: m    ! Month
    integer, intent(out) :: d    ! Day

    ! --- Local variables ---
    integer :: l, n, i, j

    l = jd + 68569
    n = ( 4 * l ) / 146097
    l = l - ( 146097 * n + 3 ) / 4
    i = ( 4000 * ( l + 1 ) ) / 1461001 
    l = l - ( 1461 * i ) / 4 + 31
    j = ( 80 * l ) / 2447
    d = l - ( 2447 * j ) / 80
    l = j / 11
    m = j + 2 - ( 12 * l )
    y = 100 * ( n - 49 ) + i + l      

  end subroutine GregorianDate



  !
  ! *********************************************************
  ! 
  subroutine vec2time(time, datevec)
  !
  ! Convert from time vector to time_type
  ! 
  !
  ! Control values:
  !   (/ 1900, 01, 01, 00, 00, 00 /)       =>  (-24979, 0)
  !   (/ 1968, 05, 24, 00, 00, 00 /)       =>  (     1, 0)
  !   (/ 2003, 09, 14, 17, 26, 07 /)       =>  ( 12897, 7264699074)
  ! 
    implicit none
  !
  ! --- Arguments ---
  !
    type(time_type), intent(out) :: time
    integer, dimension(6), intent(in)  :: datevec
      ! Time in format (/ year, month, day, hour, min, sec /)
  !
  ! --- Local variable ---
  !
    integer :: ndays
  !
  !***END DECLARATIONS VEC2TIME
  !
    call JulianDay(datevec(1), datevec(2), datevec(3), ndays)
    time%day = ndays - offset    ! Modify Julian Day
    time%sec = 3600 * datevec(4) + 60 * datevec(5) + datevec(6)
    
  end subroutine vec2time

  !
  ! --------------------------------------------------
  !
  subroutine time2vec(datevec, time)
  !
  ! Computes the time vector given the number n of days since 
  !
    implicit none
  !
  ! --- Arguments
  !
    integer, dimension(6), intent(out) :: datevec
    type(time_type), intent(in) :: time

  !
  ! --- Local variable
  !
    integer :: nsec

  !
  !***END DECLARATIONS TIME2VEC
  !
   
    call GregorianDate(time%day + offset, datevec(1), datevec(2), datevec(3))

    nsec = time%sec
    datevec(4) = nsec/3600
    nsec = nsec - 3600*datevec(4)
    datevec(5) = nsec/60
    datevec(6) = nsec - 60*datevec(5)
      
  end subroutine time2vec

  !
  ! **************************************************
  !
  subroutine str2vec(datevec, datestr)
   
  ! Parse time strings of format
  !    "ccyy-mm-dd hh:mm:ss", 
  !    "ccyy-mm-dd hh:mm",
  !    "ccyy-mm-dd"
  ! Does not work b.c.
  !
  ! Generalisere: Regulært utrykk
  ! 

       
    integer, dimension(6), intent(out) :: datevec
    character(len=*), intent(in) :: datestr
 
    character(len=len(datestr)) :: tmp 
    integer :: i

    tmp = ""                        ! Make string clean
    tmp = adjustl(datestr)          ! Skip initial blanks
    read(tmp, "(I4)") datevec(1)    ! Year has exactly 4 digits
    tmp = tmp(6:)                   ! remove "ccyy-"

    i = index(tmp, "-")             ! month is delimited by a "-"
    read(tmp(1:i-1), *) datevec(2)  
    tmp = tmp(i+1:)                 ! remove "mm-" ot "m-"

    ! day is delimited by blank, or end of string
    tmp = adjustl(tmp)     ! Hvorfor denne ????
    i = index(tmp, " ")             ! Look for blank
    read(tmp, *) datevec(3)
    if (i == 0) then                ! If no blank, end of string
      datevec(4) = 0
      datevec(5) = 0
      datevec(6) = 0
      return                          ! Finished
    else
      tmp = adjustl(tmp(i+1:))      ! else remove "dd" and following blanks
    end if
      
    i = index(tmp, ":")             ! hours is delimited by ":"
    if (i == 0) then                ! If no ":" 
      datevec(4) = 0    !*** FEIL: Kan ha timer: 1999-02-24 12 skal akspeterers
      datevec(5) = 0
      datevec(6) = 0
      return              ! Finished
    else 
      read(tmp(1:i-1), *) datevec(4)
      tmp = tmp(i+1:)    ! remove "hh:"
    end if

    ! minutes is delimited by ":" or " " or end of string
    ! [Ikke sikker på end of string, tolerere rusk etterpå]
    i = index(tmp, ":")
    if (i == 0) then  ! no seconds
      read(tmp, *) datevec(5)
      datevec(6) = 0
    else
      read(tmp(1:i-1), *) datevec(5)
      ! Require two decimals for seconds
      read(tmp(i+1:i+2), *) datevec(6) 
      !read(tmp(i+1:), *) datevec(6)
    end if
      
   end subroutine str2vec
 
   !
   ! ************************************************
   !
   subroutine vec2str(datestr, datevec)

    character(len=19), intent(out) :: datestr
    integer, dimension(6), intent(in) :: datevec
  
    character(len=*), parameter :: dateform =   "I4, '-', I2.2, '-', I2.2"
    character(len=*), parameter :: clockform =  "I2.2, ':', I2.2, ':', I2.2"
    character(len=*), parameter :: f =                              &
                     "(" // dateform // ", ' ', " // clockform // ")"

    write(datestr, f) datevec

  end subroutine vec2str

  !
  ! ************************************************
  !

  ! Wrapper routines for time_type
  subroutine str2time(time, datestr)
    type(time_type) , intent(out):: time
    character(len=*), intent(in) :: datestr
    integer, dimension(6) :: timevec
    call str2vec(timevec, datestr)
    call vec2time(time, timevec)
  end subroutine str2time

  subroutine time2str(timestr, time)
    character(len=19), intent(out) :: timestr
    type(time_type), intent(in) :: time
    integer, dimension(6) :: timevec
    call time2vec(timevec, time)
    call vec2str(timestr, timevec)
  end subroutine time2str  

! Kan vi teoretisk få sec = 86400 ???
! isåfall gjør noe mer robust
  subroutine double2time(time, datenum)
    type(time_type), intent(out) :: time
    real(kind=r8), intent(in) :: datenum
    time%day = int(datenum)
    time%sec = nint((datenum - time%day)*secday)
    if (time%sec == secday) then
      time%day = time%day + 1
      time%sec = 0
    end if
  end subroutine double2time

  subroutine real2time(time, datenum)
    type(time_type), intent(out) :: time
    real, intent(in) :: datenum
    time%day = int(datenum)
    time%sec = nint((datenum - time%day)*secday)
! det under unødvendig ???
!    if (time%sec == secday) then
!      time%day = time%day + 1
!      time%sec = 0
!    end if
  end subroutine real2time
    
  subroutine int2time(time, datenum)
    type(time_type), intent(out) :: time
    integer, intent(in) :: datenum
    time%day = datenum
    time%sec = 0
  end subroutine int2time
    
  subroutine time2num(datenum, time)
    real(kind=r8), intent(out) :: datenum
    type(time_type), intent(in) :: time
    datenum = time%day + real(time%sec, kind=r8)/secday
  end subroutine time2num
    


  !
  ! =======================================
  ! Time arithmetic
  ! =======================================
  !
  pure function timediff(time1, time2)
  ! Computes the difference time1-time2, unit = day
  ! returns standard real
    real :: timediff
    type(time_type), intent(in) :: time1
    type(time_type), intent(in) :: time2
    timediff = (time1%day-time2%day) + real(time1%sec-time2%sec)/secday
  end function timediff

  pure function timediff_sec(time1, time2)
  ! Computes the difference time1-time2, unit = seconds, 
  ! returns standard integer
    integer :: timediff_sec
    type(time_type), intent(in) :: time1
    type(time_type), intent(in) :: time2
    timediff_sec = (time1%day-time2%day)*secday + time1%sec-time2%sec
  end function timediff_sec

  pure function add_days_real(time, t) result(time2)
  ! Computes the time + t
  ! unit(t) = days, type = standard real
    ! --- Arguments ---
    type(time_type) :: time2
    type(time_type), intent(in) :: time
    real, intent(in) :: t
    ! --- Local variables ---
    integer :: iday
    integer :: isec
     
    iday = floor(t)   ! Floor does the correct thing for negative t
    isec = nint((t - iday)*secday)

    time2%day = time%day + iday
    time2%sec = time%sec + isec

    if (time2%sec >= secday) then
      time2%day = time2%day + 1
      time2%sec = time2%sec - secday
    end if
  
  end function add_days_real

  pure function add_days_double(time, t) result(time2)
  ! Computes the time + t
  ! unit(t) = days, type = double precision real
    ! --- Arguments ---
    type(time_type) :: time2
    type(time_type), intent(in) :: time
    real(kind=r8), intent(in) :: t
    ! --- Local variables ---
    integer :: iday
    integer :: isec
     
    iday = floor(t)   ! Floor does the correct thing for negative t
    isec = nint((t - iday)*secday)

    time2%day = time%day + iday
    time2%sec = time%sec + isec

    if (time2%sec >= secday) then
      time2%day = time2%day + 1
      time2%sec = time2%sec - secday
    end if
  
  end function add_days_double

  pure function add_days_int(time, t) result(time2)
  ! Computes the time + t
  ! unit(t) = days, type = standard integer
    ! --- Arguments ---
    type(time_type) :: time2
    type(time_type), intent(in) :: time
    integer, intent(in) :: t

    time2%day = time%day + t
    time2%sec = time%sec 
  
  end function add_days_int


  pure function add_seconds(time, t) result(time2)
  ! Computes the time + t
  ! unit(t) = seconds, type = standard integer
    ! --- Arguments ---
    type(time_type) :: time2
    type(time_type), intent(in) :: time
    integer, intent(in) :: t

    integer :: day
    integer :: sec

    sec = time%sec + t
    day = sec/secday
    if (sec < 0) day = day - 1
    sec = sec - day*secday
    
    time2%day = time%day + day
    time2%sec = sec
  
  end function add_seconds


  ! ----------------------------------------------
  ! Wrappers for subtraction
  ! ---------------------------------------------
 
  function sub_days_real(time, t)
    type(time_type) :: sub_days_real
    type(time_type), intent(in) :: time
    real, intent(in) :: t
    sub_days_real = add_days_real(time, -t)
  end function sub_days_real

  function sub_days_double(time, t)
    type(time_type) :: sub_days_double
    type(time_type), intent(in) :: time
    real(kind=r8), intent(in) :: t
    sub_days_double = add_days_double(time, -t)
  end function sub_days_double

  function sub_days_int(time, t)
    type(time_type) :: sub_days_int
    type(time_type), intent(in) :: time
    integer, intent(in) :: t
    sub_days_int = add_days_int(time, -t)
  end function sub_days_int


  ! 
  ! ======================================================
  ! Time comparison functions
  ! ======================================================
  !
  function eqtime(time1, time2)
     logical eqtime
     type(time_type), intent(in) :: time1
     type(time_type), intent(in) :: time2
     eqtime = (time1%day == time2%day) .and. (time1%sec == time2%sec)
  end function eqtime

  function netime(time1, time2)
     logical netime
     type(time_type), intent(in) :: time1
     type(time_type), intent(in) :: time2
     netime = .not. eqtime(time1, time2)
  end function netime

  function lttime(time1, time2)
     logical lttime
     type(time_type), intent(in) :: time1
     type(time_type), intent(in) :: time2
     lttime = (time1%day < time2%day) .or.                               &
              ((time1%day == time2%day) .and. (time1%sec < time2%sec))
  end function lttime

  function letime(time1, time2)
     logical letime
     type(time_type), intent(in) :: time1
     type(time_type), intent(in) :: time2
     letime = .not. lttime(time2, time1)
  end function letime

  function gttime(time1, time2)
     logical gttime
     type(time_type), intent(in) :: time1
     type(time_type), intent(in) :: time2
     gttime = lttime(time2, time1)
  end function gttime


  function getime(time1, time2)
     logical getime
     type(time_type), intent(in) :: time1
     type(time_type), intent(in) :: time2
     getime = letime(time2, time1)
  end function getime



  !
  ! ============================================
  !  Utility functions for time_type
  ! ============================================
  !
 
  function timestr(time) result(c)
  ! print time in format yyyy-mm-dd hh:mm:ss
    character(len=19) :: c
    type(time_type), intent(in) :: time
    call time2str(c, time)
    write(66,*) ""   ! Bug i lf95 => trenger denne
  end function timestr  
  
  !
  ! -----------------------------------------------
  !  
  function datestr(time)
  ! print date in format yyyy-mm-dd
    character(len=10) :: datestr
    type(time_type), intent(in) :: time
    integer, dimension(6) :: timevec
    call time2vec(timevec, time)
    write(datestr, "(I4, '-', I2.2, '-', I2.2)") timevec(1:3)
  end function datestr  
    
  !
  ! --------------------------------------------------------
  ! 
  function clockstr(time)
  ! Ser ut til å funke, sjekk nærmere
  ! print clock in format hh:mm:ss
    character(len=8) :: clockstr
    type(time_type), intent(in) :: time
    integer, dimension(6) :: timevec
    !real(kind=r8) :: datenum1
    !real :: fracsec
    call time2vec(timevec, time)
    write(clockstr, "(I2.2, ':', I2.2, ':', I2.2)") timevec(4:6)
  end function clockstr  

  !
  ! ----------------------------------------------------------
  !
  function weekday(time)
  ! Returns day of week, Monday = 1, ..., Sunday = 7
   
    integer :: weekday 
    type(time_type), intent(in) :: time

    integer, dimension(6) :: datevec
    integer :: y, m, d   ! Year, month, day

    call time2vec(datevec, time)
    y = datevec(1)
    m = datevec(2)
    d = datevec(3)

    ! Add january and february at the end of the previous year
    if (m < 3) then
      m = m + 12
      y = y - 1 
    end if

    weekday = mod((13*m+3)/5 + d + y + y/4 - y/100 + y/400, 7) + 1

  end function weekday 



end module time_module
