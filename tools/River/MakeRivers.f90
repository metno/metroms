!=================== CREATED ===========================================
! Who:  Andre Staalstrom (ans@niva.no)
! When: 29.mars 2010 
! What: Makes NetCDF river forcing file 
!=================== EDITED ============================================
! Who:  Jon Albretsen (jonal@imr.no)
! When: June 2010
! What: Make program complete and merge it into the NorKyst800_main.sh script
!=======================================================================
! Description
! 
! This program reads data from:
! 1) ascii file with daily river discharge from the 247 main river systems in Norway and
! 2) ascii file with daily river discharge from 2 Swedish rivers.
! Makes a NetCDF forcing file for the implementation of the ROMS
! model on a 800m grid covering the Norwegian coastline 
! The model is called "NorKyst800"
!
! A blank NetCDF file must be created in advance:
!
! >ncgen -o riverfile.nc riverfile.cdl
!
! The position and the direction of the river outlets is stored 
! in a separat ascii file.
!
! The waterdepth of the cell of the river outlets must be known.
! This information is used to distribute the river discharge 
! vertically. The discharge is distributed linearly from the 
! surface down to a maximum depth (Hmax). The distribution is 
! stored in the variable "Vshape". The sum of Vshape(k)
! must be equal to 1.
! The vertical counter "k" goes from 1 in the bottom layer to 
! kmax at in the surface. z(k) is the vertical coordinate.
! z=-H is the bottom and z=0 is the surface
!
!            Vshape_max
! Vshape(z)= ---------- * z + Vshape_max 
!               Hmax
!   
! The condition (Hmax*Vshape_max)/2==1 gives
!
! Vshape_max = 2 / Hmax
!
! The maximum depth of the river flow (Hmax) is determined 
! from a Froude number consideration
!
! The vertical coordinate of each layer must be calculated 
! from the same set of parameters used in the ROMS model
! control file
! 
! Tcline  = critical depth (<hmin)       (10)?
! theta_s = surface stretching parameter (8)?
! thete_b = bottom stretching parameter  (0.1)?
! KK      = number of layers             (35)?
! 
! IMPORTANT
! Fortran and NetCDF have different ways to spesify dimensions
! In NetCDF style as written in a .cdl file:
!    salt(Nmax, Kmax, Mmax) (time, depth, river id)
! 
! Fortran counts the dimensions in the oposite direction
!    salt(Mmax, Kmax, Nmax) (river id, depth, time)
!
! This can be very confusing!!
!
! Note also that reference date is hard coded to be 1948.1.1
!=======================================================================
program MAKERIVERS

    USE netcdf

    IMPLICIT NONE
!    include 'netcdf.inc'
!=======================================================================
! DECLEARATION
! Reference date
    real               :: r_date(8)                ! Reference date used in ROMS
! Counters
    integer            :: d, i, k, l, n, nn, m, nS ! Counters
    integer            :: Kmax                     ! Total number of vertical layers
    integer, parameter :: MmaxB = 1000             ! Maximum no. of rivers allowed
    integer            :: MmaxA                    ! Total number of rivers within whole NorKyst-800m grid
    integer            :: Mmax                     ! Total number of rivers within sub model grid
    integer            :: Nmax                     ! Total number of timesteps
! Input files
    character(len=99) :: gridfile                  ! File with bathymetry
    character(len=99) :: stafile                   ! Ascii-files with station list
    character(len=99) :: fluxfile_N                ! Ascii-files with corresponding volume fluxes from Norwegian rivers
    character(len=99) :: fluxfile_S                ! Ascii-files with corresponding volume fluxes from Swedish rivers
! Input parameters
    integer :: i0, i1, j0, j1, lim                 ! Subgrid definitions and boundary zone where rivers are not used
    integer :: no_S                                ! No. of Swedish rivers
    integer :: t0_y, t0_m, t0_d, t1_y, t1_m, t1_d  ! Input and end date
    real    :: Tcline                              ! Critical depth (<hmin)
    real    :: theta_s                             ! Surface stretching parameter
    real    :: theta_b                             ! Bottom stretching parameter
! Input data
    real, allocatable :: H(:,:)                    ! Water depth [m]
    real              :: dy                        ! Model resolution (m)
    integer           :: Lp, Mp                    ! Dimension bathymetry
    integer           :: info(5)                   ! Info on station file
    integer           :: yy, mm, dd                ! Date of runoff values
    integer           :: yy0, mm0, dd0             ! First date for data sample Norwegian rivers
    integer           :: yy1, mm1, dd1             ! Last date for data sample Norwegian rivers
    integer           :: yy0S, mm0S, dd0S          ! First date for data sample Swedish rivers
    integer           :: yy1S, mm1S, dd1S          ! Last date for data sample Swedish rivers
    integer           :: timediff1, timediff2      ! Used in date manipulation
    integer           :: jd, jdinyear              ! Used in date manipulation
    real              :: Q                         ! Mean runoff for each river
    real              :: froude, ro0, ro2          ! Used in calc. of mixing depth
    real              :: gred, hmix                ! Used in calc. of mixing depth
    real              :: Hmax                      ! Maximum depth of the river flow
    real, allocatable :: z(:), s(:)                ! Vertical distribution of levels, rho-points
    real, allocatable :: z_w(:), s_w(:), dz(:)     ! Vertical distribution of levels, omega-points
    real, allocatable :: a(:)                      ! Temporary variable
    real              :: amin                      ! Minimum value of a
    integer           :: kindex                    ! Counter
    real              :: depth                     ! Temporary water depth [m]
    real              :: Vshape_max                ! Value of max. Vshape
    real              :: sumVshape                 ! Sum of Vshape in each water column (should be approx. 1!)
    logical           :: first, firstS             ! Used in reading fluxfiles
! NetCDF variables
    integer :: istatus, ncid  
    integer :: ndim, kdim, mdim, dims(3), dim_xi_rho, dim_eta_rho, dim_ndim, dim_kdim, dim_mdim
    integer :: nid,  kid,  mid, type_id(6)
    integer :: vardim
    integer :: varid 
    integer :: start2(2),count2(2)
    integer :: start3(3),count3(3)
    character(len=80) :: xi_dimname, eta_dimname, ndim_dimname, kdim_dimname, mdim_dimname
! Variables
    real, allocatable    :: time(:)                ! River runoff time (Norwegian rivers) [days since refdate]
    real, allocatable    :: timeS(:)               ! River runoff time (Swedish rivers) [days since refdate]
    integer              :: riverA(MmaxB)          ! River runoff identification number for all NorKyst800-rivers
    integer, allocatable :: river(:)               ! River runoff identification number
    integer              :: XposA(MmaxB)           ! River XI-position at RHO-points for all NorKyst800-rivers
    integer, allocatable :: Xpos(:)                ! River XI-position at RHO-points
    integer              :: YposA(MmaxB)           ! River ETA-position at RHO-points for all NorKyst800-rivers
    integer, allocatable :: Ypos(:)                ! River ETA-position at RHO-points
    integer              :: directionA(MmaxB)      ! River runoff direction (0=east/west, 1=north/south) for all NorKyst800-rivers
    integer, allocatable :: direction(:)           ! River runoff direction (0=east/west, 1=north/south)
    integer              :: signs(MmaxB)           ! River runoff sign (-1=neg. x/y-direction, 1=pos. x/y-direction)
    integer              :: flagA(MmaxB)           ! River runoff tracer flag for all NorKyst800-rivers
    integer, allocatable :: flag(:)                ! River runoff tracer flag
    real                 :: flux(MmaxB)            ! Runoff read each day
    integer              :: count(MmaxB,366)       ! Count no. of transport values every Julian day
    real                 :: fluxclimA(MmaxB,366)   ! Daily climatological transport for all NorKyst800-rivers
    real, allocatable    :: fluxclim(:,:)          ! Daily climatological transport
    real, allocatable    :: transportA(:,:)        ! River runoff vertically integrated mass transport [m3 s-1], for all NorKyst800-rivers
    real, allocatable    :: transport(:,:)         ! River runoff vertically integrated mass transport [m3 s-1]
    real, allocatable    :: temp(:,:,:)            ! River runoff potential temperature [Celsius]
    real, allocatable    :: salt(:,:,:)            ! River runoff salinity [PSU]
    real, allocatable    :: Vshape(:,:)            ! River runoff mass transport vertical profile
! Constant values used in output
    real, parameter    :: temp0 = 5                ! Constant river temp [C]
    real, parameter    :: salt0 = 1 	           ! Constant river salt [PSU]
    integer, parameter :: flag0 = 2                ! river_flag:option_0 = "all tracers are off" ;
		                                   ! river_flag:option_1 = "only temperature is on" ;
		                                   ! river_flag:option_2 = "only salinity is on" ;
		                                   ! river_flag:option_3 = "both temperature and salinity are on" ;

!=======================================================================
! Set reference date
r_date(1) = 19480101.00  ! reference date (yyyymmdd.f)
r_date(2) = 1948.        ! year
r_date(3) = 1.           ! year day
r_date(4) = 1.           ! month
r_date(5) = 1.           ! day
r_date(6) = 0.           ! hour
r_date(7) = 0.           ! minute
r_date(8) = 0.           ! second

!=======================================================================
! Read standard input
READ(5,'(A)') gridfile                        ! Grid file
READ(5,'(A)') stafile                         ! Ascii-file with station list
READ(5,'(A)') fluxfile_N                      ! Ascii-file with river discharges from Norwegian rivers
READ(5,'(A)') fluxfile_S                      ! Ascii-file with river discharges from Swedish rivers
READ(5,*) no_S                                ! No. of Swedish rivers
READ(5,*) i0, i1, j0, j1, lim                 ! Chosen sub-domain including limits for preventing near boundary river outlets
READ(5,*) t0_y, t0_m, t0_d, t1_y, t1_m, t1_d  ! Actual period for collecting river fluxes
READ(5,*) kmax, theta_s, theta_b, Tcline, dy  ! No. of vertical levels, parameters describing the levels and grid resolution

!=======================================================================
! ENLIGHTEN THE USER 
PRINT '(A)',       '===================================================================='
PRINT '(A)',       'MakeRivers.f90'
PRINT '(A)',       '===================================================================='
PRINT '(A)',       'This program reads data from an ascii file with daily river'
PRINT '(A)',       'discharge from the 247 main river systems in Norway and'
PRINT '(A)',       'make a NetCDF forcing file for the implementation of the ROMS'
PRINT '(A)',       'model on a 800m grid covering the norwegian coastline'
PRINT '(A)',       'The model is called "NorKyst800".'
PRINT '(A)',       'Note that rivers within defined subgrid only are written to file.'
PRINT '(A)',       '===================================================================='
PRINT '(A)',       'Input files'
PRINT '(2A)',      ' Grid file:                               ', TRIM(gridfile)
PRINT '(2A)',      ' Station-file:                            ', TRIM(stafile)
PRINT '(2A)',      ' File with runoffs from Norwegian rivers: ', TRIM(fluxfile_N)
PRINT '(2A)',      ' File with runoffs from Swedish rivers:   ', TRIM(fluxfile_S)
PRINT '(A)',       'Input definitions'
PRINT '(A,4I6)',   ' Sub-grid limits of full grid (i0,i1,j0,j1): ', i0, i1, j0, j1
PRINT '(A,F6.1)', ' Model resolution (m):                        ', dy
PRINT '(A,3I5)',   ' Start collecting day (Y,M,D): ', t0_y, t0_m, t0_d
PRINT '(A,3I5)',   ' End collecting day (Y,M,D):   ', t1_y, t1_m, t1_d
PRINT '(A)',       'Parameters:'
PRINT '(A,I8,A)',  ' Kmax    = ',Kmax,' Number of vertical layers'
PRINT '(A,F8.2,A)',' temp0   = ',temp0,' Constant river temperature'
PRINT '(A,F8.2,A)',' salt0   = ',salt0,' Constant river salinity'
PRINT '(A,F8.2,A)',' Tcline  = ',Tcline,' Critical depth (<hmin)'
PRINT '(A,F8.2,A)',' theta_s = ',theta_s,' Surface stretching parameter'
PRINT '(A,F8.2,A)',' theta_b = ',theta_b,' Bottom stretching parameter'
PRINT '(A,I8,A)',  ' flag0   = ',flag0,' River runoff tracer flag'
PRINT '(A)',       '        flag_option_0 = all tracers are off'
PRINT '(A)',       '        flag_option_1 = only temperature is on'
PRINT '(A)',       '        flag_option_2 = only salinity is on'
PRINT '(A)',       '        flag_option_3 = both temperature and salinity are on'
PRINT '(A)',       '===================================================================='

!=======================================================================
! Open input grid file and get model bathymetry H
istatus = nf90_open(TRIM(gridfile),nf90_nowrite,ncid)
CALL Handle_Err( istatus )
istatus = nf90_inq_dimid(ncid,'xi_rho',dim_xi_rho)
CALL Handle_Err( istatus )
istatus = nf90_inq_dimid(ncid,'eta_rho',dim_eta_rho)
CALL Handle_Err( istatus )
istatus = nf90_Inquire_Dimension(ncid,dim_xi_rho,xi_dimname,Lp)
CALL Handle_Err( istatus )
istatus = nf90_Inquire_Dimension(ncid,dim_eta_rho,eta_dimname,Mp)
CALL Handle_Err( istatus )
PRINT '(A,2I6)',"Read bathymetry with dimension ", Lp, Mp
ALLOCATE(H(Lp,Mp))
istatus = nf90_inq_varid(ncid,'h',nid)
CALL Handle_Err( istatus )
istatus = nf90_get_var(ncid,nid,H)
CALL Handle_Err( istatus )
istatus = nf90_close(ncid)
PRINT '(A,F10.2)',"Gridfile read: H(1,1) = ",H(1,1)

!=======================================================================
! Rivers spesific info
! Note that all rivers are read initially, but rivers outside sub-domain are neglected in output to nc-file
PRINT '(A)','Initiate river outlet info by reading station list...'
m = 0   ! Count rivers within full grid
l = 0   ! Count rivers within sub grid
OPEN(UNIT=11,FILE=TRIM(stafile),ACTION='READ',STATUS='OLD')
DO WHILE (.TRUE.)
  READ(11,*,ERR=65,END=65) (info(i),i=1,5)
  m = m + 1
  riverA(m) = m
  XposA(m) = info(2)
  YposA(m) = info(3)
  directionA(m) = info(4)
  signs(m) = info(5)
  flagA(m) = flag0  ! Same flag for all rivers
  IF ( info(2) >= i0 .AND. info(2) <= i1 .AND. info(3) >= j0 .AND. info(3) <= j1) l = l + 1  ! Update counter
ENDDO  ! WHILE (.TRUE.)
65 CONTINUE
CLOSE(11)
MmaxA = m
PRINT '(A,I6)',"Total no. of NorKyst800m-rivers are      MmaxA=", MmaxA
Mmax = l
PRINT '(A,I6)',"Total no. of rivers within subdomain are  Mmax=", Mmax

Nmax = jd(t1_y,t1_m,t1_d) - jd(t0_y,t0_m,t0_d) + 1.
PRINT '(A,I6)',"No. of days within actual period are      Nmax=", Nmax

! Read river transports and corresponding dates
PRINT '(A)','Initiate river outlet fluxes by reading volume fluxes for all years - generate climatology...'
fluxclimA(:,:) = 0.
count(:,:) = 0  ! Count no. of flux values every day in a climatological year

ALLOCATE(time(Nmax), timeS(Nmax), transportA(MmaxA,Nmax))

! Read and store data from Norwegian rivers to make daily climatology
first = .TRUE.  ! Used in storage of first data date
OPEN(UNIT=12,FILE=TRIM(fluxfile_N),ACTION='READ',STATUS='OLD')
DO WHILE (.TRUE.)
  READ(12,*,ERR=66,END=66) yy, mm, dd, (flux(m),m=1,MmaxA-no_S)
  IF (first) THEN  ! Store first date in data set
    yy0 = yy
    mm0 = mm
    dd0 = dd
    first = .FALSE.
  ENDIF
  ! Find year in day
  jdinyear = jd(yy,mm,dd) - jd(yy,1,1) + 1                  ! Add 1 to make jdinyear=1 for January 1st
  IF (MOD(yy,4) /= 0 .AND. mm >= 3) jdinyear = jdinyear + 1 ! Add another 1 to compensate for Feb. 29 in non-leap years
  DO m=1,MmaxA-no_S
    count(m,jdinyear) = count(m,jdinyear) + 1
    fluxclimA(m,jdinyear) = fluxclimA(m,jdinyear) + flux(m)
  ENDDO
ENDDO  ! WHILE (.TRUE.)
66 CONTINUE
! Store last date in data set
yy1 = yy
mm1 = mm
dd1 = dd
PRINT '(2(A,3I4))', 'Flux data from Norwegian rivers ranges from ',yy0, mm0, dd0,' to ' , yy1, mm1, dd1

! Read and store data from Swedish rivers to make daily climatology
firstS = .TRUE.  ! Used in storage of first data date
OPEN(UNIT=13,FILE=TRIM(fluxfile_S),ACTION='READ',STATUS='OLD')
DO WHILE (.TRUE.)
  READ(13,*,ERR=76,END=76) yy, mm, dd, (flux(m),m=1,no_S)
  IF (firstS) THEN  ! Store first date in data set
    yy0S = yy
    mm0S = mm
    dd0S = dd
    firstS = .FALSE.
  ENDIF
  ! Find year in day
  jdinyear = jd(yy,mm,dd) - jd(yy,1,1) + 1                  ! Add 1 to make jdinyear=1 for January 1st
  IF (MOD(yy,4) /= 0 .AND. mm >= 3) jdinyear = jdinyear + 1 ! Add another 1 to compensate for Feb. 29 in non-leap years
  DO m=MmaxA-no_S+1,MmaxA
    count(m,jdinyear) = count(m,jdinyear) + 1
    fluxclimA(m,jdinyear) = fluxclimA(m,jdinyear) + flux(m-MmaxA+no_S)
  ENDDO
ENDDO  ! WHILE (.TRUE.)
76 CONTINUE
! Store last date in data set
yy1S = yy
mm1S = mm
dd1S = dd
PRINT '(2(A,3I4))', 'Flux data from Swedish   rivers ranges from ',yy0S, mm0S, dd0S,' to ' , yy1S, mm1S, dd1S

! Find daily averaged flux
DO m=1,MmaxA
  DO d=1,366
    fluxclimA(m,d) = REAL(signs(m))*fluxclimA(m,d)/REAL(count(m,d))  ! Put correct sign on fluxclim
  ENDDO
  ! Smoother transition between 28/2-29/2 and 1/3
  fluxclimA(m,60) = ( fluxclimA(m,59) + fluxclimA(m,60) + fluxclimA(m,61) )/3.  ! Compensate for fewer Feb. 29th's
ENDDO
PRINT '(A,F10.1)', 'Daily climatology is found, fluxclim(river1,day1) = ', fluxclimA(1,1)

REWIND(12)  ! Need to store actual data in transport-array (Norwegian rivers)
REWIND(13)  ! Need to store actual data in transport-array (Swedish rivers)

n = 0       ! Counter for time steps in output file (should add up to Nmax) (Norwegian rivers)
nS = 0      ! Counter for time steps in output file (should add up to Nmax) (Swedish rivers)

! Check if simulation period starts before first valid data date for Norwegian rivers: if so, fill in climatological data
timediff1 = jd(t0_y,t0_m,t0_d) - jd(yy0,mm0,dd0)
IF (timediff1 < 0) THEN
  PRINT '(A)', 'Simulation period starts before data set for Norwegian rivers is valid, fill in climatology...'
  nn = min(-timediff1,Nmax)
  DO d=1,nn
    n = n + 1  ! Update counter for days
    time(n) = REAL(jd(t0_y,t0_m,t0_d) - jd(INT(r_date(2)),INT(r_date(4)),INT(r_date(5))) + (d-1))  ! Assume that time increase with one day
    ! Find Gregorian date from the Julian day based on reference date (time + ref)
    CALL gregorian(REAL(time(n) + jd(INT(r_date(2)),INT(r_date(4)),INT(r_date(5)))),yy,mm,dd)
    ! Find year in day
    jdinyear = jd(yy,mm,dd) - jd(yy,1,1) + 1                  ! Add 1 to make jdinyear=1 for January 1st
    IF (MOD(yy,4) /= 0 .AND. mm >= 3) jdinyear = jdinyear + 1 ! Add another 1 to compensate for Feb. 29 in non-leap years
    DO m=1,MmaxA-no_S
      transportA(m,n) = fluxclimA(m,jdinyear)
    ENDDO
  ENDDO
ENDIF  ! (timediff1 < 0)
! Check if simulation period starts before first valid data date for Swedish rivers: if so, fill in climatological data
timediff2 = jd(t0_y,t0_m,t0_d) - jd(yy0S,mm0S,dd0S)
IF (timediff2 < 0) THEN
  PRINT '(A)', 'Simulation period starts before data set for Swedish rivers is valid, fill in climatology...'
  nn = min(-timediff2,Nmax)
  DO d=1,nn
    nS = nS + 1  ! Update counter for days
    timeS(nS) = REAL(jd(t0_y,t0_m,t0_d) - jd(INT(r_date(2)),INT(r_date(4)),INT(r_date(5))) + (d-1))  ! Assume that time increase with one day
    ! Find Gregorian date from the Julian day based on reference date (time + ref)
    CALL gregorian(REAL(timeS(nS) + jd(INT(r_date(2)),INT(r_date(4)),INT(r_date(5)))),yy,mm,dd)
    ! Find year in day
    jdinyear = jd(yy,mm,dd) - jd(yy,1,1) + 1                  ! Add 1 to make jdinyear=1 for January 1st
    IF (MOD(yy,4) /= 0 .AND. mm >= 3) jdinyear = jdinyear + 1 ! Add another 1 to compensate for Feb. 29 in non-leap years
    DO m=MmaxA-no_S+1,MmaxA
      transportA(m,nS) = fluxclimA(m,jdinyear)
    ENDDO
  ENDDO
ENDIF  ! (timediff2 < 0)

! Make time array and fill in data from Norwegian rivers
DO WHILE (.TRUE.)
  READ(12,*,ERR=67,END=67) yy, mm, dd, (flux(m),m=1,MmaxA-no_S)
  ! Store data that are within defined period and make time array
  timediff1 = jd(yy,mm,dd) - jd(t0_y,t0_m,t0_d)
  timediff2 = jd(yy,mm,dd) - jd(t1_y,t1_m,t1_d)
  IF (timediff1 >= 0 .AND. timediff2 <= 0) THEN  ! Check that runoffs are within valid time period
    n = n + 1  ! Update counter for days
    time(n) = REAL(jd(yy,mm,dd) - jd(INT(r_date(2)),INT(r_date(4)),INT(r_date(5))))
    DO m=1,MmaxA-no_S
      transportA(m,n) = REAL(signs(m))*flux(m)
    ENDDO
  ENDIF
ENDDO  ! WHILE (.TRUE.)
67 CONTINUE

CLOSE(12)

! Make time array and fill in data from Swedish rivers
DO WHILE (.TRUE.)
  READ(13,*,ERR=77,END=77) yy, mm, dd, (flux(m),m=1,no_S)
  ! Store data that are within defined period and make time array
  timediff1 = jd(yy,mm,dd) - jd(t0_y,t0_m,t0_d)
  timediff2 = jd(yy,mm,dd) - jd(t1_y,t1_m,t1_d)
  IF (timediff1 >= 0 .AND. timediff2 <= 0) THEN  ! Check that runoffs are within valid time period
    nS = nS + 1  ! Update counter for days
    timeS(nS) = REAL(jd(yy,mm,dd) - jd(INT(r_date(2)),INT(r_date(4)),INT(r_date(5))))
    DO m=MmaxA-no_S+1,MmaxA
      transportA(m,nS) = REAL(signs(m))*flux(m-MmaxA+no_S)
    ENDDO
  ENDIF
ENDDO  ! WHILE (.TRUE.)
77 CONTINUE

CLOSE(13)

! Check if simulation period ends after last valid data date for Norwegian rivers: if so, fill in climatological data
timediff1 = jd(t1_y,t1_m,t1_d) - jd(yy1,mm1,dd1)
timediff2 = jd(t1_y,t1_m,t1_d) - jd(t0_y,t0_m,t0_d) + 1
IF (timediff1 > 0) THEN
  PRINT '(A,I6)', 'Simulation period ends after data set for Norwegian rivers is valid, fill in climatology...'
  IF (timediff2 < timediff1) THEN
    PRINT '(A,I6)', 'Simulation period also starts after data set for Norwegian rivers is valid, fill in climatology...'
  ENDIF
  nn = min(timediff1,timediff2)
  DO d=1,nn
    n = n + 1  ! Update counter for days
    IF (timediff2 < timediff1) THEN
      time(n) = REAL(jd(t0_y,t0_m,t0_d) - jd(INT(r_date(2)),INT(r_date(4)),INT(r_date(5))) + d - 1)  ! Assume that time increase with one day
    ELSE
      time(n) = REAL(jd(yy1,mm1,dd1) - jd(INT(r_date(2)),INT(r_date(4)),INT(r_date(5))) + d)  ! Assume that time increase with one day
    ENDIF
    ! Find Gregorian date from the Julian day based on reference date (time + ref)
    CALL gregorian(REAL(time(n) + jd(INT(r_date(2)),INT(r_date(4)),INT(r_date(5)))),yy,mm,dd)
    ! Find year in day
    jdinyear = jd(yy,mm,dd) - jd(yy,1,1) + 1                  ! Add 1 to make jdinyear=1 for January 1st
    IF (MOD(yy,4) /= 0 .AND. mm >= 3) jdinyear = jdinyear + 1 ! Add another 1 to compensate for Feb. 29 in non-leap years
    DO m=1,MmaxA
      transportA(m,n) = fluxclimA(m,jdinyear)
    ENDDO
  ENDDO
ENDIF  ! (timediff1 > 0)

! Check if simulation period ends after last valid data date for Swedish rivers: if so, fill in climatological data
timediff1 = jd(t1_y,t1_m,t1_d) - jd(yy1S,mm1S,dd1S)
timediff2 = jd(t1_y,t1_m,t1_d) - jd(t0_y,t0_m,t0_d) + 1
IF (timediff1 > 0) THEN
  PRINT '(A,I6)', 'Simulation period ends after data set for Swedish rivers is valid, fill in climatology...'
  IF (timediff2 < timediff1) THEN
    PRINT '(A,I6)', 'Simulation period also starts after data set for Swedish rivers is valid, fill in climatology...'
  ENDIF
  nn = min(timediff1,timediff2)
  DO d=1,nn
    nS = nS + 1  ! Update counter for days
    IF (timediff2 < timediff1) THEN
      timeS(nS) = REAL(jd(t0_y,t0_m,t0_d) - jd(INT(r_date(2)),INT(r_date(4)),INT(r_date(5))) + d - 1)  ! Assume that time increase with one day
    ELSE
      timeS(nS) = REAL(jd(yy1S,mm1S,dd1S) - jd(INT(r_date(2)),INT(r_date(4)),INT(r_date(5))) + d)  ! Assume that time increase with one day
    ENDIF
    ! Find Gregorian date from the Julian day based on reference date (time + ref)
    CALL gregorian(REAL(timeS(nS) + jd(INT(r_date(2)),INT(r_date(4)),INT(r_date(5)))),yy,mm,dd)
    ! Find year in day
    jdinyear = jd(yy,mm,dd) - jd(yy,1,1) + 1                  ! Add 1 to make jdinyear=1 for January 1st
    IF (MOD(yy,4) /= 0 .AND. mm >= 3) jdinyear = jdinyear + 1 ! Add another 1 to compensate for Feb. 29 in non-leap years
    DO m=MmaxA-no_S+1,MmaxA
      transportA(m,nS) = fluxclimA(m,jdinyear)
    ENDDO
  ENDDO
ENDIF  ! (timediff1 > 0)

! Check that counters for time steps Norwegian and Swedish rivers are identical
IF (nS /= n) THEN
  PRINT '(A,2I4)', 'Something wrong when counting time steps for Norwegian and Swedish rivers: n, nS =', n, nS
  STOP
ENDIF
DO i=1,n  ! or nS
  IF (time(i) /= timeS(i)) THEN
    PRINT '(A,2I4,2F10.2)', 'Something wrong when comparing time array for Norwegian and Swedish rivers: n, nS, time(n), timeS(nS) =', &
                            n, nS, time(n), timeS(nS)
    STOP
  ENDIF
ENDDO

! Pick out rivers that are within subdomain and store data in new variables
ALLOCATE(river(Mmax), Xpos(Mmax), Ypos(Mmax), direction(Mmax), transport(Mmax,Nmax), flag(Mmax), fluxclim(Mmax,366))
l = 0
DO m=1,MmaxA
  IF ( XposA(m) >= i0 .AND. XposA(m) <= i1 .AND. YposA(m) >= j0 .AND. YposA(m) <= j1) THEN
    l = l + 1
    river(l) = l
    Xpos(l) = XposA(m) - (i0 - lim)  ! Compensate for new western border and subtract zone without rivers
    Ypos(l) = YposA(m) - (j0 - lim)  ! Compensate for new southern border and subtract zone without rivers
    direction(l) = directionA(m)
    flag(l) = flagA(m)
    DO n=1,Nmax
      transport(l,n) = transportA(m,n)
    ENDDO
    DO d=1,366
      fluxclim(l,d) = fluxclimA(m,d)
    ENDDO
  ENDIF
ENDDO
IF (l == Mmax) THEN
  PRINT '(A,I6)',"Final arrays for river file is made. Total no. of rivers within subdomain are still ", l
ELSE
  STOP "Something went wrong when counting rivers within subgrid!"
ENDIF

!=======================================================================
! Make temp and salt array
ALLOCATE(temp(Mmax,Kmax,Nmax), salt(Mmax,Kmax,Nmax))
PRINT '(A)', 'Initiate salinity and temperature...'    
DO n=1,Nmax
  DO k=1,Kmax
    DO m=1,Mmax
      temp(m,k,n) = temp0
      salt(m,k,n) = salt0
    ENDDO
  ENDDO
ENDDO

!=======================================================================
! Make vertical shape vector
PRINT '(A)', 'Make vertical shape vector (make note if sum(Vshape) different from one)...'
ALLOCATE(Vshape(Mmax,Kmax))
ALLOCATE(z(Kmax), s(Kmax), dz(Kmax), z_w(Kmax+1), s_w(Kmax+1), a(Kmax+1))

DO m=1,Mmax
  ! Calc mixing depth
  Q = 0.
  DO d=1,366  ! Including leap year count
    Q = Q + ABS(fluxclim(m,d))
  ENDDO
  Q = Q/REAL(366)  ! Average runoff for river no. m
  froude = 1.0
  ro0 = 999.84
  ro2 = ro0*(1. + 8.08e-4*salt0)
  gred = 9.81*(1. - ro0/ro2)
  hmix = 0.
  IF (gred > 0.) hmix = 1.5*((Q*Q)/(gred*(dy**2)*froude))**0.333333  ! Only valid if s0>0
  hmix = max(1.0,hmix)
  Hmax = 2.*hmix                    ! Scale hmix to compensate for river mouths narrower than 800m
  IF (fluxclim(m,1) >= 0) THEN      ! Check sign of flux by using clim. value at day 1
    depth = H(Xpos(m),Ypos(m))
  ELSE                              ! Rivers with negative direction
    IF (direction(m) == 0) THEN     ! Rivers along the x-axis
      depth = H(Xpos(m)-1,Ypos(m))  ! Use sea point to the west
    ELSEIF (direction(m) == 1) THEN ! Rivers along the y-axis
      depth = H(Xpos(m),Ypos(m)-1)  ! Use sea point to the south
    ENDIF
  ENDIF
  CALL zdepth(depth,Tcline,theta_s,theta_b,Kmax,z,dz,s,z_w,s_w)
  amin = 1000.
  DO k=1,Kmax+1
    a(k) = ABS(Hmax+z_w(k))
    IF (a(k) < amin) THEN
      amin = MIN(amin,a(k))
      kindex = k
    ENDIF
  ENDDO
  DO k=1,kindex-1
    dz(k) = 0.
  ENDDO
  Vshape_max = 2./Hmax
  sumVshape = 0.
  DO k=1,Kmax
    Vshape(m,k) = (Vshape_max/Hmax)*z(k) + Vshape_max
    IF (Vshape(m,k) < 0.) Vshape(m,k) = 0.
    sumVshape = sumVshape + Vshape(m,k)    !*dz(k)
  ENDDO
  DO k=1,Kmax
    Vshape(m,k) = Vshape(m,k)/sumVshape
  ENDDO
  sumVshape = 0.
  DO k=1,Kmax
    sumVshape = sumVshape + Vshape(m,k)
  ENDDO
  IF (ABS(sumVshape - 1.) > 0.01) THEN
    PRINT '(A,I4,A,F6.1,A,F6.4)',"Vshape for river no.",m," (with depth ",depth,"m) does not add up to 1.0 (+/-0.05), but ",sumVshape
  ENDIF
ENDDO

!=======================================================================
! Open NetCDF file with read-write access
     write (*,*) 'Open NetCDF file...'
     istatus = NF90_OPEN('river.nc',NF90_WRITE,ncid)
     call Handle_Err( istatus )
!==================================================
! Get dimensions
     write (*,*) 'Inquire NetCDF variable dimensions...'	
     istatus = nf90_inq_dimid(ncid, 'river_time', dim_ndim)
     istatus = nf90_Inquire_Dimension(ncid, dim_ndim, ndim_dimname, ndim)
     call Handle_Err( istatus )
     istatus = nf90_inq_dimid(ncid, 's_rho', dim_kdim)
     istatus = nf90_Inquire_Dimension(ncid, dim_kdim, kdim_dimname, kdim)
     call Handle_Err( istatus )
     istatus = nf90_inq_dimid(ncid, 'river', dim_mdim)
     istatus = nf90_Inquire_Dimension(ncid, dim_mdim, mdim_dimname, mdim)
     call Handle_Err( istatus )
! Check dimensions against these found in this program
     if (ndim /= Nmax) then
       PRINT '(A,2I5)',"Error, river_time dimension from cdl-file and program are different", ndim, Nmax
       stop
     endif
     if (kdim /= Kmax) then
       PRINT '(A,2I5)',"Error, s_rho dimension from cdl-file and program are different     ", kdim, Kmax
       stop
     endif
     if (mdim /= Mmax) then
       PRINT '(A,2I5)',"Error, river dimension from cdl-file and program are different     ", mdim, Mmax
       stop
     endif
!==================================================
! Get variable id and write data
     write (*,*) 'Get variable id and write data...'
     
     write (*,*) '...river id number...'
     istatus = nf90_inq_varid(ncid, 'river', varid)
     call Handle_Err( istatus )
     istatus = nf90_put_var(ncid,varid,river) !,1,Mmax)
     call Handle_Err( istatus )

     write (*,*) '...river X position...'
     istatus = nf90_inq_varid(ncid, 'river_Xposition', varid)
     call Handle_Err( istatus )
     istatus = nf90_put_var(ncid,varid,Xpos) !,1,Mmax)
     call Handle_Err( istatus )

     write (*,*) '...river Y position...'
     istatus = nf90_inq_varid(ncid, 'river_Eposition', varid)
     call Handle_Err( istatus )
     istatus = nf90_put_var(ncid,varid,Ypos) !,1,Mmax)
     call Handle_Err( istatus )
     
     write (*,*) '...river outlet direction...'
     istatus = nf90_inq_varid(ncid, 'river_direction', varid)
     call Handle_Err( istatus )
     istatus = nf90_put_var(ncid,varid,direction) !,1,Mmax)
     call Handle_Err( istatus )

     write (*,*) '...river tracer option flag...'
     istatus = nf90_inq_varid(ncid, 'river_flag', varid)
     call Handle_Err( istatus )
     istatus = nf90_put_var(ncid,varid,flag) !,1,Mmax)
     call Handle_Err( istatus )

     write (*,*) '...river time...'
     istatus = nf90_inq_varid(ncid, 'river_time', varid)
     call Handle_Err( istatus )
     istatus = nf90_put_var(ncid,varid,time) !,1,Nmax)
     call Handle_Err( istatus )

     write (*,*) '...river Vshape...'
     istatus = nf90_inq_varid(ncid, 'river_Vshape', varid)
     call Handle_Err( istatus )

       start2(1) = 1
       count2(1) = Mmax
       start2(2) = 1
       count2(2) = Kmax

     istatus = nf90_put_var(ncid,varid,Vshape,start2,count2)    
     call Handle_Err( istatus )

     write (*,*) '...river transport...'
     istatus = nf90_inq_varid(ncid, 'river_transport', varid)
     call Handle_Err( istatus )

       start2(1) = 1
       count2(1) = Mmax
       start2(2) = 1
       count2(2) = Nmax

     istatus = nf90_put_var(ncid,varid,transport,start2,count2)    
     call Handle_Err( istatus )

     write (*,*) '...river temperature...'
     istatus = nf90_inq_varid(ncid, 'river_temp', varid)
     call Handle_Err( istatus )

       start3(1) = 1
       count3(1) = Mmax
       start3(2) = 1
       count3(2) = Kmax
       start3(3) = 1
       count3(3) = Nmax

     istatus = nf90_put_var(ncid,varid,temp,start3,count3)    
     call Handle_Err( istatus )

     write (*,*) '...river salinity...'
     istatus = nf90_inq_varid(ncid, 'river_salt', varid)
     call Handle_Err( istatus )

       start3(1) = 1
       count3(1) = Mmax
       start3(2) = 1
       count3(2) = Kmax
       start3(3) = 1
       count3(3) = Nmax

     istatus = nf90_put_var(ncid,varid,salt,start3,count3)    
     call Handle_Err( istatus )


!=======================================================================
! Close the NetCDf file
     write (*,*) 'Close NetCDF file...'
     istatus = NF90_CLOSE(ncid)
     call Handle_Err( istatus )
     
!=======================================================================
! THAT WAS ALL FOLKS!
	write (*,*) 'THAT WAS ALL FOLKS!' 

end program MAKERIVERS


!=======================================================================
! Subroutine to handle errors
SUBROUTINE Handle_Err( istatus )
    USE netcdf
    IMPLICIT NONE

    integer istatus

    if ( istatus.ne.NF90_NOERR ) then
       write(*,*) NF90_STRERROR( istatus )
       stop 'Stopped'
    end if

!    if ( istatus.eq.NF90_NOERR ) then
!       write(*,*) 'No problems...'
!    end if

    return 
END SUBROUTINE Handle_Err


!=======================================================================
! Function to calculate julian day number given a Gregorian calendar date
integer FUNCTION jd (year,month,day)

IMPLICIT NONE

integer :: year, month, day, i, j, k

i = year
j = month
k = day

jd = k - 32075 + 1461*(i+4800+(j-14)/12)/4+367*(j-2-(j-14)/12*12)/12-3*((i+4900+(j-14)/12)/100)/4

RETURN

END FUNCTION jd


!=======================================================================
! Subroutine to find Gregorian date from julian day number
SUBROUTINE gregorian (julian, year, month, day)

IMPLICIT NONE

real,    intent(in)  :: julian
integer, intent(out) :: year, month, day

integer :: i, j, k, l, n

l = julian + 68569
n = 4*l/146097
l = l - (146097*n+3)/4
i = 4000*(l+1)/1461001
l = l - 1461*i/4 + 31
j = 80*l/2447
k = l - 2447*j/80
l = j/11
j = j+2-12*l
i = 100*(n-49) + i + l

year = i
month = j
day = k

RETURN

END SUBROUTINE gregorian


!=======================================================================
! Subroutine to calculate z coordinates as a function of depth
SUBROUTINE zdepth(h,Tcline,theta_s,theta_b,n,z,dz,s,z_w,s_w)

IMPLICIT NONE

REAL,    INTENT(IN)  :: h, Tcline, theta_s, theta_b
INTEGER, INTENT(IN)  :: n
REAL,    INTENT(OUT) :: z(n), s(n), dz(n), z_w(n+1), s_w(n+1)

REAL    :: ds, A_w, B_w, C_w, A, B, C
INTEGER :: k

ds = 1./REAL(n)

! Interface between layers
DO k=1,n+1
  s_w(k) = -1. + REAL(k-1)*ds
  A_w = sinh(theta_s*s_w(k))/sinh(theta_s)
  B_w = (tanh(theta_s*(s_w(k)+0.5))-tanh(theta_s*0.5))/(2.*tanh(theta_s*0.5))
  C_w = (1.-theta_b)*A_w + theta_b*B_w
  z_w(k) = Tcline*s_w(k) + (h-Tcline)*C_w
ENDDO

! Center of layers
DO k=1,n
  s(k) = -1. + 0.5*ds + REAL(k-1)*ds     ! s = -1+ds/2:ds:0-ds/2;
  A = sinh(theta_s*s(k))/sinh(theta_s)
  B = (tanh(theta_s*(s(k)+0.5))-tanh(theta_s*0.5))/(2.*tanh(theta_s*0.5))
  C = (1.-theta_b)*A + theta_b*B
  z(k) = Tcline*s(k) + (h-Tcline)*C
ENDDO

DO k=1,n
  dz(k) = z_w(k+1) - z_w(k)
ENDDO

RETURN

END SUBROUTINE zdepth

