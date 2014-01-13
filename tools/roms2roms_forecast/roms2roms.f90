PROGRAM romsS2romsS
! Program to obtain nesting (clima) conditions from ROMS history/avg files
! Original program roms2roms created by Jon Albretsen, IMR, Dec 2010, based on programs by Paul Budgell, IMR.
! New version handling all Vtransform and Vstretching by Nils Melsom Kristensen, met.no, Apr 2011.
! Code for Vtransform and Vstretching is copied from 'set_scoord.F' and 'set_depth.F' in the official ROMS code.
! New version does not interpolate horizontally, this is handled by FIMEX.
! Variables to be read from, and written to nc-file, are hardcoded into this program.
!
! Usage:
! ./romsS2romsS << EOF
! $pathi$rhofile
! $Ni $theta_si $theta_bi $Tclinei
! $pathi$rhofile
! $pathi$ufile
! $pathi$vfile
! $Vtransformi $Vstretchingi
! $pathgrd$grdfile
! $No $theta_so $theta_bo $Tclineo
! $patho
! $ofile
! $Vtransformo $Vstretchingo
! EOF
!
! Version 1.0: 12th of Oct. - Added FOAM spesific stuff... (NMK)
! 21. feb 2012: tries to read zeta_detided, if not exist, read zeta


use netcdf

implicit none

INTEGER :: statusi, ncgridi, ncidi
INTEGER :: dim_xi_rhoi, dim_eta_rhoi
INTEGER :: dim_timei
INTEGER :: UVarIdi, VVarIdi, SaltVarIdi, TempVarIdi, TimeVarIdi
INTEGER :: UbarVarIdi, VbarVarIdi, ZetaVarIdi
INTEGER :: id_hi, id_depthi
INTEGER :: id_rmaski
INTEGER :: Lp, Mp, L, M, Ni, i, j,n,nn
REAL, DIMENSION(:), allocatable :: depthi, depth
REAL, DIMENSION(:,:), allocatable :: hi
! Ice stuff
REAL, DIMENSION(:,:), allocatable :: snow_thick,tisrf,ti,tau_iw,chu_iw,s0mk,t0mk,sfwat,sig11,sig12,sig22 
!
REAL, DIMENSION(:,:), allocatable :: rmaski,rmasko,rmaskuv
REAL, DIMENSION(:,:,:), allocatable :: z_ri
REAL :: Tclinei, theta_si, theta_bi
real :: add_offset,scale_factor
REAL*8 :: time_in, time_in_prev, tday
character(len=80) :: xi_dimnamei, eta_dimnamei, time_dimnamei

INTEGER :: statuso, ncgrido, ncido
INTEGER :: dim_xi_rhoo, dim_eta_rhoo
INTEGER :: dim_xi_uo, dim_eta_uo
INTEGER :: dim_xi_vo, dim_eta_vo
INTEGER :: dim_s_rhoo, dim_timeo
INTEGER :: UVarIdo, VVarIdo, SaltVarIdo, TempVarIdo, TimeVarIdo
INTEGER :: UbarVarIdo, VbarVarIdo, ZetaVarIdo
INTEGER :: UiceVarIdo, ViceVarIdo
INTEGER :: UiceVarIdi, ViceVarIdi
integer :: AiceVarIdi,HiceVarIdi,HsnVarIdi,AgeiceVarIdi
integer :: AiceVarIdo,HiceVarIdo,HsnVarIdo,AgeiceVarIdo
integer :: snowVarIdo,tisrfVarIdo,tiVarIdo,tauVarIdo,chuVarIdo,s0VarIdo,t0VarIdo
integer :: sfwatVarIdo,sig11VarIdo,sig12VarIdo,sig22VarIdo
INTEGER :: id_ho,id_rmasko
INTEGER :: No

REAL, DIMENSION(:,:), allocatable :: ho

REAL, DIMENSION(:,:,:), allocatable :: z_ro
REAL :: Tclineo, theta_so, theta_bo

INTEGER :: itime, otime, iti

character (len=99) :: gridfilei, gridfileo
character (len=99) :: avgfile_rho,avgfile_u,avgfile_v
character (len=99) :: clim_root, outfile, clim_path

REAL :: pi, DTOR

REAL, DIMENSION(:,:), allocatable :: work
REAL, DIMENSION(:,:), allocatable :: scr
REAL, DIMENSION(:,:), allocatable :: scr1
REAL, DIMENSION(:,:), allocatable :: scru
REAL, DIMENSION(:,:), allocatable :: scrv
REAL, DIMENSION(:,:), allocatable :: scr1o
REAL, DIMENSION(:,:), allocatable :: scr2o
REAL, DIMENSION(:,:), allocatable :: error

INTEGER :: nvalue, mxs
REAL, PARAMETER    :: undef = 1.E+37            ! Undefined land value
REAL :: tx, critx, cor

REAL, DIMENSION(:,:,:), ALLOCATABLE :: temp_in
REAL, DIMENSION(:,:,:), ALLOCATABLE :: salt_in
REAL, DIMENSION(:,:,:), ALLOCATABLE :: u_in
REAL, DIMENSION(:,:,:), ALLOCATABLE :: v_in
REAL, DIMENSION(:,:), ALLOCATABLE :: ubar_in, uice
REAL, DIMENSION(:,:), ALLOCATABLE :: vbar_in, vice
REAL, DIMENSION(:,:), ALLOCATABLE :: zeta_in

REAL, DIMENSION(:,:,:), ALLOCATABLE :: temp_out
REAL, DIMENSION(:,:,:), ALLOCATABLE :: salt_out
REAL, DIMENSION(:,:,:), ALLOCATABLE :: u_out
REAL, DIMENSION(:,:,:), ALLOCATABLE :: v_out
REAL, DIMENSION(:,:), ALLOCATABLE :: ubar_out
REAL, DIMENSION(:,:), ALLOCATABLE :: vbar_out
REAL, DIMENSION(:,:), ALLOCATABLE :: zeta_out
REAL, DIMENSION(:,:), ALLOCATABLE :: aice
REAL, DIMENSION(:,:), ALLOCATABLE :: hice
REAL, DIMENSION(:,:), ALLOCATABLE :: hsn
REAL, DIMENSION(:,:), ALLOCATABLE :: ageice

REAL, DIMENSION(:,:,:), ALLOCATABLE :: scr3d

integer ::  Vtransi,Vtranso,Vstretchi,Vstretcho

! -----------------------------------------------------------
! Read standard input

read(5,'(a)') gridfilei                    	! Name of one of the files listed in file list
read(5,*) Ni, theta_si, theta_bi, Tclinei  	! Vertical grid parameters for input grid
read(5,'(a)') avgfile_rho                 	! input file at rho points
read(5,'(a)') avgfile_u                 	! input file at u points
read(5,'(a)') avgfile_v                 	! input file at v points
read(5,*) Vtransi, Vstretchi			! Input Vtransform and Vstretching
read(5,'(a)') gridfileo                    	! Grid file for the output domain
read(5,*) No, theta_so, theta_bo, Tclineo  	! Vertical grid parameters for output grid
read(5,'(a)') clim_path                    	! Path where new clima file is located
read(5,'(a)') clim_root                    	! Used in name of clima-file
read(5,*) Vtranso, Vstretcho			! Output Vtransform and Vstretching

pi = ATAN(1.)*4.
DTOR = pi/180.
tx = 0.9*undef
critx = 0.1
cor = 1.6
mxs = 50

! ------------------------------------------------------------
! Open input grid file and get info
write(*,*) 'gridefilei= ', gridfilei
statusi = nf90_open(trim(gridfilei),nf90_nowrite,ncgridi)
statusi = nf90_inq_dimid(ncgridi,'xi_rho',dim_xi_rhoi)
if(statusi /= nf90_NoErr) statusi = nf90_inq_dimid(ncgridi,'x',dim_xi_rhoi)
if(statusi /= nf90_NoErr) call handle_err(statusi)
statusi = nf90_inq_dimid(ncgridi,'eta_rho',dim_eta_rhoi)
if(statusi /= nf90_NoErr) statusi = nf90_inq_dimid(ncgridi,'y',dim_eta_rhoi)
if(statusi /= nf90_NoErr) call handle_err(statusi)
statusi = nf90_Inquire_Dimension(ncgridi,dim_xi_rhoi,xi_dimnamei,Lp)
statusi = nf90_Inquire_Dimension(ncgridi,dim_eta_rhoi,eta_dimnamei,Mp)

M = Mp-1
L = Lp-1

write(*,*) 'Lp, Mp = ',Lp,Mp

allocate(hi(Lp,Mp))
allocate(rmaski(Lp,Mp))
allocate(rmaskuv(Lp,Mp))
allocate(work(Lp,Mp))
allocate(scr(Lp,Mp))
allocate(scr1(Lp,Mp))
!allocate(scra(Lp,Mp))
!allocate(scrb(Lp,Mp))
allocate(scru(L,Mp))
allocate(scrv(Lp,M))
allocate(error(Lp,Mp))
allocate(depthi(Ni))
allocate(depth(Ni))

if (Vtransi == 10 .or. Vtransi == 0) then
   rmaski=1 !This will make sure no fill is done on FOAM-fields..
   statusi = nf90_inq_varid(ncgridi,'depth',id_depthi)
   if(statusi /= nf90_NoErr) call handle_err(statusi)
   statusi = nf90_get_var(ncgridi,id_depthi,depth)
   if(statusi /= nf90_NoErr) call handle_err(statusi)
   write (*,*) 'depth'
   write (*,*) depth
   WRITE (*,*) '============='
   depthi=-depth
else
   statusi = nf90_inq_varid(ncgridi,'h',id_hi)
   if(statusi /= nf90_NoErr) call handle_err(statusi)
   statusi = nf90_inq_varid(ncgridi,'mask_rho',id_rmaski)
   if(statusi /= nf90_NoErr) call handle_err(statusi)
   
   statusi = nf90_get_var(ncgridi,id_hi,hi)
   if(statusi /= nf90_NoErr) call handle_err(statusi)
   statusi = nf90_get_var(ncgridi,id_rmaski,rmaski)
   if(statusi /= nf90_NoErr) call handle_err(statusi)
   WHERE(rmaski<1) rmaski = 0
end if



! Vertical grid specification for input
allocate(z_ri(Lp,Mp,Ni))


statusi = nf90_close(ncgridi)

allocate(temp_in(Lp,Mp,Ni))
allocate(salt_in(Lp,Mp,Ni))
if (Vtransi /= 10) then
   allocate(u_in(L,Mp,Ni))
   allocate(v_in(Lp,M,Ni))
   allocate(uice(L,Mp))
   allocate(vice(Lp,M))
   allocate(hsn(Lp,Mp))
   allocate(ageice(Lp,Mp))
else
   allocate(u_in(Lp,Mp,Ni))
   allocate(v_in(Lp,Mp,Ni))
   allocate(uice(Lp,Mp))
   allocate(vice(Lp,Mp))
end if
allocate(ubar_in(L,Mp))
allocate(vbar_in(Lp,M))
allocate(zeta_in(Lp,Mp))
allocate(aice(Lp,Mp))
allocate(hice(Lp,Mp))
! ---------------------------------------------------------------------
! Open and get info on output file
write(*,*) 'gridfileo= ',gridfileo
statuso = nf90_open(trim(gridfileo),nf90_nowrite,ncgrido)
if(statuso /= nf90_NoErr) call handle_err(statuso)

allocate(rmasko(Lp,Mp))
allocate(ho(Lp,Mp))
allocate(scr3d(Lp,Mp,No))
allocate(scr1o(Lp,Mp))
allocate(scr2o(Lp,Mp))

allocate(temp_out(Lp,Mp,No))
allocate(salt_out(Lp,Mp,No))
allocate(u_out(L,Mp,No))
allocate(v_out(Lp,M,No))
allocate(ubar_out(L,Mp))
allocate(vbar_out(Lp,M))
allocate(zeta_out(Lp,Mp))
! Ice stuff
! allocate(snow_thick(Lp,Mp))
! allocate(tisrf(Lp,Mp))
! allocate(ti(Lp,Mp))
! allocate(tau_iw(Lp,Mp))
! allocate(chu_iw(Lp,Mp))
! allocate(s0mk(Lp,Mp))
! allocate(t0mk(Lp,Mp))
! allocate(sfwat(Lp,Mp))
! allocate(sig11(Lp,Mp))
! allocate(sig12(Lp,Mp))
! allocate(sig22(Lp,Mp))
! ! For now, set all ice stuff = 0
! snow_thick = 0.0
! tisrf = 0.0
! ti = 0.0
! tau_iw = 0.0
! chu_iw = 0.0
! s0mk = 0.0
! t0mk = 0.0
! sfwat = 0.0
! sig11 = 0.0
! sig12 = 0.0
! sig22 = 0.0

statuso = nf90_inq_varid(ncgrido,'h',id_ho)
if(statuso /= nf90_NoErr) call handle_err(statuso)
statusi = nf90_inq_varid(ncgrido,'mask_rho',id_rmasko)
if(statusi /= nf90_NoErr) call handle_err(statuso)
!
statuso = nf90_get_var(ncgrido,id_ho,ho)
if(statuso /= nf90_NoErr) call handle_err(statuso)
statusi = nf90_get_var(ncgrido,id_rmasko,rmasko)
if(statusi /= nf90_NoErr) call handle_err(statuso)

!Specify output vertical grid
allocate(z_ro(Lp,Mp,No))
!call spec_vert_grid(Lp,Mp,No,ho,Tclineo,theta_bo,theta_so,Vtranso,Vstretcho,z_ro)

statuso = nf90_close(ncgrido)


outfile = TRIM(clim_path) // TRIM(clim_root) // '_clm.nc'
write(*,'(2a)') 'outfile: ', TRIM(outfile)

! Prepare output netCDF file
statuso = nf90_create(trim(outfile),IOR(nf90_clobber,nf90_64BIT_OFFSET),ncido)
if(statuso /= nf90_NoErr) call handle_err(statuso)
!
! Define dimensions
!
statuso = nf90_def_dim(ncido,'xi_rho',Lp,dim_xi_rhoo)
if(statuso /= nf90_NoErr) call handle_err(statuso)
statuso = nf90_def_dim(ncido,'eta_rho',Mp,dim_eta_rhoo)
if(statuso /= nf90_NoErr) call handle_err(statuso)
statuso = nf90_def_dim(ncido,'xi_u',L,dim_xi_uo)
if(statuso /= nf90_NoErr) call handle_err(statuso)
statuso = nf90_def_dim(ncido,'eta_u',Mp,dim_eta_uo)
if(statuso /= nf90_NoErr) call handle_err(statuso)
statuso = nf90_def_dim(ncido,'xi_v',Lp,dim_xi_vo)
if(statuso /= nf90_NoErr) call handle_err(statuso)
statuso = nf90_def_dim(ncido,'eta_v',M,dim_eta_vo)
if(statuso /= nf90_NoErr) call handle_err(statuso)
statuso = nf90_def_dim(ncido,'s_rho',No,dim_s_rhoo)
if(statuso /= nf90_NoErr) call handle_err(statuso)
statuso = nf90_def_dim(ncido,'clim_time',nf90_unlimited,dim_timeo)
if(statuso /= nf90_NoErr) call handle_err(statuso)
!
! Define variables
!
statuso = nf90_def_var(ncido,'u',nf90_float,                              &
         (/dim_xi_uo, dim_eta_uo, dim_s_rhoo, dim_timeo/),UVarIdo) 
if(statuso /= nf90_NoErr) call handle_err(statuso)
statuso = nf90_def_var(ncido,'v',nf90_float,                              &
         (/dim_xi_vo, dim_eta_vo, dim_s_rhoo, dim_timeo/),VVarIdo) 
if(statuso /= nf90_NoErr) call handle_err(statuso)
statuso = nf90_def_var(ncido,'salt',nf90_float,                           &
         (/dim_xi_rhoo, dim_eta_rhoo, dim_s_rhoo, dim_timeo/),SaltVarIdo) 
if(statuso /= nf90_NoErr) call handle_err(statuso)
statuso = nf90_def_var(ncido,'temp',nf90_float,                           &
         (/dim_xi_rhoo, dim_eta_rhoo, dim_s_rhoo, dim_timeo/),TempVarIdo) 
if(statuso /= nf90_NoErr) call handle_err(statuso )
statuso = nf90_def_var(ncido,'zeta',nf90_float,                            &
         (/dim_xi_rhoo, dim_eta_rhoo, dim_timeo/),ZetaVarIdo) 
if(statuso /= nf90_NoErr) call handle_err(statuso)
statuso = nf90_def_var(ncido,'ubar',nf90_float,                            &
         (/dim_xi_uo, dim_eta_uo, dim_timeo/),UbarVarIdo) 
if(statuso /= nf90_NoErr) call handle_err(statuso)
statuso = nf90_def_var(ncido,'vbar',nf90_float,                            &
         (/dim_xi_vo, dim_eta_vo, dim_timeo/),VbarVarIdo) 
if(statuso /= nf90_NoErr) call handle_err(statuso)
statuso = nf90_def_var(ncido,'aice',nf90_float,                            &
     (/dim_xi_rhoo, dim_eta_rhoo, dim_timeo/),AiceVarIdo) 
if(statuso /= nf90_NoErr) call handle_err(statuso)
statuso = nf90_def_var(ncido,'hice',nf90_float,                            &
     (/dim_xi_rhoo, dim_eta_rhoo, dim_timeo/),HiceVarIdo) 
if(statuso /= nf90_NoErr) call handle_err(statuso)
statuso = nf90_def_var(ncido,'uice',nf90_float,                            &
     (/dim_xi_uo, dim_eta_uo, dim_timeo/),UiceVarIdo) 
if(statuso /= nf90_NoErr) call handle_err(statuso)
statuso = nf90_def_var(ncido,'vice',nf90_float,                            &
     (/dim_xi_vo, dim_eta_vo, dim_timeo/),ViceVarIdo) 
if(statuso /= nf90_NoErr) call handle_err(statuso)
!
if (Vtransi.ne.10) then
   statuso = nf90_def_var(ncido,'snow_thick',nf90_float,                            &
        (/dim_xi_rhoo, dim_eta_rhoo, dim_timeo/),HsnVarIdo) 
   if(statuso /= nf90_NoErr) call handle_err(statuso)
   statuso = nf90_def_var(ncido,'ageice',nf90_float,                            &
        (/dim_xi_rhoo, dim_eta_rhoo, dim_timeo/),AgeiceVarIdo) 
   if(statuso /= nf90_NoErr) call handle_err(statuso)
end if
! ! More ice...
! statuso = nf90_def_var(ncido,'snow_thick',nf90_float,                            &
!          (/dim_xi_rhoo, dim_eta_rhoo, dim_timeo/),snowVarIdo) 
! if(statuso /= nf90_NoErr) call handle_err(statuso)
! statuso = nf90_def_var(ncido,'tisrf',nf90_float,                            &
!          (/dim_xi_rhoo, dim_eta_rhoo, dim_timeo/),tisrfVarIdo) 
! if(statuso /= nf90_NoErr) call handle_err(statuso)
! statuso = nf90_def_var(ncido,'ti',nf90_float,                            &
!          (/dim_xi_rhoo, dim_eta_rhoo, dim_timeo/),tiVarIdo) 
! if(statuso /= nf90_NoErr) call handle_err(statuso)
! statuso = nf90_def_var(ncido,'tau_iw',nf90_float,                            &
!          (/dim_xi_rhoo, dim_eta_rhoo, dim_timeo/),tauVarIdo) 
! if(statuso /= nf90_NoErr) call handle_err(statuso)
! statuso = nf90_def_var(ncido,'chu_iw',nf90_float,                            &
!          (/dim_xi_rhoo, dim_eta_rhoo, dim_timeo/),chuVarIdo) 
! if(statuso /= nf90_NoErr) call handle_err(statuso)
! statuso = nf90_def_var(ncido,'s0mk',nf90_float,                            &
!          (/dim_xi_rhoo, dim_eta_rhoo, dim_timeo/),s0VarIdo) 
! if(statuso /= nf90_NoErr) call handle_err(statuso)
! statuso = nf90_def_var(ncido,'t0mk',nf90_float,                            &
!          (/dim_xi_rhoo, dim_eta_rhoo, dim_timeo/),t0VarIdo) 
! if(statuso /= nf90_NoErr) call handle_err(statuso)
! statuso = nf90_def_var(ncido,'sfwat',nf90_float,                            &
!          (/dim_xi_rhoo, dim_eta_rhoo, dim_timeo/),sfwatVarIdo) 
! if(statuso /= nf90_NoErr) call handle_err(statuso)
! statuso = nf90_def_var(ncido,'sig11',nf90_float,                            &
!          (/dim_xi_rhoo, dim_eta_rhoo, dim_timeo/),sig11VarIdo) 
! if(statuso /= nf90_NoErr) call handle_err(statuso)
! statuso = nf90_def_var(ncido,'sig12',nf90_float,                            &
!          (/dim_xi_rhoo, dim_eta_rhoo, dim_timeo/),sig12VarIdo) 
! if(statuso /= nf90_NoErr) call handle_err(statuso)
! statuso = nf90_def_var(ncido,'sig22',nf90_float,                            &
!          (/dim_xi_rhoo, dim_eta_rhoo, dim_timeo/),sig22VarIdo) 
! if(statuso /= nf90_NoErr) call handle_err(statuso)
!
statuso = nf90_def_var(ncido,'clim_time',nf90_double,dim_timeo,TimeVarIdo) 
if(statuso /= nf90_NoErr) call handle_err(statuso)
!
! Include variable attributes
!
statuso = nf90_put_att(ncido,UVarIdo,'long_name','u-momentum component')
if(statuso /= nf90_NoErr) call handle_err(statuso)
statuso = nf90_put_att(ncido,UVarIdo,'units','meter second-1')
if(statuso /= nf90_NoErr) call handle_err(statuso)
statuso = nf90_put_att(ncido,UVarIdo,'time','clim_time')
if(statuso /= nf90_NoErr) call handle_err(statuso)
statuso = nf90_put_att(ncido,UVarIdo,'field','u-velocity, scalar, series')
if(statuso /= nf90_NoErr) call handle_err(statuso)
! 
statuso = nf90_put_att(ncido,VVarIdo,'long_name','v-momentum component')
if(statuso /= nf90_NoErr) call handle_err(statuso)
statuso = nf90_put_att(ncido,VVarIdo,'units','meter second-1')
if(statuso /= nf90_NoErr) call handle_err(statuso)
statuso = nf90_put_att(ncido,VVarIdo,'time','clim_time')
if(statuso /= nf90_NoErr) call handle_err(statuso)
statuso = nf90_put_att(ncido,VVarIdo,'field','v-velocity, scalar, series')
if(statuso /= nf90_NoErr) call handle_err(statuso)
! 
statuso = nf90_put_att(ncido,SaltVarIdo,'long_name','salinity')
if(statuso /= nf90_NoErr) call handle_err(statuso)
statuso = nf90_put_att(ncido,SaltVarIdo,'units','PSU')
if(statuso /= nf90_NoErr) call handle_err(statuso)
statuso = nf90_put_att(ncido,SaltVarIdo,'time','clim_time')
if(statuso /= nf90_NoErr) call handle_err(statuso)
statuso = nf90_put_att(ncido,SaltVarIdo,'field','salinity, scalar, series')
if(statuso /= nf90_NoErr) call handle_err(statuso)
!
statuso = nf90_put_att(ncido,TempVarIdo,'long_name','potential temperature')
if(statuso /= nf90_NoErr) call handle_err(statuso)
statuso = nf90_put_att(ncido,TempVarIdo,'units','Celsius')
if(statuso /= nf90_NoErr) call handle_err(statuso)
statuso = nf90_put_att(ncido,TempVarIdo,'time','clim_time')
if(statuso /= nf90_NoErr) call handle_err(statuso)
statuso = nf90_put_att(ncido,TempVarIdo,'field','temperature, scalar, series')
if(statuso /= nf90_NoErr) call handle_err(statuso)
!
statuso = nf90_put_att(ncido,ZetaVarIdo,'long_name','sea level')
if(statuso /= nf90_NoErr) call handle_err(statuso)
statuso = nf90_put_att(ncido,ZetaVarIdo,'units','metres')
if(statuso /= nf90_NoErr) call handle_err(statuso)
statuso = nf90_put_att(ncido,ZetaVarIdo,'time','clim_time')
if(statuso /= nf90_NoErr) call handle_err(statuso)
statuso = nf90_put_att(ncido,ZetaVarIdo,'field','sea level, scalar, series')
if(statuso /= nf90_NoErr) call handle_err(statuso)
! 
statuso = nf90_put_att(ncido,UbarVarIdo,'long_name','u-2D momentum component')
if(statuso /= nf90_NoErr) call handle_err(statuso)
statuso = nf90_put_att(ncido,UbarVarIdo,'units','meter second-1')
if(statuso /= nf90_NoErr) call handle_err(statuso)
statuso = nf90_put_att(ncido,UbarVarIdo,'time','clim_time')
if(statuso /= nf90_NoErr) call handle_err(statuso)
statuso = nf90_put_att(ncido,UbarVarIdo,'field','u-2D velocity, scalar, series')
if(statuso /= nf90_NoErr) call handle_err(statuso)
! 
statuso = nf90_put_att(ncido,VbarVarIdo,'long_name','v-2D momentum component')
if(statuso /= nf90_NoErr) call handle_err(statuso)
statuso = nf90_put_att(ncido,VbarVarIdo,'units','meter second-1')
if(statuso /= nf90_NoErr) call handle_err(statuso)
statuso = nf90_put_att(ncido,VbarVarIdo,'time','clim_time')
if(statuso /= nf90_NoErr) call handle_err(statuso)
statuso = nf90_put_att(ncido,VbarVarIdo,'field','v-2D velocity, scalar, series')
if(statuso /= nf90_NoErr) call handle_err(statuso)
! 
statuso = nf90_put_att(ncido,AiceVarIdo,'long_name','time-averaged fraction of cell covered by ice')
if(statuso /= nf90_NoErr) call handle_err(statuso)
statuso = nf90_put_att(ncido,AiceVarIdo,'time','clim_time')
if(statuso /= nf90_NoErr) call handle_err(statuso)
statuso = nf90_put_att(ncido,AiceVarIdo,'field','ice concentration, scalar, series')
if(statuso /= nf90_NoErr) call handle_err(statuso)
! 
statuso = nf90_put_att(ncido,HiceVarIdo,'long_name','time-averaged average ice thickness in cell')
if(statuso /= nf90_NoErr) call handle_err(statuso)
statuso = nf90_put_att(ncido,HiceVarIdo,'units','metres')
if(statuso /= nf90_NoErr) call handle_err(statuso)
statuso = nf90_put_att(ncido,HiceVarIdo,'time','clim_time')
if(statuso /= nf90_NoErr) call handle_err(statuso)
statuso = nf90_put_att(ncido,HiceVarIdo,'field','ice thickness, scalar, series')
if(statuso /= nf90_NoErr) call handle_err(statuso)
! 
statuso = nf90_put_att(ncido,UiceVarIdo,'long_name','time-averaged u-component of ice velocity')
if(statuso /= nf90_NoErr) call handle_err(statuso)
statuso = nf90_put_att(ncido,UiceVarIdo,'units','meter second-1')
if(statuso /= nf90_NoErr) call handle_err(statuso)
statuso = nf90_put_att(ncido,UiceVarIdo,'time','clim_time')
if(statuso /= nf90_NoErr) call handle_err(statuso)
statuso = nf90_put_att(ncido,UiceVarIdo,'field','u-component of ice velocity, scalar, series')
if(statuso /= nf90_NoErr) call handle_err(statuso)
! 
statuso = nf90_put_att(ncido,ViceVarIdo,'long_name','time-averaged v-component of ice velocity')
if(statuso /= nf90_NoErr) call handle_err(statuso)
statuso = nf90_put_att(ncido,ViceVarIdo,'units','meter second-1')
if(statuso /= nf90_NoErr) call handle_err(statuso)
statuso = nf90_put_att(ncido,ViceVarIdo,'time','clim_time')
if(statuso /= nf90_NoErr) call handle_err(statuso)
statuso = nf90_put_att(ncido,ViceVarIdo,'field','v-component of ice velocity, scalar, series')
if(statuso /= nf90_NoErr) call handle_err(statuso)
! !
if (Vtransi.ne.10) then
   statuso = nf90_put_att(ncido,HsnVarIdo,'long_name','time-averaged average thickness of snow cover in cell')
   if(statuso /= nf90_NoErr) call handle_err(statuso)
   statuso = nf90_put_att(ncido,HsnVarIdo,'units','metres')
   if(statuso /= nf90_NoErr) call handle_err(statuso)
   statuso = nf90_put_att(ncido,HsnVarIdo,'time','clim_time')
   if(statuso /= nf90_NoErr) call handle_err(statuso)
   statuso = nf90_put_att(ncido,HsnVarIdo,'field','snow thickness, scalar, series')
   if(statuso /= nf90_NoErr) call handle_err(statuso)
   ! 
   statuso = nf90_put_att(ncido,AgeiceVarIdo,'long_name','time-averaged age of the ice')
   if(statuso /= nf90_NoErr) call handle_err(statuso)
   statuso = nf90_put_att(ncido,AgeiceVarIdo,'units','days')
   if(statuso /= nf90_NoErr) call handle_err(statuso)
   statuso = nf90_put_att(ncido,AgeiceVarIdo,'time','clim_time')
   if(statuso /= nf90_NoErr) call handle_err(statuso)
   statuso = nf90_put_att(ncido,AgeiceVarIdo,'field','ice age, scalar, series')
   if(statuso /= nf90_NoErr) call handle_err(statuso)
   ! 
   ! statuso = nf90_put_att(ncido,snowVarIdo,'time','clim_time')
   ! if(statuso /= nf90_NoErr) call handle_err(statuso)
   ! statuso = nf90_put_att(ncido,tisrfVarIdo,'time','clim_time')
   ! if(statuso /= nf90_NoErr) call handle_err(statuso)
   ! statuso = nf90_put_att(ncido,tiVarIdo,'time','clim_time')
   ! if(statuso /= nf90_NoErr) call handle_err(statuso)
   ! statuso = nf90_put_att(ncido,tauVarIdo,'time','clim_time')
   ! if(statuso /= nf90_NoErr) call handle_err(statuso)
   ! statuso = nf90_put_att(ncido,chuVarIdo,'time','clim_time')
   ! if(statuso /= nf90_NoErr) call handle_err(statuso)
   ! statuso = nf90_put_att(ncido,s0VarIdo,'time','clim_time')
   ! if(statuso /= nf90_NoErr) call handle_err(statuso)
   ! statuso = nf90_put_att(ncido,t0VarIdo,'time','clim_time')
   ! if(statuso /= nf90_NoErr) call handle_err(statuso)
   ! statuso = nf90_put_att(ncido,sfwatVarIdo,'time','clim_time')
   ! if(statuso /= nf90_NoErr) call handle_err(statuso)
   ! statuso = nf90_put_att(ncido,sig11VarIdo,'time','clim_time')
   ! if(statuso /= nf90_NoErr) call handle_err(statuso)
   ! statuso = nf90_put_att(ncido,sig12VarIdo,'time','clim_time')
   ! if(statuso /= nf90_NoErr) call handle_err(statuso)
   ! statuso = nf90_put_att(ncido,sig22VarIdo,'time','clim_time')
   ! if(statuso /= nf90_NoErr) call handle_err(statuso)
end if
! !
statuso = nf90_put_att(ncido,TimeVarIdo,'long_name','time since 1970/01/01/00:00')
if(statuso /= nf90_NoErr) call handle_err(statuso)
statuso = nf90_put_att(ncido,TimeVarIdo,'units','days')
if(statuso /= nf90_NoErr) call handle_err(statuso)
statuso = nf90_put_att(ncido,TimeVarIdo,'field','time, scalar, series')
if(statuso /= nf90_NoErr) call handle_err(statuso)
!
statuso = nf90_put_att(ncido,nf90_global,'type','ROMS/TOMS climatology file')
if(statuso /= nf90_NoErr) call handle_err(statuso)
statuso = nf90_put_att(ncido,nf90_global,'title','Nesting-file for operational run at met.no')
if(statuso /= nf90_NoErr) call handle_err(statuso)
!
! Exit definition mode
!
statuso = nf90_enddef(ncido)
if(statuso /= nf90_NoErr) call handle_err(statuso)
!
write(*,*) 'Finished file definition on output file'
!
! ! ---------------------------------------------------------------
!
otime = 0  ! Count time steps for output file
time_in = 0.
!
! Open input averages files
statusi = nf90_open(trim(avgfile_rho),nf90_nowrite,ncidi)
if(statusi /= nf90_NoErr) call handle_err(statusi)
statusi = nf90_inq_dimid(ncidi,'ocean_time',dim_timei)
if(statusi /= nf90_NoErr) statusi = nf90_inq_dimid(ncidi,'time',dim_timei)
if(statusi /= nf90_NoErr) call handle_err(statusi)
statusi = nf90_Inquire_Dimension(ncidi,dim_timei,time_dimnamei,Iti)
if(statusi /= nf90_NoErr) call handle_err(statusi)
statusi = nf90_inq_varid(ncidi,'ocean_time',TimeVarIdi)
if(statusi /= nf90_NoErr) statusi = nf90_inq_varid(ncidi,'time',TimeVarIdi)
if(statusi /= nf90_NoErr) call handle_err(statusi)

! Lop through time steps on file
! ---------------------------------------------------------------

DO itime = 1,Iti

! ---
! Date
   time_in_prev = time_in
   statusi = nf90_get_var(ncidi,TimeVarIdi,time_in,start=(/ itime /))
   if(statusi /= nf90_NoErr) call handle_err(statusi)
  
   write(*,*) 'time_in:',time_in
   
   IF (time_in <= time_in_prev) CYCLE  ! Prevent repetition of duplicates in time and/or time step backward in time
   
! Count for output file
   otime = otime + 1
!
! .......................................................................
! !
! Sea level
!
! Read in sea level
   statusi = nf90_inq_varid(ncidi,'zeta_detided',ZetaVarIdi)
   if(statusi /= nf90_NoErr) statusi = nf90_inq_varid(ncidi,'zeta',ZetaVarIdi)
   if(statusi /= nf90_NoErr) call handle_err(statusi)
   statusi = nf90_get_var(ncidi,ZetaVarIdi,zeta_in,start=(/ 1, 1, itime/))
   if(statusi /= nf90_NoErr) call handle_err(statusi)
   statusi = nf90_get_att(ncidi, ZetaVarIdi, "add_offset", add_offset)
   if(statusi /= nf90_NoErr) add_offset=0.0
   statusi = nf90_get_att(ncidi, ZetaVarIdi, "scale_factor", scale_factor)
   if(statusi /= nf90_NoErr) scale_factor=1.0
   zeta_in=(zeta_in*scale_factor)+add_offset
!
! Fill in masked-out values 
   scr = 0.
   scr = zeta_in
   WHERE (rmaski < 1) scr = undef
   CALL fill(Lp,Mp,1,Lp,1,Mp,scr,tx,critx,cor,mxs,work,error,nvalue)
   zeta_out = scr
!
! Output to netCDF file
   statuso = nf90_put_var(ncido,ZetaVarIdo,zeta_out,start=(/1,1,otime/))
!    statuso = nf90_put_var(ncido,snowVarIdo,snow_thick,start=(/1,1,otime/))
!    statuso = nf90_put_var(ncido,tisrfVarIdo,tisrf,start=(/1,1,otime/))
!    statuso = nf90_put_var(ncido,tiVarIdo,ti,start=(/1,1,otime/))
!    statuso = nf90_put_var(ncido,tauVarIdo,tau_iw,start=(/1,1,otime/))
!    statuso = nf90_put_var(ncido,chuVarIdo,chu_iw,start=(/1,1,otime/))
!    statuso = nf90_put_var(ncido,s0VarIdo,s0mk,start=(/1,1,otime/))
!    statuso = nf90_put_var(ncido,t0VarIdo,t0mk,start=(/1,1,otime/))
!    statuso = nf90_put_var(ncido,sfwatVarIdo,sfwat,start=(/1,1,otime/))
!    statuso = nf90_put_var(ncido,sig11VarIdo,sig11,start=(/1,1,otime/))
!    statuso = nf90_put_var(ncido,sig12VarIdo,sig12,start=(/1,1,otime/))
!    statuso = nf90_put_var(ncido,sig22VarIdo,sig22,start=(/1,1,otime/))
   if(statuso /= nf90_NoErr) call handle_err(statuso)

  write(*,*) 'Completed sea level'
! .......................................................................
! !
  ! aice
  !
  ! Read in aice
  statusi = nf90_inq_varid(ncidi,'aice',AiceVarIdi)
  if(statusi /= nf90_NoErr) write(*,*) "Variable aice not on infile..." !call handle_err(statusi)
  statusi = nf90_get_var(ncidi,AiceVarIdi,aice,start=(/ 1, 1, itime/))
  if(statusi /= nf90_NoErr) aice=0.0 !call handle_err(statusi)
  statusi = nf90_get_att(ncidi, AiceVarIdi, "add_offset", add_offset)
  if(statusi /= nf90_NoErr) add_offset=0.0
  statusi = nf90_get_att(ncidi, AiceVarIdi, "scale_factor", scale_factor)
  if(statusi /= nf90_NoErr) scale_factor=1.0
  aice=(aice*scale_factor)+add_offset
  
  ! Fill in masked-out values 
  WHERE (rmaski < 1) aice = undef
  CALL fill(Lp,Mp,1,Lp,1,Mp,aice,tx,critx,cor,mxs,work,error,nvalue)
  
  
  write(*,*) 'Completed aice'
  ! .......................................................................
  ! !
  ! hice
  !
  ! Read in hice
  statusi = nf90_inq_varid(ncidi,'hice',HiceVarIdi)
  if(statusi /= nf90_NoErr) write(*,*) "Variable hice not on infile..." !call handle_err(statusi)
  statusi = nf90_get_var(ncidi,HiceVarIdi,hice,start=(/ 1, 1, itime/))
  if(statusi /= nf90_NoErr) hice=0.0 !call handle_err(statusi)
  statusi = nf90_get_att(ncidi, HiceVarIdi, "add_offset", add_offset)
  if(statusi /= nf90_NoErr) add_offset=0.0
  statusi = nf90_get_att(ncidi, HiceVarIdi, "scale_factor", scale_factor)
  if(statusi /= nf90_NoErr) scale_factor=1.0
  hice=(hice*scale_factor)+add_offset
  !
  ! Fill in masked-out values 
  WHERE (rmaski < 1) hice = undef
  CALL fill(Lp,Mp,1,Lp,1,Mp,hice,tx,critx,cor,mxs,work,error,nvalue)
  !
  !
  write(*,*) 'Completed hice'
  ! .......................................................................
  ! !
  if (Vtransi.ne.10) then
     ! hsn
     !
     ! Read in hsn
     statusi = nf90_inq_varid(ncidi,'hsn',HsnVarIdi)
     if(statusi /= nf90_NoErr) statusi = nf90_inq_varid(ncidi,'hsnow',HsnVarIdi)
     if(statusi /= nf90_NoErr) statusi = nf90_inq_varid(ncidi,'snow_thick',HsnVarIdi)
     if(statusi /= nf90_NoErr) call handle_err(statusi)
     statusi = nf90_get_var(ncidi,HsnVarIdi,hsn,start=(/ 1, 1, itime/))
     if(statusi /= nf90_NoErr) call handle_err(statusi)
     statusi = nf90_get_att(ncidi, HsnVarIdi, "add_offset", add_offset)
     if(statusi /= nf90_NoErr) add_offset=0.0
     statusi = nf90_get_att(ncidi, HsnVarIdi, "scale_factor", scale_factor)
     if(statusi /= nf90_NoErr) scale_factor=1.0
     hsn=(hsn*scale_factor)+add_offset
     !
     ! Fill in masked-out values 
     WHERE (rmaski < 1) hsn = undef
     CALL fill(Lp,Mp,1,Lp,1,Mp,hsn,tx,critx,cor,mxs,work,error,nvalue)
     !
     !
     write(*,*) 'Completed hsn'
     ! .......................................................................
     ! !
     ! ageice
     !
     ! Read in ageice
     statusi = nf90_inq_varid(ncidi,'ageice',AgeiceVarIdi)
     if(statusi /= nf90_NoErr) call handle_err(statusi)
     statusi = nf90_get_var(ncidi,AgeiceVarIdi,ageice,start=(/ 1, 1, itime/))
     if(statusi /= nf90_NoErr) call handle_err(statusi)
     statusi = nf90_get_att(ncidi, AgeiceVarIdi, "add_offset", add_offset)
     if(statusi /= nf90_NoErr) add_offset=0.0
     statusi = nf90_get_att(ncidi, AgeiceVarIdi, "scale_factor", scale_factor)
     if(statusi /= nf90_NoErr) scale_factor=1.0
     ageice=(ageice*scale_factor)+add_offset
     !
     ! Fill in masked-out values 
     WHERE (rmaski < 1) ageice = undef
     CALL fill(Lp,Mp,1,Lp,1,Mp,ageice,tx,critx,cor,mxs,work,error,nvalue)
     !
     !
     ! Output to netCDF file
     statuso = nf90_put_var(ncido,AiceVarIdo,aice,start=(/1,1,otime/))
     if(statuso /= nf90_NoErr) call handle_err(statuso)
     ! Output to netCDF file
     statuso = nf90_put_var(ncido,HiceVarIdo,hice,start=(/1,1,otime/))
     if(statuso /= nf90_NoErr) call handle_err(statuso)
     ! Output to netCDF file
     statuso = nf90_put_var(ncido,HsnVarIdo,hsn,start=(/1,1,otime/))
     if(statuso /= nf90_NoErr) call handle_err(statuso)
     ! Output to netCDF file
     statuso = nf90_put_var(ncido,AgeiceVarIdo,ageice,start=(/1,1,otime/))
     if(statuso /= nf90_NoErr) call handle_err(statuso)
     write(*,*) 'Completed ageice'
     !   else
     !      write (*,*) 'Skipping reading of ice-variables, all set to zero'
     !      aice=0.0
     !      hice=0.0
     !      hsn=0.0
     !      ageice=0.0
  end if
  !
!!!!!!!!!!!!!!!!!!!!!!!!!!nå regne ut z!!
  ! Vertical grid specification for input
  write(*,*) '###########INPUT##########################'
  if (Vtransi.eq.1.or.Vtransi.eq.2) then
     call spec_vert_grid(Lp,Mp,Ni,hi,zeta_out,Tclinei,theta_bi,theta_si,Vtransi,Vstretchi,z_ri)
  else if (Vtransi == 10 .or. Vtransi == 0) then
     write (*,*) 'Input on z-levels'
     do i=1,Lp
        do j=1,Mp
           z_ri(i,j,:)=depthi
        end do
     end do
     write (*,*) z_ri(Lp,Mp,:)     
  end if
!
!Specify output vertical grid
  write(*,*) '###########OutPUT##########################'
  call spec_vert_grid(Lp,Mp,No,ho,zeta_out,Tclineo,theta_bo,theta_so,Vtranso,Vstretcho,z_ro)
  write(*,*) '###########################################'
! 
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Temperature
!
! Read in temperature
   statusi = nf90_inq_varid(ncidi,'temp',TempVarIdi)
   if(statusi /= nf90_NoErr) call handle_err(statusi)
   statusi = nf90_get_var(ncidi,TempVarIdi,temp_in,start=(/1,1,1,itime/))
   if(statusi /= nf90_NoErr) call handle_err(statusi)
   statusi = nf90_get_att(ncidi, TempVarIdi, "add_offset", add_offset)
   if(statusi /= nf90_NoErr) add_offset=0.0
   statusi = nf90_get_att(ncidi, TempVarIdi, "scale_factor", scale_factor)
   if(statusi /= nf90_NoErr) scale_factor=1.0
   temp_in=(temp_in*scale_factor)+add_offset
   !check for Kelvin...
   if (temp_in(1,1,1) > 100 ) temp_in=temp_in-273.15
!call getvar(ncidi,'temp',itime,temp_in)   !funker ikke, pga variabelnavnet må være eksakt like mange char som er spes.!!
!
! Vertical interpolation
   !write (*,*) z_ri(1,1,:)     
   call vert_int(temp_in,Mp,Lp,Ni,No,z_ri,temp_out,z_ro,undef,rmaski,rmasko) !fungerer tilfredsstillende. Ekstrapolerer ved å bare holde verdiene konstant.
!
! Output to netCDF file
  statuso = nf90_put_var(ncido,TempVarIdo,temp_out,start=(/1,1,1,otime/))
  if(statuso /= nf90_NoErr) call handle_err(statuso)
!
  write(*,*) 'Completed temperature'
! ........................................................................
!
! Salinity
!
! Read in salinity
   statusi = nf90_inq_varid(ncidi,'salt',SaltVarIdi)
   if(statusi /= nf90_NoErr) call handle_err(statusi)
   statusi = nf90_get_var(ncidi,SaltVarIdi,salt_in,start=(/ 1, 1, 1, itime/))
   if(statusi /= nf90_NoErr) call handle_err(statusi)
   statusi = nf90_get_att(ncidi, SaltVarIdi, "add_offset", add_offset)
   if(statusi /= nf90_NoErr) add_offset=0.0
   statusi = nf90_get_att(ncidi, SaltVarIdi, "scale_factor", scale_factor)
   if(statusi /= nf90_NoErr) scale_factor=1.0
   salt_in=(salt_in*scale_factor)+add_offset
!
! Vertical interpolation
  call vert_int(salt_in,Mp,Lp,Ni,No,z_ri,salt_out,z_ro,undef,rmaski,rmasko)
!
! Output to netCDF file
  statuso = nf90_put_var(ncido,SaltVarIdo,salt_out,(/1,1,1,otime/))
  if(statuso /= nf90_NoErr) call handle_err(statuso)
!
  write(*,*) 'Completed salinity'
! ! ----------------------------------------------------------------
! !
! ! 3D Velocity
!close file with rho-points
  statusi = nf90_close(ncidi)
  if(statusi /= nf90_NoErr) call handle_err(statusi)
!
!read stuff at u-points
  statusi = nf90_open(trim(avgfile_u),nf90_nowrite,ncidi)
  if(statusi /= nf90_NoErr) call handle_err(statusi)
! 
! ! Read in velocities
  statusi = nf90_inq_varid(ncidi,'u',UVarIdi)
  if(statusi /= nf90_NoErr) call handle_err(statusi)
  statusi = nf90_get_var(ncidi,UVarIdi,u_in,start=(/ 1, 1, 1, itime/))
  if(statusi /= nf90_NoErr) call handle_err(statusi)
  statusi = nf90_get_att(ncidi, UVarIdi, "add_offset", add_offset)
  if(statusi /= nf90_NoErr) add_offset=0.0
  statusi = nf90_get_att(ncidi, UVarIdi, "scale_factor", scale_factor)
  if(statusi /= nf90_NoErr) scale_factor=1.0
  u_in=(u_in*scale_factor)+add_offset
! 
  rmaskuv = rmaski
  do nn=1,Ni
     where ( u_in(:,:,nn) < -20) rmaskuv = 0
     where ( u_in(:,:,nn) < -20) u_in(:,:,nn) = undef
     CALL fill(Lp,Mp,1,Lp,1,Mp,u_in(:,:,nn),tx,critx,cor,mxs,work,error,nvalue)
  end do
! ! Vertical interpolation
  call vert_int(u_in(1:L,1:Mp,:),Mp,L,Ni,No,z_ri(1:L,1:Mp,:),u_out,z_ro(1:L,1:Mp,:),undef,rmaski,rmasko) 
! 
! ! Output to netCDF file
  statuso = nf90_put_var(ncido,UVarIdo,u_out,start=(/1,1,1,otime/))
  if(statuso /= nf90_NoErr) call handle_err(statuso)
  ! 
  statusi = nf90_inq_varid(ncidi,'ubar',UbarVarIdi)
  if(statusi /= nf90_NoErr) then
     !if (Vtransi == 10 .or. Vtransi == 0) then
     ubar_out=0.0
     !uice=0.0
  else
     ! ubar
     !
     ! Read in ubar-component
     !statusi = nf90_inq_varid(ncidi,'ubar',UbarVarIdi)
     !if(statusi /= nf90_NoErr) call handle_err(statusi)
     statusi = nf90_get_var(ncidi,UbarVarIdi,ubar_in)
     if(statusi /= nf90_NoErr) call handle_err(statusi)
     statusi = nf90_get_att(ncidi, UbarVarIdi, "add_offset", add_offset)
     if(statusi /= nf90_NoErr) add_offset=0.0
     statusi = nf90_get_att(ncidi, UbarVarIdi, "scale_factor", scale_factor)
     if(statusi /= nf90_NoErr) scale_factor=1.0
     ubar_in=(ubar_in*scale_factor)+add_offset
     !
     ! Fill in masked-out values 
     scru = 0.
     scru = ubar_in
     WHERE ((scru>1e+30)) scru = undef    !(rmaski<1).or.(rmasko<1).or.
     CALL fill(L,Mp,1,L,1,Mp,scru,tx,critx,cor,mxs,work,error,nvalue)
     ubar_out = scru
     !
     ! uice
     !
     ! Read in uice-component
     statusi = nf90_inq_varid(ncidi,'uice',UiceVarIdi)
     if(statusi /= nf90_NoErr) write(*,*) "Variable uice not on infile..." !call handle_err(statusi)
     statusi = nf90_get_var(ncidi,UiceVarIdi,uice)
     if(statusi /= nf90_NoErr) uice=0.0 !call handle_err(statusi)
     statusi = nf90_get_att(ncidi, UiceVarIdi, "add_offset", add_offset)
     if(statusi /= nf90_NoErr) add_offset=0.0
     statusi = nf90_get_att(ncidi, UiceVarIdi, "scale_factor", scale_factor)
     if(statusi /= nf90_NoErr) scale_factor=1.0
     uice=(uice*scale_factor)+add_offset
     !
     ! Fill in masked-out values 
     scru = 0.
     scru = uice
     WHERE ((scru>1e+30)) scru = undef    !(rmaski<1).or.(rmasko<1).or.
     CALL fill(L,Mp,1,L,1,Mp,scru,tx,critx,cor,mxs,work,error,nvalue)
     uice = scru
     ! Output to netCDF file
     statuso = nf90_put_var(ncido,UiceVarIdo,uice,start=(/1,1,otime/))
     if(statuso /= nf90_NoErr) call handle_err(statuso)
  end if
  ! Output to netCDF file
  statuso = nf90_put_var(ncido,UbarVarIdo,ubar_out,start=(/1,1,otime/))
  if(statuso /= nf90_NoErr) call handle_err(statuso)
  !
  write(*,*) 'Completed u-velocities'
  ! ! ........................................................................
  !close file with u-points
  statusi = nf90_close(ncidi)
  if(statusi /= nf90_NoErr) call handle_err(statusi)
  !
  !read stuff at v-points
  statusi = nf90_open(trim(avgfile_v),nf90_nowrite,ncidi)
  if(statusi /= nf90_NoErr) call handle_err(statusi)
!
  statusi = nf90_inq_varid(ncidi,'v',VVarIdi)
  if(statusi /= nf90_NoErr) call handle_err(statusi)
  statusi = nf90_get_var(ncidi,VVarIdi,v_in,start=(/ 1, 1, 1, itime/))
  if(statusi /= nf90_NoErr) call handle_err(statusi)
  statusi = nf90_get_att(ncidi, VVarIdi, "add_offset", add_offset)
  if(statusi /= nf90_NoErr) add_offset=0.0
  statusi = nf90_get_att(ncidi, VVarIdi, "scale_factor", scale_factor)
  if(statusi /= nf90_NoErr) scale_factor=1.0
  v_in=(v_in*scale_factor)+add_offset
!
  rmaskuv = rmaski
  do nn=1,Ni
     where ( v_in(:,:,nn) < -20) rmaskuv = 0
     where ( v_in(:,:,nn) < -20) v_in(:,:,nn) = undef
     CALL fill(Lp,Mp,1,Lp,1,Mp,v_in(:,:,nn),tx,critx,cor,mxs,work,error,nvalue)
  end do
! ! Vertical interpolation
  call vert_int(v_in(1:Lp,1:M,:),M,Lp,Ni,No,z_ri(1:Lp,1:M,:),v_out,z_ro(1:Lp,1:M,:),undef,rmaskuv,rmasko)
!
! Output to netCDF file
  statuso = nf90_put_var(ncido,VVarIdo,v_out,start=(/1,1,1,otime/))
  if(statuso /= nf90_NoErr) call handle_err(statuso)
!
  statusi = nf90_inq_varid(ncidi,'vbar',VbarVarIdi)
  if(statusi /= nf90_NoErr) then
     !if (Vtransi.ne.10.or.Vtransi.ne.0) then
     vbar_out=0.0
     !vice=0.0
  else
     ! Read in vbar-component
     statusi = nf90_inq_varid(ncidi,'vbar',VbarVarIdi)
     if(statusi /= nf90_NoErr) call handle_err(statusi)
     statusi = nf90_get_var(ncidi,VbarVarIdi,vbar_in)
     if(statusi /= nf90_NoErr) call handle_err(statusi)
     statusi = nf90_get_att(ncidi, VbarVarIdi, "add_offset", add_offset)
     if(statusi /= nf90_NoErr) add_offset=0.0
     statusi = nf90_get_att(ncidi, VbarVarIdi, "scale_factor", scale_factor)
     if(statusi /= nf90_NoErr) scale_factor=1.0
     vbar_in=(vbar_in*scale_factor)+add_offset
     ! fill
     scrv = 0.
     scrv = vbar_in
     WHERE ((scrv>1e+30)) scrv = undef
     CALL fill(Lp,M,1,Lp,1,M,scrv,tx,critx,cor,mxs,work,error,nvalue)
     vbar_out = scrv
     !
     ! vice
     !
     ! Read in vice-component
     statusi = nf90_inq_varid(ncidi,'vice',ViceVarIdi)
     if(statusi /= nf90_NoErr) write(*,*) "Variable vice not on infile..." !call handle_err(statusi)
     statusi = nf90_get_var(ncidi,ViceVarIdi,vice)
     if(statusi /= nf90_NoErr) vice=0.0 !call handle_err(statusi)
     statusi = nf90_get_att(ncidi, ViceVarIdi, "add_offset", add_offset)
     if(statusi /= nf90_NoErr) add_offset=0.0
     statusi = nf90_get_att(ncidi, ViceVarIdi, "scale_factor", scale_factor)
     if(statusi /= nf90_NoErr) scale_factor=1.0
     vice=(vice*scale_factor)+add_offset
     !
     ! Fill in masked-out values 
     scrv = 0.
     scrv = vice
     WHERE ((scru>1e+30)) scru = undef    !(rmaski<1).or.(rmasko<1).or.
     CALL fill(Lp,M,1,Lp,1,M,scrv,tx,critx,cor,mxs,work,error,nvalue)
     vice = scrv
     ! Output to netCDF file
     statuso = nf90_put_var(ncido,ViceVarIdo,vice,start=(/1,1,otime/))
     if(statuso /= nf90_NoErr) call handle_err(statuso)
     !
  end if
  !   
  statuso = nf90_put_var(ncido,VbarVarIdo,vbar_out,start=(/1,1,otime/))
  if(statuso /= nf90_NoErr) call handle_err(statuso)
  ! 
  write(*,*) 'Completed v-velocities'
! ! ........................................................................
  ! ! ----------------------------------------------------------------
!
  ! Write out time
  if (time_in <  1000000 ) then
     tday = time_in
  else !if (Vtransi.ne.10) then
     tday = time_in/86400.  ! Input in ROMS is in s, output should be in days
  end if
  !
  statuso = nf90_put_var(ncido,TimeVarIdo,tday,start=(/otime/))
  if(statuso /= nf90_NoErr) call handle_err(statuso)
!
  write(*,*) 'Wrote time = ',tday,' days, time_in = ', time_in
!
!Reopen file with rho-points
  statusi = nf90_close(ncidi)
  if(statusi /= nf90_NoErr) call handle_err(statusi)
  statusi = nf90_open(trim(avgfile_rho),nf90_nowrite,ncidi)
  if(statusi /= nf90_NoErr) call handle_err(statusi)
!
ENDDO
!
! ----------------------------------------------------------------
!
! ----------------------------------------------------------------
statuso = nf90_sync(ncido)
if(statuso /= nf90_NoErr) call handle_err(statuso)
statuso = nf90_close(ncido)
if(statuso /= nf90_NoErr) call handle_err(statuso)
statusi = nf90_close(ncidi)
if(statusi /= nf90_NoErr) call handle_err(statusi)
!
END PROGRAM romsS2romsS

