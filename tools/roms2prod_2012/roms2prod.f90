program roms2prod

!*********************************************************************
! Program for converting ROMS output NetCDF files                    *
! to CF-compliant NetCDF files, containing information about         *
! grid projection.                                                   *
! The program excludes the boundary points, thereby decreasing       *
! the grid dimension by 2 in both x- and y-direction.                *
! Variables given in u- or v-points are interpolated to rho-points   *
! Variables are also interpolated from s-levels to z-levels as given *
! in the setupfile roms2prod.sup                                     * 
!                                                                    * 
! Ann Kristin Sperrevik <annks@met.no>                               *
! Met.no                                                             *
! April 2010                                                         *  
!                                                                    *
!*********************************************************************
! feb2012: nilsmk@met.no: div progging for operasjonell bruk.

  use setup
  use projection
  use global_atts
  use dimensions
  use netcdf_atts
  use netcdf

  implicit none

  integer :: S, i,j,varcount

  ! variables for NetCDF I/O
  integer :: ncidin, ncidout, status, varid
  integer :: xtype,natts, varidin, varidout
  integer :: ndim
  real, dimension(:,:,:), allocatable :: Z     ! Depth of s-level
  integer, dimension(:), allocatable    :: F1
  real, dimension(:,:), allocatable   :: F2, mask
  !  real, dimension(:,:,:), allocatable :: F3
  real,dimension(:), allocatable :: xc,yc
  integer, dimension(NF_MAX_DIMS) :: dimid
  integer :: n
  character (len=100) :: text1, text2
  real, parameter ::undef=NF_FILL_REAL

  ! read setupfile
  call readsup('roms2prod.sup')
  ! read projectionfile
  call readproj(proj_file)
  call  init_varinfo
  ! Open Infile
  print *, infilename
  status = nf_open(trim(infilename), NF_NOWRITE,ncidin); call check_err(status)

  ! Get infile dimensions
  call  get_infile_dims(ncidin)
  if (nlev == -1) then
    print *,"output on s-levels"
    nz=s_rho
  else
    nz=nlev
  end if

  if (subarea.eq.1) then
     X=xend-xstart+1; Y= yend-ystart+1
     allocate(xc(xi_rho-2),yc(eta_rho-2))
     xc(1)=sw_lon
     yc(1)=sw_lat
     do i=2,xi_rho
        xc(i)=xc(i-1)+dlon
     end do
     do i=2,eta_rho
        yc(i)=yc(i-1)+dlat
     end do
  else
     xstart=2;   ystart=2
     xend=xi_rho-1; yend=eta_rho-1
     X=xi_rho-2; Y=eta_rho-2
     allocate(xc(xi_rho-2),yc(eta_rho-2))
     xc(1)=sw_lon
     yc(1)=sw_lat
     do i=2,xi_rho-2
        xc(i)=xc(i-1)+dlon
     end do
     do i=2,eta_rho-2
        yc(i)=yc(i-1)+dlat
     end do
  end if

  outtime=time

  !  Create outfile
  status = nf_create(trim(outfilename),IOR(NF_CLOBBER,NF_64BIT_OFFSET), ncidout); call check_err(status)

  ! Define global attributes
  call readglobal_atts(global_att_file)

  do n=1,gtext
     read(txt_g_attname(n),*) text1
     read(txt_g_attvalue(n),'(a100)') text2 
     status = nf_put_att_text(ncidout,NF_GLOBAL,trim(text1),len_trim(text2),trim(text2)); call check_err(status)
  end do

  status = nf_put_att_text(ncidout, NF_GLOBAL, "grid_projection", len_trim(proj_name),trim(proj_name)); call check_err(status)

  do n=1,greal
     read(real_g_attname(n),*) text1
     status = nf_put_att_real(ncidout,NF_GLOBAL,trim(text1),NF_DOUBLE,1,real_attvalue(n)); call check_err(status)
  end do

  ! Define dimensions
  call set_outfile_dims(ncidout)
 
  ! Create projection variable, add attributes as given in inputfile projection.in
  status = nf_def_var(ncidout,trim(proj_name),NF_CHAR,0,0,varid); call check_err(status)
 
  do n=1,ntext

     read(txt_attname(n),*) text1
     read(txt_attvalue(n),'(a100)') text2
 
     status = nf_put_att_text(ncidout,varid,trim(text1),len_trim(text2),trim(text2)); call check_err(status)
  end do

  do n=1,nreal
     read(real_attname(n),*) text1
     status = nf_put_att_real(ncidout,varid,trim(text1),NF_DOUBLE,1,real_attvalue(n)); call check_err(status)
  end do

  ! Create standard variables, defining grid coordinates and land/sea masks
  status = nf_def_var(ncidout,"X", NF_REAL, 1, XID, varid); call check_err(status)
  status = nf_put_att_text(ncidout,varid, "axis",1, "X"); call check_err(status)
  status = nf_put_att_text(ncidout,varid, "standard_name",23, "projection_x_coordinate"); call check_err(status)
  status = nf_put_att_text(ncidout,varid,"units", 2, "km"); call  check_err(status);

  status = nf_def_var(ncidout,"Y", NF_REAL, 1, YID, varid); call check_err(status)
  status = nf_put_att_text(ncidout,varid, "axis",1, "Y"); call check_err(status)
  status = nf_put_att_text(ncidout,varid, "standard_name",23, "projection_y_coordinate"); call check_err(status)
  status = nf_put_att_text(ncidout,varid,"units", 2, "km"); call  check_err(status);

  status =  nf_def_var(ncidout,"longitude",NF_REAL,2,(/ XID, YID /),varid); call check_err(status)
  status = nf_put_att_text(ncidout,varid,"standard_name",9,"longitude"); call check_err(status)
  status = nf_put_att_text(ncidout,varid,"units",12, "degrees_east"); call check_err(status)
  status = nf_put_att_text(ncidout,varid,"long_name",9,"longitude"); call check_err(status)

  status =  nf_def_var(ncidout,"latitude",NF_REAL,2,(/ XID, YID /),varid); call check_err(status)
  status = nf_put_att_text(ncidout,varid,"standard_name",8,"latitude"); call check_err(status)
  status = nf_put_att_text(ncidout,varid,"units",13, "degrees_north"); call check_err(status)
  status = nf_put_att_text(ncidout,varid,"long_name",8,"latitude"); call check_err(status)

  status =  nf_def_var(ncidout,"depth",NF_REAL,1, nzID,varid); call check_err(status)
  status = nf_put_att_text(ncidout,varid,"standard_name",5,"depth"); call check_err(status)
  status = nf_put_att_text(ncidout,varid,"units",6, "meters"); call check_err(status)
  status = nf_put_att_text(ncidout,varid,"positive",4,"down"); call check_err(status)
  status = nf_put_att_text(ncidout,varid,"long_name",5,"depth"); call check_err(status)

  status = nf_def_var(ncidout,"time", NF_DOUBLE, 1, outtimeID, varid); call check_err(status)
  status = nf_put_att_text(ncidout,varid, "axis",1, "T"); call check_err(status)
  status = nf_put_att_text(ncidout,varid,"standard_name",4,"time"); call check_err(status)
  status = nf_put_att_text(ncidout,varid,"units",33, "seconds since 1970-01-01 00:00:00"); call check_err(status)
  status = nf_put_att_text(ncidout,varid,"calendar", 9,"gregorian"); call check_err(status)
  status = nf_put_att_text(ncidout,varid,"long_name",25,"time since initialization"); call check_err(status)

  status = nf_def_var(ncidout,"h",NF_REAL,2,(/ XID, YID /),varid); call check_err(status)
  status = nf_put_att_text(ncidout,varid,"standard_name",31,"sea_floor_depth_below_sea_level"); call check_err(status)
  status = nf_put_att_text(ncidout,varid,"units",6, "meters"); call check_err(status)
  status = nf_put_att_text(ncidout,varid,"long_name",16,"model bathymetry"); call check_err(status)
  status = nf_put_att_real(ncidout,varid,"_FillValue",NF_REAL,1,undef); call check_err(status)
  status = nf_put_att_text(ncidout,varid,"grid_mapping",len_trim(proj_name),trim(proj_name)); call check_err(status)
  status = nf_put_att_text(ncidout,varid,"coordinates",18,"longitude latitude"); call check_err(status)

  status = nf_def_var(ncidout,"mask",NF_REAL,2,(/ XID, YID /),varid); call check_err(status)
  status = nf_put_att_text(ncidout,varid,"standard_name",9,"area_type"); call check_err(status)
  status = nf_put_att_text(ncidout,varid,"units",0, ""); call check_err(status)
  status = nf_put_att_text(ncidout,varid,"long_name",9,"land mask"); call check_err(status)
  status = nf_put_att_text(ncidout,varid,"option_0",4, "land"); call check_err(status)
  status = nf_put_att_text(ncidout,varid,"option_1",5,"water"); call check_err(status)
  status = nf_put_att_text(ncidout,varid,"grid_mapping",len_trim(proj_name),trim(proj_name)); call check_err(status)
  status = nf_put_att_text(ncidout,varid,"coordinates",18,"longitude latitude"); call check_err(status)

  ! Define variables as specified in roms2prod.sup
  do  varcount=1, nvar
     !call defvar(vars(varcount),ncidin,ncidout,proj_name)
     call defvar(vars(varcount),ncidin,ncidout)
  end do

  status = nf_enddef(ncidout);  call check_err(status)

  allocate(F2(X,Y),F1(time),mask(X,Y))
  ! Add standard variables to output file
  status = nf_inq_varid(ncidout,"depth",varid); call check_err(status)
  status = nf_put_var_real(ncidout,varid,zlev); call check_err(status)
  status = nf_inq_varid(ncidin,"lon_rho",varid); call check_err(status)
  status = nf_get_vara_real(ncidin,varid, (/ xstart, ystart /), (/ X, Y /), F2); call check_err(status)
  status = nf_inq_varid(ncidout,"longitude",varid); call check_err(status)
  status = nf_put_var_real(ncidout,varid,F2); call check_err(status)
  status = nf_inq_varid(ncidin,"lat_rho",varid); call check_err(status)
  status = nf_get_vara_real(ncidin,varid, (/ xstart, ystart /), (/ X, Y /), F2); call check_err(status)
  status = nf_inq_varid(ncidout,"latitude",varid); call check_err(status)
  status = nf_put_var_real(ncidout,varid,F2); call check_err(status)

  status = nf_inq_varid(ncidin,"mask_rho",varid); call check_err(status)
  status = nf_get_vara_real(ncidin,varid, (/ xstart, ystart /), (/ X, Y /), F2); call check_err(status)
  status = nf_inq_varid(ncidout,"mask",varid); call check_err(status)
  status = nf_put_var_real(ncidout,varid,F2); call check_err(status)
  status = nf_inq_varid(ncidin,"h",varid); call check_err(status)
  status = nf_get_vara_real(ncidin,varid, (/ xstart, ystart /), (/ X, Y /), F2); call check_err(status)
  status = nf_inq_varid(ncidout,"h",varid); call check_err(status)
  status = nf_put_var_real(ncidout,varid,F2); call check_err(status)
  status = nf_inq_varid(ncidin,"ocean_time",varid); call check_err(status)
  !status = nf_get_var_real(ncidin,varid, F1); call check_err(status)
  status = nf90_get_var(ncidin,varid, F1); call check_err(status)
  status = nf_inq_varid(ncidout,"time",varid); call check_err(status)
  !status = nf_put_var_real(ncidout,varid,F1); call check_err(status)
  status = nf90_put_var(ncidout,varid,F1); call check_err(status)
  print *,F1

  deallocate(F1,F2)
  status = nf_inq_varid(ncidout,"X",varid); call check_err(status)
  ! Make sure Y and X axis are correct when output on subarea
  if (subarea.eq.1) then
     status = nf_put_var_real(ncidout,varid,xc(xstart-1:xend-1)); call check_err(status)
  else
     status = nf_put_var_real(ncidout,varid,xc); call check_err(status)
  end if

  status = nf_inq_varid(ncidout,"Y",varid); call check_err(status)

  if (subarea.eq.1) then
     status = nf_put_var_real(ncidout,varid,yc(ystart-1:yend-1)); call check_err(status)
  else
     status = nf_put_var_real(ncidout,varid,yc); call check_err(status)
  end if

  ! Add values for variables specified in roms2prod.sup
  do  varcount=1, nvar
     call addvar(vars(varcount),ncidin,ncidout)
  end do

  status = nf_close(ncidout);  call check_err(status)
  status = nf_close(ncidin);  call check_err(status)
  print *, "roms2prod executed successfully. Results are in ", trim(outfilename)

end program roms2prod
