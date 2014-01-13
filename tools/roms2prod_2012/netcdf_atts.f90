module netcdf_atts
!*****************************************************
! In this module attributes for the output variables !
! are specified. If a new variable type is to be     !
! included in the output files, it first needs to be !
! defined here                                       !
! Currently set up for output of these variables:    !
! zeta, u, v, ubar, vbar, salt, temp, rho, uice,     !
! vice, aice, hice, ageice, hsn                      !
!*****************************************************
  use projection
  type varinfo
     character (len=20)  :: rname         ! name used in ROMS output files
     character (len=20)  :: vname         ! variabel name for output file
     character (len=80)  :: standard_name ! Standard name, CF-conventions
     character (len=80)  :: long_name     ! A descriptive name
     character (len=20)  :: units         ! units, CF...
     real                :: max_val       ! max allowed value
     real                :: min_val       ! min allowed value
     real                :: scale_factor  ! calculated from max/min
     real                :: add_offset    ! calculated from max/min
     integer*2           :: fillvalue     ! Value for no data
     integer             :: num_dim       ! number of dimensions for variable
     character (len=10)  :: time          ! name of var specifying time of variable 
     character (len=80)  :: coordinates   ! coordinates of variable
     character (len=80)  :: grid_mapping  ! grid projection of variable
  end type varinfo

  integer, parameter :: maxvar=50
  type (varinfo) :: var_info(maxvar)

contains
  subroutine init_varinfo()
    integer irec, nbits

    nbits=2**16

    ! Default, same for all variables
    var_info(:)%time='time'
    var_info(:)%coordinates='longitude latitude'
    var_info(:)%grid_mapping=trim(proj_name)

    ! Variables are specified below
    irec=1
    var_info(irec)%rname='zeta'
    var_info(irec)%vname='zeta'
    var_info(irec)%standard_name='sea_surface_elevation'
    var_info(irec)%long_name='Sea Surface height'
    var_info(irec)%units='meters'
    var_info(irec)%max_val=20.
    var_info(irec)%min_val=-20.
    var_info(irec)%add_offset = (var_info(irec)%max_val + var_info(irec)%min_val)/2.
    var_info(irec)%scale_factor = (var_info(irec)%max_val - var_info(irec)%min_val)/(nbits-10.)
    var_info(irec)%fillvalue = -(nbits-2)/2
    var_info(irec)%num_dim = 3

    irec=irec+1
    var_info(irec)%rname='zeta_detided'
    var_info(irec)%vname='zeta_detided'
    var_info(irec)%standard_name='sea_surface_elevation_detided_smoothed'
    var_info(irec)%long_name='Detided Sea Surface height - Stormsurge - Filetered'
    var_info(irec)%units='meters'
    var_info(irec)%max_val=20.
    var_info(irec)%min_val=-20.
    var_info(irec)%add_offset = (var_info(irec)%max_val + var_info(irec)%min_val)/2.
    var_info(irec)%scale_factor = (var_info(irec)%max_val - var_info(irec)%min_val)/(nbits-10.)
    var_info(irec)%fillvalue = -(nbits-2)/2
    var_info(irec)%num_dim = 3

    irec=irec+1
    var_info(irec)%rname='zeta_detided_raw'
    var_info(irec)%vname='zeta_detided_raw'
    var_info(irec)%standard_name='sea_surface_elevation_detided_raw'
    var_info(irec)%long_name='Detided Sea Surface height - Stormsurge - Raw'
    var_info(irec)%units='meters'
    var_info(irec)%max_val=20.
    var_info(irec)%min_val=-20.
    var_info(irec)%add_offset = (var_info(irec)%max_val + var_info(irec)%min_val)/2.
    var_info(irec)%scale_factor = (var_info(irec)%max_val - var_info(irec)%min_val)/(nbits-10.)
    var_info(irec)%fillvalue = -(nbits-2)/2
    var_info(irec)%num_dim = 3

    irec=irec+1
    var_info(irec)%rname='u'
    var_info(irec)%vname='u'
    var_info(irec)%standard_name= "x_sea_water_velocity"
    var_info(irec)%long_name="u-momentum component"
    var_info(irec)%units='meter second-1'
    var_info(irec)%max_val=10.
    var_info(irec)%min_val=-10.
    var_info(irec)%add_offset = (var_info(irec)%max_val + var_info(irec)%min_val)/2.
    var_info(irec)%scale_factor = (var_info(irec)%max_val - var_info(irec)%min_val)/(nbits-10.)
    var_info(irec)%fillvalue = -(nbits-2)/2
    var_info(irec)%num_dim = 4

    irec=irec+1
    var_info(irec)%rname='v'
    var_info(irec)%vname='v'
    var_info(irec)%standard_name= "y_sea_water_velocity"
    var_info(irec)%long_name="v-momentum component"
    var_info(irec)%units='meter second-1'
    var_info(irec)%max_val=10.
    var_info(irec)%min_val=-10.
    var_info(irec)%add_offset = (var_info(irec)%max_val + var_info(irec)%min_val)/2.
    var_info(irec)%scale_factor = (var_info(irec)%max_val - var_info(irec)%min_val)/(nbits-10.)
    var_info(irec)%fillvalue = -(nbits-2)/2
    var_info(irec)%num_dim = 4

    irec=irec+1
    var_info(irec)%rname='ubar'
    var_info(irec)%vname='ubar'
    var_info(irec)%standard_name= "barotropic_sea_water_x_velocity"
    var_info(irec)%long_name= "vertically integrated u-momentum component"
    var_info(irec)%units='meter second-1'
    var_info(irec)%max_val=5.
    var_info(irec)%min_val=-5.
    var_info(irec)%add_offset = (var_info(irec)%max_val + var_info(irec)%min_val)/2.
    var_info(irec)%scale_factor = (var_info(irec)%max_val - var_info(irec)%min_val)/(nbits-10.)
    var_info(irec)%fillvalue = -(nbits-2)/2
    var_info(irec)%num_dim = 3

    irec=irec+1
    var_info(irec)%rname='vbar'
    var_info(irec)%vname='vbar'
    var_info(irec)%standard_name= "barotropic_sea_water_y_velocity"
    var_info(irec)%long_name= "vertically integrated v-momentum component"
    var_info(irec)%units='meter second-1'
    var_info(irec)%max_val=5.
    var_info(irec)%min_val=-5.
    var_info(irec)%add_offset = (var_info(irec)%max_val + var_info(irec)%min_val)/2.
    var_info(irec)%scale_factor = (var_info(irec)%max_val - var_info(irec)%min_val)/(nbits-10.)
    var_info(irec)%fillvalue = -(nbits-2)/2
    var_info(irec)%num_dim = 3

    irec=irec+1
    var_info(irec)%rname='temp'
    var_info(irec)%vname='temperature'
    var_info(irec)%standard_name=  "sea_water_potential_temperature"
    var_info(irec)%long_name="potential temperature"
    var_info(irec)%units='Celsius'
    var_info(irec)%max_val=45.
    var_info(irec)%min_val=-3.
    var_info(irec)%add_offset = (var_info(irec)%max_val + var_info(irec)%min_val)/2.
    var_info(irec)%scale_factor = (var_info(irec)%max_val - var_info(irec)%min_val)/(nbits-10.)
    var_info(irec)%fillvalue = -(nbits-2)/2
    var_info(irec)%num_dim = 4

    irec=irec+1
    var_info(irec)%rname='salt'
    var_info(irec)%vname='salinity'
    var_info(irec)%standard_name=  "sea_water_salinity"
    var_info(irec)%long_name="salinity"
    var_info(irec)%units='1e-3'
    var_info(irec)%max_val=50.
    var_info(irec)%min_val=0.
    var_info(irec)%add_offset = (var_info(irec)%max_val + var_info(irec)%min_val)/2.
    var_info(irec)%scale_factor = (var_info(irec)%max_val - var_info(irec)%min_val)/(nbits-10.)
    var_info(irec)%fillvalue = -(nbits-2)/2
    var_info(irec)%num_dim = 4


    irec=irec+1
    var_info(irec)%rname='rho'
    var_info(irec)%vname='density'
    var_info(irec)%standard_name=  "sea_water_density"
    var_info(irec)%long_name="density anomaly"
    var_info(irec)%units='kilogram meter-3'
    var_info(irec)%max_val=50.
    var_info(irec)%min_val=0.
    var_info(irec)%add_offset = (var_info(irec)%max_val + var_info(irec)%min_val)/2.
    var_info(irec)%scale_factor = (var_info(irec)%max_val - var_info(irec)%min_val)/(nbits-10.)
    var_info(irec)%fillvalue = -(nbits-2)/2
    var_info(irec)%num_dim = 4


    irec=irec+1
    var_info(irec)%rname='uice'
    var_info(irec)%vname='uice'
    var_info(irec)%standard_name=  "sea_ice_x_velocity"
    var_info(irec)%long_name=  "u-component of sea ice velocity"
    var_info(irec)%units='meter second-1'
    var_info(irec)%max_val=2.
    var_info(irec)%min_val=-2.
    var_info(irec)%add_offset = (var_info(irec)%max_val + var_info(irec)%min_val)/2.
    var_info(irec)%scale_factor = (var_info(irec)%max_val - var_info(irec)%min_val)/(nbits-10.)
    var_info(irec)%fillvalue = -(nbits-2)/2
    var_info(irec)%num_dim = 3

    irec=irec+1
    var_info(irec)%rname='vice'
    var_info(irec)%vname='vice'
    var_info(irec)%standard_name=  "sea_ice_y_velocity"
    var_info(irec)%long_name=  "v-component of sea ice velocity"
    var_info(irec)%units='meter second-1'
    var_info(irec)%max_val=2.
    var_info(irec)%min_val=-2.
    var_info(irec)%add_offset = (var_info(irec)%max_val + var_info(irec)%min_val)/2.
    var_info(irec)%scale_factor = (var_info(irec)%max_val - var_info(irec)%min_val)/(nbits-10.)
    var_info(irec)%fillvalue = -(nbits-2)/2
    var_info(irec)%num_dim = 3

    irec=irec+1
    var_info(irec)%rname='aice'
    var_info(irec)%vname='aice'
    var_info(irec)%standard_name= "sea_ice_area_fraction"
    var_info(irec)%long_name=   "fraction of cell covered by ice"
    var_info(irec)%units=''
    var_info(irec)%max_val=1.01
    var_info(irec)%min_val=-0.01
    var_info(irec)%add_offset = (var_info(irec)%max_val + var_info(irec)%min_val)/2.
    var_info(irec)%scale_factor = (var_info(irec)%max_val - var_info(irec)%min_val)/(nbits-10.)
    var_info(irec)%fillvalue = -(nbits-2)/2
    var_info(irec)%num_dim = 3

    irec=irec+1
    var_info(irec)%rname='hice'
    var_info(irec)%vname='hice'
    var_info(irec)%standard_name= "sea_ice_thickness"
    var_info(irec)%long_name=  "average ice thickness in cell"
    var_info(irec)%units='meter'
    var_info(irec)%max_val=15.
    var_info(irec)%min_val=-0.01
    var_info(irec)%add_offset = (var_info(irec)%max_val + var_info(irec)%min_val)/2.
    var_info(irec)%scale_factor = (var_info(irec)%max_val - var_info(irec)%min_val)/(nbits-10.)
    var_info(irec)%fillvalue = -(nbits-2)/2
    var_info(irec)%num_dim = 3

    irec=irec+1
    var_info(irec)%rname='hsn'
    var_info(irec)%vname='hsn'
    var_info(irec)%standard_name=  "surface_snow_thickness"
    var_info(irec)%long_name= "average thickness of snow cover in cell"
    var_info(irec)%units='meter'
    var_info(irec)%max_val=15.
    var_info(irec)%min_val=-0.01
    var_info(irec)%add_offset = (var_info(irec)%max_val + var_info(irec)%min_val)/2.
    var_info(irec)%scale_factor = (var_info(irec)%max_val - var_info(irec)%min_val)/(nbits-10.)
    var_info(irec)%fillvalue = -(nbits-2)/2
    var_info(irec)%num_dim = 3


    irec=irec+1
    var_info(irec)%rname='ageice'
    var_info(irec)%vname='ageice'
    var_info(irec)%standard_name=   "sea_ice_age"
    var_info(irec)%long_name=  "age of the ice"
    var_info(irec)%units='days'
    var_info(irec)%max_val=1000.*365.
    var_info(irec)%min_val=-0.01
    var_info(irec)%add_offset = (var_info(irec)%max_val + var_info(irec)%min_val)/2.
    var_info(irec)%scale_factor = (var_info(irec)%max_val - var_info(irec)%min_val)/(nbits-10.)
    var_info(irec)%fillvalue = -(nbits-2)/2
    var_info(irec)%num_dim = 3


  end subroutine init_varinfo
end module netcdf_atts
