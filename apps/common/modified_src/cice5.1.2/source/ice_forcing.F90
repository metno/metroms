!  SVN:$Id: ice_forcing.F90 925 2015-03-04 00:34:27Z eclare $
!=======================================================================
!
! Reads and interpolates forcing data for atmosphere and ocean quantities.
!
! authors: Elizabeth C. Hunke and William H. Lipscomb, LANL
!
! 2004 WHL: Block structure added
! 2005 WHL: ECMWF option added
! 2006 ECH: LY option added
! 2006 WHL: Module name changed from ice_flux_in
! 2006 ECH: Fixed bugs, rearranged routines, edited comments, etc.
!           Added NCAR ocean forcing file
!           Converted to free source form (F90)
! 2007: netcdf version of read_data added by Alison McLaren, Met Office
!
      module ice_forcing

      use ice_kinds_mod
      use ice_blocks, only: nx_block, ny_block
      use ice_domain_size, only: ncat, max_blocks, nx_global, ny_global, nilyr
      use ice_communicate, only: my_task, master_task
      use ice_calendar, only: istep, istep1, time, time_forc, year_init, &
                              sec, mday, month, nyr, yday, daycal, dayyr, &
                              daymo, days_per_year, idate, idate0
      use ice_fileunits, only: nu_diag, nu_forcing
      use ice_atmo, only: calc_strair
      use ice_exit, only: abort_ice
      use ice_read_write, only: ice_open, ice_read, &
                                ice_open_nc, ice_read_nc, ice_close_nc
      use ice_therm_shared, only: ktherm

      implicit none
      private
      public :: init_forcing_atmo, init_forcing_ocn, &
                get_forcing_atmo, get_forcing_ocn, &
                read_clim_data, read_clim_data_nc, &
                interpolate_data, interp_coeff_monthly, & !Pedro stuff begins
                init_forcing_bry, get_forcing_bry !Pedro stuff ends
      save

      integer (kind=int_kind), public :: &
         ycycle          , & ! number of years in forcing cycle
         fyear_init      , & ! first year of data in forcing cycle
         fyear           , & ! current year in forcing cycle
         fyear_final         ! last year in cycle

      character (char_len_long) :: &        ! input data file names
         height_file, &
          uwind_file, &
          vwind_file, &
           wind_file, &
          strax_file, &
          stray_file, &
           potT_file, &
           tair_file, &
          humid_file, &
           rhoa_file, &
            fsw_file, &
            flw_file, &
           rain_file, &
            sst_file, &
            sss_file, &
           pslv_file, &
         sublim_file, &
           snow_file, &
           bry_file   !Pedro changes

      character (char_len_long), dimension(ncat) :: &  ! input data file names
        topmelt_file, &
        botmelt_file

      real (kind=dbl_kind) :: &
           c1intp, c2intp , & ! interpolation coefficients
           ftime              ! forcing time (for restart)

      integer (kind=int_kind) :: &
!METNO START       
           oldrecnum = 0 ,&     ! old record number (save between steps)
           oldrecnum6 = 0, &
           oldrecnum12 = 0
!METNO END 

      real (kind=dbl_kind), dimension(nx_block,ny_block,max_blocks) :: &
          cldf                ! cloud fraction

      real (kind=dbl_kind), dimension(nx_block,ny_block,2,max_blocks) :: &
            fsw_data, & ! field values at 2 temporal data points
           cldf_data, &
          fsnow_data, &
           Tair_data, &
           uatm_data, &
           vatm_data, &
           wind_data, &
          strax_data, &
          stray_data, &
             Qa_data, &
           rhoa_data, &
           potT_data, &
           zlvl_data, &
            flw_data, &
            sst_data, &
            sss_data, & 
           uocn_data, &
           vocn_data, &
         sublim_data, &
          frain_data

      real (kind=dbl_kind), & 
           dimension(nx_block,ny_block,2,max_blocks,ncat) :: &
        topmelt_data, &
        botmelt_data

      character(char_len), public :: & 
         atm_data_format, & ! 'bin'=binary or 'nc'=netcdf
         ocn_data_format, & ! 'bin'=binary or 'nc'=netcdf
         atm_data_type, & ! 'default', 'monthly', 'ncar', 
                          ! 'LYq' or 'hadgem' or 'oned'
         sss_data_type, & ! 'default', 'clim', 'ncar', 'oned'
         sst_data_type, & ! 'default', 'clim', 'ncar', 'oned',
                          ! 'hadgem_sst' or 'hadgem_sst_uvocn'
         precip_units,  & ! 'mm_per_month', 'mm_per_sec', 'mks','m_per_12hr'
         !Pedro changes begin
         sea_ice_bry      ! 'default', 'daily'
         !Pedro changes end
 
      character(char_len_long), public :: & 
         atm_data_dir , & ! top directory for atmospheric data
         ocn_data_dir , & ! top directory for ocean data
         oceanmixed_file, &  ! file name for ocean forcing data
         !Pedro changes begin
         sea_ice_bry_dir
         !Pedro changes end

      integer (kind=int_kind), parameter :: & 
         nfld = 8   ! number of fields to search for in forcing file

      ! as in the dummy atm (latm)
      real (kind=dbl_kind), parameter, public :: &
         frcvdr = 0.28_dbl_kind, & ! frac of incoming sw in vis direct band
         frcvdf = 0.24_dbl_kind, & ! frac of incoming sw in vis diffuse band
         frcidr = 0.31_dbl_kind, & ! frac of incoming sw in near IR direct band
         frcidf = 0.17_dbl_kind    ! frac of incoming sw in near IR diffuse band

      real (kind=dbl_kind), &
       dimension (nx_block,ny_block,max_blocks,nfld,12) :: & 
         ocn_frc_m   ! ocn data for 12 months

      logical (kind=log_kind), public :: &
         restore_sst                 ! restore sst if true

      integer (kind=int_kind), public :: &
         trestore                    ! restoring time scale (days)

      real (kind=dbl_kind), public :: & 
         trest                       ! restoring time scale (sec)

      logical (kind=log_kind), public :: &
         dbug             ! prints debugging output if true

      !Arrays to store sea-ice boundary values per ice category

      !Boundary arrays 
      
      real (kind=dbl_kind), &
         dimension(nx_block,ny_block,ncat,max_blocks),public :: &
         aicen_bry, &     ! concentration of ice  
         vicen_bry, &     ! volume per unit area of ice          (m) 
         vsnon_bry, &     ! volume per unit area of snow         (m)
         Tsfc_bry,  &     ! ice/snow surface temperature         (oC)
         alvln_bry, &     ! concentration of level ice
         vlvln_bry, &     ! volume per unit of area of level ice (m)
         apondn_bry,&     ! melt pond fraction category 
         hpondn_bry,&     ! melt pond depth category (m) 
         ipondn_bry,&     ! mean pond ice thickness over sea ice (m)
         !hbrine_bry,&     ! brine height (m)
         !fbrine_bry,&     !
         iage_bry         ! ie age  
      !real (kind=dbl_kind), &
      !   dimension(nx_block,ny_block,max_blocks),public :: &
      !   Tsfc_bry         ! ice/snow surface temperature         (oC)

      real (kind=dbl_kind), &
         dimension(nx_block,ny_block,nilyr,ncat,max_blocks),public :: &
         Tinz_bry, &     ! sea-ice innner temperature  (CICE grid layers) 
         Sinz_bry        ! sea-ice inner bulk salinity (CICE grid layers)  
!  -    
!  Metroms
!  "Calendar"/"dictionary" to keep track of foricng files and timesteps

      real (kind=dbl_kind) :: metroms_offset

      ! index pair (+ readflag)
      integer (kind=int_kind), dimension(3) ::  &
           metroms_ip_vwind = 0, &
           metroms_ip_uwind = 0, &
           metroms_ip_tair = 0, &
           metroms_ip_rhoa = 0, &
           metroms_ip_rain = 0, &
           metroms_ip_humid = 0, &
           metroms_ip_cldf = 0


      integer (kind=int_kind), dimension(:), allocatable :: &
           metroms_index_vwind, &
           metroms_index_uwind, &
           metroms_index_tair, &
           metroms_index_rhoa, &
           metroms_index_rain, &
           metroms_index_humid, &
           metroms_index_cldf

      character (char_len_long), dimension (:), allocatable :: &
           metroms_fname_vwind, &
           metroms_fname_uwind, &
           metroms_fname_tair, &
           metroms_fname_rhoa, &
           metroms_fname_rain, &
           metroms_fname_humid, &
           metroms_fname_cldf
        
      real (kind=dbl_kind), dimension(:), allocatable :: &
           metroms_dates_vwind, &
           metroms_dates_uwind, &
           metroms_dates_tair, &
           metroms_dates_rhoa, &
           metroms_dates_rain, &
           metroms_dates_humid, &
           metroms_dates_cldf

      interface read_bry_ice_data_nc
        module procedure read_bry_ice_data_nc_2D, &
                         read_bry_ice_data_nc_3D, &
                         read_bry_ice_data_nc_4D
                         
      end interface 
!=======================================================================

      contains

!=======================================================================

      subroutine init_forcing_atmo

! Determine the current and final year of the forcing cycle based on
! namelist input; initialize the atmospheric forcing data filenames.

      fyear       = fyear_init + mod(nyr-1,ycycle) ! current year
      fyear_final = fyear_init + ycycle - 1 ! last year in forcing cycle

      if (trim(atm_data_type) /= 'default' .and. &
                          my_task == master_task) then
         write (nu_diag,*) ' Initial forcing data year = ',fyear_init
         write (nu_diag,*) ' Final   forcing data year = ',fyear_final
      endif

    !-------------------------------------------------------------------
    ! Get filenames for input forcing data     
    !-------------------------------------------------------------------

      ! default forcing values from init_flux_atm
      if (trim(atm_data_type) == 'ncar') then
         call NCAR_files(fyear)
      elseif (trim(atm_data_type) == 'LYq') then
         call LY_files(fyear)
      elseif (trim(atm_data_type) == 'hadgem') then
         call hadgem_files(fyear)
!METNO START
      elseif (trim(atm_data_type) == 'ecmwf') then
         call ecmwf_files(fyear)
!METNO END
      elseif (trim(atm_data_type) == 'monthly') then
         call monthly_files(fyear)
      elseif (trim(atm_data_type) == 'oned') then
         call oned_files(fyear)
      elseif (trim(atm_data_type) == 'metroms') then
         call metroms_init
      endif

      end subroutine init_forcing_atmo

!=======================================================================

      subroutine init_forcing_ocn(dt)

! Set sea surface salinity and freezing temperature to annual mean value 
!  using a 12-month climatology.
! Read sst data for current month, and adjust sst based on freezing
! temperature.  No interpolation in time.

! Note: SST is subsequently prognosed if CICE is run with a mixed layer
! ocean (oceanmixed_ice = T), and can be restored to data 
! (restore_sst = T). SSS is not prognosed by CICE. 

      use ice_blocks, only: nx_block, ny_block
      use ice_constants, only: c0, c12, c1000, secday, depressT, &
          field_loc_center, field_type_scalar
      use ice_domain, only: nblocks
      use ice_domain_size, only: max_blocks
      use ice_flux, only: sss, sst, Tf
      use ice_zbgc_shared, only: restore_bgc
#ifdef ncdf
      use netcdf
#endif

      real (kind=dbl_kind), intent(in) :: &
         dt                   ! time step

      ! local variables

      integer (kind=int_kind) :: &
         i, j, iblk       , & ! horizontal indices
         k                , & ! month index
         fid              , & ! file id for netCDF file 
         nbits

      logical (kind=log_kind) :: diag

      character (char_len) :: & 
         fieldname            ! field name in netcdf file

      real (kind=dbl_kind), dimension (nx_block,ny_block,max_blocks) :: &
         work1

      nbits = 64              ! double precision data

      if (restore_sst .or. restore_bgc) then
         if (trestore == 0) then
            trest = dt        ! use data instantaneously
         else
            trest = real(trestore,kind=dbl_kind) * secday ! seconds
         endif
      endif

    !-------------------------------------------------------------------
    ! Sea surface salinity (SSS)
    ! initialize to annual climatology created from monthly data
    !-------------------------------------------------------------------

      if (trim(sss_data_type) == 'clim') then

         sss_file = trim(ocn_data_dir)//'sss.mm.100x116.da' ! gx3 only

         if (my_task == master_task) then
            write (nu_diag,*) ' '
            write (nu_diag,*) 'SSS climatology computed from:'
            write (nu_diag,*) trim(sss_file)
         endif

         if (my_task == master_task) &
              call ice_open (nu_forcing, sss_file, nbits)

         sss(:,:,:) = c0

         do k = 1,12            ! loop over 12 months
            call ice_read (nu_forcing, k, work1, 'rda8', dbug, &
                           field_loc_center, field_type_scalar)
            !$OMP PARALLEL DO PRIVATE(iblk,i,j)
            do iblk = 1, nblocks
               do j = 1, ny_block
               do i = 1, nx_block
                  sss(i,j,iblk) = sss(i,j,iblk) + work1(i,j,iblk)
               enddo
               enddo
            enddo
            !$OMP END PARALLEL DO
         enddo                  ! k

         !$OMP PARALLEL DO PRIVATE(iblk,i,j)
         do iblk = 1, nblocks
            do j = 1, ny_block
            do i = 1, nx_block
               sss(i,j,iblk) = sss(i,j,iblk) / c12   ! annual average
               sss(i,j,iblk) = max(sss(i,j,iblk),c0)
            enddo
            enddo
         enddo
         !$OMP END PARALLEL DO

         call ocn_freezing_temperature

         if (my_task == master_task) close(nu_forcing)

      endif                     ! sss_data_type

    !-------------------------------------------------------------------
    ! Sea surface temperature (SST)
    ! initialize to data for current month
    !-------------------------------------------------------------------

      if (trim(sst_data_type) == 'clim') then

         if (nx_global == 320) then ! gx1
            sst_file = trim(ocn_data_dir)//'sst_clim_hurrell.dat'
         else                   ! gx3
            sst_file = trim(ocn_data_dir)//'sst.mm.100x116.da'
         endif

         if (my_task == master_task) then
            write (nu_diag,*) ' '
            write (nu_diag,*) 'Initial SST file:', trim(sst_file)
         endif

         if (my_task == master_task) &
              call ice_open (nu_forcing, sst_file, nbits)

         call ice_read (nu_forcing, month, sst, 'rda8', dbug, &
                        field_loc_center, field_type_scalar)

         if (my_task == master_task) close(nu_forcing)

         ! Make sure sst is not less than freezing temperature Tf
         !$OMP PARALLEL DO PRIVATE(iblk,i,j)
         do iblk = 1, nblocks
            do j = 1, ny_block
            do i = 1, nx_block
               sst(i,j,iblk) = max(sst(i,j,iblk),Tf(i,j,iblk))
            enddo
            enddo
         enddo
         !$OMP END PARALLEL DO

      endif                     ! init_sst_data


      if (trim(sst_data_type) == 'hadgem_sst' .or.  &
          trim(sst_data_type) == 'hadgem_sst_uvocn') then

       	 diag = .true.   ! write diagnostic information 

         sst_file = trim (ocn_data_dir)//'MONTHLY/sst.1997.nc'

       	 if (my_task == master_task) then

             write (nu_diag,*) ' '
             write (nu_diag,*) 'Initial SST file:', trim(sst_file)

             call ice_open_nc(sst_file,fid)

         endif
 
         fieldname='sst'
         call ice_read_nc(fid,month,fieldname,sst,diag)

         if (my_task == master_task) call ice_close_nc(fid)  

         ! Make sure sst is not less than freezing temperature Tf
         !$OMP PARALLEL DO PRIVATE(iblk,i,j)
         do iblk = 1, nblocks
            do j = 1, ny_block
            do i = 1, nx_block
               sst(i,j,iblk) = max(sst(i,j,iblk),Tf(i,j,iblk))
            enddo
            enddo
         enddo
         !$OMP END PARALLEL DO

      endif                        ! sst_data_type

      if (trim(sst_data_type) == 'ncar' .or.  &
          trim(sss_data_type) == 'ncar') then
!         call ocn_data_ncar_init
         call ocn_data_ncar_init_3D
      endif

      end subroutine init_forcing_ocn

!=======================================================================

      subroutine ocn_freezing_temperature

 ! Compute ocean freezing temperature Tf based on tfrz_option
 ! 'minus1p8'         Tf = -1.8 C (default)
 ! 'linear_salt'      Tf = -depressT * sss
 ! 'mushy'            Tf conforms with mushy layer thermo (ktherm=2)

      use ice_blocks, only: nx_block, ny_block
      use ice_constants, only: depressT, c1000
      use ice_domain, only: nblocks
      use ice_flux, only: sss, Tf
      use ice_ocean, only: tfrz_option

      ! local variables

      integer (kind=int_kind) :: &
         i, j, iblk           ! horizontal indices

      !$OMP PARALLEL DO PRIVATE(iblk,i,j)
      do iblk = 1, nblocks
         do j = 1, ny_block
         do i = 1, nx_block
            if (trim(tfrz_option) == 'mushy') then
               Tf(i,j,iblk) =  sss(i,j,iblk) / (-18.48_dbl_kind &
                            + ((18.48_dbl_kind/c1000) * sss(i,j,iblk)))
            elseif (trim(tfrz_option) == 'linear_salt') then
               Tf(i,j,iblk) = -depressT * sss(i,j,iblk) ! deg C
            else
               Tf(i,j,iblk) = -1.8_dbl_kind
            endif
         enddo
         enddo
      enddo
      !$OMP END PARALLEL DO

      end subroutine ocn_freezing_temperature

!=======================================================================

      subroutine get_forcing_atmo

! Get atmospheric forcing data and interpolate as necessary

      use ice_blocks, only: block, get_block
      use ice_constants, only: field_loc_center, field_type_scalar
      use ice_boundary, only: ice_HaloUpdate
      use ice_domain, only: nblocks, blocks_ice, halo_info
      use ice_flux, only: Tair, fsw, flw, frain, fsnow, Qa, rhoa, &
          uatm, vatm, strax, stray, zlvl, wind, swvdr, swvdf, swidr, swidf, &
          potT, sst
      use ice_state, only: aice, trcr, nt_Tsfc
      use ice_grid, only: ANGLET, hm
      use ice_timers, only: ice_timer_start, ice_timer_stop, timer_bound

      integer (kind=int_kind) :: &
         iblk, &              ! block index
         ilo,ihi,jlo,jhi      ! beginning and end of physical domain

      type (block) :: &
         this_block           ! block information for current block
      
      fyear = fyear_init + mod(nyr-1,ycycle)  ! current year
      if (trim(atm_data_type) /= 'default' .and. istep <= 1 &
                   .and. my_task == master_task) then
         write (nu_diag,*) ' Current forcing data year = ',fyear
      endif

      ftime = time         ! forcing time
      time_forc = ftime    ! for restarting

    !-------------------------------------------------------------------
    ! Read and interpolate atmospheric data
    !-------------------------------------------------------------------

      if (trim(atm_data_type) == 'ncar') then
         call ncar_data
      elseif (trim(atm_data_type) == 'LYq') then
         call LY_data
      elseif (trim(atm_data_type) == 'hadgem') then
         call hadgem_data
!METNO START
      elseif (trim(atm_data_type) == 'ecmwf') then
         call ecmwf_data
!METNO END
      elseif (trim(atm_data_type) == 'monthly') then
         call monthly_data
      elseif (trim(atm_data_type) == 'oned') then
         call oned_data
      elseif (trim(atm_data_type) == 'metroms') then
         call metroms_data
      else    ! default values set in init_flux
         return
      endif
    !-------------------------------------------------------------------
    ! Convert forcing data to fields needed by ice model
    !-------------------------------------------------------------------

      !$OMP PARALLEL DO PRIVATE(iblk,ilo,ihi,jlo,jhi,this_block)
      do iblk = 1, nblocks

         this_block = get_block(blocks_ice(iblk),iblk)         
         ilo = this_block%ilo
         ihi = this_block%ihi
         jlo = this_block%jlo
         jhi = this_block%jhi

         call prepare_forcing (nx_block, ny_block, &
                               ilo, ihi, jlo, jhi, &
                               hm    (:,:,iblk),   &
                               Tair  (:,:,iblk),   &
                               fsw   (:,:,iblk),   &   
                               cldf  (:,:,iblk),   &
                               flw   (:,:,iblk),   &
                               frain (:,:,iblk),   &
                               fsnow (:,:,iblk),   &
                               Qa    (:,:,iblk),   &
                               rhoa  (:,:,iblk),   &
                               uatm  (:,:,iblk),   &
                               vatm  (:,:,iblk),   &
                               strax (:,:,iblk),   &
                               stray (:,:,iblk),   &
                               zlvl  (:,:,iblk),   &
                               wind  (:,:,iblk),   &
                               swvdr (:,:,iblk),   &
                               swvdf (:,:,iblk),   &
                               swidr (:,:,iblk),   &
                               swidf (:,:,iblk),   &
                               potT  (:,:,iblk),   &
                               ANGLET(:,:,iblk),   &
                               trcr  (:,:,nt_Tsfc,iblk), &
                               sst   (:,:,iblk),   &
                               aice  (:,:,iblk) )

      enddo                     ! iblk
      !$OMP END PARALLEL DO

      call ice_timer_start(timer_bound)
      call ice_HaloUpdate (swvdr,             halo_info, &
                           field_loc_center,  field_type_scalar)
      call ice_HaloUpdate (swvdf,             halo_info, &
                           field_loc_center,  field_type_scalar)
      call ice_HaloUpdate (swidr,             halo_info, &
                           field_loc_center,  field_type_scalar)
      call ice_HaloUpdate (swidf,             halo_info, &
                           field_loc_center,  field_type_scalar)
      call ice_timer_stop(timer_bound)

      end subroutine get_forcing_atmo

!=======================================================================

      subroutine get_forcing_ocn (dt)

! Read and interpolate annual climatologies of SSS and SST.
! Restore model SST to data if desired.
! Interpolate ocean fields to U grid if necessary.

      real (kind=dbl_kind), intent(in) :: &
         dt      ! time step

      if (trim(sst_data_type) == 'clim' .or.  &
          trim(sss_data_type) == 'clim') then
         call ocn_data_clim(dt)
      elseif (trim(sst_data_type) == 'ncar' .or.  &
              trim(sss_data_type) == 'ncar') then
         call ocn_data_ncar(dt)      
      elseif (trim(sst_data_type) == 'hadgem_sst' .or.  &
              trim(sst_data_type) == 'hadgem_sst_uvocn') then
         call ocn_data_hadgem(dt) 
      elseif (trim(sst_data_type) == 'oned' .or.  &
              trim(sss_data_type) == 'oned') then
         call ocn_data_oned(dt)   
      endif

      end subroutine get_forcing_ocn

!=======================================================================

      subroutine read_data (flag, recd, yr, ixm, ixx, ixp, &
                            maxrec, data_file, field_data, &
                            field_loc, field_type)

! If data is at the beginning of a one-year record, get data from
!  the previous year.
! If data is at the end of a one-year record, get data from the
!  following year.
! If no earlier data exists (beginning of fyear_init), then
!  (1) For monthly data, get data from the end of fyear_final.
!  (2) For more frequent data, let the ixm value equal the
!      first value of the year.
! If no later data exists (end of fyear_final), then
!  (1) For monthly data, get data from the beginning of fyear_init.
!  (2) For more frequent data, let the ixp value
!      equal the last value of the year.
! In other words, we assume persistence when daily or 6-hourly
!   data is missing, and we assume periodicity when monthly data
!   is missing.

      use ice_diagnostics, only: check_step
      use ice_timers, only: ice_timer_start, ice_timer_stop, timer_readwrite

      logical (kind=log_kind), intent(in) :: flag

      integer (kind=int_kind), intent(in) :: &
         recd                , & ! baseline record number
         yr                  , & ! year of forcing data
         ixm, ixx, ixp       , & ! record numbers of 3 data values
                                 ! relative to recd
         maxrec                  ! maximum record value

      real (kind=dbl_kind), dimension(nx_block,ny_block,2,max_blocks), &
         intent(out) :: &
         field_data              ! 2 values needed for interpolation

      integer (kind=int_kind), intent(in) :: &
           field_loc, &      ! location of field on staggered grid
           field_type        ! type of field (scalar, vector, angle)

      ! local variables

      character (char_len_long) :: &
         data_file               ! data file to be read

      integer (kind=int_kind) :: &
         nbits            , & ! = 32 for single precision, 64 for double
         nrec             , & ! record number to read
         n2, n4           , & ! like ixm and ixp, but
                              ! adjusted at beginning and end of data
         arg                  ! value of time argument in field_data

      call ice_timer_start(timer_readwrite)  ! reading/writing

      nbits = 64              ! double precision data

      if (istep1 > check_step) dbug = .true.  !! debugging

      if (my_task==master_task .and. (dbug)) then
         write(nu_diag,*) '  ', trim(data_file)
      endif

      if (flag) then

      !-----------------------------------------------------------------
      ! Initialize record counters
      ! (n2, n4 will change only at the very beginning or end of
      !  a forcing cycle.)
      !-----------------------------------------------------------------
         n2 = ixm
         n4 = ixp
         arg = 0

      !-----------------------------------------------------------------
      ! read data
      !-----------------------------------------------------------------

         if (ixm /= -99) then
         ! currently in first half of data interval
            if (ixx <= 1) then
               if (yr > fyear_init) then ! get data from previous year
                  call file_year (data_file, yr-1)
               else             ! yr = fyear_init, no prior data exists
                  if (maxrec > 12) then ! extrapolate from first record
                     if (ixx == 1) n2 = ixx
                  else          ! go to end of fyear_final
                     call file_year (data_file, fyear_final)
                  endif
               endif            ! yr > fyear_init
            endif               ! ixx <= 1

            call ice_open (nu_forcing, data_file, nbits)

            arg = 1
            nrec = recd + n2
            call ice_read (nu_forcing, nrec, field_data(:,:,arg,:), &
                           'rda8', dbug, field_loc, field_type)

            if (ixx==1 .and. my_task == master_task) close(nu_forcing)
         endif                  ! ixm ne -99

         ! always read ixx data from data file for current year
         call file_year (data_file, yr)
         call ice_open (nu_forcing, data_file, nbits)

         arg = arg + 1
         nrec = recd + ixx
         call ice_read (nu_forcing, nrec, field_data(:,:,arg,:), &
                        'rda8', dbug, field_loc, field_type)

         if (ixp /= -99) then
         ! currently in latter half of data interval
            if (ixx==maxrec) then
               if (yr < fyear_final) then ! get data from following year
                  if (my_task == master_task) close(nu_forcing)
                  call file_year (data_file, yr+1)
                  call ice_open (nu_forcing, data_file, nbits)
               else             ! yr = fyear_final, no more data exists
                  if (maxrec > 12) then ! extrapolate from ixx
                     n4 = ixx
                  else          ! go to beginning of fyear_init
                     if (my_task == master_task) close(nu_forcing)
                     call file_year (data_file, fyear_init)

                     call ice_open (nu_forcing, data_file, nbits)

                  endif
               endif            ! yr < fyear_final
            endif               ! ixx = maxrec

            arg = arg + 1
            nrec = recd + n4
            call ice_read (nu_forcing, nrec, field_data(:,:,arg,:), &
                           'rda8', dbug, field_loc, field_type)
         endif                  ! ixp /= -99

         if (my_task == master_task) close(nu_forcing)

      endif                     ! flag

      call ice_timer_stop(timer_readwrite)  ! reading/writing

      end subroutine read_data

!=======================================================================

      subroutine read_data_nc (flag, recd, yr, ixm, ixx, ixp, &
                            maxrec, data_file, fieldname, field_data, &
                            field_loc, field_type)

! If data is at the beginning of a one-year record, get data from
!  the previous year.
! If data is at the end of a one-year record, get data from the
!  following year.
! If no earlier data exists (beginning of fyear_init), then
!  (1) For monthly data, get data from the end of fyear_final.
!  (2) For more frequent data, let the ixm value equal the
!      first value of the year.
! If no later data exists (end of fyear_final), then
!  (1) For monthly data, get data from the beginning of fyear_init.
!  (2) For more frequent data, let the ixp value
!      equal the last value of the year.
! In other words, we assume persistence when daily or 6-hourly
!   data is missing, and we assume periodicity when monthly data
!   is missing.
!
! Adapted by Alison McLaren, Met Office from read_data

      use ice_constants, only: c0
      use ice_diagnostics, only: check_step
      use ice_timers, only: ice_timer_start, ice_timer_stop, timer_readwrite

      logical (kind=log_kind), intent(in) :: flag

      integer (kind=int_kind), intent(in) :: &
         recd                , & ! baseline record number
         yr                  , & ! year of forcing data
         ixm, ixx, ixp       , & ! record numbers of 3 data values
                                 ! relative to recd
         maxrec                  ! maximum record value

      character (char_len_long) :: &
         data_file               ! data file to be read

      character (char_len), intent(in) :: &
         fieldname               ! field name in netCDF file

      integer (kind=int_kind), intent(in) :: &
           field_loc, &      ! location of field on staggered grid
           field_type        ! type of field (scalar, vector, angle)

      real (kind=dbl_kind), dimension(nx_block,ny_block,2,max_blocks), &
         intent(out) :: &
         field_data              ! 2 values needed for interpolation

      ! local variables

      logical ::debug

#ifdef ncdf 
      integer (kind=int_kind) :: &
         nrec             , & ! record number to read
         n2, n4           , & ! like ixm and ixp, but
                              ! adjusted at beginning and end of data
         arg              , & ! value of time argument in field_data
         fid                  ! file id for netCDF routines

      call ice_timer_start(timer_readwrite)  ! reading/writing

      if (istep1 > check_step) dbug = .true.  !! debugging

!jd      debug=.true.
      debug=.false.
      if (dbug) debug=.true.


!METNO START
      if (flag) then

      if (my_task==master_task .and. (dbug)) then
         write(nu_diag,*) ' ', trim(data_file), '  ', trim(fieldname)
      endif
!METNO END
      !-----------------------------------------------------------------
      ! Initialize record counters
      ! (n2, n4 will change only at the very beginning or end of
      !  a forcing cycle.)
      !-----------------------------------------------------------------
         n2 = ixm
         n4 = ixp
         arg = 0

      !-----------------------------------------------------------------
      ! read data
      !-----------------------------------------------------------------

         if (ixm /= -99) then
         ! currently in first half of data interval
            if (ixx <= 1) then
               if (yr > fyear_init) then ! get data from previous year
                  call file_year (data_file, yr-1)
               else             ! yr = fyear_init, no prior data exists
                  if (maxrec > 12) then ! extrapolate from first record
                     if (ixx == 1) n2 = ixx
                  else          ! go to end of fyear_final
                     call file_year (data_file, fyear_final)
                  endif
               endif            ! yr > fyear_init
            endif               ! ixx <= 1

            call ice_open_nc (data_file, fid)

            arg = 1
            nrec = recd + n2

!jd
            if (my_task==master_task .and. (debug)) &
                 write(nu_diag,*) ' ', trim(data_file), trim(fieldname) &
                 ,' reading nrec ', nrec, ' into slot ', arg
!jd
            call ice_read_nc & 
                 (fid, nrec, fieldname, field_data(:,:,arg,:), dbug, &
                 field_loc, field_type)
!METNO START
            call ice_close_nc(fid)
!METNO END
         endif                  ! ixm ne -99

         ! always read ixx data from data file for current year
         call file_year (data_file, yr)
         call ice_open_nc (data_file, fid)

         arg = arg + 1
         nrec = recd + ixx

!jd
         if (my_task==master_task .and. (debug)) &
              write(nu_diag,*) ' ', trim(data_file), trim(fieldname) &
              ,' reading nrec ', nrec, ' into slot ', arg
!jd
         call ice_read_nc & 
              (fid, nrec, fieldname, field_data(:,:,arg,:), dbug, &
               field_loc, field_type)

         if (ixp /= -99) then
         ! currently in latter half of data interval
            if (ixx==maxrec) then
               if (yr < fyear_final) then ! get data from following year
                  call ice_close_nc(fid)
                  call file_year (data_file, yr+1)
                  call ice_open_nc (data_file, fid)
               else             ! yr = fyear_final, no more data exists
                  if (maxrec > 12) then ! extrapolate from ixx
                     n4 = ixx
                  else          ! go to beginning of fyear_init
                     call ice_close_nc(fid)
                     call file_year (data_file, fyear_init)
                     call ice_open_nc (data_file, fid)

                  endif
               endif            ! yr < fyear_final
            endif               ! ixx = maxrec

            arg = arg + 1
            nrec = recd + n4

!jd
            if (my_task==master_task .and. (debug)) &
                 write(nu_diag,*) ' ', trim(data_file), trim(fieldname) &
                 ,' reading nrec ', nrec, ' into slot ', arg
!jd

            call ice_read_nc & 
                 (fid, nrec, fieldname, field_data(:,:,arg,:), dbug, &
                  field_loc, field_type)
         endif                  ! ixp /= -99

         call ice_close_nc(fid)

      endif                     ! flag

      call ice_timer_stop(timer_readwrite)  ! reading/writing
      dbug=.false.
#else
      field_data = c0 ! to satisfy intent(out) attribute
#endif
      end subroutine read_data_nc

!=======================================================================

      subroutine read_clim_data (readflag, recd, ixm, ixx, ixp, &
                                 data_file, field_data, &
                                 field_loc, field_type)

! Read data needed for interpolation, as in read_data.
! Assume a one-year cycle of climatological data, so that there is
!  no need to get data from other years or to extrapolate data beyond
!  the forcing time period.

      use ice_diagnostics, only: check_step
      use ice_timers, only: ice_timer_start, ice_timer_stop, timer_readwrite

      logical (kind=log_kind),intent(in) :: readflag

      integer (kind=int_kind), intent(in) :: &
        recd            , & ! baseline record number
        ixm,ixx,ixp         ! record numbers of 3 data values
                            ! relative to recd

      character (char_len_long), intent(in) ::  data_file

      integer (kind=int_kind), intent(in) :: &
           field_loc, &      ! location of field on staggered grid
           field_type        ! type of field (scalar, vector, angle)

      real (kind=dbl_kind), dimension(nx_block,ny_block,2,max_blocks), &
        intent(out) :: &
        field_data         ! 2 values needed for interpolation

      ! local variables

      integer (kind=int_kind) :: &
        nbits          , & ! = 32 for single precision, 64 for double
        nrec           , & ! record number to read
        arg                ! value of time argument in field_data

      call ice_timer_start(timer_readwrite)  ! reading/writing

      nbits = 64                ! double precision data

      if (istep1 > check_step) dbug = .true.  !! debugging

      if (my_task==master_task .and. (dbug)) &
        write(nu_diag,*) '  ', trim(data_file)

      if (readflag) then

      !-----------------------------------------------------------------
      ! read data
      !-----------------------------------------------------------------

         call ice_open (nu_forcing, data_file, nbits)

         arg = 0
         if (ixm /= -99) then
            arg = 1
            nrec = recd + ixm
            call ice_read (nu_forcing, nrec, field_data(:,:,arg,:), &
                           'rda8', dbug, field_loc, field_type)
         endif

         arg = arg + 1
         nrec = recd + ixx
         call ice_read (nu_forcing, nrec, field_data(:,:,arg,:), &
                        'rda8', dbug, field_loc, field_type)

         if (ixp /= -99) then
            arg = arg + 1
            nrec = recd + ixp
            call ice_read (nu_forcing, nrec, field_data(:,:,arg,:), &
                           'rda8', dbug, field_loc, field_type)
         endif

         if (my_task == master_task) close (nu_forcing)
      endif                     ! readflag

      call ice_timer_stop(timer_readwrite)  ! reading/writing

      end subroutine read_clim_data

!=======================================================================

      subroutine read_clim_data_nc (readflag, recd, ixm, ixx, ixp, &
                                 data_file, fieldname, field_data, &
                                 field_loc, field_type)

! Read data needed for interpolation, as in read_data.
! Assume a one-year cycle of climatological data, so that there is
!  no need to get data from other years or to extrapolate data beyond
!  the forcing time period.

      use ice_diagnostics, only: check_step
      use ice_timers, only: ice_timer_start, ice_timer_stop, timer_readwrite

      logical (kind=log_kind),intent(in) :: readflag

      integer (kind=int_kind), intent(in) :: &
        recd            , & ! baseline record number
        ixm,ixx,ixp         ! record numbers of 3 data values
                            ! relative to recd

      character (char_len_long), intent(in) ::  data_file

      character (char_len), intent(in) :: &
         fieldname               ! field name in netCDF file

      integer (kind=int_kind), intent(in) :: &
           field_loc, &      ! location of field on staggered grid
           field_type        ! type of field (scalar, vector, angle)

      real (kind=dbl_kind), dimension(nx_block,ny_block,2,max_blocks), &
        intent(out) :: &
        field_data         ! 2 values needed for interpolation

      ! local variables

      integer (kind=int_kind) :: &
        nbits          , & ! = 32 for single precision, 64 for double
        nrec           , & ! record number to read
        arg            , & ! value of time argument in field_data
        fid                ! file id for netCDF routines

      call ice_timer_start(timer_readwrite)  ! reading/writing

      nbits = 64                ! double precision data

      if (istep1 > check_step) dbug = .true.  !! debugging

      if (my_task==master_task .and. (dbug)) &
        write(nu_diag,*) '  ', trim(data_file)

      if (readflag) then

      !-----------------------------------------------------------------
      ! read data
      !-----------------------------------------------------------------

         call ice_open_nc (data_file, fid)

         arg = 0
         if (ixm /= -99) then
            arg = 1
            nrec = recd + ixm
            call ice_read_nc & 
                 (fid, nrec, fieldname, field_data(:,:,arg,:), &
                  dbug, field_loc, field_type)
         endif

         arg = arg + 1
         nrec = recd + ixx
         call ice_read_nc & 
                 (fid, nrec, fieldname, field_data(:,:,arg,:), &
                  dbug, field_loc, field_type)

         if (ixp /= -99) then
            arg = arg + 1
            nrec = recd + ixp
            call ice_read_nc & 
                 (fid, nrec, fieldname, field_data(:,:,arg,:), &
                  dbug, field_loc, field_type)
         endif

         if (my_task == master_task) call ice_close_nc (fid)
      endif                     ! readflag

      call ice_timer_stop(timer_readwrite)  ! reading/writing

      end subroutine read_clim_data_nc

!=======================================================================

      subroutine interp_coeff_monthly (recslot)

! Compute coefficients for interpolating monthly data to current time step.

      use ice_constants, only: c1, secday

      integer (kind=int_kind), intent(in) :: &
          recslot         ! slot (1 or 2) for current record

      ! local variables

      real (kind=dbl_kind) :: &
          tt           , & ! seconds elapsed in current year
          t1, t2           ! seconds elapsed at month midpoint

      real (kind=dbl_kind) :: &
          daymid(0:13)     ! month mid-points

      daymid(1:13) = 14._dbl_kind   ! time frame ends 0 sec into day 15
      daymid(0)    = 14._dbl_kind - daymo(12)  ! Dec 15, 0 sec

      ! make time cyclic
      tt = mod(ftime/secday,dayyr)

      ! Find neighboring times

      if (recslot==2) then      ! first half of month
        t2 = daycal(month) + daymid(month)   ! midpoint, current month
        if (month == 1) then
          t1 = daymid(0)                 ! Dec 15 (0 sec)
        else
          t1 = daycal(month-1) + daymid(month-1) ! midpoint, previous month
        endif
      else                      ! second half of month
        t1 = daycal(month) + daymid(month)    ! midpoint, current month
        t2 = daycal(month+1) + daymid(month+1)! day 15 of next month (0 sec)
      endif

      ! Compute coefficients
      c1intp = (t2 - tt) / (t2 - t1)
      c2intp =  c1 - c1intp

      end subroutine interp_coeff_monthly

!=======================================================================

      subroutine interp_coeff (recnum, recslot, secint, dataloc)

! Compute coefficients for interpolating data to current time step.
! Works for any data interval that divides evenly into a
!  year (daily, 6-hourly, etc.)
! Use interp_coef_monthly for monthly data.

      use ice_constants, only: c1, p5, secday

      integer (kind=int_kind), intent(in) :: &
          recnum      , & ! record number for current data value
          recslot     , & ! spline slot for current record
          dataloc         ! = 1 for data located in middle of time interval
                          ! = 2 for date located at end of time interval

      real (kind=dbl_kind), intent(in) :: &
          secint                    ! seconds in data interval

      ! local variables

      real (kind=dbl_kind) :: &
          secyr            ! seconds in a year

      real (kind=dbl_kind) :: &
          tt           , & ! seconds elapsed in current year
          t1, t2       , & ! seconds elapsed at data points
          rcnum            ! recnum => dbl_kind

!jd Start
!jd      secyr = dayyr * secday         ! seconds in a year
!jd      tt = mod(ftime,secyr)

        tt=secday*(yday - c1) + sec
!jd Slutt

      ! Find neighboring times
      rcnum = real(recnum,kind=dbl_kind)
      if (recslot==2) then           ! current record goes in slot 2
         if (dataloc==1) then        ! data located at middle of interval
            t2 = (rcnum-p5)*secint
         else                        !  data located at end of interval
            t2 = rcnum*secint
         endif
         t1 = t2 - secint            !  - 1 interval
      else                           ! recslot = 1
         if (dataloc==1) then        ! data located at middle of interval
            t1 = (rcnum-p5)*secint
         else                        
            t1 = rcnum*secint        ! data located at end of interval
         endif
         t2 = t1 + secint            !  + 1 interval
      endif

      ! Compute coefficients
      c1intp =  abs((t2 - tt) / (t2 - t1))
      c2intp =  c1 - c1intp

      end subroutine interp_coeff

!=======================================================================

      subroutine interpolate_data (field_data, field)

! Linear interpolation

! author: Elizabeth C. Hunke, LANL

      use ice_domain, only: nblocks

      real (kind=dbl_kind), dimension(nx_block,ny_block,2,max_blocks), &
        intent(in) :: &
        field_data    ! 2 values used for interpolation

      real (kind=dbl_kind), dimension(nx_block,ny_block,max_blocks), &
        intent(out) :: &
        field         ! interpolated field

      ! local variables

      integer (kind=int_kind) :: i,j, iblk

      !$OMP PARALLEL DO PRIVATE(iblk,i,j)
      do iblk = 1, nblocks
         do j = 1, ny_block
         do i = 1, nx_block
            field(i,j,iblk) = c1intp * field_data(i,j,1,iblk) &
                            + c2intp * field_data(i,j,2,iblk)
         enddo
         enddo
      enddo
      !$OMP END PARALLEL DO

      end subroutine interpolate_data

!=======================================================================

      subroutine file_year (data_file, yr)

! Construct the correct name of the atmospheric data file
! to be read, given the year and assuming the naming convention
! that filenames end with 'yyyy.dat' or 'yyyy.r' or 'yyyy.nc'.

      character (char_len_long), intent(inout) ::  data_file

      integer (kind=int_kind), intent(in) :: yr

      character (char_len_long) :: tmpname

      integer (kind=int_kind) :: i

      if (trim(atm_data_type) == 'hadgem') then ! netcdf
         i = index(data_file,'.nc') - 5
         tmpname = data_file
         write(data_file,'(a,i4.4,a)') tmpname(1:i), yr, '.nc'
!METNO START
      elseif (trim(atm_data_type) == 'ecmwf') then ! current ERA naming
         i = index(data_file,'_unlim.nc') - 5
         tmpname = data_file
         write(data_file,'(a,i4.4,a)') tmpname(1:i), yr, '_unlim.nc'
!METNO END
      else                                     ! LANL/NCAR naming convention
         i = index(data_file,'.dat') - 5
         tmpname = data_file
         write(data_file,'(a,i4.4,a)') tmpname(1:i), yr, '.dat'
      endif

      end subroutine file_year

!=======================================================================

      subroutine prepare_forcing (nx_block, ny_block, &
                                  ilo, ihi, jlo, jhi, &
                                  hm,                 &
                                  Tair,     fsw,      &    
                                  cldf,     flw,      &
                                  frain,    fsnow,    &
                                  Qa,       rhoa,     &
                                  uatm,     vatm,     &
                                  strax,    stray,    &
                                  zlvl,     wind,     &
                                  swvdr,    swvdf,    &
                                  swidr,    swidf,    &
                                  potT,     ANGLET,   &
                                  Tsfc,     sst,      &
                                  aice)

      use ice_constants, only: c0, c1, c10, c12, c4, &
          secday, Tffresh, stefan_boltzmann, &
          emissivity, qqqocn, TTTocn

      integer (kind=int_kind), intent(in) :: &
         nx_block, ny_block, & ! block dimensions
         ilo,ihi,jlo,jhi       ! beginning and end of physical domain

      real (kind=dbl_kind), dimension(nx_block,ny_block), intent(in) :: &
         Tair    , & ! air temperature  (K)
         ANGLET  , & ! ANGLE converted to T-cells
         Tsfc    , & ! ice skin temperature
         sst     , & ! sea surface temperature
         aice    , & ! ice area fraction
         hm          ! land mask
     
      real (kind=dbl_kind), dimension(nx_block,ny_block), &
         intent(inout) :: &
         fsw     , & ! incoming shortwave radiation (W/m^2)
         cldf    , & ! cloud fraction
         frain   , & ! rainfall rate (kg/m^2 s)
         fsnow   , & ! snowfall rate (kg/m^2 s)
         Qa      , & ! specific humidity (kg/kg)
         rhoa    , & ! air density (kg/m^3)
         uatm    , & ! wind velocity components (m/s)
         vatm    , &
         strax   , & ! wind stress components (N/m^2)
         stray   , &
         zlvl    , & ! atm level height (m)
         wind    , & ! wind speed (m/s)
         flw     , & ! incoming longwave radiation (W/m^2)
         swvdr   , & ! sw down, visible, direct  (W/m^2)
         swvdf   , & ! sw down, visible, diffuse (W/m^2)
         swidr   , & ! sw down, near IR, direct  (W/m^2)
         swidf   , & ! sw down, near IR, diffuse (W/m^2)
         potT        ! air potential temperature  (K)

      ! local variables

      integer (kind=int_kind) :: &
         i, j

      real (kind=dbl_kind) :: workx, worky, &
         precip_factor, zlvl0

      do j = jlo, jhi
      do i = ilo, ihi

      !-----------------------------------------------------------------
      ! make sure interpolated values are physically realistic
      !-----------------------------------------------------------------
         cldf (i,j) = max(min(cldf(i,j),c1),c0)
         fsw  (i,j) = max(fsw(i,j),c0)
         fsnow(i,j) = max(fsnow(i,j),c0)
         rhoa (i,j) = max(rhoa(i,j),c0)
         Qa   (i,j) = max(Qa(i,j),c0)

      enddo                     ! i
      enddo                     ! j

      !-----------------------------------------------------------------
      ! calculations specific to datasets
      !-----------------------------------------------------------------

      if (trim(atm_data_type) == 'ncar') then

         ! precip is in mm/month

         zlvl0 = c10

         do j = jlo, jhi
         do i = ilo, ihi
            ! correct known biases in NCAR data (as in CCSM latm)
            Qa (i,j) = Qa (i,j) * 0.94_dbl_kind
            fsw(i,j) = fsw(i,j) * 0.92_dbl_kind

            ! downward longwave as in Parkinson and Washington (1979)
            call longwave_parkinson_washington(Tair(i,j), cldf(i,j), &
                                              flw(i,j))
         enddo
         enddo

!METNO START
      elseif (trim(atm_data_type) == 'LYq' .or. trim(atm_data_type) ==  'ecmwf' &
          .or.trim(atm_data_type) == 'metroms') then
!METNO END

         ! precip is in mm/s

         zlvl0 = c10

         do j = jlo, jhi
         do i = ilo, ihi
            ! longwave based on Rosati and Miyakoda, JPO 18, p. 1607 (1988)
            call longwave_rosati_miyakoda(cldf(i,j), Tsfc(i,j), &
                                          aice(i,j), sst(i,j),  &
                                          Qa(i,j),   Tair(i,j), &
                                          hm(i,j),   flw(i,j))
         enddo
         enddo

      elseif (trim(atm_data_type) == 'oned') then  ! rectangular grid 

         ! precip is in kg/m^2/s

         zlvl0 = c10
         
         do j = jlo, jhi
         do i = ilo, ihi

      !-----------------------------------------------------------------
      ! compute downward longwave as in Parkinson and Washington (1979)
      !-----------------------------------------------------------------

            ! downward longwave as in Parkinson and Washington (1979)
            call longwave_parkinson_washington(Tair(i,j), cldf(i,j), &
                                               flw(i,j))

            ! longwave based on Rosati and Miyakoda, JPO 18, p. 1607 (1988)
!            call longwave_rosati_miyakoda(cldf(i,j), Tsfc(i,j), &
!                                          aice(i,j), sst(i,j),  &
!                                          Qa(i,j),   Tair(i,j), &
!                                          hm(i,j),   flw(i,j))
         enddo
         enddo

      endif                     ! atm_data_type

      !-----------------------------------------------------------------
      ! Compute other fields needed by model
      !-----------------------------------------------------------------

      ! convert precipitation units to kg/m^2 s
      if (trim(precip_units) == 'mm_per_month') then
         precip_factor = c12/(secday*days_per_year) 
      elseif (trim(precip_units) == 'mm_per_day') then
         precip_factor = c1/secday
!METNO START
      elseif (trim(precip_units) == 'm_per_12hr') then
         precip_factor = c1/43.2_dbl_kind
!METNO END
      elseif (trim(precip_units) == 'mm_per_sec' .or. &
              trim(precip_units) == 'mks') then 
         precip_factor = c1    ! mm/sec = kg/m^2 s
      endif

!jd      if (my_task == master_task) write(nu_diag,*) &
!jd           ' precip_units, precip_factor ', trim(precip_units), precip_factor

!jd START  No time interpolation of precip fields in this version
      if (trim(atm_data_type) ==  'ecmwf' .or. &
          trim(atm_data_type) == 'metroms') then
         precip_factor = c1
      end if
!jd END

      do j = jlo, jhi
      do i = ilo, ihi

         zlvl(i,j) = zlvl0
         potT(i,j) = Tair(i,j)

        ! divide shortwave into spectral bands
         swvdr(i,j) = fsw(i,j)*frcvdr        ! visible direct
         swvdf(i,j) = fsw(i,j)*frcvdf        ! visible diffuse
         swidr(i,j) = fsw(i,j)*frcidr        ! near IR direct
         swidf(i,j) = fsw(i,j)*frcidf        ! near IR diffuse
                 
        ! convert precipitation units to kg/m^2 s
         fsnow(i,j) = fsnow(i,j) * precip_factor
      enddo                     ! i
      enddo                     ! j

      ! determine whether precip is rain or snow
      ! HadGEM forcing provides separate snowfall and rainfall rather 
      ! than total precipitation
      if (trim(atm_data_type) /= 'hadgem') then

        do j = jlo, jhi
        do i = ilo, ihi
           frain(i,j) = c0                     
           if (Tair(i,j) >= Tffresh) then
               frain(i,j) = fsnow(i,j)
               fsnow(i,j) = c0
           endif
        enddo                     ! i
        enddo                     ! j

      endif

      if (calc_strair) then

        do j = jlo, jhi
        do i = ilo, ihi

            wind(i,j) = sqrt(uatm(i,j)**2 + vatm(i,j)**2)

      !-----------------------------------------------------------------
      ! Rotate zonal/meridional vectors to local coordinates.
      ! Velocity comes in on T grid, but is oriented geographically ---
      ! need to rotate to pop-grid FIRST using ANGLET
      ! then interpolate to the U-cell centers  (otherwise we
      ! interpolate across the pole).
      ! Use ANGLET which is on the T grid !
      ! Atmo variables are needed in T cell centers in subroutine 
      ! atmo_boundary_layer, and are interpolated to the U grid later as 
      ! necessary.
      !-----------------------------------------------------------------
           workx      = uatm(i,j) ! wind velocity, m/s
           worky      = vatm(i,j)
!METNO START
           if (trim(atm_data_type) /= 'ecmwf' .and. &
               trim(atm_data_type) /= 'metroms') then
              uatm (i,j) = workx*cos(ANGLET(i,j)) & ! convert to POP grid
                         + worky*sin(ANGLET(i,j))   ! note uatm, vatm, wind
              vatm (i,j) = worky*cos(ANGLET(i,j)) & !  are on the T-grid here
                         - workx*sin(ANGLET(i,j))
           endif
!METNO END
        enddo                     ! i
        enddo                     ! j

      else  ! strax, stray, wind are read from files

        do j = jlo, jhi
        do i = ilo, ihi

           workx      = strax(i,j) ! wind stress
           worky      = stray(i,j)
           strax(i,j) = workx*cos(ANGLET(i,j)) & ! convert to POP grid
                      + worky*sin(ANGLET(i,j))   ! note strax, stray, wind
           stray(i,j) = worky*cos(ANGLET(i,j)) & !  are on the T-grid here
                      - workx*sin(ANGLET(i,j))

        enddo                     ! i
        enddo                     ! j

      endif                   ! calc_strair

      end subroutine prepare_forcing

!=======================================================================

      subroutine longwave_parkinson_washington(Tair, cldf, flw)

      use ice_constants, only: c1, Tffresh, stefan_boltzmann

      ! compute downward longwave as in Parkinson and Washington (1979)
      ! (for now)
      ! Parkinson, C. L. and W. M. Washington (1979),
      ! Large-scale numerical-model of sea ice,
      ! JGR, 84, 311-337, doi:10.1029/JC084iC01p00311 

      real(kind=dbl_kind), intent(in) :: &
           Tair , & ! air temperature  (K)
           cldf     ! cloud fraction
      
      real(kind=dbl_kind), intent(out) :: &
           flw      ! incoming longwave radiation (W/m^2)
      
      flw = stefan_boltzmann*Tair**4 &
             * (c1 - 0.261_dbl_kind &
             * exp(-7.77e-4_dbl_kind*(Tffresh - Tair)**2)) &
             * (c1 + 0.275_dbl_kind*cldf)
        
      end subroutine longwave_parkinson_washington

!=======================================================================

      subroutine longwave_rosati_miyakoda(cldf, Tsfc, &
                                          aice, sst,  &
                                          Qa,   Tair, &
                                          hm,   flw)

      use ice_constants, only: c1, c4, c1000, &
          Tffresh, stefan_boltzmann, emissivity

      ! based on 
      ! Rosati, A. and K. Miyakoda (1988), 
      ! A general-circulation model for upper ocean simulation, 
      ! J. Physical Oceanography, 18, 1601-1626, 
      ! doi:10.1175/1520-0485(1988)018<1601:AGCMFU>2.0.CO;2 

      real(kind=dbl_kind), intent(in) :: &
           cldf , & ! cloud fraction
           Tsfc , & ! ice skin temperature
           aice , & ! ice area fraction
           sst  , & ! sea surface temperature
           Qa   , & ! specific humidity (kg/kg)
           Tair , & ! air temperature  (K)
           hm       ! land mask

      real(kind=dbl_kind), intent(out) :: &
           flw      ! incoming longwave radiation (W/m^2)

      real(kind=dbl_kind) :: &
           fcc  , & ! cloudiness modification
           sstk , & ! ice/ocean surface temperature (K)
           rtea , & ! square root of the vapour pressure
           ptem , & ! potential air temperature (K)
           qlwm 

      fcc = c1 - 0.8_dbl_kind * cldf
      sstk = (Tsfc * aice &
           + sst * (c1 - aice)) + Tffresh
      rtea = sqrt(c1000*Qa /  &
           (0.622_dbl_kind+0.378_dbl_kind*Qa))
      ptem = Tair    ! get this from stability?
      qlwm = ptem * ptem * ptem  &
                 * ( ptem*(0.39_dbl_kind-0.05_dbl_kind*rtea)*fcc  &
                 + c4*(sstk-ptem) )
      flw = emissivity*stefan_boltzmann * ( sstk**4 - qlwm )
      flw = flw * hm ! land mask
      
      end subroutine longwave_rosati_miyakoda

!=======================================================================
! NCAR atmospheric forcing
!=======================================================================

      subroutine ncar_files (yr)

! Construct filenames based on the LANL naming conventions for NCAR data.
! Edit for other directory structures or filenames.
! Note: The year number in these filenames does not matter, because
!       subroutine file_year will insert the correct year.

      integer (kind=int_kind), intent(in) :: &
           yr                   ! current forcing year

      fsw_file = &
           trim(atm_data_dir)//'ISCCPM/MONTHLY/RADFLX/swdn.1996.dat'
      call file_year(fsw_file,yr)

      flw_file = &
           trim(atm_data_dir)//'ISCCPM/MONTHLY/RADFLX/cldf.1996.dat'
      call file_year(flw_file,yr)

      rain_file = &
           trim(atm_data_dir)//'MXA/MONTHLY/PRECIP/prec.1996.dat'
      call file_year(rain_file,yr)

      uwind_file = &
           trim(atm_data_dir)//'NCEP/4XDAILY/STATES/u_10.1996.dat'
      call file_year(uwind_file,yr)

      vwind_file = &
           trim(atm_data_dir)//'NCEP/4XDAILY/STATES/v_10.1996.dat'
      call file_year(vwind_file,yr)

      tair_file = &
           trim(atm_data_dir)//'NCEP/4XDAILY/STATES/t_10.1996.dat'
      call file_year(tair_file,yr)

      humid_file = &
           trim(atm_data_dir)//'NCEP/4XDAILY/STATES/q_10.1996.dat'
      call file_year(humid_file,yr)

      rhoa_file = &
           trim(atm_data_dir)//'NCEP/4XDAILY/STATES/dn10.1996.dat'
      call file_year(rhoa_file,yr)

      if (my_task == master_task) then
         write (nu_diag,*) ' '
         write (nu_diag,*) 'Forcing data year =', fyear
         write (nu_diag,*) 'Atmospheric data files:'
         write (nu_diag,*) trim(fsw_file)
         write (nu_diag,*) trim(flw_file)
         write (nu_diag,*) trim(rain_file)
         write (nu_diag,*) trim(uwind_file)
         write (nu_diag,*) trim(vwind_file)
         write (nu_diag,*) trim(tair_file)
         write (nu_diag,*) trim(humid_file)
         write (nu_diag,*) trim(rhoa_file)
      endif                     ! master_task

      end subroutine ncar_files

!=======================================================================

      subroutine ncar_data

      use ice_constants, only: c4, p5, secday, &
          field_loc_center, field_type_scalar, field_type_vector
      use ice_flux, only: fsw, fsnow, Tair, uatm, vatm, rhoa, Qa

      integer (kind=int_kind) :: &
          ixm,ixx,ixp , & ! record numbers for neighboring months
          recnum      , & ! record number
          maxrec      , & ! maximum record number
          recslot     , & ! spline slot for current record
          dataloc     , & ! = 1 for data located in middle of time interval
                          ! = 2 for date located at end of time interval
          midmonth        ! middle day of month

      real (kind=dbl_kind) :: &
          sec6hr              ! number of seconds in 6 hours

      logical (kind=log_kind) :: readm, read6

    !-------------------------------------------------------------------
    ! monthly data
    !
    ! Assume that monthly data values are located in the middle of the
    ! month.
    !-------------------------------------------------------------------

      midmonth = 15  ! data is given on 15th of every month
!      midmonth = fix(p5 * real(daymo(month)))  ! exact middle

      ! Compute record numbers for surrounding months
      maxrec = 12
      ixm  = mod(month+maxrec-2,maxrec) + 1
      ixp  = mod(month,         maxrec) + 1
      if (mday >= midmonth) ixm = -99  ! other two points will be used
      if (mday <  midmonth) ixp = -99

      ! Determine whether interpolation will use values 1:2 or 2:3
      ! recslot = 2 means we use values 1:2, with the current value (2)
      !  in the second slot
      ! recslot = 1 means we use values 2:3, with the current value (2)
      !  in the first slot
      recslot = 1                             ! latter half of month
      if (mday < midmonth) recslot = 2        ! first half of month

      ! Find interpolation coefficients
      call interp_coeff_monthly (recslot)

      ! Read 2 monthly values
      readm = .false.
      if (istep==1 .or. (mday==midmonth .and. sec==0)) readm = .true.

      if (trim(atm_data_format) == 'bin') then
         call read_data (readm, 0, fyear, ixm, month, ixp, &
                         maxrec, fsw_file, fsw_data, &
                         field_loc_center, field_type_scalar)
         call read_data (readm, 0, fyear, ixm, month, ixp, &
                         maxrec, flw_file, cldf_data, &
                         field_loc_center, field_type_scalar)
         call read_data (readm, 0, fyear, ixm, month, ixp, &
                         maxrec, rain_file, fsnow_data, &
                         field_loc_center, field_type_scalar)
      else
         call abort_ice ('nonbinary atm_data_format unavailable')
!        The routine exists, for example:  
!         call read_data_nc (readm, 0, fyear, ixm, month, ixp, &
!                            maxrec, fsw_file, 'fsw', fsw_data, &
!                            field_loc_center, field_type_scalar)
!         call read_data_nc (readm, 0, fyear, ixm, month, ixp, &
!                            maxrec, flw_file, 'cldf',cldf_data, &
!                            field_loc_center, field_type_scalar)
!         call read_data_nc (readm, 0, fyear, ixm, month, ixp, &
!                            maxrec, rain_file,'prec',fsnow_data, &
!                            field_loc_center, field_type_scalar)
      endif

      ! Interpolate to current time step
      call interpolate_data (fsw_data,   fsw)
      call interpolate_data (cldf_data,  cldf)
      call interpolate_data (fsnow_data, fsnow)

    !-------------------------------------------------------------------
    ! 6-hourly data
    !
    ! Assume that the 6-hourly value is located at the end of the
    !  6-hour period.  This is the convention for NCEP reanalysis data.
    !  E.g. record 1 gives conditions at 6 am GMT on 1 January.
    !-------------------------------------------------------------------

      dataloc = 2               ! data located at end of interval
      sec6hr = secday/c4        ! seconds in 6 hours
      maxrec = 1460             ! 365*4

      ! current record number
      recnum = 4*int(yday) - 3 + int(real(sec,kind=dbl_kind)/sec6hr)

      ! Compute record numbers for surrounding data

      ixm = mod(recnum+maxrec-2,maxrec) + 1
      ixx = mod(recnum-1,       maxrec) + 1
!      ixp = mod(recnum,         maxrec) + 1

      ! Compute interpolation coefficients
      ! If data is located at the end of the time interval, then the
      !  data value for the current record always goes in slot 2.

      recslot = 2
      ixp = -99
      call interp_coeff (recnum, recslot, sec6hr, dataloc)

      ! Read
      read6 = .false.
      if (istep==1 .or. oldrecnum /= recnum) read6 = .true.

      if (trim(atm_data_format) == 'bin') then
         call read_data (read6, 0, fyear, ixm, ixx, ixp, &
                         maxrec, tair_file, Tair_data, &
                         field_loc_center, field_type_scalar)
         call read_data (read6, 0, fyear, ixm, ixx, ixp, &
                         maxrec, uwind_file, uatm_data, &
                         field_loc_center, field_type_vector)
         call read_data (read6, 0, fyear, ixm, ixx, ixp, &
                         maxrec, vwind_file, vatm_data, &
                         field_loc_center, field_type_vector)
         call read_data (read6, 0, fyear, ixm, ixx, ixp, &
                         maxrec, rhoa_file, rhoa_data, &
                         field_loc_center, field_type_scalar)
         call read_data (read6, 0, fyear, ixm, ixx, ixp, &
                         maxrec, humid_file, Qa_data, &
                         field_loc_center, field_type_scalar)
      else
         call abort_ice ('nonbinary atm_data_format unavailable')
      endif

      ! Interpolate
      call interpolate_data (Tair_data, Tair)
      call interpolate_data (uatm_data, uatm)
      call interpolate_data (vatm_data, vatm)
      call interpolate_data (rhoa_data, rhoa)
      call interpolate_data (Qa_data,   Qa)

      ! Save record number for next time step
      oldrecnum = recnum

      end subroutine ncar_data

!=======================================================================
! Large and Yeager forcing (AOMIP style)
!=======================================================================

      subroutine LY_files (yr)

! Construct filenames based on the LANL naming conventions for CORE
! (Large and Yeager) data.
! Edit for other directory structures or filenames.
! Note: The year number in these filenames does not matter, because
!       subroutine file_year will insert the correct year.

! author: Elizabeth C. Hunke, LANL

      integer (kind=int_kind), intent(in) :: &
           yr                   ! current forcing year

      flw_file = &
           trim(atm_data_dir)//'MONTHLY/cldf.omip.dat'

      rain_file = &
           trim(atm_data_dir)//'MONTHLY/prec.nmyr.dat'

      uwind_file = &
           trim(atm_data_dir)//'4XDAILY/u_10.1996.dat'
      call file_year(uwind_file,yr)

      vwind_file = &
           trim(atm_data_dir)//'4XDAILY/v_10.1996.dat'
      call file_year(vwind_file,yr)

      tair_file = &
           trim(atm_data_dir)//'4XDAILY/t_10.1996.dat'
      call file_year(tair_file,yr)

      humid_file = &
           trim(atm_data_dir)//'4XDAILY/q_10.1996.dat'
      call file_year(humid_file,yr)

      if (my_task == master_task) then
         write (nu_diag,*) ' '
         write (nu_diag,*) 'Forcing data year = ', fyear         
         write (nu_diag,*) 'Atmospheric data files:'
         write (nu_diag,*) trim(flw_file)
         write (nu_diag,*) trim(rain_file)
         write (nu_diag,*) trim(uwind_file)
         write (nu_diag,*) trim(vwind_file)
         write (nu_diag,*) trim(tair_file)
         write (nu_diag,*) trim(humid_file)
      endif                     ! master_task

      end subroutine LY_files

!=======================================================================
!
! read Large and Yeager atmospheric data
!        note:  also uses AOMIP protocol, in part

      subroutine LY_data

      use ice_blocks, only: block, get_block
      use ice_constants, only: c4, p1, p5, secday, Tffresh, &
          field_loc_center, field_type_scalar, field_type_vector
      use ice_global_reductions, only: global_minval, global_maxval
      use ice_domain, only: nblocks, distrb_info, blocks_ice
      use ice_flux, only: fsnow, Tair, uatm, vatm, Qa, fsw
      use ice_grid, only: hm, tlon, tlat, tmask, umask
      use ice_state, only: aice

      integer (kind=int_kind) :: & 
          i, j        , &
          ixm,ixx,ixp , & ! record numbers for neighboring months
          recnum      , & ! record number
          maxrec      , & ! maximum record number
          recslot     , & ! spline slot for current record
          midmonth    , & ! middle day of month
          dataloc     , & ! = 1 for data located in middle of time interval
                          ! = 2 for date located at end of time interval
          iblk        , & ! block index
          ilo,ihi,jlo,jhi ! beginning and end of physical domain

      real (kind=dbl_kind) :: &
          sec6hr          , & ! number of seconds in 6 hours
          vmin, vmax

      logical (kind=log_kind) :: readm, read6

      type (block) :: &
         this_block           ! block information for current block

    !-------------------------------------------------------------------
    ! monthly data 
    !
    ! Assume that monthly data values are located in the middle of the 
    ! month.
    !-------------------------------------------------------------------

      midmonth = 15  ! data is given on 15th of every month
!      midmonth = fix(p5 * real(daymo(month)))  ! exact middle

      ! Compute record numbers for surrounding months
      maxrec = 12
      ixm  = mod(month+maxrec-2,maxrec) + 1
      ixp  = mod(month,         maxrec) + 1
      if (mday >= midmonth) ixm = -99  ! other two points will be used
      if (mday <  midmonth) ixp = -99

      ! Determine whether interpolation will use values 1:2 or 2:3
      ! recslot = 2 means we use values 1:2, with the current value (2)
      !  in the second slot
      ! recslot = 1 means we use values 2:3, with the current value (2)
      !  in the first slot
      recslot = 1                             ! latter half of month
      if (mday < midmonth) recslot = 2        ! first half of month

      ! Find interpolation coefficients
      call interp_coeff_monthly (recslot)

      ! Read 2 monthly values 
      readm = .false.
      if (istep==1 .or. (mday==midmonth .and. sec==0)) readm = .true.

      call read_clim_data (readm, 0, ixm, month, ixp,  &
             flw_file, cldf_data, field_loc_center, field_type_scalar)
      call read_clim_data (readm, 0, ixm, month, ixp,  &
             rain_file, fsnow_data, field_loc_center, field_type_scalar)

      call interpolate_data (cldf_data, cldf)
      call interpolate_data (fsnow_data, fsnow)  ! units mm/s = kg/m^2/s

    !-------------------------------------------------------------------
    ! 6-hourly data
    ! 
    ! Assume that the 6-hourly value is located at the end of the
    !  6-hour period.  This is the convention for NCEP reanalysis data.
    !  E.g. record 1 gives conditions at 6 am GMT on 1 January.
    !-------------------------------------------------------------------

      dataloc = 2               ! data located at end of interval
      sec6hr = secday/c4        ! seconds in 6 hours
      maxrec = 1460             ! 365*4

      ! current record number
      recnum = 4*int(yday) - 3 + int(real(sec,kind=dbl_kind)/sec6hr)

      ! Compute record numbers for surrounding data (2 on each side)

      ixm = mod(recnum+maxrec-2,maxrec) + 1
      ixx = mod(recnum-1,       maxrec) + 1
!     ixp = mod(recnum,         maxrec) + 1

      ! Compute interpolation coefficients
      ! If data is located at the end of the time interval, then the
      !  data value for the current record goes in slot 2

      recslot = 2
      ixp = -99
      call interp_coeff (recnum, recslot, sec6hr, dataloc)

      ! Read
      read6 = .false.
      if (istep==1 .or. oldrecnum .ne. recnum) read6 = .true.

      if (trim(atm_data_format) == 'bin') then
         call read_data (read6, 0, fyear, ixm, ixx, ixp, maxrec, &
                         tair_file, Tair_data, &
                         field_loc_center, field_type_scalar)
         call read_data (read6, 0, fyear, ixm, ixx, ixp, maxrec, &
                         uwind_file, uatm_data, &
                         field_loc_center, field_type_vector)
         call read_data (read6, 0, fyear, ixm, ixx, ixp, maxrec, &
                         vwind_file, vatm_data, &
                         field_loc_center, field_type_vector)
         call read_data (read6, 0, fyear, ixm, ixx, ixp, maxrec, &
                         humid_file, Qa_data, &
                         field_loc_center, field_type_scalar)
      else
         call abort_ice ('nonbinary atm_data_format unavailable')
      endif

      ! Interpolate
      call interpolate_data (Tair_data, Tair)
      call interpolate_data (uatm_data, uatm)
      call interpolate_data (vatm_data, vatm)
      call interpolate_data (Qa_data, Qa)

      !$OMP PARALLEL DO PRIVATE(iblk,i,j,ilo,ihi,jlo,jhi,this_block)
      do iblk = 1, nblocks
        ! limit summer Tair values where ice is present
        do j = 1, ny_block
          do i = 1, nx_block
            if (aice(i,j,iblk) > p1) Tair(i,j,iblk) = min(Tair(i,j,iblk), Tffresh+p1)
          enddo
        enddo

        call Qa_fixLY(nx_block,  ny_block, &
                                 Tair (:,:,iblk), &
                                 Qa   (:,:,iblk))

        do j = 1, ny_block
          do i = 1, nx_block
            Qa  (i,j,iblk) = Qa  (i,j,iblk) * hm(i,j,iblk)
            Tair(i,j,iblk) = Tair(i,j,iblk) * hm(i,j,iblk)
            uatm(i,j,iblk) = uatm(i,j,iblk) * hm(i,j,iblk)
            vatm(i,j,iblk) = vatm(i,j,iblk) * hm(i,j,iblk)
          enddo
        enddo

      ! AOMIP
        this_block = get_block(blocks_ice(iblk),iblk)         
        ilo = this_block%ilo
        ihi = this_block%ihi
        jlo = this_block%jlo
        jhi = this_block%jhi

        call compute_shortwave(nx_block, ny_block, &
                               ilo, ihi, jlo, jhi, &
                               TLON (:,:,iblk), &
                               TLAT (:,:,iblk), &
                               hm   (:,:,iblk), &
                               Qa   (:,:,iblk), &
                               cldf (:,:,iblk), &
                               fsw  (:,:,iblk))

      enddo  ! iblk
      !$OMP END PARALLEL DO

      ! Save record number
      oldrecnum = recnum

         if (dbug) then
           if (my_task == master_task) write (nu_diag,*) 'LY_bulk_data'
           vmin = global_minval(fsw,distrb_info,tmask)
                               
           vmax = global_maxval(fsw,distrb_info,tmask)
           if (my_task.eq.master_task)  &
               write (nu_diag,*) 'fsw',vmin,vmax 
           vmin = global_minval(cldf,distrb_info,tmask)
           vmax = global_maxval(cldf,distrb_info,tmask)
           if (my_task.eq.master_task) & 
               write (nu_diag,*) 'cldf',vmin,vmax
           vmin =global_minval(fsnow,distrb_info,tmask)
           vmax =global_maxval(fsnow,distrb_info,tmask)
           if (my_task.eq.master_task) & 
               write (nu_diag,*) 'fsnow',vmin,vmax
           vmin = global_minval(Tair,distrb_info,tmask)
           vmax = global_maxval(Tair,distrb_info,tmask)
           if (my_task.eq.master_task) & 
               write (nu_diag,*) 'Tair',vmin,vmax
           vmin = global_minval(uatm,distrb_info,umask)
           vmax = global_maxval(uatm,distrb_info,umask)
           if (my_task.eq.master_task) & 
               write (nu_diag,*) 'uatm',vmin,vmax
           vmin = global_minval(vatm,distrb_info,umask)
           vmax = global_maxval(vatm,distrb_info,umask)
           if (my_task.eq.master_task) & 
               write (nu_diag,*) 'vatm',vmin,vmax
           vmin = global_minval(Qa,distrb_info,tmask)
           vmax = global_maxval(Qa,distrb_info,tmask)
           if (my_task.eq.master_task)  &
               write (nu_diag,*) 'Qa',vmin,vmax

        endif                   ! dbug

      end subroutine LY_data

!=======================================================================
!
! AOMIP shortwave forcing
! standard calculation using solar declination angle
! then shortwave is reduced using a function of cloud fraction

      subroutine compute_shortwave(nx_block,  ny_block, &
                                   ilo, ihi, jlo, jhi, &
                                   TLON, TLAT, hm, Qa, cldf, fsw)

!---!-------------------------------------------------------------------
!---!-------------------------------------------------------------------

      use ice_constants, only: c0, c1, c12, c2, c180, c365, &
          c3600, p1, p5, p6, pi, secday

      integer (kind=int_kind), intent(in) :: &
         nx_block, ny_block, & ! block dimensions
         ilo,ihi,jlo,jhi       ! beginning and end of physical domain

      real (kind=dbl_kind), dimension(nx_block,ny_block), intent(in) :: &
         TLON, TLAT     , & ! longitude, latitude
         Qa             , & ! specific humidity
         cldf           , & ! cloud fraction
         hm                 ! land mask

      real (kind=dbl_kind), dimension(nx_block,ny_block),  &
         intent(inout) :: &
         fsw                ! shortwave

      real (kind=dbl_kind) :: &
         hour_angle, &
         solar_time, &
         declin    , &
         cosZ      , &
         e, d      , &
         sw0       , &
         deg2rad   

      integer (kind=int_kind) :: &
         i, j

      do j=jlo,jhi
       do i=ilo,ihi
        deg2rad = pi/c180
        solar_time = mod(real(sec,kind=dbl_kind),secday)/c3600 &
                   + c12*sin(p5*TLON(i,j))
        hour_angle = (c12 - solar_time)*pi/c12
        declin = 23.44_dbl_kind*cos((172._dbl_kind-yday) &
                 * c2*pi/c365)*deg2rad     ! use dayyr instead of c365???
        cosZ = sin(TLAT(i,j))*sin(declin) &
             + cos(TLAT(i,j))*cos(declin)*cos(hour_angle)
        cosZ = max(cosZ,c0)
        e = 1.e5*Qa(i,j)/(0.622_dbl_kind + 0.378_dbl_kind*Qa(i,j))
        d = (cosZ+2.7_dbl_kind)*e*1.e-5_dbl_kind+1.085_dbl_kind*cosZ+p1
        sw0 = 1353._dbl_kind*cosZ**2/d
        sw0 = max(sw0,c0)

        ! total downward shortwave for cice
        Fsw(i,j) = sw0*(c1-p6*cldf(i,j)**3) 
        Fsw(i,j) = Fsw(i,j)*hm(i,j)
       enddo
      enddo

      end subroutine compute_shortwave

!=======================================================================
!
! prevents humidity from being super-saturated

      subroutine Qa_fixLY(nx_block, ny_block, Tair, Qa)

      use ice_constants, only: c1, c10, c2, Tffresh, puny

      integer (kind=int_kind), intent(in) :: &
         nx_block, ny_block ! block dimensions

      real (kind=dbl_kind), dimension(nx_block,ny_block), intent(in) :: &
         Tair               ! air temperature

      real (kind=dbl_kind), dimension(nx_block,ny_block), &
         intent(inout) :: &
         Qa                 ! specific humidity

      real (kind=dbl_kind), dimension (nx_block,ny_block) :: &
         worka

      worka = Tair - Tffresh
      worka = c2 + (0.7859_dbl_kind + 0.03477_dbl_kind*worka) &
                     /(c1 + 0.00412_dbl_kind*worka) & ! 2+ converts ea mb -> Pa
                + 0.00422_dbl_kind*worka              ! for ice
      ! vapor pressure
      worka = (c10**worka)      ! saturated 
      worka = max(worka,puny)   ! puny over land to prevent division by zero
      ! specific humidity
      worka = 0.622_dbl_kind*worka/(1.e5_dbl_kind-0.378_dbl_kind*worka)

      Qa = min(Qa, worka)

      end subroutine Qa_fixLY

!=======================================================================
! HadGEM or HadGAM atmospheric forcing
!=======================================================================

      subroutine hadgem_files (yr)

! Construct filenames based on selected model options
!
! Note: The year number in these filenames does not matter, because
!       subroutine file_year will insert the correct year.
!
! author: Alison McLaren, Met Office

      use ice_therm_shared, only: calc_Tsfc 
      use ice_ocean, only: oceanmixed_ice

      integer (kind=int_kind), intent(in) :: &
           yr                   ! current forcing year

      integer (kind=int_kind) :: &
           n           ! thickness category index

      ! -----------------------------------------------------------
      ! Rainfall and snowfall
      ! -----------------------------------------------------------

      snow_file = &
           trim(atm_data_dir)//'MONTHLY/snowfall.1996.nc'
           call file_year(snow_file,yr)

      rain_file = &
           trim(atm_data_dir)//'MONTHLY/rainfall.1996.nc'
           call file_year(rain_file,yr)

      if (my_task == master_task) then
         write (nu_diag,*) ' '
         write (nu_diag,*) 'Atmospheric data files:'
         write (nu_diag,*) trim(rain_file)
         write (nu_diag,*) trim(snow_file)
      endif

      if (calc_strair) then

         ! --------------------------------------------------------
         ! Wind velocity
         ! --------------------------------------------------------

         uwind_file = &
           trim(atm_data_dir)//'MONTHLY/u_10.1996.nc'
           call file_year(uwind_file,yr)

         vwind_file = &
           trim(atm_data_dir)//'MONTHLY/v_10.1996.nc'
           call file_year(vwind_file,yr)

         if (my_task == master_task) then
            write (nu_diag,*) trim(uwind_file)
            write (nu_diag,*) trim(vwind_file)
         endif

      else

         ! --------------------------------------------------------
         ! Wind stress
         ! --------------------------------------------------------

         strax_file = &
              trim(atm_data_dir)//'MONTHLY/taux.1996.nc'
         call file_year(strax_file,yr)

         stray_file = &
              trim(atm_data_dir)//'MONTHLY/tauy.1996.nc'
         call file_year(stray_file,yr)

         if (my_task == master_task) then
            write (nu_diag,*) trim(strax_file)
            write (nu_diag,*) trim(stray_file)
         endif

         if (calc_Tsfc .or. oceanmixed_ice) then

            ! --------------------------------------------------
            ! Wind speed
            ! --------------------------------------------------

            wind_file = &
               trim(atm_data_dir)//'MONTHLY/wind_10.1996.nc'
            call file_year(wind_file,yr)

            if (my_task == master_task) then
               write (nu_diag,*) trim(wind_file)
            endif

         endif   ! calc_Tsfc or oceanmixed_ice

      endif  ! calc_strair

      ! --------------------------------------------------------------
      ! Atmosphere properties.  Even if these fields are not 
      ! being used to force the ice (i.e. calc_Tsfc=.false.), they
      ! are still needed to generate forcing for mixed layer model or
      ! to calculate wind stress
      ! --------------------------------------------------------------

       if (calc_Tsfc .or. oceanmixed_ice .or. calc_strair) then  

         fsw_file = &
           trim(atm_data_dir)//'MONTHLY/SW_incoming.1996.nc'
           call file_year(fsw_file,yr)

         flw_file = &
           trim(atm_data_dir)//'MONTHLY/LW_incoming.1996.nc'
           call file_year(flw_file,yr)

         tair_file = &
           trim(atm_data_dir)//'MONTHLY/t_10.1996.nc'
           call file_year(tair_file,yr)

         humid_file = &
           trim(atm_data_dir)//'MONTHLY/q_10.1996.nc'
           call file_year(humid_file,yr)

         rhoa_file = &
           trim(atm_data_dir)//'MONTHLY/rho_10.1996.nc'
           call file_year(rhoa_file,yr)

         if (my_task == master_task) then
            write (nu_diag,*) trim(fsw_file)
            write (nu_diag,*) trim(flw_file)
            write (nu_diag,*) trim(tair_file)
            write (nu_diag,*) trim(humid_file)
            write (nu_diag,*) trim(rhoa_file)
         endif                     ! master_task

      endif ! calc_Tsfc or oceanmixed_ice  or calc_strair

      if (.not. calc_Tsfc) then

         ! ------------------------------------------------------
         ! Sublimation, topmelt and botmelt
         ! ------------------------------------------------------

         do n = 1, ncat

            ! 'topmelt' = fsurf - fcondtop.
            write(topmelt_file(n), '(a,i1,a)')  &
              trim(atm_data_dir)//'MONTHLY/topmeltn',n,'.1996.nc'
              call file_year(topmelt_file(n),yr)

            ! 'botmelt' = fcondtop. 
            write(botmelt_file(n), '(a,i1,a)')  &
              trim(atm_data_dir)//'MONTHLY/botmeltn',n,'.1996.nc'
              call file_year(botmelt_file(n),yr)

         enddo

         ! 'sublim' = - flat / Lsub. 
         sublim_file = &
           trim(atm_data_dir)//'MONTHLY/sublim.1996.nc'
           call file_year(sublim_file,yr)

         if (my_task == master_task) then
            do n = 1, ncat
               write (nu_diag,*) trim(topmelt_file(n))
               write (nu_diag,*) trim(botmelt_file(n))
            enddo
            write (nu_diag,*) trim(sublim_file)

         endif

      endif  ! .not. calc_Tsfc

      end subroutine hadgem_files

!=======================================================================

! read HadGEM or HadGAM atmospheric data

      subroutine hadgem_data

! authors: Alison McLaren, Met Office

      use ice_constants, only: p5, Lsub, &
          field_loc_center, field_type_scalar, field_type_vector
      use ice_domain, only: nblocks
      use ice_flux, only: fsnow, frain, uatm, vatm, strax, stray, wind, &
          fsw, flw, Tair, rhoa, Qa, fcondtopn_f, fsurfn_f, flatn_f
      use ice_state, only: aice,aicen
      use ice_ocean, only: oceanmixed_ice
      use ice_therm_shared, only: calc_Tsfc

      integer (kind=int_kind) :: &
          i, j        , & ! horizontal indices
          n           , & ! thickness category index
          iblk        , & ! block index
          ixm,ixp     , & ! record numbers for neighboring months
          maxrec      , & ! maximum record number
          recslot     , & ! spline slot for current record
          midmonth        ! middle day of month

      logical (kind=log_kind) :: readm

      real (kind=dbl_kind), dimension(nx_block,ny_block,max_blocks) :: &
            topmelt, & ! temporary fields
            botmelt, &
            sublim

      character (char_len) :: & 
            fieldname    ! field name in netcdf file

    !-------------------------------------------------------------------
    ! monthly data
    !
    ! Assume that monthly data values are located in the middle of the
    ! month.
    !-------------------------------------------------------------------

      midmonth = 15  ! data is given on 15th of every month
!      midmonth = fix(p5 * real(daymo(month)))  ! exact middle

      ! Compute record numbers for surrounding months
      maxrec = 12
      ixm  = mod(month+maxrec-2,maxrec) + 1
      ixp  = mod(month,         maxrec) + 1
      if (mday >= midmonth) ixm = -99  ! other two points will be used
      if (mday <  midmonth) ixp = -99

      ! Determine whether interpolation will use values 1:2 or 2:3
      ! recslot = 2 means we use values 1:2, with the current value (2)
      !  in the second slot
      ! recslot = 1 means we use values 2:3, with the current value (2)
      !  in the first slot
      recslot = 1                             ! latter half of month
      if (mday < midmonth) recslot = 2        ! first half of month

      ! Find interpolation coefficients
      call interp_coeff_monthly (recslot)

      ! Read 2 monthly values
      readm = .false.
      if (istep==1 .or. (mday==midmonth .and. sec==0)) readm = .true.

      ! -----------------------------------------------------------
      ! Rainfall and snowfall
      ! -----------------------------------------------------------

      fieldname='rainfall'
      call read_data_nc (readm, 0, fyear, ixm, month, ixp, &
                      maxrec, rain_file, fieldname, frain_data, &
                      field_loc_center, field_type_scalar)
      fieldname='snowfall'
      call read_data_nc (readm, 0, fyear, ixm, month, ixp, &
                      maxrec, snow_file, fieldname, fsnow_data, &
                      field_loc_center, field_type_scalar)

      ! Interpolate to current time step
      call interpolate_data (fsnow_data, fsnow)
      call interpolate_data (frain_data, frain)

      if (calc_strair) then

         ! --------------------------------------------------------
         ! Wind velocity
         ! --------------------------------------------------------

         fieldname='u_10'
         call read_data_nc (readm, 0, fyear, ixm, month, ixp, &
                      maxrec, uwind_file, fieldname, uatm_data, &
                      field_loc_center, field_type_vector)
         fieldname='v_10'
         call read_data_nc (readm, 0, fyear, ixm, month, ixp, &
                      maxrec, vwind_file, fieldname, vatm_data, &
                      field_loc_center, field_type_vector)

         ! Interpolate to current time step
         call interpolate_data (uatm_data, uatm)
         call interpolate_data (vatm_data, vatm)

      else

         ! --------------------------------------------------------
         ! Wind stress
         ! --------------------------------------------------------

         fieldname='taux'
         call read_data_nc (readm, 0, fyear, ixm, month, ixp, &
                      maxrec, strax_file, fieldname, strax_data, &
                      field_loc_center, field_type_vector)
         fieldname='tauy'
         call read_data_nc (readm, 0, fyear, ixm, month, ixp, &
                      maxrec, stray_file, fieldname, stray_data, &
                      field_loc_center, field_type_vector)

         ! Interpolate to current time step
         call interpolate_data (strax_data, strax)
         call interpolate_data (stray_data, stray)

         if (calc_Tsfc .or. oceanmixed_ice) then

            ! --------------------------------------------------
            ! Wind speed
            ! --------------------------------------------------

            fieldname='wind_10'
            call read_data_nc (readm, 0, fyear, ixm, month, ixp, &
                      maxrec, wind_file, fieldname, wind_data, &
                      field_loc_center, field_type_scalar)

            ! Interpolate to current time step
            call interpolate_data (wind_data, wind)

         endif   ! calc_Tsfc or oceanmixed_ice

      endif      ! calc_strair

      ! -----------------------------------------------------------
      ! SW incoming, LW incoming, air temperature, density and 
      ! humidity at 10m.  
      !
      ! Even if these fields are not being used to force the ice 
      ! (i.e. calc_Tsfc=.false.), they are still needed to generate 
      ! forcing for mixed layer model or to calculate wind stress
      ! -----------------------------------------------------------

      if (calc_Tsfc .or. oceanmixed_ice .or. calc_strair) then  

         fieldname='SW_incoming'
         call read_data_nc (readm, 0, fyear, ixm, month, ixp, &
                      maxrec, fsw_file, fieldname, fsw_data, &
                      field_loc_center, field_type_scalar)
         fieldname='LW_incoming'
         call read_data_nc (readm, 0, fyear, ixm, month, ixp, &
                      maxrec, flw_file, fieldname, flw_data, &
                      field_loc_center, field_type_scalar)
         fieldname='t_10'
         call read_data_nc (readm, 0, fyear, ixm, month, ixp, &
                      maxrec, tair_file, fieldname, Tair_data, &
                      field_loc_center, field_type_scalar)
         fieldname='rho_10'
         call read_data_nc (readm, 0, fyear, ixm, month, ixp, &
                      maxrec, rhoa_file, fieldname, rhoa_data, &
                      field_loc_center, field_type_scalar)
         fieldname='q_10'
         call read_data_nc (readm, 0, fyear, ixm, month, ixp, &
                      maxrec, humid_file, fieldname, Qa_data, &
                      field_loc_center, field_type_scalar)

         ! Interpolate onto current timestep

         call interpolate_data (fsw_data,   fsw)
         call interpolate_data (flw_data,  flw)
         call interpolate_data (Tair_data, Tair)
         call interpolate_data (rhoa_data, rhoa)
         call interpolate_data (Qa_data,   Qa)

      endif       ! calc_Tsfc or oceanmixed_ice or calc_strair

      if (.not. calc_Tsfc) then

         ! ------------------------------------------------------
         ! Sublimation, topmelt and botmelt
         ! ------------------------------------------------------

         fieldname='sublim'
         call read_data_nc (readm, 0, fyear, ixm, month, ixp, &
                      maxrec, sublim_file, fieldname, sublim_data, &
                      field_loc_center, field_type_scalar)

         ! Interpolate to current time step
         call interpolate_data (sublim_data, sublim)

         do n = 1, ncat
            write(fieldname, '(a,i1)') 'topmeltn',n
            call read_data_nc (readm, 0, fyear, ixm, month, ixp, &
              maxrec, topmelt_file(n), fieldname, topmelt_data(:,:,:,:,n), &
              field_loc_center, field_type_scalar)

            write(fieldname, '(a,i1)') 'botmeltn',n
            call read_data_nc (readm, 0, fyear, ixm, month, ixp, &
              maxrec, botmelt_file(n), fieldname, botmelt_data(:,:,:,:,n), &
              field_loc_center, field_type_scalar)

            call interpolate_data (topmelt_data(:,:,:,:,n), topmelt)
            call interpolate_data (botmelt_data(:,:,:,:,n), botmelt)

            !--------------------------------------------------------
            ! Convert from UM variables to CICE variables
            !  topmelt = fsurf - fcondtop
            !  botmelt = fcondtop  (as zero layer)
            !
            ! Convert UM sublimation data into CICE LH flux
            ! (sublim = - flatn / Lsub) and have same value for all 
            ! categories
            !--------------------------------------------------------

            !$OMP PARALLEL DO PRIVATE(iblk,i,j)
            do iblk = 1, nblocks
               do j = 1, ny_block
               do i = 1, nx_block
                  fcondtopn_f(i,j,n,iblk) = botmelt(i,j,iblk)
                  fsurfn_f(i,j,n,iblk)    = topmelt(i,j,iblk) & 
                                            + botmelt(i,j,iblk)
                  flatn_f(i,j,n,iblk)    = - sublim(i,j,iblk)*Lsub
               enddo
               enddo
            enddo
            !$OMP END PARALLEL DO

         enddo  ! ncat

      endif   ! .not. calc_Tsfc 

      end subroutine hadgem_data

!=======================================================================
! METROMS 
!=======================================================================

      subroutine metroms_init()
      character (char_len_long) :: info_file

      info_file = &
        trim(atm_data_dir)//'vwind.txt'
      call metroms_init_forcing_cal(info_file, &
         metroms_dates_vwind, &
         metroms_index_vwind, &
         metroms_fname_vwind)

      info_file = &
        trim(atm_data_dir)//'uwind.txt'
      call metroms_init_forcing_cal(info_file, &
         metroms_dates_uwind, &
         metroms_index_uwind, &
         metroms_fname_uwind)

      info_file = &
        trim(atm_data_dir)//'tair.txt'
      call metroms_init_forcing_cal(info_file, &
         metroms_dates_tair, &
         metroms_index_tair, &
         metroms_fname_tair)

      info_file = &
        trim(atm_data_dir)//'rhoa.txt'
      call metroms_init_forcing_cal(info_file, &
         metroms_dates_rhoa, &
         metroms_index_rhoa, &
         metroms_fname_rhoa)

      info_file = &
        trim(atm_data_dir)//'rain.txt'
      call metroms_init_forcing_cal(info_file, &
         metroms_dates_rain, &
         metroms_index_rain, &
         metroms_fname_rain)

      info_file = &
        trim(atm_data_dir)//'humid.txt'
      call metroms_init_forcing_cal(info_file, &
         metroms_dates_humid, &
         metroms_index_humid, &
         metroms_fname_humid)

      info_file = &
        trim(atm_data_dir)//'cldf.txt'
      call metroms_init_forcing_cal(info_file, &
         metroms_dates_cldf, &
         metroms_index_cldf, &
         metroms_fname_cldf)

      end subroutine metroms_init
!- - - - 

      subroutine metroms_init_forcing_cal(info_file, &
            metroms_dates, metroms_index, metroms_path)

      character (char_len_long), intent(in) :: &
         info_file  ! path to file containg dates, index and filepaths
      real (kind=dbl_kind), dimension(:), allocatable, intent(out) :: &
         metroms_dates
      integer (kind=int_kind), dimension(:), allocatable, intent(out) :: &
         metroms_index
      character (char_len_long), dimension(:), allocatable, intent(out) :: &
         metroms_path

      character (char_len_long) :: &
         path         ! forcing file path
      real (dbl_kind)  :: &
         date         ! forcing date

      integer (kind=int_kind) :: &
         u, & ! file unit
         lines, & ! number of lines in info file
         ind, &   ! index in a forcing file
         refdate, &  ! forcing reference date
         i
      open(unit=u,file=info_file)
      read(unit=u,fmt="(I7,A20)") lines, path ! path is just some bogus
       ! keep it open for loop  below

      refdate = 19700101
      allocate(metroms_dates(lines), &
               metroms_index(lines), &
               metroms_path(lines))

      metroms_offset = metroms_init_cal_offset(refdate)

      do i=1,lines
         read(unit=u,fmt="(F8.2,I5,A120)") date, ind, path ! FIXME cleanup fmt..
         metroms_dates(i) = date
         metroms_index(i) = ind
         metroms_path(i) = trim(adjustl(path))
      enddo
 
      close(unit=u)

      end subroutine metroms_init_forcing_cal
!- - - - - - - -
      function  metroms_init_cal_offset(refdate) result(days)

      use ice_calendar, only: daymo360, daymo365, daymo366, idate0, &
                               use_leap_years
      integer (kind=int_kind) :: refdate, &
         ERA_refY, ERA_refM, ERA_refD, &
         cice_refY, cice_refM, cice_refD, &
         y, y1, m1, d1, y2, m2, d2

      real (kind=dbl_kind) :: days

      ERA_refY = refdate/10000
      ERA_refM = (refdate-ERA_refY*10000)/100
      ERA_refD = (refdate-ERA_refY*10000-ERA_refM*100)

      cice_refY = idate0/10000
      cice_refM = (idate0-cice_refY*10000)/100
      cice_refD = (idate0-cice_refY*10000-cice_refM*100)

      ! count days between reference dates
      if (refdate<idate0) then
         y1 = ERA_refY
         y2 = cice_refY
         m1 = ERA_refM
         m2 = cice_refM
         d1 = ERA_refD
         d2 = cice_refD
      else
         y1 = cice_refY
         y2 = ERA_refY
         m1 = cice_refM
         m2 = ERA_refM
         d1 = cice_refD
         d2 = ERA_refD
      end if

      days = 0
      do y=y1,y2
        ! set daymo
        if (use_leap_years) then
           if (is_leap_year(y)) then
              daymo = daymo366
           else
              daymo = daymo365
           endif
        else if (days_per_year == 365) then
           daymo = daymo365
        else
           daymo = daymo360
        endif
        ! add days for year y
        if (y==y1 .and. y==y2) then
           if (m2-m1 > 0) then
              days = sum(daymo(m1:m2-1)) - d1 + d2 !
           else
              days = d2-d1
           endif
        elseif (y==y1) then
           days = daymo(m1)-d1+1 !include first day
           if (m1<12) then
              days = days + sum(daymo(m1+1:12))
           endif
        elseif (y==y2) then
           days = days + d2-1 ! exclude last day
           if (m2>1) then
              days = days + sum(daymo(1:m2-1))
           endif
        else
             days =  days + sum(daymo(:))
        endif
      end do
      end function metroms_init_cal_offset

!METNO START
!=======================================================================
! ECMWF atmospheric forcing
!=======================================================================

      subroutine ecmwf_files (yr)

! Construct filenames for ECMWF data.
! Edit for other directory structures or filenames.
! Note: The year number in these filenames does not matter, because
!       subroutine file_year will insert the correct year.
! authors: Keguang Wang, met.no

      integer (kind=int_kind), intent(in) :: yr ! current forcing year

! rain, downward solar radiation and longwave radition in the same file
      rain_file  = trim(atm_data_dir)//'FC_1996_unlim.nc'
      call file_year(rain_file,yr)

! Uwind, Vwind, Pair, Qair, Tair, cloud, d2m in the same file
      uwind_file = trim(atm_data_dir)//'AN_1996_unlim.nc'
      call file_year(uwind_file,yr)

      if (my_task == master_task) then
         write (nu_diag,*) ' '
         write (nu_diag,*) 'Forcing data year =', fyear
         write (nu_diag,*) 'Atmospheric data files:'
         write (nu_diag,*) trim(rain_file)
         write (nu_diag,*) trim(uwind_file)
      endif                     ! master_task

      end subroutine ecmwf_files

!======================================================================

! read atmospheric data using metroms scheme

! author: Sebastian Mårtensson, graa-software.se

      subroutine metroms_data

      use ice_flux, only: fsnow, frain, uatm, vatm, strax, stray, wind, &
          fsw, flw, Tair, rhoa, Qa, fcondtopn_f, fsurfn_f, flatn_f

      use ice_constants, only: p5, c1, c2, c4, c12, secday, &
        field_loc_center, field_type_vector, field_type_scalar, Tffresh

      use ice_calendar, only: tday, yday
      use ice_grid, only: hm, tlon, tlat
      use ice_blocks, only: block, get_block
      use ice_domain, only: nblocks, blocks_ice

      integer (kind=int_kind), dimension(3) :: ip !index_pair
      character (char_len) :: &
        fieldname

      integer (kind=int_kind) :: &
          i, j        , & ! horizontal indices
          iblk        , & ! block index
          ilo,ihi,jlo,jhi, & ! beginning and end of physical domain
          recnum, maxrec
          
      type (block) :: &
         this_block           ! block information for current block

      logical (kind=log_kind) :: should_loop, read12

      real (kind=dbl_kind) :: &
          qa_cff1,qa_cff2,qa_cff,    &! coefficients (used to calculate specific humidity)
          e_sat,              &! saturation vapour pressure (used to calculate specific humidity)
          vap_p,               &! vapour pressure (used to calculate specific humidity)
          precip_factor, now, sec6hr, &
          sec12hr


      should_loop = .false.

      fieldname = 'rain'

     ! now = metroms_offset + tday + mod(time,secday)/secday
      now = metroms_offset + time/secday
      ip = metroms_get_index(metroms_dates_rain, now,should_loop,metroms_ip_rain)
      if (ip(3) == 1) then ! ip(3) = read/don't read flag. Don't read if index hasn't been updated
         metroms_ip_rain = ip
         call metroms_read( &
            metroms_fname_rain(ip(2)), metroms_index_rain(ip(2)), &
            fieldname, fsnow, .false., &
            field_loc_center, field_type_scalar)    

         if (trim(precip_units) == 'mm_per_month') then
            precip_factor = c12/(secday*days_per_year)
         elseif (trim(precip_units) == 'mm_per_day') then
            precip_factor = c1/secday
         elseif (trim(precip_units) == 'm_per_12hr') then
            precip_factor = c1/43.2_dbl_kind
         elseif (trim(precip_units) == 'mm_per_sec' .or. &
              trim(precip_units) == 'mks') then
            precip_factor = c1    ! mm/sec = kg/m^2 s
         endif
         do iblk = 1, nblocks !
            fsnow(:,:,iblk)=fsnow(:,:,iblk)*precip_factor
         end do

      endif

      fieldname = 'Uwind'
      call metroms_read_and_interpolate(fieldname, &
        metroms_dates_uwind, metroms_index_uwind, &
        metroms_fname_uwind, uatm, uatm_data, &
        field_loc_center, field_type_vector, &
        should_loop, metroms_ip_uwind)

      fieldname = 'Vwind'
      call metroms_read_and_interpolate(fieldname, &
        metroms_dates_vwind, metroms_index_vwind, &
        metroms_fname_vwind, vatm, vatm_data, &
        field_loc_center, field_type_vector, &
        should_loop, metroms_ip_vwind)

      fieldname = 'Tair'
      call metroms_read_and_interpolate(fieldname, &
        metroms_dates_tair,  metroms_index_tair, &
        metroms_fname_tair, Tair, Tair_data,  &
        field_loc_center, field_type_scalar, &
        should_loop, metroms_ip_tair)

      !Tair = Tair + Tffresh

      fieldname = 'Pair'
      call metroms_read_and_interpolate(fieldname, &
        metroms_dates_rhoa,  metroms_index_rhoa, &
        metroms_fname_rhoa, rhoa, rhoa_data,  &
        field_loc_center, field_type_scalar, &
        should_loop, metroms_ip_rhoa)

      !rhoa = rhoa / (Tair * 287.058_dbl_kind)

      fieldname = 'Qair'
      call metroms_read_and_interpolate(fieldname, &
        metroms_dates_humid, metroms_index_humid, &
        metroms_fname_humid, Qa, Qa_data,&
        field_loc_center, field_type_scalar, &
        should_loop, metroms_ip_humid)

      ! Conversions

      ! Test if humidity is relative or specific
      if (maxval(Qa) > 10.0) then
      	 ! Relative humidity to specific humidity
      	 do iblk=1,nblocks
            do j=1,ny_block
	       do i=1,nx_block
	       	  qa_cff1 = 0.7859_dbl_kind + 0.03477_dbl_kind*Tair(i,j,iblk)
	    	  qa_cff2 = 1.0_dbl_kind + 0.00412_dbl_kind*Tair(i,j,iblk)
	    	  vap_p = Qa(i,j,iblk)*10.0_dbl_kind**(qa_cff1/qa_cff2 + 2.0_dbl_kind)
	    	  Qa(i,j,iblk)=0.622_dbl_kind*vap_p/(rhoa(i,j,iblk) - 0.378_dbl_kind*vap_p)
	       enddo
	    enddo
         enddo	    
       endif	        

      ! Celsius to kelvin
      Tair = Tair + Tffresh

      ! Pressure to density
      rhoa = rhoa / (Tair * 287.058_dbl_kind)

      fieldname = 'cloud'
      call metroms_read_and_interpolate(fieldname, &
        metroms_dates_cldf, metroms_index_cldf, &
        metroms_fname_cldf, cldf, cldf_data, &
        field_loc_center, field_type_scalar, &
        should_loop, metroms_ip_cldf)


      !$OMP PARALLEL DO PRIVATE(iblk,i,j,ilo,ihi,jlo,jhi,this_block)
      do iblk = 1, nblocks
      ! AOMIP
        this_block = get_block(blocks_ice(iblk),iblk)
        ilo = this_block%ilo
        ihi = this_block%ihi
        jlo = this_block%jlo
        jhi = this_block%jhi

        call compute_shortwave(nx_block, ny_block, &
                               ilo, ihi, jlo, jhi, &
                               TLON (:,:,iblk), &
                               TLAT (:,:,iblk), &
                               hm   (:,:,iblk), &
                               Qa   (:,:,iblk), &
                               cldf (:,:,iblk), &
                               fsw  (:,:,iblk))

      enddo  ! iblk
      !$OMP END PARALLEL DO

      end subroutine metroms_data

! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! Helper routines and functions

      subroutine metroms_read_and_interpolate(fieldname, &
             dates, ind, paths, field,field_data, &
             field_loc, field_type, should_loop, old_ip)
       

      use ice_calendar, only: tday
      use ice_constants, only: secday,c1


      integer (kind=int_kind), intent(in) :: &
           field_loc, &      ! location of field on staggered grid
           field_type        ! type of field (scalar, vector, angle)
 
     logical (kind=log_kind), intent(in) :: &
         should_loop

        character (char_len), intent(in) :: &
           fieldname
        real (kind=dbl_kind), dimension(:), intent(in) :: &
           dates
        integer (kind=int_kind), dimension(:), intent(in) :: &
           ind
        character (char_len_long), dimension(:), intent(in) :: &
           paths
        real (kind=dbl_kind), dimension(nx_block,ny_block,max_blocks), &
              intent(out) :: &
           field
        integer (kind=int_kind), dimension(3), intent(inout) :: &
           old_ip
        real (kind=dbl_kind), dimension(nx_block,ny_block,2,max_blocks), &
           intent(inout) :: &
           field_data

        ! local vars
        integer (kind=int_kind), dimension(3) :: & ! index_pair + read flag
             ip
        logical (kind=log_kind) :: &
           diag = .false.
        real (kind=dbl_kind) :: &
           now


      !tt=secday*(yday - c1) + sec
        !now = metroms_offset + tday-c1 + mod(time,secday)/secday 
        now = metroms_offset + time/secday
        diag = .true.

        ip = metroms_get_index(dates, now,should_loop,old_ip) 
       
 
        if (ip(3) == 1) then ! ip(3) = read/don't read flag. Don't read if index hasn't been updated
           if (ip(1) == old_ip(2)) then
!jd Old time-level allready present, should ideally swap indexes, but copy array for now
              field_data(:,:,1,:) = field_data(:,:,2,:)
           else
!jd Have to read old field also
              call metroms_read(paths(ip(1)), ind(ip(1)), fieldname, &
                   field_data(:,:,1,:), diag, &
                   field_loc, field_type)
           end if
           call metroms_read(paths(ip(2)), ind(ip(2)), fieldname, &
                 field_data(:,:,2,:), diag, &
                 field_loc, field_type)
           old_ip = ip

        endif
        call interpolate_data(field_data, field)
        
      end subroutine metroms_read_and_interpolate

!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

      function metroms_get_index(dates,now,should_loop,old_ip) result(index_pair)

!      use ice_calendar, only: sec, yday
      use ice_constants, only: c1


!== 

      use ice_constants, only:  p5, c4, secday

      integer (kind=int_kind) :: &
          recnum  
 
      real (kind=dbl_kind) :: &
          sec6hr                    ! seconds in data interval

      ! local variables

      real (kind=dbl_kind) :: &
          secyr            ! seconds in a year

      real (kind=dbl_kind) :: &
          tt           , & ! seconds elapsed in current year
          t1, t2       , & ! seconds elapsed at data points
          rcnum, &          ! recnum => dbl_kind
          c1intp_, c2intp_
!==

      real (kind=dbl_kind), dimension(:) :: &
         dates
      real (kind=dbl_kind) ::  &
         now ! current model time
      logical (kind=log_kind) :: &
         should_loop
      integer (kind=int_kind), dimension(3) :: &
         old_ip
      integer (kind=int_kind), dimension(3) :: &
         index_pair

      integer (kind=int_kind) :: &
         i
      real (kind=dbl_kind) :: &
         dt

      if (should_loop) then
        index_pair(1) = size(dates)
        index_pair(2) = 1
      else ! if not loop, reuse last/first datapoint
        index_pair(1) = 1
        index_pair(2) = size(dates)
      endif
      do i=1,size(dates)
         if (dates(i) <= now) then
            index_pair(1) = i !keep updating as long as 'now' is not reached
         elseif (dates(i) > now) then
            index_pair(2) = i
            exit ! both are now set
         endif
      enddo
      if (index_pair(1) == old_ip(1) .and. &
          index_pair(2) == old_ip(2)) then
         index_pair(3) = 0
      else
         index_pair(3) = 1
      endif

      !interpolation constants
      ! this version has reduced accuracy by a factor 10 because we use seconds since
      ! some year, not seconds in this year. Don't think it matters
      ! though... (i.e., numbers are 10x bigger, difference is still the
      ! same -> less precision) (unless 'sec' already has lost some
      ! precision)
      if (dates(index_pair(1)) == dates(index_pair(2))) then
         c1intp = 0.5
         c2intp = 0.5
      elseif (dates(index_pair(1)) < dates(index_pair(2))) then
         c1intp = (dates(index_pair(2)) - now) / &
                     (dates(index_pair(2)) - dates(index_pair(1)))
         c2intp = c1 - c1intp
      else
         print *, 'Looping forcing data is not correctly implemented'
         !assume timesteps are constant so we can use any pair.
         dt = dates(2) - dates(1) ! assume constant dt
         if (now > dates(index_pair(1))) then ! at beginning, ip(1) = last record
            ! this code path is never taken, right?
            c1intp = (dates(index_pair(2))-dt - now) / dt 
            c2intp = c1 - c1intp
         else
            c1intp = (dates(index_pair(1))+dt - now) / dt
            c2intp = c1 - c1intp
         endif
      endif


!      sec6hr = secday/c4        ! seconds in 6 hours

!      maxrec=4*nint(dayyr)          !  Takes acount of leap-years
!      recnum = 1 + 4*int(yday-1)  + int(real(sec,kind=dbl_kind)/sec6hr)

!      tt=secday*(yday - c1) + sec

      ! Find neighboring times
!      rcnum = real(recnum,kind=dbl_kind)
!      t2 = rcnum*sec6hr
!      t1 = t2 - sec6hr            !  - 1 interval

      ! Compute coefficients
!      c1intp_ =  abs((t2 - tt) / (t2 - t1))
!      c2intp_ =  c1 - c1intp
      
      
!      write(507,*) 'c1-c1_, c1, now, my task: ', c1intp-c1intp_, c1intp, now, my_task

!      c1intp = c1intp_
!      c2intp = c2intp_

      end function metroms_get_index

! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

      subroutine metroms_read(data_file, nrec, varname, field, &
             diag, field_loc, field_type)

      use ice_global_reductions, only: global_minval, global_maxval
      use ice_domain, only: distrb_info
      use ice_grid, only: tmask

      character (char_len_long), intent(in) :: &
         data_file           ! data file to be read

      character (char_len), intent(in) :: &
         varname             ! field name in netCDF file

      integer (kind=int_kind), intent(in):: &
         nrec                ! record number to read

      integer (kind=int_kind), intent(in) :: &
           field_loc, &      ! location of field on staggered grid
           field_type        ! type of field (scalar, vector, angle)

      logical (kind=log_kind), intent(in) :: &
           diag              ! if true, write diagnostic output

     ! logical (kind=log_kind), optional, intent(in) :: &
     !      restart_ext       ! if true, read extended grid

      real (kind=dbl_kind), dimension(nx_block,ny_block,max_blocks), &
         intent(out) :: field

      ! locals
      integer (kind=int_kind) :: &
         fid                  ! file id for netCDF routines

      real (kind=dbl_kind) :: fmin,fmax

      call ice_open_nc(data_file, fid)
      call ice_read_nc(fid, nrec, varname, field, .false., &
                   field_loc, field_type)
      call ice_close_nc(fid)
      if (diag) then
           fmin = global_minval(field,distrb_info,tmask)
           fmax = global_maxval(field,distrb_info,tmask)
           if (my_task.eq.master_task)  &
               write (nu_diag,*) trim(varname),fmin,fmax
      endif

      end subroutine metroms_read

! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

      function is_leap_year(year) result(leapyear)
      integer (kind=int_kind) :: year
      logical (kind=log_kind) :: leapyear

      if (mod(year,4)/=0) then
        leapyear = .false.
      elseif (mod(year,100)/=0) then
        leapyear = .true.
      elseif (mod(year,400)/=0) then
        leapyear = .false.
      else
        leapyear = .true.
      endif
      end function is_leap_year

! - - - - end helper functions metroms - - - - - - - - - - - - - - - -

!=======================================================================

! read ecmwf atmospheric data

      subroutine ecmwf_data

! authors: Keguang Wang, Met.no
! Modified: Jens Boldingh Debernard, Met.no

      use ice_diagnostics, only: check_step
      use ice_blocks, only: block, get_block
      use ice_constants, only: p5, c1, c2, c4, c12, Lsub, secday, &
          field_loc_center, field_type_scalar, field_type_vector, Tffresh
      use ice_domain, only: nblocks, blocks_ice
      use ice_flux, only: fsnow, frain, uatm, vatm, strax, stray, wind, &
          fsw, flw, Tair, rhoa, Qa, fcondtopn_f, fsurfn_f, flatn_f
      use ice_state, only: aice,aicen
      use ice_ocean, only: oceanmixed_ice
      use ice_therm_shared, only: calc_Tsfc
      use ice_grid, only: hm, tlon, tlat, tmask, umask

      integer (kind=int_kind) :: &
          i, j        , & ! horizontal indices
          n           , & ! thickness category index
          ixm,ixx,ixp , & ! record numbers for neighboring months
          recnum      , & ! record number
          baserec     , & ! account for files not starting 1/1: 00
          fid         , & ! File unit
          dataloc     , & ! = 1 for data located in middle of time interval
                          ! = 2 for date located at end of time interval
          iblk        , & ! block index
          maxrec      , & ! maximum record number
          recslot     , & ! spline slot for current record
          midmonth    , & ! middle day of month
          ilo,ihi,jlo,jhi ! beginning and end of physical domain

      logical (kind=log_kind) :: readm, read6, read12,dbug

      real (kind=dbl_kind) :: &
          sec6hr,             &! number of seconds in 6 hours
          sec12hr,            &! number of seconds in 12 hours
          precip_factor,      &! Help
	  qa_cff1,qa_cff2,qa_cff,    &! coefficients (used to calculate specific humidity)
	  e_sat,              &! saturation vapour pressure (used to calculate specific humidity)
	  vap_p               ! vapour pressure (used to calculate specific humidity)

      character (char_len) :: & 
           fieldname    ! field name in netcdf file

      character (char_len_long) :: &
           data_file               ! data file to be read

      type (block) :: &
         this_block           ! block information for current block

      dbug=.false.
      if (istep1 > check_step) dbug = .true.  !! debugging

    !-------------------------------------------------------------------
    ! 12-hourly data
    !
    ! Assume that the 12-hourly value accumulated or averaged values over the 
    ! last period. The actuall valid time for the value is therefore
    ! 12/2 = 6 hours earlier.
    !  This is the convention for ECMWF reanalysis data.
    !  E.g. record 1 is and average of the first 12 hours 1 January.
    ! We do not time interpolate these data.
    !-------------------------------------------------------------------

      sec12hr = secday/c2       ! seconds in 12 hours
      sec6hr = secday/c4        ! seconds in 6 hours

      maxrec=2*dayyr             ! Takes acount of leap-years 
                                 ! from 12UTC 1/1 to 24UTC 31/12

! current record number
!jd First record in flux file is at 12 UTC 1/1, and valid for the preciding 12-hrs. 
      recnum = 1 + 2*(int(yday)-1) + int(real(sec,kind=dbl_kind)/sec12hr)

      ! Read
      read12 = .false.
      if (oldrecnum12 .ne. recnum) read12 = .true.
      ! Save record number for next time step
      oldrecnum12 = recnum

      ! -----------------------------------------------------------
      ! read atmospheric forcing 
      ! -----------------------------------------------------------

      if (read12) then

         if ( my_task == master_task ) then
            write (nu_diag,'(a,i10,a,i02,a,2i7,a,i4)') &
                ' Reads 12 hourly fields at', fyear*10000+month*100+mday &
                ,':',sec/3600, ' recnum, maxrec ',recnum, maxrec &
                , ' days_in-year', int(dayyr)
            write(nu_diag,*) trim(fieldname),' read from ', trim(data_file)
         end if

         call file_year(rain_file, fyear) ! Ensure correct year-file
         call ice_open_nc(rain_file,fid)

         fieldname='rain'
         call ice_read_nc (fid, recnum, fieldname, fsnow, dbug, &
              field_loc_center, field_type_scalar)

         call ice_close_nc(fid)

      endif

    !-------------------------------------------------------------------
    ! 6-hourly data
    ! 
    ! Assume that the 6-hourly value is located at the initial of the
    !  6-hour period.  This is the convention for ECMWF reanalysis data.
    !  E.g. record 1 gives conditions at 0 am GMT on 1 January.
    !-------------------------------------------------------------------

      sec6hr = secday/c4        ! seconds in 6 hours
      maxrec=4*nint(dayyr)      !  Takes acount of leap-years

      ! current record number
      recnum = 1 + 4*int(yday-1)  + int(real(sec,kind=dbl_kind)/sec6hr)

      ! Compute record numbers for surrounding data (2 on each side)
      ixm = -99
      ixx = mod(recnum-1,       maxrec) + 1
      ixp = mod(recnum,         maxrec) + 1

      ! Compute interpolation coefficients
      ! If data is located at the end of the time interval, then the
      !  data value for the current record goes in slot 2

      recslot = 2
      dataloc = 2
      call interp_coeff (recnum, recslot, sec6hr, dataloc)
      if (my_task == master_task) &
          write(nu_diag,*) '6-hr interp_coeff ', c1intp, c2intp 

      ! Read
      read6 = .false.
      if (istep==1 .or. oldrecnum6 .ne. recnum) read6 = .true.
      ! Save record number for next time step
      oldrecnum6 = recnum

      if (read6) then

         if (my_task == master_task ) &
             write (nu_diag,'(a,i10,a,i02,a,2i5,a,i4)') &
             ' Reads 6 hourly fields at',fyear*10000+month*100+mday,':',sec/3600 &
             , ' recnum, maxrec ',recnum, maxrec, ' days_in-year', int(dayyr) 

      ! -----------------------------------------------------------
      ! read atmospheric forcing 
      ! -----------------------------------------------------------

         call file_year(uwind_file, fyear) ! Ensure correct year-file
         call ice_open_nc(uwind_file,fid)

         fieldname = 'Uwind'
         call ice_read_nc (fid, ixx, fieldname, uatm_data(:,:,1,:), dbug, &
              field_loc_center, field_type_scalar)
         fieldname = 'Vwind'
         call ice_read_nc (fid, ixx, fieldname, vatm_data(:,:,1,:), dbug, &
              field_loc_center, field_type_scalar)
         fieldname = 'Tair'
         call ice_read_nc (fid, ixx, fieldname, Tair_data(:,:,1,:), dbug, &
              field_loc_center, field_type_scalar)
         fieldname = 'Pair'
         call ice_read_nc (fid, ixx, fieldname, rhoa_data(:,:,1,:), dbug, &
              field_loc_center, field_type_scalar)
         fieldname = 'Qair'
         call ice_read_nc (fid, ixx, fieldname, Qa_data(:,:,1,:), dbug, &
              field_loc_center, field_type_scalar)
         fieldname = 'cloud'
         call ice_read_nc (fid, ixx, fieldname, cldf_data(:,:,1,:), dbug, &
              field_loc_center, field_type_scalar)

         if (ixp < ixx) then
            if (fyear < fyear_final) then
               call ice_close_nc(fid)
               call file_year(uwind_file, fyear+1) ! Ensure correct year-file
               call ice_open_nc(uwind_file,fid)
            else
               ixp = ixx
            endif
         endif

         fieldname = 'Uwind'
         call ice_read_nc (fid, ixp, fieldname, uatm_data(:,:,2,:), dbug, &
              field_loc_center, field_type_scalar)
         fieldname = 'Vwind'
         call ice_read_nc (fid, ixp, fieldname, vatm_data(:,:,2,:), dbug, &
              field_loc_center, field_type_scalar)
         fieldname = 'Tair'
         call ice_read_nc (fid, ixp, fieldname, Tair_data(:,:,2,:), dbug, &
              field_loc_center, field_type_scalar)
         fieldname = 'Pair'
         call ice_read_nc (fid, ixp, fieldname, rhoa_data(:,:,2,:), dbug, &
              field_loc_center, field_type_scalar)
         fieldname = 'Qair'
         call ice_read_nc (fid, ixp, fieldname, Qa_data(:,:,2,:), dbug, &
              field_loc_center, field_type_scalar)
         fieldname = 'cloud'
         call ice_read_nc (fid, ixp, fieldname, cldf_data(:,:,2,:), dbug, &
              field_loc_center, field_type_scalar)

         call ice_close_nc(fid)

      endif

      ! Interpolate to current time step
      call interpolate_data (uatm_data, uatm)
      call interpolate_data (vatm_data, vatm)
      call interpolate_data (Tair_data, Tair)
      ! note here rhoa represents Pair in the original file
      call interpolate_data (rhoa_data, rhoa)
      call interpolate_data (Qa_data, Qa)
      call interpolate_data (cldf_data, cldf)

      ! Conversions

      ! Relative humidity to specific humidity
      do iblk=1,nblocks
        do j=1,ny_block
	  do i=1,nx_block
	    qa_cff1 = 0.7859_dbl_kind + 0.03477_dbl_kind*Tair(i,j,iblk)
	    qa_cff2 = 1.0_dbl_kind + 0.00412_dbl_kind*Tair(i,j,iblk)
	    vap_p = Qa(i,j,iblk)*10.0_dbl_kind**(qa_cff1/qa_cff2 + 2.0_dbl_kind)
	    Qa(i,j,iblk)=0.622_dbl_kind*vap_p/(rhoa(i,j,iblk) - 0.378_dbl_kind*vap_p)
	  enddo
	enddo
      enddo	    

      ! Celsius to kelvin
      Tair = Tair + Tffresh

      ! Pressure to density
      rhoa = rhoa / (Tair * 287.058_dbl_kind)





      !$OMP PARALLEL DO PRIVATE(iblk,i,j,ilo,ihi,jlo,jhi,this_block)
      do iblk = 1, nblocks

!jd This is done in prepare_forcing routine
!jd
!jd        do j = 1, ny_block
!jd          do i = 1, nx_block
!jd             call longwave_parkinson_washington(Tair(i,j,iblk), &
!jd                                     cldf(i,j,iblk), flw(i,j,iblk))
!jd          enddo
!jd        enddo

      ! AOMIP
        this_block = get_block(blocks_ice(iblk),iblk)
        ilo = this_block%ilo
        ihi = this_block%ihi
        jlo = this_block%jlo
        jhi = this_block%jhi

        call compute_shortwave(nx_block, ny_block, &
                               ilo, ihi, jlo, jhi, &
                               TLON (:,:,iblk), &
                               TLAT (:,:,iblk), &
                               hm   (:,:,iblk), &
                               Qa   (:,:,iblk), &
                               cldf (:,:,iblk), &
                               fsw  (:,:,iblk))

      enddo  ! iblk
      !$OMP END PARALLEL DO

      end subroutine ecmwf_data
!METNO END

!=======================================================================
! monthly forcing 
!=======================================================================

      subroutine monthly_files (yr)

! Construct filenames based on the LANL naming conventions for NCAR data.
! Edit for other directory structures or filenames.
! Note: The year number in these filenames does not matter, because
!       subroutine file_year will insert the correct year.

! author: Elizabeth C. Hunke, LANL

      integer (kind=int_kind), intent(in) :: &
           yr                   ! current forcing year

      flw_file = &
           trim(atm_data_dir)//'MONTHLY/cldf.omip.dat'

      rain_file = &
           trim(atm_data_dir)//'MONTHLY/prec.nmyr.dat'

      tair_file = &
           trim(atm_data_dir)//'MONTHLY/t_10.1996.dat'
      call file_year(tair_file,yr)

      humid_file = &
           trim(atm_data_dir)//'MONTHLY/q_10.1996.dat'
      call file_year(humid_file,yr)

      ! stress/speed is used instead of wind components
      strax_file = &
           trim(atm_data_dir)//'MONTHLY/strx.1996.dat'
      call file_year(strax_file,yr)

      stray_file = &
           trim(atm_data_dir)//'MONTHLY/stry.1996.dat'
      call file_year(stray_file,yr)

      wind_file = &
           trim(atm_data_dir)//'MONTHLY/wind.1996.dat'
      call file_year(wind_file,yr)

      if (my_task == master_task) then
         write (nu_diag,*) ' '
         write (nu_diag,*) 'Forcing data year = ', fyear         
         write (nu_diag,*) 'Atmospheric data files:'
         write (nu_diag,*) trim(flw_file)
         write (nu_diag,*) trim(rain_file)
         write (nu_diag,*) trim(tair_file)
         write (nu_diag,*) trim(humid_file)
         write (nu_diag,*) trim(uwind_file)
         write (nu_diag,*) trim(vwind_file)
      endif                     ! master_task

      end subroutine monthly_files

!=======================================================================
! read monthly atmospheric data

      subroutine monthly_data

      use ice_blocks, only: block, get_block
      use ice_constants, only: p5, &
          field_loc_center, field_type_scalar, field_type_vector
      use ice_global_reductions, only: global_minval, global_maxval
      use ice_domain, only: nblocks, distrb_info, blocks_ice
      use ice_flux, only: fsnow, Tair, Qa, wind, strax, stray, fsw
      use ice_grid, only: hm, tlon, tlat, tmask, umask

      integer (kind=int_kind) :: & 
          i, j        , &
          ixm,ixp     , & ! record numbers for neighboring months
          maxrec      , & ! maximum record number
          recslot     , & ! spline slot for current record
          midmonth    , & ! middle day of month
          iblk        , & ! block index
          ilo,ihi,jlo,jhi ! beginning and end of physical domain

      real (kind=dbl_kind) :: &
          vmin, vmax

      logical (kind=log_kind) :: readm

      type (block) :: &
         this_block           ! block information for current block
      
    !-------------------------------------------------------------------
    ! monthly data 
    !
    ! Assume that monthly data values are located in the middle of the 
    ! month.
    !-------------------------------------------------------------------

      midmonth = 15  ! data is given on 15th of every month
!      midmonth = fix(p5 * real(daymo(month)))  ! exact middle

      ! Compute record numbers for surrounding months
      maxrec = 12
      ixm  = mod(month+maxrec-2,maxrec) + 1
      ixp  = mod(month,         maxrec) + 1
      if (mday >= midmonth) ixm = -99  ! other two points will be used
      if (mday <  midmonth) ixp = -99

      ! Determine whether interpolation will use values 1:2 or 2:3
      ! recslot = 2 means we use values 1:2, with the current value (2)
      !  in the second slot
      ! recslot = 1 means we use values 2:3, with the current value (2)
      !  in the first slot
      recslot = 1                             ! latter half of month
      if (mday < midmonth) recslot = 2        ! first half of month

      ! Find interpolation coefficients
      call interp_coeff_monthly (recslot)

      ! Read 2 monthly values 
      readm = .false.
      if (istep==1 .or. (mday==midmonth .and. sec==0)) readm = .true.

      call read_clim_data (readm, 0, ixm, month, ixp,  &
             flw_file, cldf_data, &
             field_loc_center, field_type_scalar)
      call read_clim_data (readm, 0, ixm, month, ixp,  &
             rain_file, fsnow_data, &
             field_loc_center, field_type_scalar)
      call read_clim_data (readm, 0, ixm, month, ixp,  &
             tair_file, Tair_data, &
             field_loc_center, field_type_scalar)
      call read_clim_data (readm, 0, ixm, month, ixp,  &
             humid_file, Qa_data, &
             field_loc_center, field_type_scalar)
      call read_clim_data (readm, 0, ixm, month, ixp,  &
             wind_file, wind_data, &
             field_loc_center, field_type_scalar)
      call read_clim_data (readm, 0, ixm, month, ixp,  &
             strax_file, strax_data, &
             field_loc_center, field_type_vector)
      call read_clim_data (readm, 0, ixm, month, ixp,  &
             stray_file, stray_data, &
             field_loc_center, field_type_vector)

      call interpolate_data (cldf_data, cldf)
      call interpolate_data (fsnow_data, fsnow)  ! units mm/s = kg/m^2/s
      call interpolate_data (Tair_data, Tair)
      call interpolate_data (Qa_data, Qa)
      call interpolate_data (wind_data, wind)
      call interpolate_data (strax_data, strax)
      call interpolate_data (stray_data, stray)

      !$OMP PARALLEL DO PRIVATE(iblk,i,j,ilo,ihi,jlo,jhi,this_block)
      do iblk = 1, nblocks
        call Qa_fixLY(nx_block,  ny_block, &
                                 Tair (:,:,iblk), &
                                 Qa   (:,:,iblk))

        do j = 1, ny_block
          do i = 1, nx_block
            Qa   (i,j,iblk) = Qa   (i,j,iblk) * hm(i,j,iblk)
            Tair (i,j,iblk) = Tair (i,j,iblk) * hm(i,j,iblk)
            wind (i,j,iblk) = wind (i,j,iblk) * hm(i,j,iblk)
            strax(i,j,iblk) = strax(i,j,iblk) * hm(i,j,iblk)
            stray(i,j,iblk) = stray(i,j,iblk) * hm(i,j,iblk)
          enddo
        enddo

      ! AOMIP
      this_block = get_block(blocks_ice(iblk),iblk)         
      ilo = this_block%ilo
      ihi = this_block%ihi
      jlo = this_block%jlo
      jhi = this_block%jhi

      call compute_shortwave(nx_block, ny_block, &
                             ilo, ihi, jlo, jhi, &
                             TLON (:,:,iblk), &
                             TLAT (:,:,iblk), &
                             hm   (:,:,iblk), &
                             Qa   (:,:,iblk), &
                             cldf (:,:,iblk), &
                             fsw  (:,:,iblk))

      enddo  ! iblk
      !$OMP END PARALLEL DO

         if (dbug) then
           if (my_task == master_task) write (nu_diag,*) 'LY_bulk_data'
           vmin = global_minval(fsw,distrb_info,tmask)
           vmax = global_maxval(fsw,distrb_info,tmask)
           if (my_task.eq.master_task)  &
               write (nu_diag,*) 'fsw',vmin,vmax 
           vmin = global_minval(cldf,distrb_info,tmask)
           vmax = global_maxval(cldf,distrb_info,tmask)
           if (my_task.eq.master_task) & 
               write (nu_diag,*) 'cldf',vmin,vmax
           vmin =global_minval(fsnow,distrb_info,tmask)
           vmax =global_maxval(fsnow,distrb_info,tmask)
           if (my_task.eq.master_task) & 
               write (nu_diag,*) 'fsnow',vmin,vmax
           vmin = global_minval(Tair,distrb_info,tmask)
           vmax = global_maxval(Tair,distrb_info,tmask)
           if (my_task.eq.master_task) & 
               write (nu_diag,*) 'Tair',vmin,vmax
           vmin = global_minval(wind,distrb_info,umask)
           vmax = global_maxval(wind,distrb_info,umask)
           if (my_task.eq.master_task) & 
               write (nu_diag,*) 'wind',vmin,vmax
           vmin = global_minval(strax,distrb_info,umask)
           vmax = global_maxval(strax,distrb_info,umask)
           if (my_task.eq.master_task) & 
               write (nu_diag,*) 'strax',vmin,vmax
           vmin = global_minval(stray,distrb_info,umask)
           vmax = global_maxval(stray,distrb_info,umask)
           if (my_task.eq.master_task) & 
               write (nu_diag,*) 'stray',vmin,vmax
           vmin = global_minval(Qa,distrb_info,tmask)
           vmax = global_maxval(Qa,distrb_info,tmask)
           if (my_task.eq.master_task)  &
               write (nu_diag,*) 'Qa',vmin,vmax

        endif                   ! dbug

      end subroutine monthly_data

!=======================================================================
! Oned atmospheric data
!=======================================================================

      subroutine oned_data

      use ice_blocks, only: block, get_block
      use ice_constants, only: p001, p01, p25, c0, c1
      use ice_domain, only: nblocks, blocks_ice
      use ice_flux, only: uatm, vatm, Tair, fsw, fsnow, Qa, rhoa, frain

#ifdef ncdf 
      use netcdf

      ! local parameters

      character (char_len_long) :: & 
         met_file,   &    ! netcdf filename
         fieldname        ! field name in netcdf file

      integer (kind=int_kind) :: &
         fid              ! file id for netCDF file 

      real (kind=dbl_kind):: &
         work             ! temporary variable

      logical (kind=log_kind) :: diag

      integer (kind=int_kind) :: &
         status           ! status flag

      integer (kind=int_kind) :: &
         iblk, &          ! block index
         ilo,jlo          ! beginning of physical domain

      type (block) :: &
         this_block       ! block information for current block
      
      real (kind=dbl_kind) :: & ! used to determine specific humidity
         Temp               , & ! air temperature (K)
         rh                 , & ! relative humidity (%)
         Psat               , & ! saturation vapour pressure (hPa)
         ws                     ! saturation mixing ratio

      real (kind=dbl_kind), parameter :: & ! coefficients for Hyland-Wexler Qa 
         ps1 = 0.58002206e4_dbl_kind,    & ! (K) 
         ps2 = 1.3914993_dbl_kind,       & !
         ps3 = 0.48640239e-1_dbl_kind,   & ! (K^-1) 
         ps4 = 0.41764768e-4_dbl_kind,   & ! (K^-2)
         ps5 = 0.14452093e-7_dbl_kind,   & ! (K^-3)
         ps6 = 6.5459673_dbl_kind,       & !
         ws1 = 621.97_dbl_kind,          & ! for saturation mixing ratio 
         Pair = 1020._dbl_kind             ! Sea level pressure (hPa) 
       
      diag = .false.   ! write diagnostic information 
   
      do iblk = 1, nblocks
         this_block = get_block(blocks_ice(iblk),iblk)         
         ilo = this_block%ilo
         jlo = this_block%jlo

      if (trim(atm_data_format) == 'nc') then     ! read nc file

        ! hourly data beginning Jan 1, 1989, 01:00   
        ! HARDWIRED for dt = 1 hour!
        met_file = uwind_file
        call ice_open_nc(met_file,fid)

        fieldname='Uatm' 
        call ice_read_nc(fid,istep1,fieldname,work,diag)   
        uatm(:,:,:) = work

        fieldname='Vatm' 
        call ice_read_nc(fid,istep1,fieldname,work,diag)   
        vatm(:,:,:) = work

        fieldname='Tair' 
        call ice_read_nc(fid,istep1,fieldname,work,diag)   
        Temp = work
        Tair(:,:,:) = Temp 

        if (my_task == master_task) status = nf90_close(fid)

        ! hourly solar data beginning Jan 1, 1989, 01:00          
        met_file = fsw_file
        call ice_open_nc(met_file,fid)

        fieldname='fsw' 
        call ice_read_nc(fid,istep1,fieldname,work,diag)   
        fsw(:,:,:) = work

        if (my_task == master_task) status = nf90_close(fid)

        ! hourly interpolated monthly  data beginning Jan 1, 1989, 01:00  
        met_file = humid_file
        call ice_open_nc(met_file,fid)

        fieldname='rh' 
        call ice_read_nc(fid,istep1,fieldname,work,diag)   
        rh = work
     
        fieldname='fsnow' 
        call ice_read_nc(fid,istep1,fieldname,work,diag)   
        fsnow(:,:,:) = work

        if (my_task == master_task) status = nf90_close(fid)

      !-------------------------------------------------------------------
      ! Find specific humidity using Hyland-Wexler formulation
      ! Hyland, R.W. and A. Wexler, Formulations for the Thermodynamic 
      ! Properties of the saturated phases of H20 from 173.15K to 473.15K, 
      ! ASHRAE Trans, 89(2A), 500-519, 1983
      !-------------------------------------------------------------------
      
        Psat = exp(-ps1/Temp + ps2 - ps3*Temp + ps4*Temp**2 - ps5 * Temp**3  & 
              + ps6 * log(Temp))*p01          ! saturation vapour pressure
        ws = ws1 * Psat/(Pair - Psat)         ! saturation mixing ratio
        Qa(:,:,:) = rh * ws * p01/(c1 + rh * ws * p01) * p001  
                                              ! specific humidity (kg/kg)
      endif ! atm_data_format

      ! flw calculated in prepare_forcing
        rhoa (:,:,:) = 1.3_dbl_kind ! air density (kg/m^3)
        cldf (:,:,:) = p25          ! cloud fraction
        frain(:,:,:) = c0           ! this is available in hourlymet_rh file
  
      enddo ! nblocks

#endif

      end subroutine oned_data

!=======================================================================

      subroutine oned_files(yr)

      integer (kind=int_kind), intent(in) :: &
           yr                   ! current forcing year

      fsw_file = &
           trim(atm_data_dir)//'hourlysolar_brw1989_5yr.nc'

      rain_file = &
           trim(atm_data_dir)//'hourlymet_rh_5yr.nc'

      uwind_file = &
           trim(atm_data_dir)//'hourlymet_brw1989_5yr.nc'

      vwind_file = &
           trim(atm_data_dir)//'hourlymet_brw1989_5yr.nc'

      tair_file = &
           trim(atm_data_dir)//'hourlymet_brw1989_5yr.nc'

      humid_file = &
           trim(atm_data_dir)//'hourlymet_rh_5yr.nc'

      if (my_task == master_task) then
         write (nu_diag,*) ' '
         write (nu_diag,*) 'Atmospheric data files:'
         write (nu_diag,*) trim(fsw_file)
         write (nu_diag,*) trim(rain_file)
         write (nu_diag,*) trim(uwind_file)
         write (nu_diag,*) trim(vwind_file)
         write (nu_diag,*) trim(tair_file)
         write (nu_diag,*) trim(humid_file)
      endif                     ! master_task

      end subroutine oned_files

!=======================================================================
! Climatological ocean forcing
!=======================================================================

      subroutine ocn_data_clim (dt)

! Interpolate monthly sss, sst data to timestep.
! Restore prognostic sst to data.
! Interpolate fields from U grid to T grid if necessary.

! author: Elizabeth C. Hunke and William H. Lipscomb, LANL

      use ice_constants, only: c0, p5, c1000, depressT, &
          field_loc_center, field_type_scalar
      use ice_domain, only: nblocks
      use ice_flux, only: Tf, sss, sst, uocn, vocn, ss_tltx, ss_tlty

      real (kind=dbl_kind), intent(in) :: &
         dt      ! time step

      ! local variables

      integer (kind=int_kind) :: &
          i, j, iblk  , & ! horizontal indices
          ixm,ixp     , & ! record numbers for neighboring months
          maxrec      , & ! maximum record number
          recslot     , & ! spline slot for current record
          midmonth        ! middle day of month

      real (kind=dbl_kind), dimension(nx_block,ny_block,max_blocks) :: &
          sstdat              ! data value toward which SST is restored

      logical (kind=log_kind) :: readm

      if (my_task == master_task .and. istep == 1) then
         if (trim(sss_data_type)=='clim') then
            write (nu_diag,*) ' '
            write (nu_diag,*) 'SSS data interpolated to timestep:'
            write (nu_diag,*) trim(sss_file)
         endif
         if (trim(sst_data_type)=='clim') then
            write (nu_diag,*) ' '
            write (nu_diag,*) 'SST data interpolated to timestep:'
            write (nu_diag,*) trim(sst_file)
            if (restore_sst) write (nu_diag,*) &
              'SST restoring timescale (days) =', trestore
         endif
      endif                     ! my_task, istep

    !-------------------------------------------------------------------
    ! monthly data
    !
    ! Assume that monthly data values are located in the middle of the
    ! month.
    !-------------------------------------------------------------------

      if (trim(sss_data_type)=='clim' .or.  &
          trim(sst_data_type)=='clim') then

         midmonth = 15          ! data is given on 15th of every month
!!!      midmonth = fix(p5 * real(daymo(month)))  ! exact middle

         ! Compute record numbers for surrounding months
         maxrec = 12
         ixm  = mod(month+maxrec-2,maxrec) + 1
         ixp  = mod(month,         maxrec) + 1
         if (mday >= midmonth) ixm = -99 ! other two points will be used
         if (mday <  midmonth) ixp = -99

         ! Determine whether interpolation will use values 1:2 or 2:3
         ! recslot = 2 means we use values 1:2, with the current value (2)
         !  in the second slot
         ! recslot = 1 means we use values 2:3, with the current value (2)
         !  in the first slot
         recslot = 1            ! latter half of month
         if (mday < midmonth) recslot = 2 ! first half of month

         ! Find interpolation coefficients
         call interp_coeff_monthly (recslot)

         readm = .false.
         if (istep==1 .or. (mday==midmonth .and. sec==0)) readm = .true.

      endif   ! sss/sst_data_type

    !-------------------------------------------------------------------
    ! Read two monthly SSS values and interpolate.
    ! Note: SSS is restored instantaneously to data.
    !-------------------------------------------------------------------

      if (trim(sss_data_type)=='clim') then
         call read_clim_data (readm, 0, ixm, month, ixp, &
                              sss_file, sss_data, &
                              field_loc_center, field_type_scalar)
         call interpolate_data (sss_data, sss)

         !$OMP PARALLEL DO PRIVATE(iblk,i,j)
         do iblk = 1, nblocks
            do j = 1, ny_block
            do i = 1, nx_block
               sss(i,j,iblk) = max(sss(i,j,iblk), c0)
            enddo
            enddo
         enddo
         !$OMP END PARALLEL DO

         call ocn_freezing_temperature
      endif

    !-------------------------------------------------------------------
    ! Read two monthly SST values and interpolate.
    ! Restore toward interpolated value.
    !-------------------------------------------------------------------

      if (trim(sst_data_type)=='clim') then
         call read_clim_data (readm, 0, ixm, month, ixp, &
                              sst_file, sst_data, &
                              field_loc_center, field_type_scalar)
         call interpolate_data (sst_data, sstdat)

         if (restore_sst) then
         !$OMP PARALLEL DO PRIVATE(iblk,i,j)
         do iblk = 1, nblocks
            do j = 1, ny_block
            do i = 1, nx_block
               sst(i,j,iblk) = sst(i,j,iblk)  &
                         + (sstdat(i,j,iblk)-sst(i,j,iblk))*dt/trest
            enddo
            enddo
         enddo
         !$OMP END PARALLEL DO
         endif
      endif

      end subroutine ocn_data_clim

!=======================================================================
! NCAR CCSM M-configuration (AIO) ocean forcing
!=======================================================================

      subroutine ocn_data_ncar_init

! Reads NCAR pop ocean forcing data set 'pop_frc_gx1v3_010815.nc'
! 
! List of ocean forcing fields: Note that order is important!
! (order is determined by field list in vname).
! 
! For ocean mixed layer-----------------------------units 
! 
! 1  sst------temperature---------------------------(C)   
! 2  sss------salinity------------------------------(ppt) 
! 3  hbl------depth---------------------------------(m)   
! 4  u--------surface u current---------------------(m/s) 
! 5  v--------surface v current---------------------(m/s) 
! 6  dhdx-----surface tilt x direction--------------(m/m) 
! 7  dhdy-----surface tilt y direction--------------(m/m) 
! 8  qdp------ocean sub-mixed layer heat flux-------(W/m2)
!
! Fields 4, 5, 6, 7 are on the U-grid; 1, 2, 3, and 8 are
! on the T-grid.

! authors: Bruce Briegleb, NCAR
!          Elizabeth Hunke, LANL

      use ice_blocks, only: nx_block, ny_block
      use ice_constants, only: c0, &
          field_loc_center, field_loc_NEcorner, &
          field_type_scalar, field_type_vector
      use ice_domain_size, only: max_blocks
#ifdef ncdf
      use netcdf
#endif

      integer (kind=int_kind) :: & 
        n   , & ! field index
        m   , & ! month index
        nrec, & ! record number for direct access
        nbits

      character(char_len) :: &
        vname(nfld) ! variable names to search for in file
      data vname /  &
           'T',      'S',      'hblt',  'U',     'V', &
           'dhdx',   'dhdy',   'qdp' /

      integer (kind=int_kind) :: &
        fid        , & ! file id 
        dimid          ! dimension id 

      integer (kind=int_kind) :: &
        status  , & ! status flag
        nlat    , & ! number of longitudes of data
        nlon        ! number of latitudes  of data

      real (kind=dbl_kind), dimension (nx_block,ny_block,max_blocks) :: &
         work1

      if (my_task == master_task) then

         write (nu_diag,*) 'WARNING: evp_prep calculates surface tilt'
         write (nu_diag,*) 'WARNING: stress from geostrophic currents,'
         write (nu_diag,*) 'WARNING: not data from ocean forcing file.'
         write (nu_diag,*) 'WARNING: Alter ice_dyn_evp.F if desired.'

         if (restore_sst) write (nu_diag,*)  &
             'SST restoring timescale = ',trestore,' days' 

         sst_file = trim(ocn_data_dir)//oceanmixed_file ! not just sst

        !---------------------------------------------------------------
        ! Read in ocean forcing data from an existing file
        !---------------------------------------------------------------
        write (nu_diag,*) 'ocean mixed layer forcing data file = ', &
                           trim(sst_file)

      endif ! master_task

      if (trim(ocn_data_format) == 'nc') then
#ifdef ncdf
        if (my_task == master_task) then
          call ice_open_nc(sst_file, fid)

!          status = nf90_inq_dimid(fid,'nlon',dimid)
          status = nf90_inq_dimid(fid,'ni',dimid)
          status = nf90_inquire_dimension(fid,dimid,len=nlon)
  
!          status = nf90_inq_dimid(fid,'nlat',dimid)
          status = nf90_inq_dimid(fid,'nj',dimid)
          status = nf90_inquire_dimension(fid,dimid,len=nlat)

          if( nlon .ne. nx_global ) then
            call abort_ice ('ice: ocn frc file nlon ne nx_global')
          endif
          if( nlat .ne. ny_global ) then
            call abort_ice ('ice: ocn frc file nlat ne ny_global')
          endif

        endif ! master_task

        ! Read in ocean forcing data for all 12 months
        do n=1,nfld
          do m=1,12
                
            ! Note: netCDF does single to double conversion if necessary
            if (n >= 4 .and. n <= 7) then
               call ice_read_nc(fid, m, vname(n), work1, dbug, &
                                field_loc_NEcorner, field_type_vector)
            else
               call ice_read_nc(fid, m, vname(n), work1, dbug, &
                                field_loc_center, field_type_scalar)
            endif
            ocn_frc_m(:,:,:,n,m) = work1(:,:,:)

          enddo               ! month loop
        enddo               ! field loop

        if (my_task == master_task) status = nf90_close(fid)
#endif

      else  ! binary format

        nbits = 64
        call ice_open (nu_forcing, sst_file, nbits)

        nrec = 0
        do n=1,nfld
           do m=1,12
              nrec = nrec + 1
              if (n >= 4 .and. n <= 7) then
                call ice_read (nu_forcing, nrec, work1, 'rda8', dbug, &
                               field_loc_NEcorner, field_type_vector)
              else
                call ice_read (nu_forcing, nrec, work1, 'rda8', dbug, &
                               field_loc_center, field_type_scalar)
              endif
              ocn_frc_m(:,:,:,n,m) = work1(:,:,:)
           enddo               ! month loop
        enddo               ! field loop
        close (nu_forcing)

      endif

!echmod - currents cause Fram outflow to be too large
              ocn_frc_m(:,:,:,4,:) = c0
              ocn_frc_m(:,:,:,5,:) = c0
!echmod

      end subroutine ocn_data_ncar_init

!=======================================================================

      subroutine ocn_data_ncar_init_3D

! Reads NCAR pop ocean forcing data set 'oceanmixed_ice_depth.nc'
! 
! List of ocean forcing fields: Note that order is important!
! (order is determined by field list in vname).
! 
! For ocean mixed layer-----------------------------units 
! 
! 1  sst------temperature---------------------------(C)   
! 2  sss------salinity------------------------------(ppt) 
! 3  hbl------depth---------------------------------(m)   
! 4  u--------surface u current---------------------(m/s) 
! 5  v--------surface v current---------------------(m/s) 
! 6  dhdx-----surface tilt x direction--------------(m/m) 
! 7  dhdy-----surface tilt y direction--------------(m/m) 
! 8  qdp------ocean sub-mixed layer heat flux-------(W/m2)
!
! All fields are on the T-grid.
!
! authors: Bruce Briegleb, NCAR
!          Elizabeth Hunke, LANL

      use ice_blocks, only: nx_block, ny_block
      use ice_constants, only: c0, &
          field_loc_center, field_type_scalar 
      use ice_domain_size, only: max_blocks
      use ice_grid, only: to_ugrid, ANGLET
#ifdef ncdf
      use netcdf
#endif

      integer (kind=int_kind) :: & 
        n   , & ! field index
        m       ! month index

      character(char_len) :: &
        vname(nfld) ! variable names to search for in file
      data vname /  &
           'T',      'S',      'hblt',  'U',     'V', &
           'dhdx',   'dhdy',   'qdp' /

      integer (kind=int_kind) :: &
        fid        , & ! file id 
        dimid          ! dimension id 

      integer (kind=int_kind) :: &
        status  , & ! status flag
        nlat    , & ! number of longitudes of data
        nlon        ! number of latitudes  of data

      real (kind=dbl_kind), dimension (nx_block,ny_block,max_blocks) :: &
         work1, work2

      if (my_task == master_task) then

         write (nu_diag,*) 'WARNING: evp_prep calculates surface tilt'
         write (nu_diag,*) 'WARNING: stress from geostrophic currents,'
         write (nu_diag,*) 'WARNING: not data from ocean forcing file.'
         write (nu_diag,*) 'WARNING: Alter ice_dyn_evp.F if desired.'

         if (restore_sst) write (nu_diag,*)  &
             'SST restoring timescale = ',trestore,' days' 

         sst_file = trim(ocn_data_dir)//oceanmixed_file ! not just sst

        !---------------------------------------------------------------
        ! Read in ocean forcing data from an existing file
        !---------------------------------------------------------------
        write (nu_diag,*) 'ocean mixed layer forcing data file = ', &
                           trim(sst_file)
        write (nu_diag,*)

      endif ! master_task

      if (trim(ocn_data_format) == 'nc') then
#ifdef ncdf
        if (my_task == master_task) then
          call ice_open_nc(sst_file, fid)

!          status = nf90_inq_dimid(fid,'nlon',dimid)
          status = nf90_inq_dimid(fid,'ni',dimid)
          status = nf90_inquire_dimension(fid,dimid,len=nlon)
  
!          status = nf90_inq_dimid(fid,'nlat',dimid)
          status = nf90_inq_dimid(fid,'nj',dimid)
          status = nf90_inquire_dimension(fid,dimid,len=nlat)

          if( nlon .ne. nx_global ) then
            call abort_ice ('ice: ocn frc file nlon ne nx_global')
          endif
          if( nlat .ne. ny_global ) then
            call abort_ice ('ice: ocn frc file nlat ne ny_global')
          endif

        endif ! master_task

        ! Read in ocean forcing data for all 12 months
        do n=1,nfld
          do m=1,12
                
            ! Note: netCDF does single to double conversion if necessary
            call ice_read_nc(fid, m, vname(n), work1, dbug, &
                             field_loc_center, field_type_scalar)

            ! the land mask used in ocean_mixed_depth.nc does not 
            ! match our gx1v3 mask (hm)
            where (work1(:,:,:) < -900.) work1(:,:,:) = c0

            ocn_frc_m(:,:,:,n,m) = work1(:,:,:)

          enddo               ! month loop
        enddo               ! field loop

        if (my_task == master_task) status = nf90_close(fid)

        ! Rotate vector quantities and shift to U-grid
        do n=4,6,2
          do m=1,12

             work1(:,:,:) = ocn_frc_m(:,:,:,n  ,m)
             work2(:,:,:) = ocn_frc_m(:,:,:,n+1,m)
             ocn_frc_m(:,:,:,n  ,m) = work1(:,:,:)*cos(ANGLET(:,:,:)) &
                                    + work2(:,:,:)*sin(ANGLET(:,:,:))
             ocn_frc_m(:,:,:,n+1,m) = work2(:,:,:)*cos(ANGLET(:,:,:)) &
                                    - work1(:,:,:)*sin(ANGLET(:,:,:))

             work1(:,:,:) = ocn_frc_m(:,:,:,n  ,m)*cos(ANGLET(:,:,:)) &
                          + ocn_frc_m(:,:,:,n+1,m)*sin(ANGLET(:,:,:))
             work2(:,:,:) = ocn_frc_m(:,:,:,n+1,m)*cos(ANGLET(:,:,:)) &
                          - ocn_frc_m(:,:,:,n  ,m)*sin(ANGLET(:,:,:))
             call to_ugrid(work1,ocn_frc_m(:,:,:,n  ,m))
             call to_ugrid(work2,ocn_frc_m(:,:,:,n+1,m))

          enddo               ! month loop
        enddo               ! field loop

#endif

      else  ! binary format

        call abort_ice ('new ocean forcing is netcdf only')

      endif

      end subroutine ocn_data_ncar_init_3D

!=======================================================================

      subroutine ocn_data_ncar(dt)

! Interpolate monthly ocean data to timestep.
! Restore sst if desired. sst is updated with surface fluxes in ice_ocean.F.

      use ice_blocks, only: nx_block, ny_block
      use ice_constants, only: c0, c1, p5, depressT
      use ice_global_reductions, only: global_minval, global_maxval
      use ice_domain, only: nblocks, distrb_info
      use ice_domain_size, only: max_blocks
      use ice_flux, only: sss, sst, Tf, uocn, vocn, ss_tltx, ss_tlty, &
            qdp, hmix
      use ice_restart_shared, only: restart
      use ice_grid, only: hm, tmask, umask

      real (kind=dbl_kind), intent(in) :: &
         dt      ! time step

      integer (kind=int_kind) :: & 
          i, j, n, iblk   , &
          ixm,ixp         , & ! record numbers for neighboring months
          maxrec          , & ! maximum record number
          recslot         , & ! spline slot for current record
          midmonth            ! middle day of month

      real (kind=dbl_kind) :: &
          vmin, vmax

      real (kind=dbl_kind), dimension (nx_block,ny_block,max_blocks) :: &
         work1

    !-------------------------------------------------------------------
    ! monthly data 
    !
    ! Assume that monthly data values are located in the middle of the 
    ! month.
    !-------------------------------------------------------------------
      
      midmonth = 15  ! data is given on 15th of every month
!      midmonth = fix(p5 * real(daymo(month),kind=dbl_kind))  ! exact middle

      ! Compute record numbers for surrounding months
      maxrec = 12
      ixm  = mod(month+maxrec-2,maxrec) + 1
      ixp  = mod(month,         maxrec) + 1
      if (mday >= midmonth) ixm = -99  ! other two points will be used
      if (mday <  midmonth) ixp = -99

      ! Determine whether interpolation will use values 1:2 or 2:3
      ! recslot = 2 means we use values 1:2, with the current value (2)
      !  in the second slot
      ! recslot = 1 means we use values 2:3, with the current value (2)
      !  in the first slot
      recslot = 1                             ! latter half of month
      if (mday < midmonth) recslot = 2        ! first half of month

      ! Find interpolation coefficients
      call interp_coeff_monthly (recslot)

      do n = nfld, 1, -1
        !$OMP PARALLEL DO PRIVATE(iblk,i,j)
        do iblk = 1, nblocks
        ! use sst_data arrays as temporary work space until n=1
        if (ixm /= -99) then  ! first half of month
          sst_data(:,:,1,iblk) = ocn_frc_m(:,:,iblk,n,ixm)
          sst_data(:,:,2,iblk) = ocn_frc_m(:,:,iblk,n,month)
        else                 ! second half of month
          sst_data(:,:,1,iblk) = ocn_frc_m(:,:,iblk,n,month)
          sst_data(:,:,2,iblk) = ocn_frc_m(:,:,iblk,n,ixp)
        endif
        enddo
        !$OMP END PARALLEL DO
        call interpolate_data (sst_data,work1)
        ! masking by hm is necessary due to NaNs in the data file
        do j = 1, ny_block 
          do i = 1, nx_block 
            if (n == 2) sss    (i,j,:) = c0
            if (n == 3) hmix   (i,j,:) = c0
            if (n == 4) uocn   (i,j,:) = c0
            if (n == 5) vocn   (i,j,:) = c0
            if (n == 6) ss_tltx(i,j,:) = c0
            if (n == 7) ss_tlty(i,j,:) = c0
            if (n == 8) qdp    (i,j,:) = c0
            do iblk = 1, nblocks
              if (hm(i,j,iblk) == c1) then
                if (n == 2) sss    (i,j,iblk) = work1(i,j,iblk)
                if (n == 3) hmix   (i,j,iblk) = work1(i,j,iblk)
                if (n == 4) uocn   (i,j,iblk) = work1(i,j,iblk)
                if (n == 5) vocn   (i,j,iblk) = work1(i,j,iblk)
                if (n == 6) ss_tltx(i,j,iblk) = work1(i,j,iblk)
                if (n == 7) ss_tlty(i,j,iblk) = work1(i,j,iblk)
                if (n == 8) qdp    (i,j,iblk) = work1(i,j,iblk)
              endif
            enddo
          enddo
        enddo
      enddo

      do j = 1, ny_block 
         do i = 1, nx_block 
            sss (i,j,:) = max (sss(i,j,:), c0) 
            hmix(i,j,:) = max(hmix(i,j,:), c0) 
         enddo 
      enddo 

      call ocn_freezing_temperature

      if (restore_sst) then
        do j = 1, ny_block 
         do i = 1, nx_block 
           sst(i,j,:) = sst(i,j,:) + (work1(i,j,:)-sst(i,j,:))*dt/trest 
         enddo 
        enddo 
!     else sst is only updated in ice_ocean.F
      endif

      ! initialize sst properly on first step
      if (istep1 <= 1 .and. .not. (restart)) then
        call interpolate_data (sst_data,sst)
        !$OMP PARALLEL DO PRIVATE(iblk,i,j)
        do iblk = 1, nblocks
         do j = 1, ny_block 
          do i = 1, nx_block 
            if (hm(i,j,iblk) == c1) then
              sst(i,j,iblk) =  max (sst(i,j,iblk), Tf(i,j,iblk)) 
            else
              sst(i,j,iblk) = c0
            endif
          enddo 
         enddo 
        enddo 
        !$OMP END PARALLEL DO
      endif

      if (dbug) then
         if (my_task == master_task)  &
               write (nu_diag,*) 'ocn_data_ncar'
           vmin = global_minval(Tf,distrb_info,tmask)
           vmax = global_maxval(Tf,distrb_info,tmask)
           if (my_task.eq.master_task)  &
               write (nu_diag,*) 'Tf',vmin,vmax
           vmin = global_minval(sst,distrb_info,tmask)
           vmax = global_maxval(sst,distrb_info,tmask)
           if (my_task.eq.master_task)  &
               write (nu_diag,*) 'sst',vmin,vmax
           vmin = global_minval(sss,distrb_info,tmask)
           vmax = global_maxval(sss,distrb_info,tmask)
           if (my_task.eq.master_task)  &
               write (nu_diag,*) 'sss',vmin,vmax
           vmin = global_minval(hmix,distrb_info,tmask)
           vmax = global_maxval(hmix,distrb_info,tmask)
           if (my_task.eq.master_task)  &
               write (nu_diag,*) 'hmix',vmin,vmax
           vmin = global_minval(uocn,distrb_info,umask)
           vmax = global_maxval(uocn,distrb_info,umask)
           if (my_task.eq.master_task)  &
               write (nu_diag,*) 'uocn',vmin,vmax
           vmin = global_minval(vocn,distrb_info,umask)
           vmax = global_maxval(vocn,distrb_info,umask)
           if (my_task.eq.master_task)  &
               write (nu_diag,*) 'vocn',vmin,vmax
           vmin = global_minval(ss_tltx,distrb_info,umask)
           vmax = global_maxval(ss_tltx,distrb_info,umask)
           if (my_task.eq.master_task)  &
               write (nu_diag,*) 'ss_tltx',vmin,vmax
           vmin = global_minval(ss_tlty,distrb_info,umask)
           vmax = global_maxval(ss_tlty,distrb_info,umask)
           if (my_task.eq.master_task)  &
               write (nu_diag,*) 'ss_tlty',vmin,vmax
           vmin = global_minval(qdp,distrb_info,tmask)
           vmax = global_maxval(qdp,distrb_info,tmask)
           if (my_task.eq.master_task)  &
               write (nu_diag,*) 'qdp',vmin,vmax
      endif

      end subroutine ocn_data_ncar

!=======================================================================
! ocean data for oned configuration
! Current (released) values are the same as the defaults (ice_flux.F90)

      subroutine ocn_data_oned(dt)

      use ice_constants, only: c0, c20, p001, depressT
      use ice_flux, only: sss, sst, Tf, uocn, vocn, ss_tltx, ss_tlty, &
            qdp, hmix, frzmlt
      !use ice_therm_mushy, only: liquidus_temperature_mush

      real (kind=dbl_kind), intent(in) :: &
         dt      ! time step
 
      sss    (:,:,:) = 34.0_dbl_kind   ! sea surface salinity (ppt)

      call ocn_freezing_temperature

      sst    (:,:,:) = Tf(:,:,:)       ! sea surface temp (C)
      uocn   (:,:,:) = c0              ! surface ocean currents (m/s)
      vocn   (:,:,:) = c0
      ss_tltx(:,:,:) = c0              ! sea surface tilt (m/m)
      ss_tlty(:,:,:) = c0
      frzmlt (:,:,:) = c0              ! freezing/melting potential (W/m^2)
      qdp    (:,:,:) = c0              ! deep ocean heat flux (W/m^2)
      hmix   (:,:,:) = c20             ! ocean mixed layer depth

      end subroutine ocn_data_oned

!=======================================================================

      subroutine ocn_data_hadgem(dt)

!  Reads in HadGEM ocean forcing data as required from netCDF files
!  Current options (selected by sst_data_type)
!  hadgem_sst: 		read in sst only 
!  hadgem_sst_uvocn:	read in sst plus uocn and vocn	

! authors: Ann Keen, Met Office

      use ice_constants, only: p5, cm_to_m, &
          field_loc_center, field_type_scalar, field_type_vector
      use ice_domain, only: nblocks
      use ice_flux, only: sst, uocn, vocn
      use ice_grid, only: t2ugrid_vector, ANGLET

      real (kind=dbl_kind), intent(in) :: &
         dt      ! time step
 
     integer (kind=int_kind) :: &
          i, j        , & ! horizontal indices
          iblk        , & ! block index
          ixm,ixp     , & ! record numbers for neighboring months
          maxrec      , & ! maximum record number
          recslot     , & ! spline slot for current record
          midmonth        ! middle day of month

      real (kind=dbl_kind), dimension(nx_block,ny_block,max_blocks) :: &
          sstdat              ! data value toward which SST is restored

      real (kind=dbl_kind) :: workx, worky

      logical (kind=log_kind) :: readm

      character (char_len) :: & 
            fieldname    	! field name in netcdf file

      character (char_len_long) :: & 
            filename    	! name of netCDF file

    !-------------------------------------------------------------------
    ! monthly data
    !
    ! Assume that monthly data values are located in the middle of the
    ! month.
    !-------------------------------------------------------------------

      midmonth = 15  ! data is given on 15th of every month
!      midmonth = fix(p5 * real(daymo(month)))  ! exact middle

      ! Compute record numbers for surrounding months
      maxrec = 12
      ixm  = mod(month+maxrec-2,maxrec) + 1
      ixp  = mod(month,         maxrec) + 1
      if (mday >= midmonth) ixm = -99  ! other two points will be used
      if (mday <  midmonth) ixp = -99

      ! Determine whether interpolation will use values 1:2 or 2:3
      ! recslot = 2 means we use values 1:2, with the current value (2)
      !  in the second slot
      ! recslot = 1 means we use values 2:3, with the current value (2)
      !  in the first slot
      recslot = 1                             ! latter half of month
      if (mday < midmonth) recslot = 2        ! first half of month

      ! Find interpolation coefficients
      call interp_coeff_monthly (recslot)

      ! Read 2 monthly values
      readm = .false.
      if (istep==1 .or. (mday==midmonth .and. sec==0)) readm = .true.

      if (my_task == master_task .and. istep == 1) then
         write (nu_diag,*) ' '
         write (nu_diag,*) 'SST data interpolated to timestep:'
         write (nu_diag,*) trim(ocn_data_dir)//'MONTHLY/sst.1997.nc'
         if (restore_sst) write (nu_diag,*) &
              'SST restoring timescale (days) =', trestore
         if (trim(sst_data_type)=='hadgem_sst_uvocn') then
            write (nu_diag,*) ' '
            write (nu_diag,*) 'uocn and vocn interpolated to timestep:'
            write (nu_diag,*) trim(ocn_data_dir)//'MONTHLY/uocn.1997.nc'
            write (nu_diag,*) trim(ocn_data_dir)//'MONTHLY/vocn.1997.nc'
         endif
      endif                     ! my_task, istep

      ! -----------------------------------------------------------
      ! SST
      ! -----------------------------------------------------------
      sst_file = trim(ocn_data_dir)//'MONTHLY/sst.1997.nc'	
      fieldname='sst'
      call read_data_nc (readm, 0, fyear, ixm, month, ixp, &
                      maxrec, sst_file, fieldname, sst_data, &
                      field_loc_center, field_type_scalar)
      
      ! Interpolate to current time step
      call interpolate_data (sst_data, sstdat)

      ! Restore SSTs if required
        if (restore_sst) then
         !$OMP PARALLEL DO PRIVATE(iblk,i,j)
         do iblk = 1, nblocks
            do j = 1, ny_block
            do i = 1, nx_block
               sst(i,j,iblk) = sst(i,j,iblk)  &
                         + (sstdat(i,j,iblk)-sst(i,j,iblk))*dt/trest
            enddo
            enddo
         enddo
         !$OMP END PARALLEL DO
         endif      

      ! -----------------------------------------------------------
      ! Ocean currents
      ! --------------
      ! Values read in are on T grid and oriented geographically, hence 
      ! vectors need to be rotated to model grid and then interpolated
      ! to U grid.   
      ! Also need to be converted from cm s-1 (UM) to m s-1 (CICE)
      ! -----------------------------------------------------------

      if (trim(sst_data_type)=='hadgem_sst_uvocn') then

      	filename = trim(ocn_data_dir)//'MONTHLY/uocn.1997.nc'	
      	fieldname='uocn'
      	call read_data_nc (readm, 0, fyear, ixm, month, ixp, &
                      maxrec, filename, fieldname, uocn_data, &
                      field_loc_center, field_type_vector)
      
      	! Interpolate to current time step
      	call interpolate_data (uocn_data, uocn)

      	filename = trim(ocn_data_dir)//'MONTHLY/vocn.1997.nc'	
      	fieldname='vocn'
      	call read_data_nc (readm, 0, fyear, ixm, month, ixp, &
                      maxrec, filename, fieldname, vocn_data, &
                      field_loc_center, field_type_vector)
      
      	! Interpolate to current time step
      	call interpolate_data (vocn_data, vocn)

     !----------------------------------------------------------------- 
     ! Rotate zonal/meridional vectors to local coordinates, 
     ! and change  units
     !----------------------------------------------------------------- 

         !$OMP PARALLEL DO PRIVATE(iblk,i,j)
         do iblk = 1, nblocks
            do j = 1, ny_block
            do i = 1, nx_block

               workx      = uocn(i,j,iblk) 
               worky      = vocn(i,j,iblk)
               uocn(i,j,iblk) = workx*cos(ANGLET(i,j,iblk)) & 
                                  + worky*sin(ANGLET(i,j,iblk))   
               vocn(i,j,iblk) = worky*cos(ANGLET(i,j,iblk)) & 
                                  - workx*sin(ANGLET(i,j,iblk))

               uocn(i,j,iblk) = uocn(i,j,iblk) * cm_to_m
               vocn(i,j,iblk) = vocn(i,j,iblk) * cm_to_m

            enddo		! i
            enddo		! j
         enddo		! nblocks
         !$OMP END PARALLEL DO

     !----------------------------------------------------------------- 
     ! Interpolate to U grid 
     !----------------------------------------------------------------- 

         call t2ugrid_vector(uocn)
         call t2ugrid_vector(vocn)

     endif    !   sst_data_type = hadgem_sst_uvocn

     end subroutine ocn_data_hadgem

!=======================================================================

!Pedro stuff begins

      subroutine init_forcing_bry
       
      use ice_constants, only: c0
      use ice_domain, only: nblocks
      integer (kind=int_kind) :: &
          i, j        , & ! horizontal indices
          n           , & ! thickness category index
          k           , & ! layer index
          iblk            ! block index
! Determine the current and final year of the forcing cycle based on
! namelist input; initialize the sea-ice boundary forcing data filenames.

      fyear       = fyear_init + mod(nyr-1,ycycle) ! current year
      fyear_final = fyear_init + ycycle - 1 ! last year in forcing cycle

      
      !if (my_task == master_task ) then
      !     write (nu_diag,*) 'init_forcing_bry'
      !     write (nu_diag,*) 'fyear= ',fyear,&
      !                      ' fyear_final= ',fyear_final
      !end if 
    !-------------------------------------------------------------------
    ! Get filenames for input forcing data     
    !-------------------------------------------------------------------

      call boundary_files(fyear)
      
      !Initialize boundary arrays
      do iblk = 1, max_blocks
         do j = 1, ny_block
            do i = 1, nx_block
               !Tsfc_bry(i,j,iblk) = c0;
               do n = 1, ncat 
                  aicen_bry(i,j,n,iblk) = c0;
                  vicen_bry(i,j,n,iblk) = c0;
                  vsnon_bry(i,j,n,iblk) = c0;
                  Tsfc_bry(i,j,n,iblk) = c0;
                  alvln_bry(i,j,n,iblk) = c0;     
                  vlvln_bry(i,j,n,iblk) = c0;     
                  apondn_bry(i,j,n,iblk) = c0;     
                  hpondn_bry(i,j,n,iblk) = c0;     
                  ipondn_bry(i,j,n,iblk) = c0;     
                  !hbrine_bry(i,j,n,iblk) = c0;     
                  !fbrine_bry(i,j,n,iblk) = c0;     
                  iage_bry(i,j,n,iblk) = c0; 
                  do k = 1,nilyr
                     Tinz_bry(i,j,k,n,iblk) = c0;
                     Sinz_bry(i,j,k,n,iblk) = c0;
                  enddo  
               enddo
            enddo
         enddo 
      enddo

      end subroutine init_forcing_bry

!=======================================================================
      subroutine boundary_files(yr)

      
      ! Construct filenames for sea-ice boundary data.
      ! Edit for other directory structures or filenames.
      ! Note: The year number in these filenames does not matter, because
      !       subroutine file_year_bry will insert the correct year.
      ! authors: Pedro Duarte, NPI

      integer (kind=int_kind), intent(in) :: &
           yr                   ! current forcing year

      bry_file = &
           trim(sea_ice_bry_dir)//'BRY_1996.nc'
      call file_year_bry(bry_file,yr)

      !if (my_task == master_task) then
      !   write (nu_diag,*) ' '
      !   write (nu_diag,*) 'Sea-ice boundary data year =', fyear
      !   write (nu_diag,*) 'Sea-ice boundary data file:'
      !   write (nu_diag,*) trim(bry_file)
         
      !endif                     ! master_task

      end subroutine boundary_files 
!=======================================================================
      subroutine file_year_bry (data_file, yr)

! Construct the correct name of the sea-ice boundary file
! to be read, given the year and assuming the naming convention
! that filenames end with 'yyyy.nc'.

      character (char_len_long), intent(inout) ::  data_file

      integer (kind=int_kind), intent(in) :: yr

      character (char_len_long) :: tmpname

      integer (kind=int_kind) :: i

      i = index(data_file,'.nc') - 5
      tmpname = data_file
      write(data_file,'(a,i4.4,a)') tmpname(1:i), yr, '.nc'

      end subroutine file_year_bry

!=======================================================================
      subroutine get_forcing_bry

      ! Get interpolate sea-ice boundary data 
      !write (nu_diag,*) 'get_forcing_bry'

      call boundary_data      
      
      end subroutine get_forcing_bry
!=======================================================================
       subroutine boundary_data

! This sub-routine is used to read daily time-varying sea-ice boundary data
! It is assumed that data is at zero hours of each day
! Therefore, noly on data slot is considered. 
! authors: Pedro Duarte, Norwegian Polar Institute
! Modified:Nov 2017 

      use ice_diagnostics, only: check_step    
      use ice_constants, only: field_loc_center, field_type_scalar,secday
      integer (kind=int_kind) :: &
         ! i,j         , & ! horizontal indices
         ! n           , & ! thickness category index
         ! k           , & ! layer index
          ixm,ixx,ixp , & ! record numbers for neighboring days
          recnum      , & ! record number
          dataloc     , & ! = 1 for data located in middle of time interval
                          ! = 2 for date located at end of time interval
         ! iblk        , & ! block index
          maxrec      , & ! maximum record number
          recslot         ! spline slot for current record
          

      logical (kind=log_kind) :: read1, dbug
  
      character (char_len) :: & 
           fieldname1, &    ! field name in netcdf file
           fieldname2, &
           fieldname3, &
           fieldname4
 
      character (char_len_long) :: &
           data_file               ! data file to be read

      real (kind=dbl_kind), &
      dimension(nx_block,ny_block,ncat,2,max_blocks) :: &
            aicen_work_bry, & ! field values at 2 temporal data points
            vicen_work_bry, &
            vsnon_work_bry, &
            Tsfc_work_bry,  &
            alvln_work_bry, &     
            vlvln_work_bry, &   
            apondn_work_bry,&     
            hpondn_work_bry,&    
            ipondn_work_bry,&     
            !hbrine_work_bry,&     
            !fbrine_work_bry,&    
            iage_work_bry
      !real (kind=dbl_kind), dimension(nx_block,ny_block,2,max_blocks) :: &
      !      Tsfc_work_bry
      
      real (kind=dbl_kind), &
      dimension(nx_block,ny_block,nilyr,ncat,2,max_blocks) :: &
            Tinz_work_bry, & ! field values at 2 temporal data points
            Sinz_work_bry
      !write (nu_diag,*) 'boundary_data'

      dbug=.false.
      if (istep1 > check_step) dbug = .true.  !! debugging

     !-------------------------------------------------------------------
     ! 
     ! daily data located at the end of the 24-hour period. 
     !-------------------------------------------------------------------
      
      
     ! dataloc = 1    ! data located in middle of interval (state variables)
      dataloc = 2    ! data located at end of interval (state variables)
      

      maxrec=nint(dayyr)          !  Takes acount of leap-years

      ! current record number
      recnum = int(yday)   

      ! Compute record numbers for surrounding data (2 on each side)

      ixm = -99
      ixx = mod(recnum-1,       maxrec) + 1
      ixp = mod(recnum,         maxrec) + 1

      ! Compute interpolation coefficients
      ! If data is located at the end of the time interval, then the
      !  data value for the current record goes in slot 2

      recslot = 2

      ! Read
      read1 = .false.
      if (istep==1 .or. oldrecnum .ne. recnum) read1 = .true.
      !if ((oldrecnum.ne.recnum).or.(idate.eq.idate0)) read1 = .true.
      ! Save record number for next time step
      oldrecnum = recnum
      
      !if (my_task == master_task ) then
      !     write (nu_diag,*) 'boundary_data 1'
      !end if 
      ! -----------------------------------------------------------
      ! read sea-ice boundary forcing 
      ! -----------------------------------------------------------
      data_file = bry_file;
      
      call file_year_bry (data_file, fyear) ! Ensure correct year-file
      !if (my_task == master_task ) then
      !     write (nu_diag,*) 'boundary_data 2'
      !     write (nu_diag,*) 'data_file =',data_file
      !     write (nu_diag,*) 'fyear =', fyear 
      !end if   
      ! Ice concentration boundaries
      fieldname1='aicen_N_bry'
      fieldname2='aicen_S_bry'
      fieldname3='aicen_W_bry'
      fieldname4='aicen_E_bry'
      
      call read_bry_ice_data_nc (read1, 0, fyear, ixm, ixx, ixp, &
                maxrec, data_file,fieldname1,fieldname2, &
                fieldname3,fieldname4,aicen_work_bry, &
                field_loc_center, field_type_scalar)
      
      call interp_coeff (recnum, recslot, secday, dataloc)
      call interpolate_data_n (aicen_work_bry, aicen_bry)

      !if (my_task == master_task ) then
      !write (nu_diag,*) 'nx_block= ',nx_block
      !write (nu_diag,*) 'ny_block= ',ny_block
      !write (nu_diag,*) 'aicen_N_bry =',aicen_work_bry(nx_block,ny_block,2,1,1:max_blocks)
      !write (nu_diag,*) 'aicen_E_bry =',aicen_work_bry(200,ny_block,2,1,1:max_blocks)
      !write (nu_diag,*) 'aicen_N =',aicen_bry(nx_block,ny_block,2,12)
      !endif
      
      fieldname1='vicen_N_bry'
      fieldname2='vicen_S_bry'
      fieldname3='vicen_W_bry'
      fieldname4='vicen_E_bry'

      call read_bry_ice_data_nc (read1, 0, fyear, ixm, ixx, ixp, &
                maxrec, data_file,fieldname1,fieldname2, &
                fieldname3,fieldname4,vicen_work_bry, &
                field_loc_center, field_type_scalar)     

      call interp_coeff (recnum, recslot, secday, dataloc)
      call interpolate_data_n (vicen_work_bry, vicen_bry)

      !if (my_task == master_task ) then
      !write (nu_diag,*) 'vicen_N_bry =',vicen_work_bry(nx_block,ny_block,2,1,1:max_blocks)
      !write (nu_diag,*) 'vicen_N =',vicen_bry(nx_block,ny_block,2,12)
      !endif

      fieldname1='vsnon_N_bry'
      fieldname2='vsnon_S_bry'
      fieldname3='vsnon_W_bry'
      fieldname4='vsnon_E_bry'

      call read_bry_ice_data_nc (read1, 0, fyear, ixm, ixx, ixp, &
                maxrec, data_file,fieldname1,fieldname2, &
                fieldname3,fieldname4,vsnon_work_bry, &
                field_loc_center, field_type_scalar)
       
      call interp_coeff (recnum, recslot, secday, dataloc)
      call interpolate_data_n (vsnon_work_bry, vsnon_bry)
  
      fieldname1='Tsfc_N_bry'
      fieldname2='Tsfc_S_bry'
      fieldname3='Tsfc_W_bry'
      fieldname4='Tsfc_E_bry'

      call read_bry_ice_data_nc (read1, 0, fyear, ixm, ixx, ixp, &
                maxrec, data_file,fieldname1,fieldname2, &
                fieldname3,fieldname4,Tsfc_work_bry, &
                field_loc_center, field_type_scalar)
       
      call interp_coeff (recnum, recslot, secday, dataloc)
      call interpolate_data_n (Tsfc_work_bry, Tsfc_bry)

      fieldname1='alvln_N_bry'
      fieldname2='alvln_S_bry'
      fieldname3='alvln_W_bry'
      fieldname4='alvln_E_bry'

      call read_bry_ice_data_nc (read1, 0, fyear, ixm, ixx, ixp, &
                maxrec, data_file,fieldname1,fieldname2, &
                fieldname3,fieldname4,alvln_work_bry, &
                field_loc_center, field_type_scalar)
       
      call interp_coeff (recnum, recslot, secday, dataloc)
      call interpolate_data_n (alvln_work_bry, alvln_bry)

      fieldname1='vlvln_N_bry'
      fieldname2='vlvln_S_bry'
      fieldname3='vlvln_W_bry'
      fieldname4='vlvln_E_bry'

      call read_bry_ice_data_nc (read1, 0, fyear, ixm, ixx, ixp, &
                maxrec, data_file,fieldname1,fieldname2, &
                fieldname3,fieldname4,vlvln_work_bry, &
                field_loc_center, field_type_scalar)
       
      call interp_coeff (recnum, recslot, secday, dataloc)
      call interpolate_data_n (vlvln_work_bry, vlvln_bry)

      fieldname1='apondn_N_bry'
      fieldname2='apondn_S_bry'
      fieldname3='apondn_W_bry'
      fieldname4='apondn_E_bry'

      call read_bry_ice_data_nc (read1, 0, fyear, ixm, ixx, ixp, &
                maxrec, data_file,fieldname1,fieldname2, &
                fieldname3,fieldname4,apondn_work_bry, &
                field_loc_center, field_type_scalar)
       
      call interp_coeff (recnum, recslot, secday, dataloc)
      call interpolate_data_n (apondn_work_bry, apondn_bry)

      fieldname1='hpondn_N_bry'
      fieldname2='hpondn_S_bry'
      fieldname3='hpondn_W_bry'
      fieldname4='hpondn_E_bry'

      call read_bry_ice_data_nc (read1, 0, fyear, ixm, ixx, ixp, &
                maxrec, data_file,fieldname1,fieldname2, &
                fieldname3,fieldname4,hpondn_work_bry, &
                field_loc_center, field_type_scalar)
       
      call interp_coeff (recnum, recslot, secday, dataloc)
      call interpolate_data_n (hpondn_work_bry, hpondn_bry)      

      fieldname1='ipondn_N_bry'
      fieldname2='ipondn_S_bry'
      fieldname3='ipondn_W_bry'
      fieldname4='ipondn_E_bry'

      call read_bry_ice_data_nc (read1, 0, fyear, ixm, ixx, ixp, &
                maxrec, data_file,fieldname1,fieldname2, &
                fieldname3,fieldname4,ipondn_work_bry, &
                field_loc_center, field_type_scalar)
       
      call interp_coeff (recnum, recslot, secday, dataloc)
      call interpolate_data_n (ipondn_work_bry, ipondn_bry) 

      !fieldname1='hbrine_N_bry'
      !fieldname2='hbrine_S_bry'
      !fieldname3='hbrine_W_bry'
      !fieldname4='hbrine_E_bry'

      !call read_bry_ice_data_nc (read1, 0, fyear, ixm, ixx, ixp, &
      !          maxrec, data_file,fieldname1,fieldname2, &
      !          fieldname3,fieldname4,hbrine_work_bry, &
      !          field_loc_center, field_type_scalar)
       
      !call interp_coeff (recnum, recslot, secday, dataloc)
      !call interpolate_data_n (hbrine_work_bry, hbrine_bry) 

      !fieldname1='fbrine_N_bry'
      !fieldname2='fbrine_S_bry'
      !fieldname3='fbrine_W_bry'
      !fieldname4='fbrine_E_bry'

      !call read_bry_ice_data_nc (read1, 0, fyear, ixm, ixx, ixp, &
      !          maxrec, data_file,fieldname1,fieldname2, &
      !          fieldname3,fieldname4,fbrine_work_bry, &
      !          field_loc_center, field_type_scalar)
      ! 
      !call interp_coeff (recnum, recslot, secday, dataloc)
      !call interpolate_data_n (fbrine_work_bry, fbrine_bry) 
  
      fieldname1='iage_N_bry'
      fieldname2='iage_S_bry'
      fieldname3='iage_W_bry'
      fieldname4='iage_E_bry'

      call read_bry_ice_data_nc (read1, 0, fyear, ixm, ixx, ixp, &
                maxrec, data_file,fieldname1,fieldname2, &
                fieldname3,fieldname4,iage_work_bry, &
                field_loc_center, field_type_scalar)
       
      call interp_coeff (recnum, recslot, secday, dataloc)
      call interpolate_data_n (iage_work_bry, iage_bry) 

      fieldname1='Tinz_N_bry'
      fieldname2='Tinz_S_bry'
      fieldname3='Tinz_W_bry'
      fieldname4='Tinz_E_bry'

      call read_bry_ice_data_nc (read1, 0, fyear, ixm, ixx, ixp, &
                maxrec, data_file,fieldname1,fieldname2, &
                fieldname3,fieldname4,Tinz_work_bry, &
                field_loc_center, field_type_scalar)
       
      call interp_coeff (recnum, recslot, secday, dataloc)
    
      call interpolate_data_n_layer &
              (Tinz_work_bry,Tinz_bry)
   
      fieldname1='Sinz_N_bry'
      fieldname2='Sinz_S_bry'
      fieldname3='Sinz_W_bry'
      fieldname4='Sinz_E_bry'

      call read_bry_ice_data_nc (read1, 0, fyear, ixm, ixx, ixp, &
                maxrec, data_file,fieldname1,fieldname2, &
                fieldname3,fieldname4,Sinz_work_bry, &
                field_loc_center, field_type_scalar)
       
      call interp_coeff (recnum, recslot, secday, dataloc)
      
      call interpolate_data_n_layer &
              (Sinz_work_bry,Sinz_bry)
           
      !if (my_task == master_task ) then
      !write (nu_diag,*) 'vsnon_N_bry =',vsnon_work_bry(2,ny_block,2,1,1:max_blocks)
      !write (nu_diag,*) 'vsnon_N =',vsnon_bry(2,ny_block,2,1:max_blocks)
      !write (nu_diag,*) 'Tinz_N_w =',Tinz_work_bry(2,ny_block,:,2,1,1:max_blocks),&
      !                   'Tinz_N =',Tinz_bry(2,ny_block,:,2,1:max_blocks) 
      !endif

      !if (my_task == master_task ) then
      !     write (nu_diag,*) 'boundary_data end'
      !end if 
      

      end subroutine boundary_data

!=======================================================================
 subroutine read_bry_ice_data_nc_2D (flag, recd, yr, ixm, ixx, ixp, &
                            maxrec, data_file, fieldname1, &
                            fieldname2,fieldname3,fieldname4,&
                            field_data, field_loc, field_type)
        
! This routine reads daily boundary data
! If data is at the end of a one-year record, get data from the
! following year.
! If no later data exists (end of fyear_final), then
! let the ixp value equal the last value of the year.

!
! Adapted by Pedro Duarte, Norwegian Polar Institute from read_data
! Modified:Nov 2017 

      use ice_constants, only: c0
      use ice_diagnostics, only: check_step
      use ice_timers, only: ice_timer_start, ice_timer_stop, timer_readwrite

      logical (kind=log_kind), intent(in) :: flag

      integer (kind=int_kind), intent(in) :: &
         recd                , & ! baseline record number
         yr                  , & ! year of forcing data
         ixm, ixx, ixp       , & ! record numbers of 3 data values
                                 ! relative to recd
         maxrec                  ! maximum record value

      character (char_len_long) :: &
         data_file               ! data file to be read

      character (char_len), intent(in) :: &
         fieldname1, &               ! field name in netCDF file
         fieldname2, &  
         fieldname3, &  
         fieldname4  
 
      integer (kind=int_kind), intent(in) :: &
           field_loc, &      ! location of field on staggered grid
           field_type        ! type of field (scalar, vector, angle)
           

      real (kind=dbl_kind), dimension(nx_block,ny_block,2,max_blocks), &
         intent(out) :: &
         field_data              ! 2 values needed for interpolation

      ! local variables

      logical ::debug

#ifdef ncdf 
      integer (kind=int_kind) :: &
         nrec             , & ! record number to read
         n4               , & ! like ixp, but
                              ! adjusted at beginning and end of data
         arg              , & ! value of time argument in field_data
         fid                  ! file id for netCDF routines

      call ice_timer_start(timer_readwrite)  ! reading/writing

      if (istep1 > check_step) dbug = .true.  !! debugging

!jd      debug=.true.
      debug=.false.
      if (dbug) debug=.true.
      !if (my_task == master_task ) then
      !     write (nu_diag,*) 'flag =',flag
      !end if

!METNO START
      if (flag) then
      !if (my_task == master_task ) then
      !     write (nu_diag,*) 'flag 2 =',flag
      !end if
      !if (my_task==master_task .and. (dbug)) then
      !   write(nu_diag,*) ' ', trim(data_file),' ',&
      !        trim(fieldname1),' ',trim(fieldname2),' ',&
      !        trim(fieldname3),' ',trim(fieldname4)
      !endif
!METNO END
      !-----------------------------------------------------------------
      ! Initialize record counters
      ! (n4 will change only at the end of
      !  a forcing cycle.)
      !-----------------------------------------------------------------

         n4 = ixp
         arg = 0

      !-----------------------------------------------------------------
      ! read data
      !-----------------------------------------------------------------

         ! always read ixx data from data file for current year
         
         call ice_open_nc (data_file, fid)

         arg = arg + 1
         nrec = recd + ixx

!jd
         if (my_task==master_task .and. (debug)) &
              write(nu_diag,*) ' ', trim(data_file),' ',&
              trim(fieldname1),' ',trim(fieldname2),' ',&
              trim(fieldname3),' ',trim(fieldname4),' ', &
              ' reading nrec ', nrec, ' into slot ', arg
!jd
         call ice_read_nc & 
              (fid, nrec, fieldname1, fieldname2, &
               fieldname3, fieldname4, field_data(:,:,arg,:), dbug, &
               field_loc, field_type)

         if (ixp /= -99) then
         ! currently in latter half of data interval
            if (ixx==maxrec) then
               if (yr < fyear_final) then ! get data from following year
                  call ice_close_nc(fid)
                  call file_year (data_file, yr+1)
                  call ice_open_nc (data_file, fid)
               else             ! yr = fyear_final, no more data exists
                  if (maxrec > 12) then ! extrapolate from ixx
                     n4 = ixx
                  else          ! go to beginning of fyear_init
                     call ice_close_nc(fid)
                     call file_year (data_file, fyear_init)
                     call ice_open_nc (data_file, fid)

                  endif
               endif            ! yr < fyear_final
            endif               ! ixx = maxrec

            arg = arg + 1
            nrec = recd + n4

!jd
!            if (my_task==master_task .and. (debug)) &
!                 write(nu_diag,*) ' ', trim(data_file),' ',&
!                 trim(fieldname1),' ',trim(fieldname2),' ',&
!                 trim(fieldname3),' ',trim(fieldname4),' ',&
!                 ' reading nrec 2D', nrec, ' into slot ', arg
!jd

            call ice_read_nc & 
                 (fid, nrec, fieldname1, fieldname2, &
                  fieldname3, fieldname4, field_data(:,:,arg,:), dbug, &
                  field_loc, field_type)
         endif                  ! ixp /= -99

         call ice_close_nc(fid)

      endif                     ! flag

      call ice_timer_stop(timer_readwrite)  ! reading/writing
      dbug=.false.
#else
      field_data = c0 ! to satisfy intent(out) attribute
#endif
      end subroutine read_bry_ice_data_nc_2D

!=======================================================================
      subroutine read_bry_ice_data_nc_3D (flag, recd, yr, ixm, ixx, ixp, &
                            maxrec, data_file, fieldname1, &
                            fieldname2,fieldname3,fieldname4,&
                            field_data, field_loc, field_type)
        
! This routine reads daily boundary data
! If data is at the end of a one-year record, get data from the
! following year.
! If no later data exists (end of fyear_final), then
! let the ixp value equal the last value of the year.
!
! Adapted by Pedro Duarte, Norwegian Polar Institute from read_data
! Modified:Nov 2017 

      use ice_constants, only: c0
      use ice_diagnostics, only: check_step
      use ice_timers, only: ice_timer_start, ice_timer_stop, timer_readwrite

      logical (kind=log_kind), intent(in) :: flag

      integer (kind=int_kind), intent(in) :: &
         recd                , & ! baseline record number
         yr                  , & ! year of forcing data
         ixm, ixx, ixp       , & ! record numbers of 3 data values
                                 ! relative to recd
         maxrec                  ! maximum record value

      character (char_len_long) :: &
         data_file               ! data file to be read

      character (char_len), intent(in) :: &
         fieldname1, &               ! field name in netCDF file
         fieldname2, &  
         fieldname3, &  
         fieldname4  
 
      integer (kind=int_kind), intent(in) :: &
           field_loc, &      ! location of field on staggered grid
           field_type        ! type of field (scalar, vector, angle)
           

      real (kind=dbl_kind), dimension(nx_block,ny_block,ncat,2,max_blocks), &
         intent(out) :: &
         field_data              ! 2 values needed for interpolation

      ! local variables

      logical ::debug

#ifdef ncdf 
      integer (kind=int_kind) :: &
         nrec             , & ! record number to read
         n4               , & ! like ixp, but
                              ! adjusted at beginning and end of data
         arg              , & ! value of time argument in field_data
         fid                  ! file id for netCDF routines

      call ice_timer_start(timer_readwrite)  ! reading/writing

      if (istep1 > check_step) dbug = .true.  !! debugging

!jd      debug=.true.
      debug=.false.
      if (dbug) debug=.true.
      !if (my_task == master_task ) then
      !     write (nu_diag,*) 'flag =',flag
      !end if

!METNO START
      if (flag) then
      !if (my_task == master_task ) then
      !     write (nu_diag,*) 'flag 2 =',flag
      !end if
      !if (my_task==master_task .and. (dbug)) then
      !   write(nu_diag,*) ' ', trim(data_file),' ',&
      !        trim(fieldname1),' ',trim(fieldname2),' ',&
      !        trim(fieldname3),' ',trim(fieldname4)
      !endif
!METNO END
      !-----------------------------------------------------------------
      ! Initialize record counters
      ! (n4 will change only at the end of
      !  a forcing cycle.)
      !-----------------------------------------------------------------
         n4 = ixp
         arg = 0

      !-----------------------------------------------------------------
      ! read data
      !-----------------------------------------------------------------

         ! always read ixx data from data file for current year
         
         call ice_open_nc (data_file, fid)

         arg = arg + 1
         nrec = recd + ixx

!jd
!         if (my_task==master_task .and. (debug)) &
!              write(nu_diag,*) ' ', trim(data_file),' ',&
!              trim(fieldname1),' ',trim(fieldname2),' ',&
!              trim(fieldname3),' ',trim(fieldname4),' ', &
!              ' reading nrec 3D', nrec, ' into slot ', arg
!jd
         call ice_read_nc & 
              (fid, nrec, fieldname1, fieldname2, &
               fieldname3, fieldname4, field_data(:,:,:,arg,:), dbug, &
               field_loc, field_type)

         if (ixp /= -99) then
         ! currently in latter half of data interval
            if (ixx==maxrec) then
               if (yr < fyear_final) then ! get data from following year
                  call ice_close_nc(fid)
                  call file_year (data_file, yr+1)
                  call ice_open_nc (data_file, fid)
               else             ! yr = fyear_final, no more data exists
                  if (maxrec > 12) then ! extrapolate from ixx
                     n4 = ixx
                  else          ! go to beginning of fyear_init
                     call ice_close_nc(fid)
                     call file_year (data_file, fyear_init)
                     call ice_open_nc (data_file, fid)

                  endif
               endif            ! yr < fyear_final
            endif               ! ixx = maxrec

            arg = arg + 1
            nrec = recd + n4

!jd
            if (my_task==master_task .and. (debug)) &
                 write(nu_diag,*) ' ', trim(data_file),' ',&
                 trim(fieldname1),' ',trim(fieldname2),' ',&
                 trim(fieldname3),' ',trim(fieldname4),' ',&
                 ' reading nrec ', nrec, ' into slot ', arg
!jd

            call ice_read_nc & 
                 (fid, nrec, fieldname1, fieldname2, &
                  fieldname3, fieldname4, field_data(:,:,:,arg,:), dbug, &
                  field_loc, field_type)
         endif                  ! ixp /= -99

         call ice_close_nc(fid)

      endif                     ! flag

      call ice_timer_stop(timer_readwrite)  ! reading/writing
      dbug=.false.
#else
      field_data = c0 ! to satisfy intent(out) attribute
#endif
      end subroutine read_bry_ice_data_nc_3D

!=======================================================================
 subroutine read_bry_ice_data_nc_4D (flag, recd, yr, ixm, ixx, ixp, &
                            maxrec, data_file, fieldname1, &
                            fieldname2,fieldname3,fieldname4,&
                            field_data, field_loc, field_type)
        
! This routine reads daily boundary data
! If data is at the end of a one-year record, get data from the
! following year.
! If no later data exists (end of fyear_final), then
! let the ixp value equal the last value of the year.

!
! Adapted by Pedro Duarte, Norwegian Polar Institute from read_data
! Modified:Nov 2017 

      use ice_constants, only: c0
      use ice_diagnostics, only: check_step
      use ice_timers, only: ice_timer_start, ice_timer_stop, timer_readwrite

      logical (kind=log_kind), intent(in) :: flag

      integer (kind=int_kind), intent(in) :: &
         recd                , & ! baseline record number
         yr                  , & ! year of forcing data
         ixm, ixx, ixp       , & ! record numbers of 3 data values
                                 ! relative to recd
         maxrec                  ! maximum record value

      character (char_len_long) :: &
         data_file               ! data file to be read

      character (char_len), intent(in) :: &
         fieldname1, &               ! field name in netCDF file
         fieldname2, &  
         fieldname3, &  
         fieldname4  
 
      integer (kind=int_kind), intent(in) :: &
           field_loc, &      ! location of field on staggered grid
           field_type        ! type of field (scalar, vector, angle)
           

      real (kind=dbl_kind), dimension(nx_block,ny_block,nilyr,ncat,2,max_blocks), &
         intent(out) :: &
         field_data              ! 2 values needed for interpolation

      ! local variables

      logical ::debug

#ifdef ncdf 
      integer (kind=int_kind) :: &
         nrec             , & ! record number to read
         n4               , & ! like ixp, but
                              ! adjusted at beginning and end of data
         arg              , & ! value of time argument in field_data
         fid                  ! file id for netCDF routines

      call ice_timer_start(timer_readwrite)  ! reading/writing

      if (istep1 > check_step) dbug = .true.  !! debugging

!jd      debug=.true.
      debug=.false.
      if (dbug) debug=.true.
      !if (my_task == master_task ) then
      !     write (nu_diag,*) 'flag =',flag
      !end if

!METNO START
      if (flag) then
      !if (my_task == master_task ) then
      !     write (nu_diag,*) 'flag 2 =',flag
      !end if
      !if (my_task==master_task .and. (dbug)) then
      !   write(nu_diag,*) ' ', trim(data_file),' ',&
      !        trim(fieldname1),' ',trim(fieldname2),' ',&
      !        trim(fieldname3),' ',trim(fieldname4)
      !endif
!METNO END
      !-----------------------------------------------------------------
      ! Initialize record counters
      ! (n4 will change only at the end of
      !  a forcing cycle.)
      !-----------------------------------------------------------------

         n4 = ixp
         arg = 0

      !-----------------------------------------------------------------
      ! read data
      !-----------------------------------------------------------------

         ! always read ixx data from data file for current year
         
         call ice_open_nc (data_file, fid)

         arg = arg + 1
         nrec = recd + ixx

!jd
         if (my_task==master_task .and. (debug)) &
              write(nu_diag,*) ' ', trim(data_file),' ',&
              trim(fieldname1),' ',trim(fieldname2),' ',&
              trim(fieldname3),' ',trim(fieldname4),' ', &
              ' reading nrec ', nrec, ' into slot ', arg
!jd
         call ice_read_nc & 
              (fid, nrec, fieldname1, fieldname2, &
               fieldname3, fieldname4, field_data(:,:,:,:,arg,:), dbug, &
               field_loc, field_type)

         if (ixp /= -99) then
         ! currently in latter half of data interval
            if (ixx==maxrec) then
               if (yr < fyear_final) then ! get data from following year
                  call ice_close_nc(fid)
                  call file_year (data_file, yr+1)
                  call ice_open_nc (data_file, fid)
               else             ! yr = fyear_final, no more data exists
                  if (maxrec > 12) then ! extrapolate from ixx
                     n4 = ixx
                  else          ! go to beginning of fyear_init
                     call ice_close_nc(fid)
                     call file_year (data_file, fyear_init)
                     call ice_open_nc (data_file, fid)

                  endif
               endif            ! yr < fyear_final
            endif               ! ixx = maxrec

            arg = arg + 1
            nrec = recd + n4

!jd
!            if (my_task==master_task .and. (debug)) &
!                 write(nu_diag,*) ' ', trim(data_file),' ',&
!                 trim(fieldname1),' ',trim(fieldname2),' ',&
!                 trim(fieldname3),' ',trim(fieldname4),' ',&
!                 ' reading nrec 4D', nrec, ' into slot ', arg
!jd

            call ice_read_nc & 
                 (fid, nrec, fieldname1, fieldname2, &
                  fieldname3, fieldname4, field_data(:,:,:,:,arg,:), dbug, &
                  field_loc, field_type)
         endif                  ! ixp /= -99

         call ice_close_nc(fid)

      endif                     ! flag

      call ice_timer_stop(timer_readwrite)  ! reading/writing
      dbug=.false.
#else
      field_data = c0 ! to satisfy intent(out) attribute
#endif
      end subroutine read_bry_ice_data_nc_4D
!=======================================================================

      subroutine interpolate_data_n (field_data, field)

! Linear interpolation for variables belonging to various ice types

! author: ! Adapted by Pedro Duarte, Norwegian Polar Institute, from interpolate_data by Elizabeth C. Hunke, LANL
! Modified:Nov 2017 

      use ice_domain, only: nblocks

      real (kind=dbl_kind), dimension(nx_block,ny_block,ncat,2,max_blocks), &
        intent(in) :: &
        field_data    ! 2 values used for interpolation

      real (kind=dbl_kind), dimension(nx_block,ny_block,ncat,max_blocks), &
        intent(out) :: &
        field         ! interpolated field

      ! local variables

      integer (kind=int_kind) :: n,i,j, iblk

      !$OMP PARALLEL DO PRIVATE(iblk,i,j)
      do iblk = 1, nblocks
         do j = 1, ny_block
         do i = 1, nx_block
         do n = 1, ncat
            field(i,j,n,iblk) = c1intp * field_data(i,j,n,1,iblk) &
                            + c2intp * field_data(i,j,n,2,iblk)
         enddo
         enddo
         enddo
      enddo
      !$OMP END PARALLEL DO

      end subroutine interpolate_data_n

!=======================================================================

      subroutine interpolate_data_n_layer (field_data, field)

! Linear interpolation for variables belonging to various ice types and layers

! author: ! Adapted by Pedro Duarte, Norwegian Polar Institute, from interpolate_data by Elizabeth C. Hunke, LANL
! Modified:Nov 2017 

      use ice_domain, only: nblocks

      real (kind=dbl_kind), dimension(nx_block,ny_block,nilyr,ncat,2,max_blocks), &
        intent(in) :: &
        field_data    ! 2 values used for interpolation

      real (kind=dbl_kind), dimension(nx_block,ny_block,nilyr,ncat,max_blocks), &
        intent(out) :: &
        field         ! interpolated field

      ! local variables

      integer (kind=int_kind) :: n,i,j,k,iblk

      !$OMP PARALLEL DO PRIVATE(iblk,i,j)
      do iblk = 1, nblocks
         do k = 1, nilyr
         do j = 1, ny_block
         do i = 1, nx_block
         do n = 1, ncat
            field(i,j,k,n,iblk) = c1intp * field_data(i,j,k,n,1,iblk) &
                            + c2intp * field_data(i,j,k,n,2,iblk)
         enddo
         enddo
         enddo
         enddo
      enddo
      !$OMP END PARALLEL DO

      end subroutine interpolate_data_n_layer

!Pedro stuff ends  
!=======================================================================
    
     end module ice_forcing

!=======================================================================
