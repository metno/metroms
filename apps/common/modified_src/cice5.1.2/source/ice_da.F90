!  SVN:$Id: ice_da.F90 2019-01-08 $
!=======================================================================
!
! Perform data assimilate for sea ice state, using
! 1) Combined Optimal Interpolation and Nudging (COIN) scheme
!
! authors: Keguang Wang, MET.no

      module ice_da

      use ice_kinds_mod
      use ice_constants, only: c0, c1, puny, &
                               field_loc_center, field_type_scalar
      use ice_blocks, only: nx_block, ny_block
      use ice_domain_size, only: ncat, max_blocks, max_ntrcr
      use ice_grid, only: tmask
      use ice_forcing, only: dbug
      use ice_state, only: aicen, vicen, vsnon, trcrn, ntrcr, bound_state, &
                           aice_init, aice0, aice, vice, vsno, trcr, trcr_depend
      use ice_timers, only: ice_timer_start, ice_timer_stop, timer_da
      use ice_calendar, only: istep, idate, new_day, yday, dt
      use ice_read_write, only: ice_open_nc, ice_read_nc, ice_close_nc

      implicit none
      private
      public :: init_da, ice_da_run
      save

      logical (kind=log_kind), public :: &
         da_ice ,       & ! perform data assimilation if true
         da_sic ,       & ! perform da of sic if true
         da_sit ,       & ! perform da of sea ice thickess if true
         da_sno           ! for snow depth if true

     character (char_len), public :: &
         da_method        ! data assimilation method

      character (char_len_long), public :: &
         da_data_dir       ! top directory for data to assimilate 

      !-----------------------------------------------------------------
      ! observed & model ice/snow variables & uncertainties
      !-----------------------------------------------------------------

      real (kind=dbl_kind), dimension (:,:,:), allocatable :: &
         aice_obs     ,  & ! observed SIC 
         aice_obs_err ,  & ! observed SIC std
         aice_mod_err ,  & ! modeled SIC std
         aice_gain    ,  & ! Kalman gain for aice     
         aice_nw           ! Optimal nudging weight for SIC

      real (kind=dbl_kind), dimension (:,:,:), allocatable :: &
         vice_obs     ,  & ! observed ice volume (m) 
         vice_obs_err ,  & ! observed ice volume std (m) 
         vice_mod_err ,  & ! modeled ice volume std (m) 
         vice_gain    ,  & ! Kalman gain for vice
         vice_nw           ! optimal nudging weigth for SIV

      real (kind=dbl_kind), dimension (:,:,:), allocatable :: &
         vsno_obs     ,  & ! observed snow volume (m) 
         vsno_obs_err ,  & ! observed snow volume std (m) 
         vsno_gain    ,  & ! Kalman gain for vsno
         vsno_mod_err ,  & ! modeled snow volume std (m) 
         vsno_nw           ! optimal nudging weigth for snow volume

      real (kind=dbl_kind), dimension (:,:,:,:), allocatable :: &
         trcr_obs, trcr_err     ! tracers

      !-----------------------------------------------------------------
      ! optimal nudging weigth for categories
      !-----------------------------------------------------------------

      real (kind=dbl_kind), dimension (:,:,:,:), allocatable :: &
         aicen_nw , & ! nudging weight for SIC categories
         vicen_nw , & ! for sea ice volume categories
         vsnon_nw     ! for snow volume categories

      real (kind=dbl_kind), dimension (:,:,:,:,:), allocatable :: &
         trcrn_da     ! tracers

!=======================================================================

      contains

!=======================================================================

!  Allocates and initializes arrays needed for data assimilation, & 
!  read observation data.


 subroutine init_da

      use ice_blocks, only: block, get_block, nblocks_x, nblocks_y
      use ice_communicate, only: my_task, master_task
      use ice_constants, only: c0
      use ice_domain, only: ew_boundary_type, ns_boundary_type, &
          nblocks, blocks_ice
      use ice_fileunits, only: nu_diag, nu_da
      use ice_grid, only: tmask
      use ice_flux, only: sst, Tf, Tair, salinz, Tmltz
      use ice_itd, only: aggregate
      use ice_restart_shared, only: restart_ext

   integer (int_kind) :: &
     i,j,iblk,nt,n,k,    &! dummy loop indices
     ilo,ihi,jlo,jhi,    &! beginning and end of physical domain
     iglob(nx_block),    &! global indices
     jglob(ny_block),    &! global indices
     iblock, jblock,     &! block indices
     ibc,                &! ghost cell column or row
     npad                 ! padding column/row counter

   type (block) :: &
     this_block  ! block info for current block

!=======================================================================
   if (ew_boundary_type == 'open' .and. &
       ns_boundary_type == 'open' .and. .not.(restart_ext)) then
      if (my_task == master_task) write (nu_diag,*) &
            'WARNING: Setting restart_ext = T for open boundaries'
      restart_ext = .true.
   endif

   if (da_sic) then
      allocate (aice_obs(nx_block,ny_block,max_blocks), &
                aice_obs_err(nx_block,ny_block,max_blocks), &
                aice_mod_err(nx_block,ny_block,max_blocks), &
                aice_gain(nx_block,ny_block,max_blocks), &
                aice_nw(nx_block,ny_block,max_blocks), &
                aicen_nw(nx_block,ny_block,ncat,max_blocks))
      aice_obs = c0
      aice_obs_err = c0
      aice_mod_err = c0
      aice_gain = c0
      aice_nw = c0
      aicen_nw = c0
   endif

   if (da_sit) then
      allocate (vice_obs(nx_block,ny_block,max_blocks), &
                vice_obs_err(nx_block,ny_block,max_blocks), &
                vice_mod_err(nx_block,ny_block,max_blocks), &
                vice_gain(nx_block,ny_block,max_blocks), &
                vice_nw(nx_block,ny_block,max_blocks), &
                vicen_nw(nx_block,ny_block,ncat,max_blocks))
      vice_obs = c0
      vice_obs_err = c0
      vice_mod_err = c0
      vice_gain = c0
      vice_nw = c0
      vicen_nw = c0
   endif

   if (da_sno) then
      allocate (vsno_obs(nx_block,ny_block,max_blocks), &
                vsno_obs_err(nx_block,ny_block,max_blocks), &
                vsno_mod_err(nx_block,ny_block,max_blocks), &
                vsno_gain(nx_block,ny_block,max_blocks), &
                vsno_nw(nx_block,ny_block,max_blocks), &
                vsnon_nw(nx_block,ny_block,ncat,max_blocks))
      vsno_obs = c0
      vsno_obs_err = c0
      vsno_mod_err = c0
      vsno_gain = c0
      vsno_nw = c0
      vsnon_nw = c0
   endif


 end subroutine init_da

!=======================================================================

!  This subroutine read observations, and call assimilation subroutines,

 subroutine ice_da_run

      use ice_blocks, only: block, get_block, nblocks_x, nblocks_y
      use ice_calendar, only: dt
      use ice_constants, only: secday
      use ice_domain, only: ew_boundary_type, ns_boundary_type, &
          nblocks, blocks_ice

!-----------------------------------------------------------------------
!  local variables
!-----------------------------------------------------------------------

   integer (int_kind) :: &
     i,j,iblk,nt,n,      & ! dummy loop indices
     ilo,ihi,jlo,jhi,    & ! beginning and end of physical domain
     iglob(nx_block),    & ! global indices
     jglob(ny_block),    & ! global indices
     iblock, jblock,     & ! block indices
     ibc,                & ! ghost cell column or row
     nda,                & ! number of model steps between two observations
     npad,               & ! padding column/row counter
     fid                   ! file id for netCDF routines

   character (char_len_long) :: &
     data_file             ! data file for observations

   character (char_len) :: &
     da_date,            & ! date for data assimilation
     fieldname             ! field name in netcdf file

   type (block) :: &
     this_block            ! block info for current block

   call ice_timer_start(timer_da)

!-----------------------------------------------------------------------
!  Read observations
!-----------------------------------------------------------------------

   if (new_day) then

   ! sea ice concentration & uncertainties
      write(da_date,'(i4)') idate/10000

      data_file = trim(da_data_dir)//'osisaf_'//trim(da_date)//'.nc'
      call ice_open_nc(data_file,fid)

      fieldname = 'obsAice'
      call ice_read_nc (fid, int(yday), fieldname, aice_obs, dbug, &
           field_loc_center, field_type_scalar)
         
      fieldname = 'obsAerr'
      call ice_read_nc (fid, int(yday), fieldname, aice_obs_err, dbug, &
           field_loc_center, field_type_scalar)

      call ice_close_nc(fid)

   endif
         
   if (da_method == 'coin') then

      nda = secday / dt

      ! perform data assimilation
      !$OMP PARALLEL DO PRIVATE(iblk,ilo,ihi,jlo,jhi,this_block, &
      !$OMP                     iglob,jglob,iblock,jblock)
      do iblk = 1, nblocks
         this_block = get_block(blocks_ice(iblk),iblk)         
         ilo = this_block%ilo
         ihi = this_block%ihi
         jlo = this_block%jlo
         jhi = this_block%jhi
         iglob = this_block%i_glob
         jglob = this_block%j_glob
         iblock = this_block%iblock
         jblock = this_block%jblock

         if (da_sic == .true.) then

            do j = jlo, jhi
            do i = ilo, ihi

               if (tmask(i,j,iblk)) then
                  aice_mod_err(i,j,iblk) = aice(i,j,iblk) - aice_obs(i,j,iblk)
                  aice_gain(i,j,iblk) = aice_mod_err(i,j,iblk)**2 /  &
                       (aice_mod_err(i,j,iblk)**2 + puny + &
                        aice_obs_err(i,j,iblk)**2)
                  aice_nw(i,j,iblk) = c1 - (c1 - aice_gain(i,j,iblk)) &
                       **(c1/real(nda,kind=dbl_kind))
               else
                  aice_nw(i,j,iblk) = c0
               endif

               if (aice(i,j,iblk) > c0) then
                  do n=1, ncat
                     aicen(i,j,n,iblk) = aicen(i,j,n,iblk) * (1 - aice_nw(i,j,iblk) &
                        * (aice_obs(i,j,iblk)/aice(i,j,iblk) - 1))
                  enddo
               else
                  if (tmask(i,j,iblk)) then
                     aicen(i,j,1,iblk) = aicen(i,j,1,iblk) + aice_nw(i,j,iblk) * &
                                        (aice_obs(i,j,iblk) - aicen(i,j,1,iblk))
                  endif

               endif
            enddo
            enddo
         endif
      enddo
   endif

   call ice_timer_stop(timer_da)

end subroutine ice_da_run

!=======================================================================

      end module ice_da

!=======================================================================
