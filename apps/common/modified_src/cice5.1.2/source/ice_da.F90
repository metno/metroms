!  SVN:$Id: ice_da.F90 2019-01-08 $
!=======================================================================
!
! Perform data assimilate for sea ice state, using
! 1) Combined Optimal Interpolation and Nudging (COIN) scheme
!
! authors: Keguang Wang, MET.no

      module ice_da

      use ice_kinds_mod
      use ice_constants, only: c0, c1, p1, p01, secday, puny, &
                               field_loc_center, field_type_scalar
      use ice_blocks, only: nx_block, ny_block
      use ice_domain_size, only: ncat, max_blocks, max_ntrcr
      use ice_communicate, only: my_task, master_task
      use ice_fileunits, only: nu_diag
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
         vice_obs     ,  & ! observed ice volume (m) 
         vice_obs_err ,  & ! observed ice volume std (m) 
         vsno_obs     ,  & ! observed snow volume (m) 
         vsno_obs_err      ! observed snow volume std (m) 

      real (kind=dbl_kind), dimension (:,:,:,:), allocatable :: &
         trcr_obs, trcr_err     ! tracers

!=======================================================================

      contains

!=======================================================================

!  Allocates and initializes arrays needed for data assimilation, & 
!  read observation data.


 subroutine init_da

      use ice_blocks, only: block, get_block, nblocks_x, nblocks_y
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

   call ice_timer_start(timer_da)

!=======================================================================
   if (ew_boundary_type == 'open' .and. &
       ns_boundary_type == 'open' .and. .not.(restart_ext)) then
      if (my_task == master_task) write (nu_diag,*) &
            'WARNING: Setting restart_ext = T for open boundaries'
      restart_ext = .true.
   endif

   if (da_sic) then
      allocate (aice_obs(nx_block,ny_block,max_blocks), &
                aice_obs_err(nx_block,ny_block,max_blocks))
      aice_obs = c0
      aice_obs_err = c0
   endif

   if (da_sit) then
      allocate (vice_obs(nx_block,ny_block,max_blocks), &
                vice_obs_err(nx_block,ny_block,max_blocks))
      vice_obs = c0
      vice_obs_err = c0
   endif

   if (da_sno) then
      allocate (vsno_obs(nx_block,ny_block,max_blocks), &
                vsno_obs_err(nx_block,ny_block,max_blocks))
      vsno_obs = c0
      vsno_obs_err = c0
   endif

   call ice_timer_stop(timer_da)

 end subroutine init_da

!=======================================================================

!  This subroutine read observations, and call assimilation subroutines,

 subroutine ice_da_run

      use ice_blocks, only: block, get_block, nblocks_x, nblocks_y
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

   if ((istep == 1) .or. new_day) then

   ! sea ice concentration & uncertainties
      write(da_date,'(i4)') idate/10000
      data_file = trim(da_data_dir)//'osisaf_'//trim(da_date)//'.nc'
      write(nu_diag,*) 'DA data file = ', data_file

      call ice_open_nc(data_file,fid)

      fieldname = 'obsAice'
      call ice_read_nc (fid, int(yday), fieldname, aice_obs, dbug, &
           field_loc_center, field_type_scalar)
        
      fieldname = 'obsAerr'
      call ice_read_nc (fid, int(yday), fieldname, aice_obs_err, dbug, &
           field_loc_center, field_type_scalar)

      where ((aice_obs >= c0) .and. (aice_obs) <= 100)
           aice_obs = aice_obs * p01
           aice_obs_err = aice_obs_err * p01
      elsewhere
           aice_obs = c0
           aice_obs_err = c0
      endwhere

      call ice_close_nc(fid)

   endif
         
   if (da_method == 'coin') then

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

         call da_coin  (nx_block,            ny_block,      &
                        ilo, ihi,            jlo, jhi,      &
                        iglob,               jglob,         &
                        iblock,              jblock,        &
                        tmask(:,:,    iblk),                &
                        aice(:,:,     iblk),                &
                        aice_obs(:,:, iblk),                &
                        aice_obs_err(:,:, iblk),            &
                        aicen(:,:,  :,iblk),                &
                        vicen(:,:,  :,iblk),                &
                        vsnon(:,:,  :,iblk),                &
                        trcrn(:,:,:,:,iblk), ntrcr)         
      enddo ! iblk
      !$OMP END PARALLEL DO
   endif

   call ice_timer_stop(timer_da)

end subroutine ice_da_run

!=======================================================================

subroutine da_coin    (nx_block,            ny_block,      &
                       ilo, ihi,            jlo, jhi,      &
                       iglob,               jglob,         &
                       iblock,              jblock,        &
                       tmask,               aice,          &
                       aice_obs,            aice_obs_err,  &
                       aicen,     vicen,    vsnon,         &
                       trcrn,     ntrcr)         

      use ice_blocks, only: nblocks_x, nblocks_y

      integer (kind=int_kind), intent(in) :: &
         nx_block, ny_block, & ! block dimensions
         ilo, ihi          , & ! physical domain indices
         jlo, jhi          , & !
         iglob(nx_block)   , & ! global indices
         jglob(ny_block)   , & !
         iblock            , & ! block indices
         jblock            , & !
         ntrcr                 ! number of tracers in use

      logical (kind=log_kind), dimension (nx_block,ny_block), &
         intent(inout) :: &
         tmask                 ! true for ice/ocean cells

      real (kind=dbl_kind), dimension (nx_block,ny_block), &
         intent(in) :: &
         aice,               & ! model aggregate sic
         aice_obs,           & ! observed aggregate sic
         aice_obs_err          ! observed aggregate sic_err
         
      real (kind=dbl_kind), dimension (nx_block,ny_block,ncat), &
         intent(inout) :: &
         aicen , & ! concentration of ice
         vicen , & ! volume per unit area of ice          (m)
         vsnon     ! volume per unit area of snow         (m)

      real (kind=dbl_kind), dimension (nx_block,ny_block,ntrcr,ncat), &
         intent(inout) :: &
         trcrn     ! ice tracers
                   ! 1: surface temperature of ice/snow (C)

      ! local variables

      integer (kind=int_kind) :: &
         i, j        , & ! horizontal indices
         ij          , & ! horizontal index, combines i and j loops
         ibc         , & ! ghost cell column or row
         npad        , & ! padding column/row counter
         k           , & ! ice layer index
         n           , & ! thickness category index
         it          , & ! tracer index
         icells          ! number of cells initialized with ice

      integer (kind=int_kind), dimension(nx_block*ny_block) :: &
         indxi, indxj    ! compressed indices for cells with restoring

      real (kind=dbl_kind) :: &
         mod_err,      & ! model error
         gain,         & ! Kalman gain, optimal estimated
         weight,       & ! nudging weight, increamental Kalman gain
         weightn,      & ! nudging weigth distributed among categories
         rda             ! dt/dT, where dT is observation time step

      call ice_timer_start(timer_da)

      !-----------------------------------------------------------------
      ! assimilate sic on grid
      !-----------------------------------------------------------------

      rda = dt / real(secday,kind=dbl_kind)
      if (my_task == master_task) write(nu_diag,*) 'rda = ', rda

      if (da_sic == .true.) then

         do j = 1, ny_block
         do i = 1, nx_block
            if (tmask(i,j)) then
               mod_err = aice(i,j) - aice_obs(i,j)
               gain   = mod_err**2/(mod_err**2+puny+aice_obs_err(i,j)**2)
               weight = c1 - (c1 - gain)**rda
            else
               weight = c0
            endif

            if (aice(i,j) > p1) then
               do n=1, ncat
                  aicen(i,j,n) = aicen(i,j,n) * (c1 + weight &
                        * (aice_obs(i,j)/aice(i,j) - c1))
               enddo
            else
               if (tmask(i,j)) then
                  aicen(i,j,1) = aicen(i,j,1) + weight * &
                                (aice_obs(i,j) - aicen(i,j,1))
               endif
            endif
         enddo
         enddo
      endif

      call ice_timer_stop(timer_da)

end subroutine da_coin

!=======================================================================

      end module ice_da

!=======================================================================
