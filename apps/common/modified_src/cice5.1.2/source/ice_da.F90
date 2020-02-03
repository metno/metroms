!  SVN:$Id: ice_da.F90 2019-01-08 $
!=======================================================================
!
! Perform data assimilate for sea ice state, using
! 1) Combined Optimal Interpolation and Nudging (COIN) scheme
!
! authors: Keguang Wang, MET.no

      module ice_da

      use ice_kinds_mod
      use ice_constants, only: c0, c1, c5, p1, p5, p01, puny, &
                   Tsmelt, Tffresh, rhoi, cp_ice, cp_ocn, Lfresh, &
                   secday, field_loc_center, field_type_scalar
      use ice_blocks, only: nx_block, ny_block
      use ice_domain_size, only: ncat, nilyr, max_blocks, max_ntrcr
      use ice_communicate, only: my_task, master_task
      use ice_fileunits, only: nu_diag
      use ice_grid, only: tmask
      use ice_forcing, only: dbug
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
         da_sno ,       & ! for snow depth if true
         da_insert,     & ! direct insertion
         corr_bias        ! perform bias correction if true

     character (char_len), public :: &
         da_method        ! data assimilation method

      character (char_len_long), public :: &
         da_data_dir      ! top directory for data to assimilate 

      real (kind=dbl_kind), public :: &
         Tobs             ! time step for observations, in seconds

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

      call ice_timer_start(timer_da)

!-----------------------------------------------------------------------
!     allocate variables for sea ice observations
!-----------------------------------------------------------------------
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
      use ice_state, only: aicen, vicen, vsnon, trcrn, ntrcr, bound_state, &
                           aice_init, aice0, aice, vice, vsno, trcr, trcr_depend
      use ice_itd, only: aggregate
      use ice_flux, only: Tf, Tair, sst, salinz, Tmltz

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
                        Tf(:,:,       iblk),                &
                        Tair(:,:,     iblk),                &
                        Tmltz(:,:,:,  iblk),                &
                        salinz(:,:,:, iblk),                &
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

   !-----------------------------------------------------------------
   ! aggregate tracers
   !-----------------------------------------------------------------

   !$OMP PARALLEL DO PRIVATE(iblk)
   do iblk = 1, nblocks

      call aggregate (nx_block, ny_block, &
                      aicen(:,:,:,iblk),  &
                      trcrn(:,:,:,:,iblk),&
                      vicen(:,:,:,iblk),  &
                      vsnon(:,:,:,iblk),  &
                      aice (:,:,  iblk),  &
                      trcr (:,:,:,iblk),  &
                      vice (:,:,  iblk),  &
                      vsno (:,:,  iblk),  &
                      aice0(:,:,  iblk),  &
                      tmask(:,:,  iblk),  &
                      max_ntrcr,          &
                      trcr_depend)

   enddo
   !$OMP END PARALLEL DO

   call ice_timer_stop(timer_da)

end subroutine ice_da_run

!=======================================================================

subroutine da_coin    (nx_block,            ny_block,      &
                       ilo, ihi,            jlo, jhi,      &
                       iglob,               jglob,         &
                       iblock,              jblock,        &
                       Tf,                  Tair,          &
                       Tmltz,               salinz,        &
                       tmask,               aice,          &
                       aice_obs,            aice_obs_err,  &
                       aicen,     vicen,    vsnon,         &
                       trcrn,     ntrcr)         

      use ice_blocks, only: nblocks_x, nblocks_y
      use ice_state, only: nt_Tsfc, nt_qice, nt_qsno, nt_sice, &
                           nt_fbri, tr_brine
      use ice_therm_mushy, only: enthalpy_mush
      use ice_therm_shared, only: ktherm

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
         intent(inout) ::    &
         tmask                 ! true for ice/ocean cells

      real (kind=dbl_kind), dimension(nx_block,ny_block,nilyr+1), & 
         intent(in) :: &
         salinz           ,  & ! initial salinity  profile (ppt)   
         Tmltz                 ! initial melting temperature (^oC)

      real (kind=dbl_kind), dimension (nx_block,ny_block), intent(in) :: &
         Tair              , & ! air temperature  (K)
         Tf                    ! freezing temperature (C) 

      real (kind=dbl_kind), dimension (nx_block,ny_block), &
         intent(in) :: &
         aice,               & ! model aggregate sic
         aice_obs,           & ! observed aggregate sic
         aice_obs_err          ! observed aggregate sic_err
         
      real (kind=dbl_kind), dimension (nx_block,ny_block,ncat), &
         intent(inout) :: &
         aicen , & ! concentration of ice
         vicen , & ! volume per unit area of ice  (m)
         vsnon     ! volume per unit area of snow (m)

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
         mod_err2,     & ! model error squre
         gain,         & ! Kalman gain, optimal estimated
         weight,       & ! nudging weight, increamental Kalman gain
         weightn,      & ! nudging weigth distributed among categories
         rda,          & ! dt/dT, where dT is observation time step
         radd            ! incremental ratio

      real (kind=dbl_kind) :: &
         slope,        & !
         Ti              !

      !-----------------------------------------------------------------
      ! assimilate sic on grid
      !-----------------------------------------------------------------

      rda = dt / Tobs
      if (my_task == master_task) write(nu_diag,*) 'rda = ', rda

      if (da_sic == .true.) then

         do j = 1, ny_block
         do i = 1, nx_block
            if (tmask(i,j)) then
               mod_err = aice(i,j) - aice_obs(i,j)
               mod_err2 = mod_err**2 + aice_obs_err(i,j)**2

               gain   = mod_err2 / (mod_err2 + puny + aice_obs_err(i,j)**2)
               if (da_insert) gain = c1

               weight = c1 - (c1 - gain)**rda
               if (corr_bias) then
                  weight = weight * exp(-(aice(i,j)+aice_obs(i,j)*2.5_dbl_kind))
               endif
            else
               weight = c0
            endif

            if (aice(i,j) > puny) then
               radd = c1 + weight * (aice_obs(i,j)/max(aice(i,j),p1) - c1)

               do n=1,ncat
                  aicen(i,j,n) = aicen(i,j,n) * radd
                  vicen(i,j,n) = vicen(i,j,n) * radd
                  vsnon(i,j,n) = vsnon(i,j,n) * radd
               enddo
            else
               if (tmask(i,j)) then
                  radd = weight * (aice_obs(i,j) - aice(i,j))
                  aicen(i,j,1) = aicen(i,j,1) + radd 
                  vicen(i,j,1) = vicen(i,j,1) + radd 

                  do n=1,ncat
                     trcrn(i,j,nt_Tsfc,n) = min(Tsmelt, Tair(i,j)-Tffresh) 
                     if (tr_brine) trcrn(i,j,nt_fbri,n) = c1

                     do k=1,nilyr
                        ! assume linear temp profile and compute enthalpy
                        slope = Tf(i,j) - trcrn(i,j,nt_Tsfc,n)
                        Ti = trcrn(i,j,nt_Tsfc,n) &
                           + slope*(real(k,kind=dbl_kind)-p5) &
                           / real(nilyr,kind=dbl_kind)

                        if (ktherm == 2) then
                           ! enthalpy
                           trcrn(i,j,nt_qice+k-1,n) = &
                              enthalpy_mush(Ti, c5)
                        else
                           trcrn(i,j,nt_qice+k-1,n) = &
                             - (rhoi * (cp_ice*(Tmltz(i,j,k)-Ti) &
                             + Lfresh*(c1-Tmltz(i,j,k)/Ti) &
                             - cp_ocn*Tmltz(i,j,k)))
                        endif
                      ! trcrn(i,j,nt_sice+k-1,n) = salinz(i,j,k)
                       trcrn(i,j,nt_sice+k-1,n) = c5
                     enddo
                  enddo
               endif
            endif
         enddo
         enddo
      endif

end subroutine da_coin

!=======================================================================

      end module ice_da

!=======================================================================
