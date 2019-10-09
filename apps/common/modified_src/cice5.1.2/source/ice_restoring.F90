!  SVN:$Id: ice_restoring.F90 825 2014-08-29 15:37:09Z eclare $
!=======================================================================
!
! Reads and interpolates forcing data for atmosphere and ocean quantities.
!
! authors: Elizabeth C. Hunke, LANL

      module ice_restoring

      use ice_kinds_mod
      use ice_blocks, only: nx_block, ny_block
      use ice_domain_size, only: ncat, max_blocks, max_ntrcr, nilyr
      use ice_forcing, only: trestore, trest, &
          aicen_bry, vicen_bry, vsnon_bry,    &
          Tsfc_bry, Tinz_bry, Sinz_bry, alvln_bry,vlvln_bry,&
          apondn_bry, hpondn_bry, ipondn_bry,iage_bry
      use ice_state, only: aicen, vicen, vsnon, trcrn, ntrcr, bound_state, &
                           aice_init, aice0, aice, vice, vsno, trcr, &
                           trcr_depend, tr_pond_lvl, nbtrcr
      use ice_timers, only: ice_timer_start, ice_timer_stop, timer_bound
      use ice_domain, only:sea_ice_time_bry

      implicit none
      private
      public :: ice_HaloRestore_init, ice_HaloRestore
      save

      logical (kind=log_kind), public :: &
         restore_ice                 ! restore ice state if true

      !-----------------------------------------------------------------
      ! state of the ice for each category
      !-----------------------------------------------------------------

      real (kind=dbl_kind), dimension (:,:,:,:), allocatable :: &
         aicen_rest , & ! concentration of ice
         vicen_rest , & ! volume per unit area of ice          (m)
         vsnon_rest     ! volume per unit area of snow         (m)

      real (kind=dbl_kind), dimension (:,:,:,:,:), allocatable :: &
         trcrn_rest     ! tracers

!=======================================================================

      contains

!=======================================================================

!  Allocates and initializes arrays needed for restoring the ice state 
!  in cells surrounding the grid.


 subroutine ice_HaloRestore_init

      use ice_blocks, only: block, get_block, nblocks_x, nblocks_y
      use ice_communicate, only: my_task, master_task
      use ice_constants, only: c0
      use ice_domain, only: ew_boundary_type, ns_boundary_type, &
          nblocks, blocks_ice
      use ice_fileunits, only: nu_diag
      use ice_grid, only: tmask
      use ice_flux, only: sst, Tf, Tair, salinz, Tmltz
      use ice_itd, only: aggregate
      use ice_restart_shared, only: restart_ext

   integer (int_kind) :: &
     i,j,iblk,nt,n,      &! dummy loop indices
     ilo,ihi,jlo,jhi,    &! beginning and end of physical domain
     iglob(nx_block),    &! global indices
     jglob(ny_block),    &! global indices
     iblock, jblock,     &! block indices
     ibc,                &! ghost cell column or row
     npad                 ! padding column/row counter

   character (len=7), parameter :: &
!     restore_ic = 'defined' ! otherwise restore to initial ice state
     restore_ic = 'initial' ! restore to initial ice state

   type (block) :: &
     this_block  ! block info for current block

   if (.not. restore_ice) return

   if (ew_boundary_type == 'open' .and. &
       ns_boundary_type == 'open' .and. .not.(restart_ext)) then
      if (my_task == master_task) write (nu_diag,*) &
            'WARNING: Setting restart_ext = T for open boundaries'
      restart_ext = .true.
   endif

   allocate (aicen_rest(nx_block,ny_block,ncat,max_blocks), &
             vicen_rest(nx_block,ny_block,ncat,max_blocks), &
             vsnon_rest(nx_block,ny_block,ncat,max_blocks), &
             trcrn_rest(nx_block,ny_block,ntrcr,ncat,max_blocks))

!-----------------------------------------------------------------------
! initialize
! halo cells have to be filled manually at this stage
! these arrays could be set to values read from a file...
!-----------------------------------------------------------------------

   if (trim(restore_ic) == 'defined') then

      ! restore to defined ice state
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

         call set_restore_var (nx_block,            ny_block,            &
                               ilo, ihi,            jlo, jhi,            &
                               iglob,               jglob,               &
                               iblock,              jblock,              &
                               Tair (:,:,    iblk), sst  (:,:,    iblk), &
                               Tf   (:,:,    iblk),                      &
                               salinz(:,:,:, iblk), Tmltz(:,:,:,  iblk), &
                               tmask(:,:,    iblk),                      &
                               aicen_rest(:,:,  :,iblk), &
                               trcrn_rest(:,:,:,:,iblk), ntrcr,         &
                               vicen_rest(:,:,  :,iblk), &
                               vsnon_rest(:,:,  :,iblk))
      enddo ! iblk
      !$OMP END PARALLEL DO

   else  ! restore_ic

   ! restore to initial ice state

! the easy way
!   aicen_rest(:,:,:,:) = aicen(:,:,:,:)
!   vicen_rest(:,:,:,:) = vicen(:,:,:,:)
!   vsnon_rest(:,:,:,:) = vsnon(:,:,:,:)
!   trcrn_rest(:,:,:,:,:) = trcrn(:,:,1:ntrcr,:,:)

! the more precise way
   !$OMP PARALLEL DO PRIVATE(iblk,ilo,ihi,jlo,jhi,this_block, &
   !$OMP                     i,j,n,nt,ibc,npad)
   do iblk = 1, nblocks
      this_block = get_block(blocks_ice(iblk),iblk)         
         ilo = this_block%ilo
         ihi = this_block%ihi
         jlo = this_block%jlo
         jhi = this_block%jhi

      if (this_block%iblock == 1) then              ! west edge
         if (trim(ew_boundary_type) /= 'cyclic') then
            do n = 1, ncat
            do j = 1, ny_block
            do i = 1, ilo
               aicen_rest(i,j,n,iblk) = aicen(ilo,j,n,iblk)
               vicen_rest(i,j,n,iblk) = vicen(ilo,j,n,iblk)
               vsnon_rest(i,j,n,iblk) = vsnon(ilo,j,n,iblk)
               do nt = 1, ntrcr
                  trcrn_rest(i,j,nt,n,iblk) = trcrn(ilo,j,nt,n,iblk)
               enddo
            enddo
            enddo
            enddo
         endif
      endif

      if (this_block%iblock == nblocks_x) then  ! east edge
         if (trim(ew_boundary_type) /= 'cyclic') then
            ! locate ghost cell column (avoid padding)
            ibc = nx_block
            do i = nx_block, 1, -1
               npad = 0
               if (this_block%i_glob(i) == 0) then
                  do j = 1, ny_block
                     npad = npad + this_block%j_glob(j)
                  enddo
               endif
               if (npad /= 0) ibc = ibc - 1
            enddo

            do n = 1, ncat
            do j = 1, ny_block
            do i = ihi, ibc
               aicen_rest(i,j,n,iblk) = aicen(ihi,j,n,iblk)
               vicen_rest(i,j,n,iblk) = vicen(ihi,j,n,iblk)
               vsnon_rest(i,j,n,iblk) = vsnon(ihi,j,n,iblk)
               do nt = 1, ntrcr
                  trcrn_rest(i,j,nt,n,iblk) = trcrn(ihi,j,nt,n,iblk)
               enddo
            enddo
            enddo
            enddo
         endif
      endif

      if (this_block%jblock == 1) then              ! south edge
         if (trim(ns_boundary_type) /= 'cyclic') then
            do n = 1, ncat
            do j = 1, jlo
            do i = 1, nx_block
               aicen_rest(i,j,n,iblk) = aicen(i,jlo,n,iblk)
               vicen_rest(i,j,n,iblk) = vicen(i,jlo,n,iblk)
               vsnon_rest(i,j,n,iblk) = vsnon(i,jlo,n,iblk)
               do nt = 1, ntrcr
                  trcrn_rest(i,j,nt,n,iblk) = trcrn(ilo,j,nt,n,iblk)
               enddo
            enddo
            enddo
            enddo
         endif
      endif

      if (this_block%jblock == nblocks_y) then  ! north edge
         if (trim(ns_boundary_type) /= 'cyclic' .and. &
             trim(ns_boundary_type) /= 'tripole' .and. &
             trim(ns_boundary_type) /= 'tripoleT') then
            ! locate ghost cell row (avoid padding)
            ibc = ny_block
            do j = ny_block, 1, -1
               npad = 0
               if (this_block%j_glob(j) == 0) then
                  do i = 1, nx_block
                     npad = npad + this_block%i_glob(i)
                  enddo
               endif
               if (npad /= 0) ibc = ibc - 1
            enddo

            do n = 1, ncat
            do j = jhi, ibc
            do i = 1, nx_block
               aicen_rest(i,j,n,iblk) = aicen(i,jhi,n,iblk)
               vicen_rest(i,j,n,iblk) = vicen(i,jhi,n,iblk)
               vsnon_rest(i,j,n,iblk) = vsnon(i,jhi,n,iblk)
               do nt = 1, ntrcr
                  trcrn_rest(i,j,nt,n,iblk) = trcrn(ihi,j,nt,n,iblk)
               enddo
            enddo
            enddo
            enddo
         endif
      endif

   enddo ! iblk
   !$OMP END PARALLEL DO

   endif ! restore_ic

   if (my_task == master_task) &
      write (nu_diag,*) 'ice restoring timescale = ',trestore,' days' 

 end subroutine ice_HaloRestore_init

!=======================================================================

! initialize restoring variables, based on set_state_var
! this routine assumes boundaries are not cyclic

    subroutine set_restore_var (nx_block, ny_block, &
                                ilo, ihi, jlo, jhi, &
                                iglob,    jglob,    &
                                iblock,   jblock,   &
                                Tair,     sst,      &
                                Tf,                 &
                                salinz,   Tmltz,    &
                                tmask,    aicen,    &
                                trcrn,    ntrcr,    &
                                vicen,    vsnon)

! authors: E. C. Hunke, LANL

      use ice_blocks, only: nblocks_x, nblocks_y
      use ice_constants, only: c0, c1, c2, p2, p5, rhoi, rhos, Lfresh, &
           cp_ice, cp_ocn, Tsmelt, Tffresh
      use ice_domain_size, only: nilyr, nslyr, ncat
      use ice_state, only: nt_Tsfc, nt_qice, nt_qsno, nt_sice, nt_fbri, tr_brine
      use ice_itd, only: hin_max
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

      real (kind=dbl_kind), dimension (nx_block,ny_block), intent(in) :: &
         Tair    , & ! air temperature  (K)
         Tf      , & ! freezing temperature (C) 
         sst         ! sea surface temperature (C) ! currently not used

      real (kind=dbl_kind), dimension (nx_block,ny_block,nilyr), &
         intent(in) :: &
         salinz  , & ! initial salinity profile
         Tmltz       ! initial melting temperature profile

      logical (kind=log_kind), dimension (nx_block,ny_block), &
         intent(in) :: &
         tmask      ! true for ice/ocean cells

      real (kind=dbl_kind), dimension (nx_block,ny_block,ncat), &
         intent(out) :: &
         aicen , & ! concentration of ice
         vicen , & ! volume per unit area of ice          (m)
         vsnon     ! volume per unit area of snow         (m)

      real (kind=dbl_kind), dimension (nx_block,ny_block,ntrcr,ncat), &
         intent(out) :: &
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
         slope, Ti, hbar, &
         ainit(ncat), &  ! initial ice concentration
         hinit(ncat), &  ! initial ice thickness
         hsno_init       ! initial snow thickness

      indxi(:) = 0
      indxj(:) = 0

      !-----------------------------------------------------------------
      ! Initialize restoring variables everywhere on grid
      !-----------------------------------------------------------------

      do n = 1, ncat
         do j = 1, ny_block
         do i = 1, nx_block
            aicen(i,j,n) = c0
            vicen(i,j,n) = c0
            vsnon(i,j,n) = c0
            trcrn(i,j,nt_Tsfc,n) = Tf(i,j)  ! surface temperature 
            if (ntrcr >= 2) then
               do it = 2, ntrcr
                  trcrn(i,j,it,n) = c0
               enddo
            endif
            if (tr_brine) trcrn(i,j,nt_fbri,n) = c1
         enddo
         enddo
      enddo

      !-----------------------------------------------------------------
      ! initial area and thickness in ice-occupied restoring cells
      !-----------------------------------------------------------------

      hbar = c2  ! initial ice thickness
      hsno_init = 0.20_dbl_kind ! initial snow thickness (m)
      do n = 1, ncat
         hinit(n) = c0
         ainit(n) = c0
         if (hbar > hin_max(n-1) .and. hbar < hin_max(n)) then
            hinit(n) = hbar
            ainit(n) = 0.95_dbl_kind ! initial ice concentration
         endif
      enddo

      !-----------------------------------------------------------------
      ! Define cells where ice is placed (or other values are used)
      ! Edges using initial values (zero, above) are commented out
      !-----------------------------------------------------------------

      icells = 0
      if (iblock == 1) then              ! west edge
            do j = 1, ny_block
            do i = 1, ilo
               if (tmask(i,j)) then
!               icells = icells + 1
!               indxi(icells) = i
!               indxj(icells) = j
               endif
            enddo
            enddo
      endif

      if (iblock == nblocks_x) then      ! east edge
            ! locate ghost cell column (avoid padding)
            ibc = nx_block
            do i = nx_block, 1, -1
               npad = 0
               if (iglob(i) == 0) then
                  do j = 1, ny_block
                     npad = npad + jglob(j)
                  enddo
               endif
               if (npad /= 0) ibc = ibc - 1
            enddo

            do j = 1, ny_block
            do i = ihi, ibc
               if (tmask(i,j)) then
               icells = icells + 1
               indxi(icells) = i
               indxj(icells) = j
               endif
            enddo
            enddo
      endif

      if (jblock == 1) then              ! south edge
            do j = 1, jlo
            do i = 1, nx_block
               if (tmask(i,j)) then
!               icells = icells + 1
!               indxi(icells) = i
!               indxj(icells) = j
               endif
            enddo
            enddo
      endif

      if (jblock == nblocks_y) then      ! north edge
            ! locate ghost cell row (avoid padding)
            ibc = ny_block
            do j = ny_block, 1, -1
               npad = 0
               if (jglob(j) == 0) then
                  do i = 1, nx_block
                     npad = npad + iglob(i)
                  enddo
               endif
               if (npad /= 0) ibc = ibc - 1
            enddo

            do j = jhi, ibc
            do i = 1, nx_block
               if (tmask(i,j)) then
!               icells = icells + 1
!               indxi(icells) = i
!               indxj(icells) = j
               endif
            enddo
            enddo
      endif

      !-----------------------------------------------------------------
      ! Set restoring variables
      !-----------------------------------------------------------------

         do n = 1, ncat

            ! ice volume, snow volume
!DIR$ CONCURRENT !Cray
!cdir nodep      !NEC
!ocl novrec      !Fujitsu
            do ij = 1, icells
               i = indxi(ij)
               j = indxj(ij)

               aicen(i,j,n) = ainit(n)
               vicen(i,j,n) = hinit(n) * ainit(n) ! m
               vsnon(i,j,n) = min(aicen(i,j,n)*hsno_init,p2*vicen(i,j,n))
            enddo               ! ij

               ! surface temperature
               do ij = 1, icells
                  i = indxi(ij)
                  j = indxj(ij)
                  trcrn(i,j,nt_Tsfc,n) = min(Tsmelt, Tair(i,j) - Tffresh) !deg C
               enddo

               ! ice enthalpy, salinity 
               do k = 1, nilyr
                  do ij = 1, icells
                     i = indxi(ij)
                     j = indxj(ij)

                     ! assume linear temp profile and compute enthalpy
                     slope = Tf(i,j) - trcrn(i,j,nt_Tsfc,n)
                     Ti = trcrn(i,j,nt_Tsfc,n) &
                        + slope*(real(k,kind=dbl_kind)-p5) &
                                /real(nilyr,kind=dbl_kind)

                     if (ktherm == 2) then
                        ! enthalpy
                        trcrn(i,j,nt_qice+k-1,n) = &
                             enthalpy_mush(Ti, salinz(i,j,k))
                     else
                        trcrn(i,j,nt_qice+k-1,n) = &
                            -(rhoi * (cp_ice*(Tmltz(i,j,k)-Ti) &
                            + Lfresh*(c1-Tmltz(i,j,k)/Ti) - cp_ocn*Tmltz(i,j,k)))
                     endif

                     ! salinity
                     trcrn(i,j,nt_sice+k-1,n) = salinz(i,j,k)
                  enddo            ! ij
               enddo               ! nilyr

               ! snow enthalpy
               do k = 1, nslyr
                  do ij = 1, icells
                     i = indxi(ij)
                     j = indxj(ij)
                     Ti = min(c0, trcrn(i,j,nt_Tsfc,n))
                     trcrn(i,j,nt_qsno+k-1,n) = -rhos*(Lfresh - cp_ice*Ti)
                     
                  enddo            ! ij
               enddo               ! nslyr

         enddo                  ! ncat

   end subroutine set_restore_var

!=======================================================================

!  This subroutine is intended for restoring the ice state to desired
!  values in cells surrounding the grid.
!  Note: This routine will need to be modified for nghost > 1.
!        We assume padding occurs only on east and north edges.

 subroutine ice_HaloRestore
       
      use ice_domain_size, only: nilyr, nslyr, ncat
      use ice_blocks, only: block, get_block, nblocks_x, nblocks_y
      use ice_calendar, only: dt
      use ice_constants, only: c0, secday, c1, rhoi, rhos, Lfresh, &
           cp_ice, cp_ocn, Tsmelt, Tffresh, p5
      use ice_domain, only: ew_boundary_type, ns_boundary_type, &
          nblocks, blocks_ice
      use ice_communicate, only: my_task, master_task
      use ice_fileunits, only: nu_diag
      use ice_state, only: nt_Tsfc, nt_qice, nt_qsno, nt_sice, &  
          nt_fbri, tr_brine, nt_vlvl, nt_alvl, nt_iage, &
          nt_apnd, nt_hpnd, nt_ipnd 
      use ice_therm_shared, only: ktherm
      use ice_therm_mushy, only: enthalpy_mush
      use ice_flux, only: Tmltz,Tf
!-----------------------------------------------------------------------
!
!  local variables
!
!-----------------------------------------------------------------------

   integer (int_kind) :: &
     i,j,iblk,nt,n,k,    &! dummy loop indices
     ilo,ihi,jlo,jhi,    &! beginning and end of physical domain
     ibc,                &! ghost cell column or row
     npad                 ! padding column/row counter

   type (block) :: &
     this_block  ! block info for current block

   real (dbl_kind) :: &
     ctime, &               ! dt/trest
     slope,Ti

   call ice_timer_start(timer_bound)

!-----------------------------------------------------------------------
!
!  Initialize
!
!-----------------------------------------------------------------------

      ! for now, use same restoring constant as for SST
      if (trestore == 0) then
         trest = dt          ! use data instantaneously
      else
         trest = real(trestore,kind=dbl_kind) * secday ! seconds
      endif
      ctime = dt/trest

!-----------------------------------------------------------------------
!
!  Restore values in cells surrounding the grid
!
!-----------------------------------------------------------------------

   !$OMP PARALLEL DO PRIVATE(iblk,ilo,ihi,jlo,jhi,this_block, &
   !$OMP                     i,j,n,nt,ibc,npad)
   do iblk = 1, nblocks
      this_block = get_block(blocks_ice(iblk),iblk)         
         ilo = this_block%ilo
         ihi = this_block%ihi
         jlo = this_block%jlo
         jhi = this_block%jhi

      if (this_block%iblock == 1) then              ! west edge
         if (trim(ew_boundary_type) /= 'cyclic') then
            do n = 1, ncat
            do j = 1, ny_block
            do i = 1, ilo
               if (sea_ice_time_bry) then
                  !if ((j == ny_block).and.(my_task == master_task )) then
                  !   write (nu_diag,*) 'Block west = ',iblk
                  !   write (nu_diag,*) 'n = ',n,' j = ',j
                  !   write (nu_diag,*)'aicen = ',aicen_bry(1,j,n,iblk)
                  !   write (nu_diag,*)'vicen = ',vicen_bry(1,j,n,iblk)
                  !   write (nu_diag,*)'vsnon = ',vsnon_bry(1,j,n,iblk) 
                  !endif
                  aicen_rest(i,j,n,iblk) = aicen_bry(1,j,n,iblk)
                  vicen_rest(i,j,n,iblk) = vicen_bry(1,j,n,iblk)
                  vsnon_rest(i,j,n,iblk) = vsnon_bry(1,j,n,iblk) 
                  trcrn_rest(i,j,nt_Tsfc,n,iblk) = Tsfc_bry(1,j,n,iblk)  
                  trcrn_rest(i,j,nt_alvl,n,iblk) = alvln_bry(1,j,n,iblk)
                  trcrn_rest(i,j,nt_vlvl,n,iblk) = vlvln_bry(1,j,n,iblk) 
                  trcrn_rest(i,j,nt_iage,n,iblk) = iage_bry(1,j,n,iblk) 
                  if (tr_pond_lvl) then
                     trcrn_rest(i,j,nt_apnd,n,iblk) = apondn_bry(1,j,n,iblk) 
                     trcrn_rest(i,j,nt_hpnd,n,iblk) = hpondn_bry(1,j,n,iblk)
                     trcrn_rest(i,j,nt_ipnd,n,iblk) = ipondn_bry(1,j,n,iblk)  
                  endif
                  do k = 1,nilyr
                     trcrn_rest(i,j,nt_sice+k-1,n,iblk) = Sinz_bry(1,j,k,n,iblk)
                     slope = Tf(i,j,iblk) - Tsfc_bry(1,j,n,iblk)
                     Ti = Tsfc_bry(1,j,n,iblk) &
                        + slope*(real(k,kind=dbl_kind)-p5) &
                                /real(nilyr,kind=dbl_kind)   
                     if (ktherm == 2) then
                        ! enthalpy
                        trcrn_rest(i,j,nt_qice+k-1,n,iblk) = &
                          ! enthalpy_mush(Ti,& 
                          enthalpy_mush(Tinz_bry(1,j,k,n,iblk), &
                                      Sinz_bry(1,j,k,n,iblk))
                     else
                        trcrn_rest(i,j,nt_qice+k-1,n,iblk) = &
                            -(rhoi * (cp_ice*(Tmltz(1,j,k,iblk)-&
                              Tinz_bry(1,j,k,n,iblk)) &
                            + Lfresh*(c1-Tmltz(1,j,k,iblk)/ &
                              Tinz_bry(1,j,k,n,iblk)) &
                              - cp_ocn*Tmltz(1,j,k,iblk)))
                     endif
                  enddo
                  do k = 1, nslyr  
                     trcrn_rest(i,j,nt_qsno+k-1,n,iblk) = -rhos*(Lfresh - &
                       cp_ice * min(c0,trcrn_rest(i,j,nt_Tsfc,n,iblk)))
                  enddo
               endif
               aicen(i,j,n,iblk) = aicen(i,j,n,iblk) &
                  + (aicen_rest(i,j,n,iblk)-aicen(i,j,n,iblk))*ctime
               vicen(i,j,n,iblk) = vicen(i,j,n,iblk) &
                  + (vicen_rest(i,j,n,iblk)-vicen(i,j,n,iblk))*ctime
               vsnon(i,j,n,iblk) = vsnon(i,j,n,iblk) &
                  + (vsnon_rest(i,j,n,iblk)-vsnon(i,j,n,iblk))*ctime
               do nt = 1, ntrcr-nbtrcr 
                  if  ((sea_ice_time_bry).and.((nt == nt_qice).or. &
                     (nt == nt_sice))) then
                     do k = 1,nilyr
                        trcrn(i,j,nt+k-1,n,iblk) = &
                              trcrn(i,j,nt+k-1,n,iblk) &
                              + (trcrn_rest(i,j,nt+k-1,n,iblk)- &
                                 trcrn(i,j,nt+k-1,n,iblk)) &
                              *ctime
                     enddo
                  else if ((sea_ice_time_bry).and.(nt == nt_qsno)) then  
                     do k = 1,nslyr
                        trcrn(i,j,nt+k-1,n,iblk) = &
                              trcrn(i,j,nt+k-1,n,iblk) &
                              + (trcrn_rest(i,j,nt+k-1,n,iblk)- &
                                 trcrn(i,j,nt+k-1,n,iblk)) &
                              *ctime
                     enddo 
                  else 
                        trcrn(i,j,nt,n,iblk) = trcrn(i,j,nt,n,iblk) &
                        + (trcrn_rest(i,j,nt,n,iblk)-trcrn(i,j,nt,n,iblk))&
                        *ctime
                  endif 
               enddo
            enddo
            enddo
            enddo
         endif
      endif

      if (this_block%iblock == nblocks_x) then  ! east edge
         if (trim(ew_boundary_type) /= 'cyclic') then
            ! locate ghost cell column (avoid padding)
            ibc = nx_block
            do i = nx_block, 1, -1
               npad = 0
               if (this_block%i_glob(i) == 0) then
                  do j = 1, ny_block
                     npad = npad + this_block%j_glob(j)
                  enddo
               endif
               if (npad /= 0) ibc = ibc - 1
            enddo

            do n = 1, ncat
            do j = 1, ny_block
            do i = ihi, ibc
               if (sea_ice_time_bry) then
                  !if (j == ny_block) then
                  ! write (nu_diag,*) 'Block east = ',iblk
                  ! write (nu_diag,*) 'nblocks_y= ',nblocks_y
                  ! write (nu_diag,*) 'nblocks_x= ',nblocks_x  
                  ! write (nu_diag,*) 'nblocks= ',nblocks
                  ! write (nu_diag,*) 'ctime= ',ctime
                  ! write (nu_diag,*) 'n = ',n,' i = ',j
                  ! write (nu_diag,*)'aicen=',aicen_bry(nx_block,j,n,iblk)
                  ! write (nu_diag,*)'vicen=',vicen_bry(nx_block,j,n,iblk)
                  ! write (nu_diag,*)'vsnon=',vsnon_bry(nx_block,j,n,iblk) 
                  !endif
                  aicen_rest(i,j,n,iblk) = aicen_bry(ibc,j,n,iblk)
                  vicen_rest(i,j,n,iblk) = vicen_bry(ibc,j,n,iblk)
                  vsnon_rest(i,j,n,iblk) = vsnon_bry(ibc,j,n,iblk)   
                  trcrn_rest(i,j,nt_Tsfc,n,iblk) = Tsfc_bry(ibc,j,n,iblk)  
                  trcrn_rest(i,j,nt_alvl,n,iblk) = alvln_bry(ibc,j,n,iblk)
                  trcrn_rest(i,j,nt_vlvl,n,iblk) = vlvln_bry(ibc,j,n,iblk) 
                  trcrn_rest(i,j,nt_iage,n,iblk) = iage_bry(ibc,j,n,iblk) 
                  if (tr_pond_lvl) then
                     trcrn_rest(i,j,nt_apnd,n,iblk) = apondn_bry(ibc,j,n,iblk) 
                     trcrn_rest(i,j,nt_hpnd,n,iblk) = hpondn_bry(ibc,j,n,iblk)
                     trcrn_rest(i,j,nt_ipnd,n,iblk) = ipondn_bry(ibc,j,n,iblk)  
                  endif 
                  do k = 1,nilyr
                     trcrn_rest(i,j,nt_sice+k-1,n,iblk) = Sinz_bry(ibc,j,k,n,iblk)  
                     slope = Tf(i,j,iblk) - Tsfc_bry(ibc,j,n,iblk)
                     Ti = Tsfc_bry(ibc,j,n,iblk) &
                        + slope*(real(k,kind=dbl_kind)-p5) &
                                /real(nilyr,kind=dbl_kind)   
                     if (ktherm == 2) then
                        ! enthalpy
                        trcrn_rest(i,j,nt_qice+k-1,n,iblk) = &
                         !enthalpy_mush(Ti, &
                         enthalpy_mush(Tinz_bry(ibc,j,k,n,iblk), &
                                       Sinz_bry(ibc,j,k,n,iblk))
                     else
                        trcrn_rest(i,j,nt_qice+k-1,n,iblk) = &
                            -(rhoi * (cp_ice*(Tmltz(ibc,j,k,iblk)-&
                              Tinz_bry(ibc,j,k,n,iblk)) &
                            + Lfresh*(c1-Tmltz(ibc,j,k,iblk)/ &
                              Tinz_bry(ibc,j,k,n,iblk)) - &
                              cp_ocn*Tmltz(ibc,j,k,iblk)))
                     endif
                  enddo
                  do k = 1,nslyr  
                      trcrn_rest(i,j,nt_qsno+k-1,n,iblk) = -rhos*(Lfresh - &
                       cp_ice * min(c0,trcrn_rest(i,j,nt_Tsfc,n,iblk))) 
                  enddo
               endif
               aicen(i,j,n,iblk) = aicen(i,j,n,iblk) &
                  + (aicen_rest(i,j,n,iblk)-aicen(i,j,n,iblk))*ctime
               vicen(i,j,n,iblk) = vicen(i,j,n,iblk) &
                  + (vicen_rest(i,j,n,iblk)-vicen(i,j,n,iblk))*ctime
               vsnon(i,j,n,iblk) = vsnon(i,j,n,iblk) &
                  + (vsnon_rest(i,j,n,iblk)-vsnon(i,j,n,iblk))*ctime
               do nt = 1, ntrcr-nbtrcr
                  if  ((sea_ice_time_bry).and.((nt == nt_qice).or. &
                      (nt == nt_sice))) then
                     do k = 1,nilyr
                           trcrn(i,j,nt+k-1,n,iblk) = &
                              trcrn(i,j,nt+k-1,n,iblk) &
                              + (trcrn_rest(i,j,nt+k-1,n,iblk)- &
                                 trcrn(i,j,nt+k-1,n,iblk)) &
                              *ctime
                     enddo
                  else if  ((sea_ice_time_bry).and.(nt == nt_qsno)) then
                     do k = 1,nslyr
                           trcrn(i,j,nt+k-1,n,iblk) = &
                              trcrn(i,j,nt+k-1,n,iblk) &
                              + (trcrn_rest(i,j,nt+k-1,n,iblk)- &
                                 trcrn(i,j,nt+k-1,n,iblk)) &
                              *ctime
                     enddo 
                  else 
                     trcrn(i,j,nt,n,iblk) = trcrn(i,j,nt,n,iblk) &
                        + (trcrn_rest(i,j,nt,n,iblk)-trcrn(i,j,nt,n,iblk))&
                        *ctime
                  endif 
               enddo
            enddo
            enddo
            enddo
         endif
      endif

      if (this_block%jblock == 1) then              ! south edge
         if (trim(ns_boundary_type) /= 'cyclic') then
            do n = 1, ncat
            do j = 1, jlo
            do i = 1, nx_block
               if (sea_ice_time_bry) then
                  aicen_rest(i,j,n,iblk) = aicen_bry(i,1,n,iblk)
                  vicen_rest(i,j,n,iblk) = vicen_bry(i,1,n,iblk)
                  vsnon_rest(i,j,n,iblk) = vsnon_bry(i,1,n,iblk) 
                  trcrn_rest(i,j,nt_Tsfc,n,iblk) = Tsfc_bry(i,1,n,iblk)
                  trcrn_rest(i,j,nt_alvl,n,iblk) = alvln_bry(i,1,n,iblk)
                  trcrn_rest(i,j,nt_vlvl,n,iblk) = vlvln_bry(i,1,n,iblk) 
                  trcrn_rest(i,j,nt_iage,n,iblk) = iage_bry(i,1,n,iblk) 
                  if (tr_pond_lvl) then
                     trcrn_rest(i,j,nt_apnd,n,iblk) = apondn_bry(i,1,n,iblk) 
                     trcrn_rest(i,j,nt_hpnd,n,iblk) = hpondn_bry(i,1,n,iblk)
                     trcrn_rest(i,j,nt_ipnd,n,iblk) = ipondn_bry(i,1,n,iblk)  
                  endif    
                  do k = 1,nilyr
                     trcrn_rest(i,j,nt_sice+k-1,n,iblk) = Sinz_bry(i,1,k,n,iblk) 
                     slope = Tf(i,j,iblk) - Tsfc_bry(i,1,n,iblk)
                     Ti = Tsfc_bry(i,1,n,iblk) &
                        + slope*(real(k,kind=dbl_kind)-p5) &
                                /real(nilyr,kind=dbl_kind)     
                     if (ktherm == 2) then
                        ! enthalpy
                        trcrn_rest(i,j,nt_qice+k-1,n,iblk) = &
                         !enthalpy_mush(Ti, &
                         enthalpy_mush(Tinz_bry(i,1,k,n,iblk), &
                                       Sinz_bry(i,1,k,n,iblk))
                     else
                        trcrn_rest(i,j,nt_qice+k-1,n,iblk) = &
                            -(rhoi * (cp_ice*(Tmltz(i,1,k,iblk)-&
                              Tinz_bry(i,1,k,n,iblk)) &
                            + Lfresh*(c1-Tmltz(i,1,k,iblk)/ &
                              Tinz_bry(i,1,k,n,iblk)) - &
                              cp_ocn*Tmltz(i,1,k,iblk)))
                     endif
                   enddo
                   do k = 1,nslyr  
                        trcrn_rest(i,j,nt_qsno+k-1,n,iblk) = -rhos*(Lfresh - &
                           cp_ice * min(c0,trcrn_rest(i,j,nt_Tsfc,n,iblk))) 
                  enddo
               endif  
               aicen(i,j,n,iblk) = aicen(i,j,n,iblk) &
                  + (aicen_rest(i,j,n,iblk)-aicen(i,j,n,iblk))*ctime
               vicen(i,j,n,iblk) = vicen(i,j,n,iblk) &
                  + (vicen_rest(i,j,n,iblk)-vicen(i,j,n,iblk))*ctime
               vsnon(i,j,n,iblk) = vsnon(i,j,n,iblk) &
                  + (vsnon_rest(i,j,n,iblk)-vsnon(i,j,n,iblk))*ctime
               do nt = 1, ntrcr-nbtrcr
                  if  ((sea_ice_time_bry).and.((nt == nt_qice).or. &
                      (nt == nt_sice))) then
                     do k = 1,nilyr
                           trcrn(i,j,nt+k-1,n,iblk) = &
                              trcrn(i,j,nt+k-1,n,iblk) &
                              + (trcrn_rest(i,j,nt+k-1,n,iblk)- &
                                 trcrn(i,j,nt+k-1,n,iblk)) &
                              *ctime
                     enddo
                  else if ((sea_ice_time_bry).and.(nt == nt_qsno)) then
                     do k = 1,nslyr
                           trcrn(i,j,nt+k-1,n,iblk) = &
                              trcrn(i,j,nt+k-1,n,iblk) &
                              + (trcrn_rest(i,j,nt+k-1,n,iblk)- &
                                 trcrn(i,j,nt+k-1,n,iblk)) &
                              *ctime
                     enddo 
                  else 
                     trcrn(i,j,nt,n,iblk) = trcrn(i,j,nt,n,iblk) &
                        + (trcrn_rest(i,j,nt,n,iblk)-trcrn(i,j,nt,n,iblk))&
                        *ctime
                  endif 
               enddo
            enddo
            enddo
            enddo
         endif
      endif

      if (this_block%jblock == nblocks_y) then  ! north edge
         if (trim(ns_boundary_type) /= 'cyclic' .and. &
             trim(ns_boundary_type) /= 'tripole' .and. &
             trim(ns_boundary_type) /= 'tripoleT') then
            ! locate ghost cell row (avoid padding)
            ibc = ny_block
            do j = ny_block, 1, -1
               npad = 0
               if (this_block%j_glob(j) == 0) then
                  do i = 1, nx_block
                     npad = npad + this_block%i_glob(i)
                  enddo
               endif
               if (npad /= 0) ibc = ibc - 1
            enddo
            do n = 1, ncat
            do j = jhi, ibc
            do i = 1, nx_block
               if (sea_ice_time_bry) then
                  !if ((i == nx_block).and.(my_task == master_task )) then  
                  aicen_rest(i,j,n,iblk) = aicen_bry(i,ibc,n,iblk)
                  vicen_rest(i,j,n,iblk) = vicen_bry(i,ibc,n,iblk)
                  vsnon_rest(i,j,n,iblk) = vsnon_bry(i,ibc,n,iblk) 
                  trcrn_rest(i,j,nt_Tsfc,n,iblk) = Tsfc_bry(i,ibc,n,iblk)   
                  trcrn_rest(i,j,nt_alvl,n,iblk) = alvln_bry(i,ibc,n,iblk)
                  trcrn_rest(i,j,nt_vlvl,n,iblk) = vlvln_bry(i,ibc,n,iblk) 
                  trcrn_rest(i,j,nt_iage,n,iblk) = iage_bry(i,ibc,n,iblk) 
                  if (tr_pond_lvl) then
                     trcrn_rest(i,j,nt_apnd,n,iblk) = apondn_bry(i,ibc,n,iblk) 
                     trcrn_rest(i,j,nt_hpnd,n,iblk) = hpondn_bry(i,ibc,n,iblk)
                     trcrn_rest(i,j,nt_ipnd,n,iblk) = ipondn_bry(i,ibc,n,iblk)  
                  endif    
                  do k = 1,nilyr
                     trcrn_rest(i,j,nt_sice+k-1,n,iblk) = Sinz_bry(i,ibc,k,n,iblk) 
                     slope = Tf(i,j,iblk) - Tsfc_bry(i,ibc,n,iblk)
                     Ti = Tsfc_bry(i,ibc,n,iblk) &
                        + slope*(real(k,kind=dbl_kind)-p5) &
                                /real(nilyr,kind=dbl_kind)       
                     if (ktherm == 2) then
                        ! enthalpy
                        trcrn_rest(i,j,nt_qice+k-1,n,iblk) = &
                         ! enthalpy_mush(Ti, & 
                         enthalpy_mush(Tinz_bry(i,ibc,k,n,iblk), &
                                       Sinz_bry(i,ibc,k,n,iblk))
                     else
                        trcrn_rest(i,j,nt_qice+k-1,n,iblk) = &
                            -(rhoi * (cp_ice*(Tmltz(i,ibc,k,iblk)-&
                              Tinz_bry(i,ibc,k,n,iblk)) &
                            + Lfresh*(c1-Tmltz(i,ibc,k,iblk)/ &
                              Tinz_bry(i,ibc,k,n,iblk)) - &
                              cp_ocn*Tmltz(i,ibc,k,iblk)))
                     endif
                  enddo  
                  do k = 1,nslyr  
                     trcrn_rest(i,j,nt_qsno+k-1,n,iblk) = -rhos*(Lfresh - &
                           cp_ice * min(c0,trcrn_rest(i,j,nt_Tsfc,n,iblk))) 
                  enddo
               endif 
               !if ((i == nx_block-2).and.(n == 2)) then
                  ! write (nu_diag,*) 'Block north = ',iblk
                  ! write (nu_diag,*) 'nblocks_y= ',nblocks_y
                  ! write (nu_diag,*) 'nblocks_x= ',nblocks_x  
                  ! write (nu_diag,*) 'nblocks= ',nblocks
                  ! write (nu_diag,*) 'ctime= ',ctime
                  ! write (nu_diag,*) 'n = ',n,' j = ',j
                  ! write (nu_diag,*) 'npad = ',npad,' ibd =',ibc
                  !write (nu_diag,*)'aicen_rest =',aicen_bry(i,ibc,n,iblk)                
                  !write (nu_diag,*)'vicen_rest =',vicen_bry(i,ibc,n,iblk)                   
                  !write (nu_diag,*)'vsnon_rest =',vsnon_bry(i,ibc,n,iblk)
                  !write (nu_diag,*)'Tinz_bry =',Tinz_bry(i,ibc,:,n,iblk) 
                  !write (nu_diag,*)'Sinz_bry =',Sinz_bry(i,ibc,:,n,iblk)          
                !endif
               aicen(i,j,n,iblk) = aicen(i,j,n,iblk) &
                  + (aicen_rest(i,j,n,iblk)-aicen(i,j,n,iblk))*ctime
               vicen(i,j,n,iblk) = vicen(i,j,n,iblk) &                  
                  + (vicen_rest(i,j,n,iblk)-vicen(i,j,n,iblk))*ctime
               vsnon(i,j,n,iblk) = vsnon(i,j,n,iblk) &
                  + (vsnon_rest(i,j,n,iblk)-vsnon(i,j,n,iblk))*ctime
               !if ((i == nx_block).and.(n == 2)) then
               !    write (nu_diag,*)'aicen =', aicen(i,ibc,n,iblk) 
               !    write (nu_diag,*)'vicen =', vicen(i,ibc,n,iblk)
               !    write (nu_diag,*)'vsnon =', vsnon(i,ibc,n,iblk)
               !    write (nu_diag,*) 'ctime =',ctime 
               !endif
               do nt = 1, ntrcr-nbtrcr
                  if  ((sea_ice_time_bry).and.((nt == nt_qice).or. &
                      (nt == nt_sice))) then
                     do k = 1,nilyr
                           trcrn(i,j,nt+k-1,n,iblk) = &
                              trcrn(i,j,nt+k-1,n,iblk) &
                              + (trcrn_rest(i,j,nt+k-1,n,iblk)- &
                                 trcrn(i,j,nt+k-1,n,iblk)) &
                              *ctime
                     enddo
                  else if ((sea_ice_time_bry).and.(nt == nt_qsno)) then
                     do k = 1,nslyr
                           trcrn(i,j,nt+k-1,n,iblk) = &
                              trcrn(i,j,nt+k-1,n,iblk) &
                              + (trcrn_rest(i,j,nt+k-1,n,iblk)- &
                                 trcrn(i,j,nt+k-1,n,iblk)) &
                              *ctime
                     enddo 
                  else 
                     trcrn(i,j,nt,n,iblk) = trcrn(i,j,nt,n,iblk) &
                        + (trcrn_rest(i,j,nt,n,iblk)-trcrn(i,j,nt,n,iblk))&
                        *ctime
                  endif 
               enddo
            enddo
            enddo
            enddo
         endif
      endif

   enddo ! iblk
   !$OMP END PARALLEL DO

   call ice_timer_stop(timer_bound)

 end subroutine ice_HaloRestore

!=======================================================================

      end module ice_restoring

!=======================================================================
