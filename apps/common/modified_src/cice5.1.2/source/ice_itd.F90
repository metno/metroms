!  SVN:$Id: ice_itd.F90 936 2015-03-17 15:46:44Z eclare $
!=======================================================================

! Routines to initialize the ice thickness distribution and
! utilities to redistribute ice among categories. These routines
! are not specific to a particular numerical implementation.
!
! See Bitz, C.M., and W.H. Lipscomb, 1999:
! An energy-conserving thermodynamic model of sea ice,
! J. Geophys. Res., 104, 15,669--15,677.
!
! See Bitz, C.M., M.M. Holland, A.J. Weaver, M. Eby, 2001:
! Simulating the ice-thickness distribution in a climate model,
! J. Geophys. Res., 106, 2441--2464.
!
! authors: C. M. Bitz, UW
!          William H. Lipscomb and Elizabeth C. Hunke, LANL
!
! 2003: Vectorized by Clifford Chen (Fujitsu) and William Lipscomb
!
! 2004 WHL: Added multiple snow layers, block structure, cleanup_itd
! 2006 ECH: Added WMO standard ice thickness categories as kcatbound=2
!           Streamlined for efficiency 
!           Converted to free source form (F90)

      module ice_itd

      use ice_kinds_mod
      use ice_constants
      use ice_communicate, only: my_task, master_task
      use ice_domain_size, only: ncat, max_aero, nilyr, nslyr, n_aero, nblyr
      use ice_fileunits, only: nu_diag

      implicit none
      save

      private
      public :: aggregate_area, shift_ice, column_sum, column_conservation_check, &
                aggregate, compute_tracers, init_itd, cleanup_itd, reduce_area

      integer (kind=int_kind), public :: &
         kitd        , & ! type of itd conversions
                         !   0 = delta function
                         !   1 = linear remap
         kcatbound       !   0 = old category boundary formula
                         !   1 = new formula giving round numbers
                         !   2 = WMO standard

      real (kind=dbl_kind), public :: &
         hi_min          ! minimum ice thickness allowed (m)

      real (kind=dbl_kind), public :: &
         hin_max(0:ncat) ! category limits (m)

      character (len=35) :: c_hi_range(ncat)

!-------------------------------------------------------------------
! a note regarding hi_min and hin_max(0):
! both represent a minimum ice thickness.  hin_max(0) is
! intended to be used for particular numerical implementations
! of category conversions in the ice thickness distribution.
! hi_min is a more general purpose parameter, but is specifically
! for maintaining stability in the thermodynamics.
! hin_max(0) = 0.1 m for the delta function itd
! hin_max(0) = 0.0 m for linear remapping
!
! Also note that the upper limit on the thickest category
! is only used for the linear remapping scheme
! and it is not a true upper limit on the thickness
!-------------------------------------------------------------------

!=======================================================================

      contains

!=======================================================================

! Initialize area fraction and thickness boundaries for the itd model
!
! authors: William H. Lipscomb and Elizabeth C. Hunke, LANL
!          C. M. Bitz, UW

      subroutine init_itd

      integer (kind=int_kind) :: &
           n    ! thickness category index

      real (kind=dbl_kind) :: &
           cc1, cc2, cc3, & ! parameters for kcatbound = 0
           x1           , &
           rn           , & ! real(n)
           rncat        , & ! real(ncat)
           d1           , & ! parameters for kcatbound = 1 (m)
           d2

      real (kind=dbl_kind), dimension(5) :: wmo5 ! data for wmo itd
      real (kind=dbl_kind), dimension(6) :: wmo6 ! data for wmo itd
      real (kind=dbl_kind), dimension(7) :: wmo7 ! data for wmo itd

      character(len=8) :: c_hinmax1,c_hinmax2
      character(len=2) :: c_nc

      rncat = real(ncat, kind=dbl_kind)
      d1 = 3.0_dbl_kind / rncat
      d2 = 0.5_dbl_kind / rncat

      hi_min = p01    ! minimum ice thickness allowed (m) for thermo
                      ! note hi_min is reset to 0.1 for kitd=0, below

      !-----------------------------------------------------------------
      ! Choose category boundaries based on one of four options.
      !
      ! The first formula (kcatbound = 0) was used in Lipscomb (2001) 
      !  and in CICE versions 3.0 and 3.1.
      !
      ! The second formula is more user-friendly in the sense that it
      !  is easy to obtain round numbers for category boundaries:
      !
      !    H(n) = n * [d1 + d2*(n-1)] 
      ! 
      ! Default values are d1 = 300/ncat, d2 = 50/ncat.
      ! For ncat = 5, boundaries in cm are 60, 140, 240, 360, which are 
      !  close to the standard values given by the first formula.
      ! For ncat = 10, boundaries in cm are 30, 70, 120, 180, 250, 330,
      !  420, 520, 630.    
      !
      ! The third option provides support for World Meteorological
      !  Organization classification based on thickness.  The full
      !  WMO thickness distribution is used if ncat = 7;  if ncat=5 
      !  or ncat = 6, some of the thinner categories are combined.
      ! For ncat = 5,  boundaries are         30, 70, 120, 200, >200 cm.
      ! For ncat = 6,  boundaries are     15, 30, 70, 120, 200, >200 cm.
      ! For ncat = 7,  boundaries are 10, 15, 30, 70, 120, 200, >200 cm.
      !
      ! kcatbound=-1 is available only for 1-category runs, with
      ! boundaries 0 and 100 m.
      !-----------------------------------------------------------------

      if (kcatbound == -1) then ! single category
         hin_max(0) = c0
         hin_max(1) = c100

      elseif (kcatbound == 0) then   ! original scheme

         if (kitd == 1) then
            ! linear remapping itd category limits
            cc1 = c3/rncat
            cc2 = c15*cc1
            cc3 = c3

            hin_max(0) = c0     ! minimum ice thickness, m
         else
            ! delta function itd category limits
            hi_min = p1    ! minimum ice thickness allowed (m) for thermo
            cc1 = max(1.1_dbl_kind/rncat,c1*hi_min)
            cc2 = c25*cc1
            cc3 = 2.25_dbl_kind

            ! hin_max(0) should not be zero
            ! use some caution in making it less than 0.10
            hin_max(0) = hi_min ! minimum ice thickness, m
         endif                  ! kitd

         do n = 1, ncat
            x1 = real(n-1,kind=dbl_kind) / rncat
            hin_max(n) = hin_max(n-1) &
                       + cc1 + cc2*(c1 + tanh(cc3*(x1-c1)))
         enddo

      elseif (kcatbound == 1) then  ! new scheme

         hin_max(0) = c0
         do n = 1, ncat
            rn = real(n, kind=dbl_kind)
            hin_max(n) = rn * (d1 + (rn-c1)*d2)
         enddo

      elseif (kcatbound == 2) then  ! WMO standard

        if (ncat == 5) then
         ! thinnest 3 categories combined
         data wmo5 / 0.30_dbl_kind, 0.70_dbl_kind, &
                    1.20_dbl_kind, 2.00_dbl_kind,  &
                    999._dbl_kind  /
         hin_max(0) = c0
         do n = 1, ncat
            hin_max(n) = wmo5(n)
         enddo
       elseif (ncat == 6) then
         ! thinnest 2 categories combined
         data wmo6 / 0.15_dbl_kind, &
                    0.30_dbl_kind, 0.70_dbl_kind,  &
                    1.20_dbl_kind, 2.00_dbl_kind,  &
                    999._dbl_kind /

         hin_max(0) = c0
         do n = 1, ncat
            hin_max(n) = wmo6(n)
         enddo
       elseif (ncat == 7) then
         ! all thickness categories 
         data wmo7 / 0.10_dbl_kind, 0.15_dbl_kind, &
                    0.30_dbl_kind, 0.70_dbl_kind,  &
                    1.20_dbl_kind, 2.00_dbl_kind,  &
                    999._dbl_kind  /
         hin_max(0) = c0
         do n = 1, ncat
            hin_max(n) = wmo7(n)
         enddo
       else
         write (nu_diag,*) 'kcatbound=2 (WMO) must have ncat=5, 6 or 7'
         stop
       endif

      endif ! kcatbound

      if (my_task == master_task) then
         write (nu_diag,*) ' '
         write (nu_diag,*) 'hin_max(n-1) < Cat n < hin_max(n)'
         do n = 1, ncat
            write (nu_diag,*) hin_max(n-1),' < Cat ',n, ' < ',hin_max(n)
            ! Write integer n to character string
            write (c_nc, '(i2)') n    

            ! Write hin_max to character string
            write (c_hinmax1, '(f6.3)') hin_max(n-1)
            write (c_hinmax2, '(f6.3)') hin_max(n)

            ! Save character string to write to history file
            c_hi_range(n)=c_hinmax1//'m < hi Cat '//c_nc//' < '//c_hinmax2//'m'
         enddo
         write (nu_diag,*) ' '
      endif

      end subroutine init_itd

!=======================================================================

! Aggregate ice state variables over thickness categories.
!
! authors: C. M. Bitz, UW
!          W. H. Lipscomb, LANL

      subroutine aggregate (nx_block, ny_block, &
                            aicen,    trcrn,    &
                            vicen,    vsnon,    &
                            aice,     trcr,     &
                            vice,     vsno,     &
                            aice0,    tmask,    &
                            ntrcr,    trcr_depend)

    use ice_state, only: nt_apnd, nt_alvl, nt_fbri, &
                         tr_pond_cesm, tr_pond_lvl, tr_pond_topo

      integer (kind=int_kind), intent(in) :: &
         nx_block, ny_block, & ! block dimensions
         ntrcr                 ! number of tracers in use

      real (kind=dbl_kind), dimension (nx_block,ny_block,ncat), &
         intent(in) :: &
         aicen , & ! concentration of ice
         vicen , & ! volume per unit area of ice          (m)
         vsnon     ! volume per unit area of snow         (m)

      real (kind=dbl_kind), dimension (nx_block,ny_block,ntrcr,ncat), &
         intent(in) :: &
         trcrn     ! ice tracers

      logical (kind=log_kind), dimension (nx_block,ny_block), &
         intent(in) :: &
         tmask     ! land/boundary mask, thickness (T-cell)

      integer (kind=int_kind), dimension (ntrcr), intent(in) :: &
         trcr_depend ! = 0 for aicen tracers, 1 for vicen, 2 for vsnon

      real (kind=dbl_kind), dimension (nx_block,ny_block),  &
         intent(out) :: &
         aice  , & ! concentration of ice
         vice  , & ! volume per unit area of ice          (m)
         vsno  , & ! volume per unit area of snow         (m)
         aice0     ! concentration of open water

      real (kind=dbl_kind), dimension (nx_block,ny_block,ntrcr),  &
         intent(out) :: &
         trcr      ! ice tracers

      ! local variables

      integer (kind=int_kind) :: &
        icells                ! number of ocean/ice cells

      integer (kind=int_kind), dimension (nx_block*ny_block) :: &
        indxi, &              ! compressed indices in i/j directions
        indxj

      integer (kind=int_kind) :: &
        i, j, n, it, &        ! loop indices
        ij                    ! combined i/j horizontal index

      real (kind=dbl_kind), dimension (:,:), allocatable :: &
        atrcr      ! sum of aicen*trcrn or vicen*trcrn or vsnon*trcrn

      !-----------------------------------------------------------------
      ! Initialize
      !-----------------------------------------------------------------

      icells = 0
      do j = 1, ny_block
      do i = 1, nx_block
         if (tmask(i,j)) then
            icells = icells + 1
            indxi(icells) = i
            indxj(icells) = j
         endif                  ! tmask

         aice0(i,j) = c1
         aice (i,j) = c0
         vice (i,j) = c0
         vsno (i,j) = c0
      enddo
      enddo

      if (icells > 0) then

      allocate (atrcr(icells,ntrcr))

      !-----------------------------------------------------------------
      ! Aggregate
      !-----------------------------------------------------------------

      atrcr(:,:) = c0

      do n = 1, ncat

!DIR$ CONCURRENT !Cray
!cdir nodep      !NEC
!ocl novrec      !Fujitsu
         do ij = 1, icells
            i = indxi(ij)
            j = indxj(ij)
            aice(i,j) = aice(i,j) + aicen(i,j,n)
            vice(i,j) = vice(i,j) + vicen(i,j,n)
            vsno(i,j) = vsno(i,j) + vsnon(i,j,n)
         enddo                  ! ij

         do it = 1, ntrcr
            if (trcr_depend(it) == 0) then  ! ice area tracer
!DIR$ CONCURRENT !Cray
!cdir nodep      !NEC
!ocl novrec      !Fujitsu
               do ij = 1, icells
                  i = indxi(ij)
                  j = indxj(ij)
                  atrcr(ij,it) = atrcr(ij,it)  &
                                + trcrn(i,j,it,n)*aicen(i,j,n)
               enddo            ! ij

            elseif (trcr_depend(it) == 1) then  ! ice volume tracer
!DIR$ CONCURRENT !Cray
!cdir nodep      !NEC
!ocl novrec      !Fujitsu
               do ij = 1, icells
                  i = indxi(ij)
                  j = indxj(ij)
                  atrcr(ij,it) = atrcr(ij,it)  &
                                + trcrn(i,j,it,n)*vicen(i,j,n)
               enddo            ! ij

            elseif (trcr_depend(it) == 2) then ! snow volume tracer

!DIR$ CONCURRENT !Cray
!cdir nodep      !NEC
!ocl novrec      !Fujitsu
               do ij = 1, icells
                  i = indxi(ij)
                  j = indxj(ij)
                  atrcr(ij,it) = atrcr(ij,it)  &
                                + trcrn(i,j,it,n)*vsnon(i,j,n)
               enddo            ! ij

            elseif (trcr_depend(it) == 2+nt_alvl) then ! level ice tracer

!DIR$ CONCURRENT !Cray
!cdir nodep      !NEC
!ocl novrec      !Fujitsu
               do ij = 1, icells
                  i = indxi(ij)
                  j = indxj(ij)
                  atrcr(ij,it) = atrcr(ij,it)  &
                               + trcrn(i,j,it,n)*trcrn(i,j,nt_alvl,n)*aicen(i,j,n)
               enddo            ! ij

            elseif (trcr_depend(it) == 2+nt_apnd .and. &
                   (tr_pond_cesm .or. tr_pond_topo)) then ! CESM or topo pond area tracer

!DIR$ CONCURRENT !Cray
!cdir nodep      !NEC
!ocl novrec      !Fujitsu
               do ij = 1, icells
                  i = indxi(ij)
                  j = indxj(ij)
                  atrcr(ij,it) = atrcr(ij,it)  &
                               + trcrn(i,j,it,n)*trcrn(i,j,nt_apnd,n)*aicen(i,j,n)
               enddo            ! ij

            elseif (trcr_depend(it) == 2+nt_apnd .and. &
                    tr_pond_lvl) then ! level-ice pond area tracer

!DIR$ CONCURRENT !Cray
!cdir nodep      !NEC
!ocl novrec      !Fujitsu
               do ij = 1, icells
                  i = indxi(ij)
                  j = indxj(ij)
                  atrcr(ij,it) = atrcr(ij,it)  &
                                + trcrn(i,j,it,n)*trcrn(i,j,nt_apnd,n) &
                                                 *trcrn(i,j,nt_alvl,n)*aicen(i,j,n)
               enddo            ! ij

            elseif (trcr_depend(it) == 2+nt_fbri) then ! brine tracer

!DIR$ CONCURRENT !Cray
!cdir nodep      !NEC
!ocl novrec      !Fujitsu
               do ij = 1, icells
                  i = indxi(ij)
                  j = indxj(ij)
                  atrcr(ij,it) = atrcr(ij,it)  &
                               + trcrn(i,j,it,n)*trcrn(i,j,nt_fbri,n)*vicen(i,j,n) 
               enddo            ! ij
            endif               ! trcr_depend
         enddo                  ! ntrcr
      enddo                     ! ncat

      ! Open water fraction

      do ij = 1, icells
         i = indxi(ij)
         j = indxj(ij)
         aice0(i,j) = max (c1 - aice(i,j), c0)
      enddo                     ! ij

      ! Tracers

      call compute_tracers (nx_block,   ny_block,     &
                            icells, indxi,   indxj,   &
                            ntrcr,      trcr_depend,  &
                            atrcr(:,:), aice(:,:),    &
                            vice (:,:), vsno(:,:),    &
                            trcr(:,:,:))

      deallocate (atrcr)

      endif ! icells > 0

      end subroutine aggregate

!=======================================================================

! Aggregate ice area (but not other state variables) over thickness 
! categories.
!
! authors: William H. Lipscomb, LANL
!          modified Jan 2004 by Clifford Chen, Fujitsu

      subroutine aggregate_area (nx_block, ny_block,        &
                                 aicen,    aice,     aice0)

      integer (kind=int_kind), intent(in) :: &
         nx_block, ny_block  ! block dimensions

      real (kind=dbl_kind), dimension (:,:,:), intent(in) :: &
         aicen     ! concentration of ice

      real (kind=dbl_kind), dimension (:,:), intent(inout) :: &
         aice, &   ! concentration of ice
         aice0     ! concentration of open water

      ! local variables

      integer (kind=int_kind) :: i, j, n

      !-----------------------------------------------------------------
      ! Aggregate
      !-----------------------------------------------------------------

      aice(:,:) = c0

      do n = 1, ncat
         do j = 1, ny_block
         do i = 1, nx_block
            aice(i,j) = aice(i,j) + aicen(i,j,n)
         enddo                  ! i
         enddo                  ! j
      enddo                     ! n

      do j = 1, ny_block
      do i = 1, nx_block

         ! open water fraction
         aice0(i,j) = max (c1 - aice(i,j), c0)

      enddo                     ! i
      enddo                     ! j

      end subroutine aggregate_area

!=======================================================================

! Rebins thicknesses into defined categories
!
! authors: William H. Lipscomb and Elizabeth C. Hunke, LANL

      subroutine rebin (nx_block, ny_block,        &
                        icells,   indxi,    indxj, &
                        ntrcr,    trcr_depend,     &
                        aicen,    trcrn,           &
                        vicen,    vsnon,           &
                        l_stop,                    &
                        istop,    jstop)

      integer (kind=int_kind), intent(in) :: &
         nx_block, ny_block, & ! block dimensions
         icells            , & ! number of grid cells with ice
         ntrcr                 ! number of tracers in use

       integer (kind=int_kind), dimension (nx_block*ny_block), &
         intent(in) :: &
         indxi, indxj      ! compressed i/j indices

      integer (kind=int_kind), dimension (ntrcr), intent(in) :: &
         trcr_depend ! = 0 for aicen tracers, 1 for vicen, 2 for vsnon

      real (kind=dbl_kind), dimension (nx_block,ny_block,ncat), &
         intent(inout) :: &
         aicen , & ! concentration of ice
         vicen , & ! volume per unit area of ice           (m)
         vsnon     ! volume per unit area of snow          (m)

      real (kind=dbl_kind), dimension (nx_block,ny_block,ntrcr,ncat), &
         intent(inout) :: &
         trcrn     ! ice tracers

      logical (kind=log_kind), intent(out) :: &
         l_stop    ! if true, abort on return

      integer (kind=int_kind), intent(out) :: &
         istop, jstop    ! indices of grid cell where model aborts

      ! local variables

      integer (kind=int_kind) :: &
         i,j          , & ! horizontal indices
         n            , & ! category index
         ij                ! combined horizontal index

      logical (kind=log_kind) :: &
         shiftflag          ! = .true. if ice must be shifted

      integer (kind=int_kind), dimension (icells,ncat) :: &
         donor              ! donor category index

      real (kind=dbl_kind), dimension (icells,ncat) :: &
         daice          , & ! ice area transferred
         dvice          , & ! ice volume transferred
         hicen              ! ice thickness for each cat (m)

      !-----------------------------------------------------------------
      ! Initialize
      !-----------------------------------------------------------------

      l_stop = .false.
      istop = 0
      jstop = 0

      do n = 1, ncat
         do ij = 1, icells       ! aice(i,j) > puny
            i = indxi(ij)
            j = indxj(ij)

            donor(ij,n) = 0
            daice(ij,n) = c0
            dvice(ij,n) = c0

      !-----------------------------------------------------------------
      ! Compute ice thickness.
      !-----------------------------------------------------------------
            if (aicen(i,j,n) > puny) then
               hicen(ij,n) = vicen(i,j,n) / aicen(i,j,n)
            else
               hicen(ij,n) = c0
            endif
         enddo                  ! ij
      enddo                     ! n

      !-----------------------------------------------------------------
      ! make sure thickness of cat 1 is at least hin_max(0)
      !-----------------------------------------------------------------
      do ij = 1, icells       ! aice(i,j) > puny
         i = indxi(ij)
         j = indxj(ij)

         if (aicen(i,j,1) > puny) then
            if (hicen(ij,1) <= hin_max(0) .and. hin_max(0) > c0 ) then
               aicen(i,j,1) = vicen(i,j,1) / hin_max(0)
               hicen(ij,1) = hin_max(0)
            endif
         endif
      enddo                     ! ij

      !-----------------------------------------------------------------
      ! If a category thickness is not in bounds, shift the
      ! entire area, volume, and energy to the neighboring category
      !-----------------------------------------------------------------

      !-----------------------------------------------------------------
      ! Move thin categories up
      !-----------------------------------------------------------------

      do n = 1, ncat-1          ! loop over category boundaries

      !-----------------------------------------------------------------
      ! identify thicknesses that are too big
      !-----------------------------------------------------------------
         shiftflag = .false.
         do ij = 1, icells       ! aice(i,j) > puny
            i = indxi(ij)
            j = indxj(ij)

            if (aicen(i,j,n) > puny .and. &
                hicen(ij,n) > hin_max(n)) then
               shiftflag = .true.
               donor(ij,n) = n
               daice(ij,n) = aicen(i,j,n)
               dvice(ij,n) = vicen(i,j,n)
            endif
         enddo                  ! ij

         if (shiftflag) then

      !-----------------------------------------------------------------
      ! shift ice between categories
      !-----------------------------------------------------------------
            call shift_ice (nx_block, ny_block,    &
                            indxi,    indxj,       &
                            icells,                &
                            ntrcr,    trcr_depend, &
                            aicen,    trcrn,       &
                            vicen,    vsnon,       &
                            hicen,    donor,       &
                            daice,    dvice,       &
                            l_stop,                &
                            istop,    jstop)

            if (l_stop) return

      !-----------------------------------------------------------------
      ! reset shift parameters
      !-----------------------------------------------------------------

         do ij = 1, icells       ! aice(i,j) > puny
            donor(ij,n) = 0
            daice(ij,n) = c0
            dvice(ij,n) = c0
         enddo

         endif                  ! shiftflag

      enddo                     ! n

      !-----------------------------------------------------------------
      ! Move thick categories down
      !-----------------------------------------------------------------

      do n = ncat-1, 1, -1      ! loop over category boundaries

      !-----------------------------------------------------------------
      ! identify thicknesses that are too small
      !-----------------------------------------------------------------
         shiftflag = .false.
         do ij = 1, icells       ! aice(i,j) > puny
            i = indxi(ij)
            j = indxj(ij)

            if (aicen(i,j,n+1) > puny .and. &
                hicen(ij,n+1) <= hin_max(n)) then
               shiftflag = .true.
               donor(ij,n) = n+1
               daice(ij,n) = aicen(i,j,n+1)
               dvice(ij,n) = vicen(i,j,n+1)
            endif
         enddo                  ! ij

         if (shiftflag) then

      !-----------------------------------------------------------------
      ! shift ice between categories
      !-----------------------------------------------------------------
            call shift_ice (nx_block, ny_block,    &
                            indxi,    indxj,       &
                            icells,                &
                            ntrcr,    trcr_depend, &
                            aicen,    trcrn,       &
                            vicen,    vsnon,       &
                            hicen,    donor,       &
                            daice,    dvice,       &
                            l_stop,                &
                            istop,    jstop)

            if (l_stop) return

      !-----------------------------------------------------------------
      ! reset shift parameters
      !-----------------------------------------------------------------

         do ij = 1, icells       ! aice(i,j) > puny
            donor(ij,n) = 0
            daice(ij,n) = c0
            dvice(ij,n) = c0
         enddo

         endif                  ! shiftflag

      enddo                     ! n


      end subroutine rebin

!=======================================================================

! Reduce area when ice melts for special case of ncat=1
!
! Use CSM 1.0-like method of reducing ice area
! when melting occurs: assume only half the ice volume
! change goes to thickness decrease, the other half
! to reduction in ice fraction
!
! authors: C. M. Bitz, UW
! modified by: Elizabeth C. Hunke, LANL

      subroutine reduce_area (nx_block, ny_block, &
                              ilo, ihi, jlo, jhi, &
                              tmask,              &
                              aicen,     vicen,   &
                              aicen_init,vicen_init)

      integer (kind=int_kind), intent(in) :: &
         nx_block, ny_block, & ! block dimensions
         ilo,ihi,jlo,jhi       ! beginning and end of physical domain

      logical (kind=log_kind), dimension (nx_block,ny_block), &
         intent(in) :: &
         tmask     ! land/boundary mask, thickness (T-cell)

      real (kind=dbl_kind), dimension (nx_block,ny_block), &
         intent(inout) :: &
         aicen , & ! concentration of ice
         vicen     ! volume per unit area of ice          (m)

      real (kind=dbl_kind), dimension(nx_block,ny_block), intent(in) :: &
         aicen_init, & ! old ice area for category 1 (m)
         vicen_init    ! old ice volume for category 1 (m)

      ! local variables

      integer (kind=int_kind) :: &
         i, j        ! horizontal indices

      real (kind=dbl_kind) :: &
         hi0     , & ! initial hi
         hi1     , & ! current hi
         dhi         ! hi1 - hi0

      do j = jlo, jhi
      do i = ilo, ihi
         if (tmask(i,j)) then

            hi0 = c0
            if (aicen_init(i,j) > c0) &
                hi0 = vicen_init(i,j) / aicen_init(i,j)

            hi1 = c0
            if (aicen(i,j) > c0) &
                hi1 = vicen(i,j) / aicen(i,j)

            ! make sure thickness of cat 1 is at least hin_max(0)
            if (hi1 <= hin_max(0) .and. hin_max(0) > c0 ) then
               aicen(i,j) = vicen(i,j) / hin_max(0)
               hi1 = hin_max(0)
            endif

            if (aicen(i,j) > c0) then
               dhi = hi1 - hi0
               if (dhi < c0) then
                  hi1  = vicen(i,j) / aicen(i,j)
                  aicen(i,j) = c2 * vicen(i,j) / (hi1 + hi0)
               endif
            endif

         endif                  ! tmask
      enddo                     ! i
      enddo                     ! j

      end subroutine reduce_area

!=======================================================================

! Shift ice across category boundaries, conserving area, volume, and
! energy.
!
! authors: William H. Lipscomb and Elizabeth C. Hunke, LANL

      subroutine shift_ice (nx_block, ny_block,    &
                            indxi,    indxj,       &
                            icells,                &
                            ntrcr,    trcr_depend, &
                            aicen,    trcrn,       &
                            vicen,    vsnon,       &
                            hicen,    donor,       &
                            daice,    dvice,       &
                            l_stop,                &
                            istop,    jstop)

      use ice_state, only: nt_apnd, nt_alvl, nt_fbri, &
                           tr_pond_cesm, tr_pond_lvl, tr_pond_topo

      integer (kind=int_kind), intent(in) :: &
         nx_block, ny_block, & ! block dimensions
         icells            , & ! number of ocean/ice cells
         ntrcr                 ! number of tracers in use

      integer (kind=int_kind), dimension (nx_block*ny_block), &
         intent(in) :: &
         indxi             , & ! compressed indices in i/j directions
         indxj

      integer (kind=int_kind), dimension (ntrcr), intent(in) :: &
         trcr_depend ! = 0 for aicen tracers, 1 for vicen, 2 for vsnon

      real (kind=dbl_kind), dimension (nx_block,ny_block,ncat), &
         intent(inout) :: &
         aicen , & ! concentration of ice
         vicen , & ! volume per unit area of ice          (m)
         vsnon     ! volume per unit area of snow         (m)

      real (kind=dbl_kind), dimension (nx_block,ny_block,ntrcr,ncat), &
         intent(inout) :: &
         trcrn     ! ice tracers

      ! NOTE: Third index of donor, daice, dvice should be ncat-1,
      !       except that compilers would have trouble when ncat = 1 
      integer (kind=int_kind), dimension(icells,ncat), &
         intent(in) :: &
         donor             ! donor category index

      real (kind=dbl_kind), dimension(icells,ncat), &
           intent(inout) :: &
         daice         , & ! ice area transferred across boundary
         dvice         , & ! ice volume transferred across boundary
         hicen             ! ice thickness for each cat        (m)

      logical (kind=log_kind), intent(out) :: &
         l_stop    ! if true, abort on return

      integer (kind=int_kind), intent(out) :: &
         istop, jstop    ! indices of grid cell where model aborts

      ! local variables

      integer (kind=int_kind) :: &
         i, j, m       , & ! horizontal indices
         n             , & ! thickness category index
         nr            , & ! receiver category
         nd            , & ! donor category
         it                ! tracer index

      real (kind=dbl_kind), dimension(icells,ntrcr,ncat) :: &
         atrcrn            ! aicen*trcrn   

     ! real (kind=dbl_kind), dimension(icells,ncat) :: &
     !    dvbrine            ! brine volume transferred

      real (kind=dbl_kind) :: &
         dvsnow        , & ! snow volume transferred
         datrcr            ! aicen*train transferred

      integer (kind=int_kind), dimension (icells) :: &
        indxii       , & ! compressed indices for i/j directions
        indxjj       , &
        indxij

      integer (kind=int_kind) :: &
        ishift      , & ! number of cells with ice to transfer
        ij              ! combined i/j horizontal index

      logical (kind=log_kind) :: &
        daice_negative     , & ! true if daice < -puny
        dvice_negative     , & ! true if dvice < -puny
        daice_greater_aicen, & ! true if daice > aicen
        dvice_greater_vicen    ! true if dvice > vicen

      real (kind=dbl_kind), dimension (nx_block,ny_block) :: &
         worka, workb

      !-----------------------------------------------------------------
      ! Initialize
      !-----------------------------------------------------------------

      l_stop = .false.
      istop = 0
      jstop = 0

      !-----------------------------------------------------------------
      ! Define variables equal to aicen*trcrn, vicen*trcrn, vsnon*trcrn
      !-----------------------------------------------------------------

      do n = 1, ncat
         do it = 1, ntrcr
            if (trcr_depend(it) == 0) then ! ice area tracer
               do ij = 1, icells
                  i = indxi(ij)
                  j = indxj(ij)
                  atrcrn(ij,it,n) = aicen(i,j,n)*trcrn(i,j,it,n)
               enddo
            elseif (trcr_depend(it) == 1) then  ! ice volume tracer
               do ij = 1, icells
                  i = indxi(ij)
                  j = indxj(ij)
                  atrcrn(ij,it,n) = vicen(i,j,n)*trcrn(i,j,it,n)
               enddo
            elseif (trcr_depend(it) == 2) then  ! snow volume tracer
               do ij = 1, icells
                  i = indxi(ij)
                  j = indxj(ij)
                  atrcrn(ij,it,n) = vsnon(i,j,n)*trcrn(i,j,it,n)
               enddo
            elseif (trcr_depend(it) == 2+nt_alvl) then  ! level ice tracer
               do ij = 1, icells
                  i = indxi(ij)
                  j = indxj(ij)
                  atrcrn(ij,it,n) = aicen(i,j,n) &
                                  * trcrn(i,j,nt_alvl,n) &
                                  * trcrn(i,j,it,n)
               enddo
            elseif (trcr_depend(it) == 2+nt_apnd .and. &
                   (tr_pond_cesm .or. tr_pond_topo)) then ! CESM or topo pond area tracer
               do ij = 1, icells
                  i = indxi(ij)
                  j = indxj(ij)
                  atrcrn(ij,it,n) = aicen(i,j,n) &
                                  * trcrn(i,j,nt_apnd,n) &
                                  * trcrn(i,j,it,n)
               enddo
            elseif (trcr_depend(it) == 2+nt_apnd .and. &
                    tr_pond_lvl) then ! level-ice pond area tracer
               do ij = 1, icells
                  i = indxi(ij)
                  j = indxj(ij)
                  atrcrn(ij,it,n) = aicen(i,j,n) &
                                  * trcrn(i,j,nt_alvl,n) &
                                  * trcrn(i,j,nt_apnd,n) &
                                  * trcrn(i,j,it,n)
               enddo
            elseif (trcr_depend(it) == 2+nt_fbri) then  ! brine tracer
               do ij = 1, icells
                  i = indxi(ij)
                  j = indxj(ij)
                  atrcrn(ij,it,n) = vicen(i,j,n) &   
                                  * trcrn(i,j,nt_fbri,n) &
                                  * trcrn(i,j,it,n)
               enddo
            endif
         enddo
     
      enddo

      !-----------------------------------------------------------------
      ! Check for daice or dvice out of range, allowing for roundoff error
      !-----------------------------------------------------------------

      do n = 1, ncat-1

         daice_negative = .false.
         dvice_negative = .false.
         daice_greater_aicen = .false.
         dvice_greater_vicen = .false.

         do ij = 1, icells
            i = indxi(ij)
            j = indxj(ij)

            if (donor(ij,n) > 0) then
               nd = donor(ij,n)

               if (daice(ij,n) < c0) then
                  if (daice(ij,n) > -puny*aicen(i,j,nd)) then
                     daice(ij,n) = c0 ! shift no ice
                     dvice(ij,n) = c0
                  else
                     daice_negative = .true.
                  endif
               endif
         
               if (dvice(ij,n) < c0) then
                  if (dvice(ij,n) > -puny*vicen(i,j,nd)) then   
                     daice(ij,n) = c0 ! shift no ice
                     dvice(ij,n) = c0
                  else
                     dvice_negative = .true.
                  endif
               endif

               if (daice(ij,n) > aicen(i,j,nd)*(c1-puny)) then
                  if (daice(ij,n) < aicen(i,j,nd)*(c1+puny)) then
                     daice(ij,n) = aicen(i,j,nd)
                     dvice(ij,n) = vicen(i,j,nd)
                  else
                     daice_greater_aicen = .true.
                  endif
               endif    

               if (dvice(ij,n) > vicen(i,j,nd)*(c1-puny)) then
                  if (dvice(ij,n) < vicen(i,j,nd)*(c1+puny)) then
                     daice(ij,n) = aicen(i,j,nd)
                     dvice(ij,n) = vicen(i,j,nd)
                  else
                     dvice_greater_vicen = .true.
                  endif
               endif

            endif               ! donor > 0
         enddo                  ! ij

      !-----------------------------------------------------------------
      ! error messages
      !-----------------------------------------------------------------

         if (daice_negative) then
         do ij = 1, icells
            i = indxi(ij)
            j = indxj(ij)

               if (donor(ij,n) > 0 .and.  &
                   daice(ij,n) <= -puny*aicen(i,j,nd)) then
                  write(nu_diag,*) ' '
                  write(nu_diag,*) 'shift_ice: negative daice'
                  write(nu_diag,*) 'i, j:', i, j
                  write(nu_diag,*) 'boundary, donor cat:', n, nd
                  write(nu_diag,*) 'daice =', daice(ij,n)
                  write(nu_diag,*) 'dvice =', dvice(ij,n)
                  l_stop = .true.
                  istop = i
                  jstop = j
               endif
            enddo
         endif
         if (l_stop) return

         if (dvice_negative) then
         do ij = 1, icells
            i = indxi(ij)
            j = indxj(ij)

               if (donor(ij,n) > 0 .and.  &
                   dvice(ij,n) <= -puny*vicen(i,j,nd)) then
                  write(nu_diag,*) ' '
                  write(nu_diag,*) 'shift_ice: negative dvice'
                  write(nu_diag,*) 'i, j:', i, j
                  write(nu_diag,*) 'boundary, donor cat:', n, nd
                  write(nu_diag,*) 'daice =', daice(ij,n)
                  write(nu_diag,*) 'dvice =', dvice(ij,n)
                  l_stop = .true.
                  istop = i
                  jstop = j
               endif
            enddo
         endif
         if (l_stop) return

         if (daice_greater_aicen) then
         do ij = 1, icells
            i = indxi(ij)
            j = indxj(ij)

               if (donor(ij,n) > 0) then
                  nd = donor(ij,n)
                  if (daice(ij,n) >= aicen(i,j,nd)*(c1+puny)) then
                     write(nu_diag,*) ' '
                     write(nu_diag,*) 'shift_ice: daice > aicen'
                     write(nu_diag,*) 'i, j:', i, j
                     write(nu_diag,*) 'boundary, donor cat:', n, nd
                     write(nu_diag,*) 'daice =', daice(ij,n)
                     write(nu_diag,*) 'aicen =', aicen(i,j,nd)
                     l_stop = .true.
                     istop = i
                     jstop = j
                  endif
               endif
            enddo
         endif
         if (l_stop) return

         if (dvice_greater_vicen) then
         do ij = 1, icells
            i = indxi(ij)
            j = indxj(ij)

               if (donor(ij,n) > 0) then
                  nd = donor(ij,n)
                  if (dvice(ij,n) >= vicen(i,j,nd)*(c1+puny)) then
                     write(nu_diag,*) ' '
                     write(nu_diag,*) 'shift_ice: dvice > vicen'
                     write(nu_diag,*) 'i, j:', i, j
                     write(nu_diag,*) 'boundary, donor cat:', n, nd
                     write(nu_diag,*) 'dvice =', dvice(ij,n)
                     write(nu_diag,*) 'vicen =', vicen(i,j,nd)
                     l_stop = .true.
                     istop = i
                     jstop = j
                  endif
               endif
            enddo
         endif
         if (l_stop) return

      !-----------------------------------------------------------------
      ! transfer volume and energy between categories
      !-----------------------------------------------------------------

         ishift = 0
         do ij = 1, icells
            i = indxi(ij)
            j = indxj(ij)

           if (daice(ij,n) > c0) then ! daice(n) can be < puny
             ishift = ishift + 1
             indxii(ishift) = i
             indxjj(ishift) = j
             indxij(ishift) = ij
           endif   ! tmask
         enddo

!DIR$ CONCURRENT !Cray
!cdir nodep      !NEC
!ocl novrec      !Fujitsu
         do ij = 1, ishift
            i = indxii(ij)
            j = indxjj(ij)
            m = indxij(ij)

            nd = donor(m,n)
!echmod            worka(i,j) = dvice(m,n) / vicen(i,j,nd)
            worka(i,j) = daice(m,n) / aicen(i,j,nd)
            if (nd  ==  n) then
               nr = nd+1
            else                ! nd = n+1
               nr = n
            endif

            aicen(i,j,nd) = aicen(i,j,nd) - daice(m,n)
            aicen(i,j,nr) = aicen(i,j,nr) + daice(m,n)

            vicen(i,j,nd) = vicen(i,j,nd) - dvice(m,n)
            vicen(i,j,nr) = vicen(i,j,nr) + dvice(m,n)

            dvsnow = vsnon(i,j,nd) * worka(i,j)
            vsnon(i,j,nd) = vsnon(i,j,nd) - dvsnow
            vsnon(i,j,nr) = vsnon(i,j,nr) + dvsnow
            workb(i,j) = dvsnow

         enddo                  ! ij

         do it = 1, ntrcr
!DIR$ CONCURRENT !Cray
!cdir nodep      !NEC
!ocl novrec      !Fujitsu
            do ij = 1, ishift
               i = indxii(ij)
               j = indxjj(ij)
               m = indxij(ij)

               nd = donor(m,n)
               if (nd == n) then
                  nr = nd+1
               else             ! nd = n+1
                  nr = n
               endif

               if (trcr_depend(it) == 0) then
                  datrcr = daice(m,n)*trcrn(i,j,it,nd)
               elseif (trcr_depend(it) == 1) then
                  datrcr = dvice(m,n)*trcrn(i,j,it,nd)
               elseif (trcr_depend(it) == 2) then
                  datrcr = workb(i,j)*trcrn(i,j,it,nd)
               elseif (trcr_depend(it) == 2+nt_alvl) then
                  datrcr = daice(m,n)*trcrn(i,j,nt_alvl,nd)*trcrn(i,j,it,nd)
               elseif (trcr_depend(it) == 2+nt_apnd .and. &
                      (tr_pond_cesm .or. tr_pond_topo)) then
                  datrcr = daice(m,n)*trcrn(i,j,nt_apnd,nd)*trcrn(i,j,it,nd)
               elseif (trcr_depend(it) == 2+nt_apnd .and. &
                       tr_pond_lvl) then
                  datrcr = daice(m,n)*trcrn(i,j,nt_alvl,nd) &
                                     *trcrn(i,j,nt_apnd,nd)*trcrn(i,j,it,nd)
               elseif (trcr_depend(it) == 2+nt_fbri) then
                  datrcr =  dvice(m,n)*trcrn(i,j,nt_fbri,nd)*trcrn(i,j,it,nd)
               endif

               atrcrn(m,it,nd) = atrcrn(m,it,nd) - datrcr
               atrcrn(m,it,nr) = atrcrn(m,it,nr) + datrcr
            
            enddo               ! ij
         enddo                  ! ntrcr
      enddo                     ! boundaries, 1 to ncat-1

      !-----------------------------------------------------------------
      ! Update ice thickness and tracers
      !-----------------------------------------------------------------

      do n = 1, ncat

         do ij = 1, icells
            i = indxi(ij)
            j = indxj(ij)

            if (aicen(i,j,n) > puny) then
               hicen(ij,n) = vicen (i,j,n) / aicen(i,j,n)                
            else
               hicen(ij,n) = c0
            endif
         enddo

         call compute_tracers (nx_block,        ny_block,       &
                               icells,          indxi,   indxj, &
                               ntrcr,           trcr_depend,    &
                               atrcrn(:,:,  n), aicen(:,:,  n), &
                               vicen (:,:,  n), vsnon(:,:,  n), &
                               trcrn (:,:,:,n))

      enddo                     ! ncat

      end subroutine shift_ice

!=======================================================================

! For each grid cell, sum field over all ice categories.
!
! author: William H. Lipscomb, LANL

      subroutine column_sum (nx_block, ny_block,       &
                             icells,   indxi,   indxj, &
                             nsum,                     &
                             xin,      xout)

      integer (kind=int_kind), intent(in) :: &
         nx_block, ny_block, & ! block dimensions
         nsum              , & ! number of categories/layers
         icells                ! number of ice/ocean grid cells

      integer (kind=int_kind), dimension (nx_block*ny_block), &
         intent(in) :: &
         indxi,  indxj          ! compressed i/j indices

      real (kind=dbl_kind), dimension (nx_block,ny_block,nsum), &
           intent(in) :: &
           xin              ! input field

      real (kind=dbl_kind), dimension (icells), intent(out) :: &
           xout             ! output field

      ! local variables

      integer (kind=int_kind) :: &
           i, j, ij     , & ! horizontal indices
           n                ! category/layer index

      do ij = 1, icells
         xout(ij) = c0
      enddo

      do n = 1, nsum
         do ij = 1, icells
            i = indxi(ij)
            j = indxj(ij)
            xout(ij) = xout(ij) + xin(i,j,n)
         enddo                  ! ij
      enddo                     ! n

      end subroutine column_sum

!=======================================================================

! For each physical grid cell, check that initial and final values
! of a conserved field are equal to within a small value.
!
! author: William H. Lipscomb, LANL

      subroutine column_conservation_check (nx_block, ny_block,       &
                                            icells,   indxi,   indxj, &
                                            fieldid,                  &
                                            x1,       x2,             &
                                            max_err,  l_stop,         &
                                            istop,    jstop)

      integer (kind=int_kind), intent(in) :: &
         nx_block, ny_block, & ! block dimensions
         icells                ! number of ice/ocean grid cells

      integer (kind=int_kind), dimension (nx_block*ny_block), &
         intent(in) :: &
         indxi,  indxj     ! compressed i/j indices

      real (kind=dbl_kind), dimension(icells), intent(in) :: &
         x1            , & ! initial field
         x2                ! final field

      real (kind=dbl_kind), intent(in) :: &
         max_err           ! max allowed error

      character (len=char_len), intent(in) :: &
         fieldid           ! field identifier

      logical (kind=log_kind), intent(inout) :: &
         l_stop            ! if true, abort on return

      integer (kind=int_kind), intent(inout) :: &
         istop, jstop      ! indices of grid cell where model aborts

      ! local variables

      integer (kind=int_kind) :: &
         ij                    ! horizontal indices

      do ij = 1, icells
         if (abs (x2(ij)-x1(ij)) > max_err) then
            l_stop = .true.
            istop = indxi(ij)
            jstop = indxj(ij)

            write (nu_diag,*) ' '
            write (nu_diag,*) 'Conservation error: ', trim(fieldid)
            write (nu_diag,*) 'i, j =', istop, jstop
            write (nu_diag,*) 'Initial value =', x1(ij)
            write (nu_diag,*) 'Final value =',   x2(ij)
            write (nu_diag,*) 'Difference =', x2(ij) - x1(ij)
         endif
      enddo

      end subroutine column_conservation_check

!=======================================================================

! Compute tracer fields.
! Given atrcrn = aicen*trcrn (or vicen*trcrn, vsnon*trcrn), compute trcrn.
!
! author: William H. Lipscomb, LANL

      subroutine compute_tracers (nx_block, ny_block,       &
                                  icells,   indxi,   indxj, &
                                  ntrcr,    trcr_depend,    &
                                  atrcrn,   aicen,          &
                                  vicen,    vsnon,          &
                                  trcrn)

      use ice_state, only: nt_Tsfc, nt_alvl, nt_apnd, nt_fbri, &
                           tr_pond_cesm, tr_pond_lvl, tr_pond_topo

      integer (kind=int_kind), intent(in) :: &
         nx_block, ny_block, & ! block dimensions
         icells            , & ! number of ice/ocean grid cells
         ntrcr                 ! number of tracers in use

      integer (kind=int_kind), dimension (nx_block*ny_block), &
         intent(in) :: &
         indxi,  indxj       ! compressed i/j indices

      integer (kind=int_kind), dimension (ntrcr), intent(in) :: &
         trcr_depend ! = 0 for aicen tracers, 1 for vicen, 2 for vsnon

      real (kind=dbl_kind), dimension (icells,ntrcr), &
         intent(in) :: &
         atrcrn    ! aicen*trcrn or vicen*trcrn or vsnon*trcrn

      real (kind=dbl_kind), dimension (nx_block,ny_block), &
         intent(in) :: &
         aicen , & ! concentration of ice
         vicen , & ! volume per unit area of ice          (m)
         vsnon     ! volume per unit area of snow         (m)

      real (kind=dbl_kind), dimension (nx_block,ny_block,ntrcr), &
         intent(out) :: &
         trcrn     ! ice tracers

      ! local variables

      integer (kind=int_kind) :: &
         i, j, it, ij       ! counting indices


      trcrn(:,:,:) = c0

      !-----------------------------------------------------------------
      ! Compute new tracers
      !-----------------------------------------------------------------

      do it = 1, ntrcr
         if (it == nt_Tsfc) then      ! surface temperature
            do ij = 1, icells
               i = indxi(ij)
               j = indxj(ij)
               if (aicen(i,j) > puny) then  
                  trcrn(i,j,it) = atrcrn(ij,it) / aicen(i,j)
               else
                  trcrn(i,j,it) = Tocnfrz
               endif
            enddo

         elseif (trcr_depend(it) == 0) then ! ice area tracers
            do ij = 1, icells
               i = indxi(ij)
               j = indxj(ij)
               if (aicen(i,j) > puny) then  
                  trcrn(i,j,it) = atrcrn(ij,it) / aicen(i,j)
               else
                  trcrn(i,j,it) = c0
               endif
            enddo

         elseif (trcr_depend(it) == 1) then ! ice volume tracers
            do ij = 1, icells
               i = indxi(ij)
               j = indxj(ij)
               if (vicen(i,j) > c0) then
                  trcrn(i,j,it) = atrcrn(ij,it) / vicen(i,j)
               else
                  trcrn(i,j,it) = c0
                  if (it == nt_fbri) trcrn(i,j,nt_fbri) = c1
               endif
            enddo

         elseif (trcr_depend(it) == 2) then ! snow volume tracers
            do ij = 1, icells
               i = indxi(ij)
               j = indxj(ij)
               if (vsnon(i,j) > c0) then
                  trcrn(i,j,it) = atrcrn(ij,it) / vsnon(i,j)
               else
                  trcrn(i,j,it) = c0
               endif
            enddo

         elseif (trcr_depend(it) == 2+nt_alvl) then ! level ice tracers
            do ij = 1, icells
               i = indxi(ij)
               j = indxj(ij)
               if (trcrn(i,j,nt_alvl)*aicen(i,j) > c0) then
                  trcrn(i,j,it) = atrcrn(ij,it) / (trcrn(i,j,nt_alvl)*aicen(i,j))
               else
                  trcrn(i,j,it) = c0
               endif
            enddo

         elseif (trcr_depend(it) == 2+nt_apnd .and. &
                (tr_pond_cesm .or. tr_pond_topo)) then ! CESM or topo pond area tracer
            do ij = 1, icells
               i = indxi(ij)
               j = indxj(ij)
               if (trcrn(i,j,nt_apnd)*aicen(i,j) > c0) then
                  trcrn(i,j,it) = atrcrn(ij,it) / (trcrn(i,j,nt_apnd)*aicen(i,j))
               else
                  trcrn(i,j,it) = c0
               endif
            enddo
         elseif (trcr_depend(it) == 2+nt_apnd .and. &
                 tr_pond_lvl) then ! level-ice pond area tracer
            do ij = 1, icells
               i = indxi(ij)
               j = indxj(ij)
               if (trcrn(i,j,nt_alvl)*trcrn(i,j,nt_apnd)*aicen(i,j) > c0) then
                  trcrn(i,j,it) = atrcrn(ij,it) &
                                / (trcrn(i,j,nt_alvl)*trcrn(i,j,nt_apnd)*aicen(i,j))
               else
                  trcrn(i,j,it) = c0
               endif
            enddo

         elseif (trcr_depend(it) == 2+nt_fbri) then ! brine tracers
            do ij = 1, icells
               i = indxi(ij)
               j = indxj(ij)
               if (trcrn(i,j,nt_fbri)*vicen(i,j) > c0 ) then 
                  trcrn(i,j,it) = atrcrn(ij,it) / (trcrn(i,j,nt_fbri)*vicen(i,j)) 
               else
                  trcrn(i,j,it) = c0
               endif
            enddo
         endif                  ! trcr_depend
      enddo                     ! ntrcr

      end subroutine compute_tracers

!=======================================================================

! Cleanup subroutine that rebins thickness categories if necessary,
!  eliminates very small ice areas while conserving mass and energy, 
!  aggregates state variables, and does a boundary call.  
! It is a good idea to call this subroutine after the thermodynamics
!  (thermo_vertical/thermo_itd) and again after the dynamics 
!  (evp/transport/ridging).
!
! author: William H. Lipscomb, LANL

      subroutine cleanup_itd (nx_block,    ny_block,   &
                              ilo, ihi,    jlo, jhi,   &
                              dt,          ntrcr,      &
                              aicen,       trcrn,      &
                              vicen,       vsnon,      &
                              aice0,       aice,       &
                              trcr_depend, fpond,      &
                              fresh,                   &
                              fsalt,       fhocn,      &
                              faero_ocn,   tr_aero,    &
                              tr_pond_topo,            &
                              heat_capacity,           &
                              nbtrcr,      first_ice,  &
                              flux_bio,                &
                              l_stop,                  &
                              istop,         jstop,    &
                              limit_aice_in)

      integer (kind=int_kind), intent(in) :: & 
         nx_block, ny_block, & ! block dimensions 
         ilo,ihi,jlo,jhi   , & ! beginning and end of physical domain
         ntrcr                 ! number of tracers in use
 
      real (kind=dbl_kind), intent(in) :: & 
         dt        ! time step 
 
      real (kind=dbl_kind), dimension (nx_block,ny_block,ncat),  &
         intent(inout) :: & 
         aicen , & ! concentration of ice 
         vicen , & ! volume per unit area of ice          (m) 
         vsnon     ! volume per unit area of snow         (m) 
 
      real (kind=dbl_kind), dimension (nx_block,ny_block,ntrcr,ncat),  &
         intent(inout) :: & 
         trcrn     ! ice tracers 

      real (kind=dbl_kind), dimension (nx_block,ny_block),  &
         intent(inout) :: & 
         aice  , & ! total ice concentration
         aice0     ! concentration of open water 
     
      integer (kind=int_kind), dimension(ntrcr), intent(in) :: & 
         trcr_depend  ! tracer dependency information

      logical (kind=log_kind), intent(in) :: &
         tr_aero,      & ! aerosol flag
         tr_pond_topo, & ! topo pond flag
         heat_capacity   ! if false, ice and snow have zero heat capacity

      logical (kind=log_kind), dimension(nx_block,ny_block,ncat),intent(inout) :: &
         first_ice   ! For bgc and S tracers. set to true if zapping ice.

      logical (kind=log_kind), intent(out) :: &
         l_stop    ! if true, abort on return

      integer (kind=int_kind), intent(out) :: &
         istop, jstop ! indices of grid cell where model aborts

      ! ice-ocean fluxes (required for strict conservation)

      integer (kind=int_kind), intent(in) :: &
         nbtrcr       ! number of bgc tracers
      
      real (kind=dbl_kind), dimension (nx_block,ny_block), &
         intent(inout), optional :: &
         fpond    , & ! fresh water flux to ponds (kg/m^2/s)
         fresh    , & ! fresh water flux to ocean (kg/m^2/s)
         fsalt    , & ! salt flux to ocean        (kg/m^2/s)
         fhocn        ! net heat flux to ocean     (W/m^2)

      real (kind=dbl_kind), dimension (nx_block,ny_block,nbtrcr), &
         intent(inout), optional :: &
         flux_bio     ! net tracer flux to ocean from biology (mmol/m^2/s)

      real (kind=dbl_kind), dimension (nx_block,ny_block,max_aero), &
         intent(inout), optional :: &
         faero_ocn    ! aerosol flux to ocean     (kg/m^2/s)

      logical (kind=log_kind), intent(in), optional ::   &
         limit_aice_in      ! if false, allow aice to be out of bounds
                            ! may want to allow this for unit tests

      ! local variables

      integer (kind=int_kind) :: &
         i, j             , & ! horizontal indices
         n                , & ! category index
         icells               ! number of grid cells with ice

       integer (kind=int_kind), dimension (nx_block*ny_block) :: &
         indxi, indxj      ! compressed i/j indices

      real (kind=dbl_kind), dimension (nx_block,ny_block) :: &
         dfpond   , & ! zapped pond water flux (kg/m^2/s)
         dfresh   , & ! zapped fresh water flux (kg/m^2/s)
         dfsalt   , & ! zapped salt flux   (kg/m^2/s)
         dfhocn       ! zapped energy flux ( W/m^2)

      real (kind=dbl_kind), dimension (nx_block,ny_block,max_aero) :: &
         dfaero_ocn   ! zapped aerosol flux   (kg/m^2/s)

      logical (kind=log_kind) ::   &
         limit_aice         ! if true, check for aice out of bounds

      !-----------------------------------------------------------------
      ! Initialize
      !-----------------------------------------------------------------

      if (present(limit_aice_in)) then
         limit_aice = limit_aice_in
      else
         limit_aice = .true.
      endif

      l_stop = .false.
      istop = 0
      jstop = 0

      dfpond(:,:) = c0
      dfresh(:,:) = c0
      dfsalt(:,:) = c0
      dfhocn(:,:) = c0
      dfaero_ocn(:,:,:) = c0

      !-----------------------------------------------------------------
      ! Compute total ice area.
      !-----------------------------------------------------------------

      call aggregate_area (nx_block, ny_block, &
                           aicen, &
                           aice,     aice0)

      if (limit_aice) then  ! check for aice out of bounds
      
         do j = jlo,jhi
         do i = ilo,ihi
            if (aice(i,j) > c1+puny .or. aice(i,j) < -puny) then
               l_stop = .true.
               istop = i
               jstop = j
            endif
         enddo
         enddo

         if (l_stop) then      ! area out of bounds
            i = istop
            j = jstop
            write(nu_diag,*) ' '
            write(nu_diag,*) 'aggregate ice area out of bounds'
            write(nu_diag,*) 'my_task, i, j, aice:', &
                              my_task, i, j, aice(i,j)
            do n = 1, ncat
               write(nu_diag,*) 'n, aicen:', n, aicen(i,j,n)
            enddo
            return
         endif                  ! l_stop
      endif                     ! limit_aice

      !-----------------------------------------------------------------
      ! Identify grid cells with ice.
      !-----------------------------------------------------------------

      icells = 0
      do j = jlo,jhi
      do i = ilo,ihi
         if (aice(i,j) > puny) then
            icells = icells + 1
            indxi(icells) = i
            indxj(icells) = j
         endif
      enddo
      enddo

      !-----------------------------------------------------------------
      ! Make sure ice in each category is within its thickness bounds.
      ! NOTE: The rebin subroutine is needed only in the rare cases
      !       when the linear_itd subroutine cannot transfer ice
      !       correctly (e.g., very fast ice growth).
      !-----------------------------------------------------------------

      call rebin (nx_block,   ny_block,     &
                  icells,     indxi, indxj, &
                  ntrcr,      trcr_depend,  &
                  aicen,      trcrn,        &
                  vicen,      vsnon,        &
                  l_stop,                   &
                  istop,      jstop)

      if (l_stop) return

      !-----------------------------------------------------------------
      ! Zero out ice categories with very small areas.
      !-----------------------------------------------------------------

      if (limit_aice) then
         call zap_small_areas (nx_block, ny_block,  &
                               ilo, ihi, jlo, jhi,  &
                               dt,       ntrcr,     &
                               aice,     aice0,     &
                               aicen,    trcrn,     &
                               vicen,    vsnon,     &
                               dfpond,              &
                               dfresh,   dfsalt,    &
                               dfhocn,   dfaero_ocn,&
                               tr_aero,  tr_pond_topo, &
                               first_ice,nbtrcr,    &
                               flux_bio, l_stop,    &
                               istop,    jstop)
         if (l_stop) return
      endif   ! l_limit_aice

    !-------------------------------------------------------------------
    ! Zap snow that has out of bounds temperatures
    !-------------------------------------------------------------------

      call zap_snow_temperature(nx_block,   ny_block, &
                                ilo, ihi, jlo, jhi,   &
                                dt,         ntrcr,    &
                                aicen,                &
                                trcrn,      vsnon,    &
                                dfresh,     dfhocn,   &
                                dfaero_ocn, tr_aero)

    !-------------------------------------------------------------------
    ! Update ice-ocean fluxes for strict conservation
    !-------------------------------------------------------------------

      if (present(fpond)) &
           fpond     (:,:)   = fpond(:,:)       + dfpond(:,:) 
      if (present(fresh)) &
           fresh     (:,:)   = fresh(:,:)       + dfresh(:,:) 
      if (present(fsalt)) &
           fsalt     (:,:)   = fsalt(:,:)       + dfsalt(:,:)
      if (present(fhocn)) &
           fhocn     (:,:)   = fhocn(:,:)       + dfhocn(:,:)
      if (present(faero_ocn)) &
           faero_ocn (:,:,:) = faero_ocn(:,:,:) + dfaero_ocn(:,:,:)

      !----------------------------------------------------------------
      ! If using zero-layer model (no heat capacity), check that the 
      ! energy of snow and ice is correct. 
      !----------------------------------------------------------------

      if (.not. heat_capacity) then

         call zerolayer_check(nx_block,    ny_block,   &
                              ntrcr,                   &
                              icells,  indxi,   indxj, &
                              aicen,                   &
                              vicen,       vsnon,      &
                              trcrn,       l_stop,     &
                              istop,       jstop)

      endif

      end subroutine cleanup_itd

!=======================================================================

! For each ice category in each grid cell, remove ice if the fractional
! area is less than puny.
!
! author: William H. Lipscomb, LANL

      subroutine zap_small_areas (nx_block, ny_block,   &
                                  ilo, ihi, jlo, jhi,  &
                                  dt,       ntrcr,     &
                                  aice,     aice0,     &
                                  aicen,    trcrn,     &
                                  vicen,    vsnon,     &
                                  dfpond,              &
                                  dfresh,   dfsalt,    &
                                  dfhocn,   dfaero_ocn,&
                                  tr_aero,  tr_pond_topo, &
                                  first_ice,nbtrcr,    &
                                  flux_bio, l_stop,    &
                                  istop,    jstop)

      use ice_state, only: nt_Tsfc, nt_qice, nt_qsno, nt_aero, nt_apnd, nt_hpnd, &
                           nt_fbri, tr_brine

      integer (kind=int_kind), intent(in) :: &
         nx_block, ny_block, & ! block dimensions
         ilo,ihi,jlo,jhi   , & ! beginning and end of physical domain
         ntrcr           , &   ! number of tracers in use
         nbtrcr                ! number of biology tracers

      real (kind=dbl_kind), intent(in) :: &
         dt                    ! time step

      real (kind=dbl_kind), dimension (nx_block,ny_block), &
         intent(inout) :: &
         aice     , & ! total ice concentration
         aice0        ! concentration of open water

      real (kind=dbl_kind), dimension(nx_block,ny_block,ncat), &
         intent(inout) :: &
         aicen    , & ! concentration of ice
         vicen    , & ! volume per unit area of ice          (m)
         vsnon        ! volume per unit area of snow         (m)

      real (kind=dbl_kind), dimension (nx_block,ny_block,ntrcr,ncat), &
         intent(inout) :: &
         trcrn        ! ice tracers

      real (kind=dbl_kind), dimension (nx_block,ny_block), &
         intent(out) :: &
         dfpond   , & ! zapped pond water flux (kg/m^2/s)
         dfresh   , & ! zapped fresh water flux (kg/m^2/s)
         dfsalt   , & ! zapped salt flux   (kg/m^2/s)
         dfhocn       ! zapped energy flux ( W/m^2)

     real (kind=dbl_kind), dimension (nx_block,ny_block,nbtrcr), &
         intent(inout), optional :: &
         flux_bio     ! Ocean tracer flux from biology (mmol/m^2/s)

      real (kind=dbl_kind), dimension (nx_block,ny_block,max_aero), &
         intent(out) :: &
         dfaero_ocn   ! zapped aerosol flux   (kg/m^2/s)

      logical (kind=log_kind), intent(in) :: &
         tr_aero, &   ! aerosol flag
         tr_pond_topo ! pond flag

      logical (kind=log_kind), dimension (nx_block,ny_block,ncat),intent(inout) :: &
         first_ice   ! For bgc tracers.  Set to  true if zapping ice 

      logical (kind=log_kind), intent(out) :: &
         l_stop       ! if true, abort on return

      integer (kind=int_kind), intent(out) :: &
         istop, jstop ! indices of grid cell where model aborts

      ! local variables

      integer (kind=int_kind) :: &
         i,j, n, k, it  , & ! counting indices
         icells         , & ! number of cells with ice to zap
         ij                 ! combined i/j horizontal index

      integer (kind=int_kind), dimension (nx_block*ny_block) :: &
        indxi       , & ! compressed indices for i/j directions
        indxj

      real (kind=dbl_kind) :: xtmp     ! temporary variable

      !-----------------------------------------------------------------
      ! Initialize
      !-----------------------------------------------------------------

      l_stop = .false.
      istop = 0
      jstop = 0

      !-----------------------------------------------------------------
      ! I. Zap categories with very small areas.
      !-----------------------------------------------------------------

      do n = 1, ncat

      !-----------------------------------------------------------------
      ! Count categories to be zapped.
      !-----------------------------------------------------------------

         icells = 0
         do j = jlo, jhi
         do i = ilo, ihi
            if (aicen(i,j,n) < -puny) then
               write (nu_diag,*) 'Zap ice: negative ice area'
               write (nu_diag,*) 'i, j, n, aicen =', &
                                  i, j, n, aicen(i,j,n)
               l_stop = .true.
               istop = i
               jstop = j
               return
            elseif (abs(aicen(i,j,n)) /= c0 .and. &
                    abs(aicen(i,j,n)) <= puny) then
               icells = icells + 1
               indxi(icells) = i
               indxj(icells) = j
            endif
         enddo
         enddo

         if (icells > 0) then

      !-----------------------------------------------------------------
      ! Account for tracers important for conservation
      !-----------------------------------------------------------------

         if (tr_pond_topo) then
!DIR$ CONCURRENT !Cray
!cdir nodep      !NEC
!ocl novrec      !Fujitsu
            do ij = 1, icells
               i = indxi(ij)
               j = indxj(ij)
               xtmp = aicen(i,j,n) &
                    * trcrn(i,j,nt_apnd,n) * trcrn(i,j,nt_hpnd,n)
               dfpond(i,j) = dfpond(i,j) - xtmp
            enddo                  ! ij
         endif

         if (tr_aero) then
!DIR$ CONCURRENT !Cray
!cdir nodep      !NEC
!ocl novrec      !Fujitsu
            do ij = 1, icells
               i = indxi(ij)
               j = indxj(ij)
               do it = 1, n_aero
                  xtmp = (vicen(i,j,n)*(trcrn(i,j,nt_aero+2+4*(it-1),n)     &
                                      + trcrn(i,j,nt_aero+3+4*(it-1),n)))/dt
                  dfaero_ocn(i,j,it) = dfaero_ocn(i,j,it) + xtmp
               enddo                 ! n
            enddo                  ! ij
         endif

      !-----------------------------------------------------------------
      ! Zap ice energy and use ocean heat to melt ice
      !-----------------------------------------------------------------

         do k = 1, nilyr
!DIR$ CONCURRENT !Cray
!cdir nodep      !NEC
!ocl novrec      !Fujitsu
            do ij = 1, icells
               i = indxi(ij)
               j = indxj(ij)

               xtmp = trcrn(i,j,nt_qice+k-1,n) / dt &
                    * vicen(i,j,n)/real(nilyr,kind=dbl_kind) ! < 0
               dfhocn(i,j) = dfhocn(i,j) + xtmp
               trcrn(i,j,nt_qice+k-1,n) = c0

            enddo               ! ij
         enddo                  ! k

      !-----------------------------------------------------------------
      ! Zap ice and snow volume, add water and salt to ocean
      !-----------------------------------------------------------------

!DIR$ CONCURRENT !Cray
!cdir nodep      !NEC
!ocl novrec      !Fujitsu
         do ij = 1, icells
            i = indxi(ij)
            j = indxj(ij)

            xtmp = (rhoi*vicen(i,j,n)) / dt
            dfresh(i,j) = dfresh(i,j) + xtmp

            xtmp = rhoi*vicen(i,j,n)*ice_ref_salinity*p001 / dt
            dfsalt(i,j) = dfsalt(i,j) + xtmp

            aice0(i,j) = aice0(i,j) + aicen(i,j,n)
            aicen(i,j,n) = c0
            vicen(i,j,n) = c0
            trcrn(i,j,nt_Tsfc,n) = Tocnfrz
         enddo                  ! ij

      !-----------------------------------------------------------------
      ! Zap snow
      !-----------------------------------------------------------------

         call zap_snow(nx_block,       ny_block,     &
                       icells,                       &
                       indxi,          indxj,        &
                       dt,             ntrcr,        &
                       trcrn(:,:,:,n), vsnon(:,:,n), &
                       dfresh,         dfhocn,       &
                       dfaero_ocn,     tr_aero)

      !-----------------------------------------------------------------
      ! Zap tracers
      !-----------------------------------------------------------------
         
         if (ntrcr >= 2) then
            do it = 2, ntrcr
               if (tr_brine .and. it == nt_fbri) then
                  do ij = 1, icells
                     i = indxi(ij)
                     j = indxj(ij)
                     trcrn(i,j,it,n) = c1
                  enddo
               else
                  do ij = 1, icells
                     i = indxi(ij)
                     j = indxj(ij)
                     trcrn(i,j,it,n) = c0
                  enddo               
               endif
            enddo
         endif
         do ij = 1, icells
             i = indxi(ij)
             j = indxj(ij)
             first_ice(i,j,n) = .true.
         enddo

      endif ! icells
      enddo                     ! n

      !-----------------------------------------------------------------
      ! II. Count cells with excess ice (aice > c1) due to roundoff errors.
      !     Zap a little ice in each category so that aice = c1.
      !-----------------------------------------------------------------

      icells = 0
      do j = jlo, jhi
      do i = ilo, ihi
         if (aice(i,j) > (c1+puny)) then
            write (nu_diag,*) 'Zap ice: excess ice area'
            write (nu_diag,*) 'i, j, aice =', &
                               i, j, aice(i,j)
            l_stop = .true.
            istop = i
            jstop = j
            return
         elseif (aice(i,j) > c1 .and. aice(i,j) < (c1+puny)) then
            icells = icells + 1
            indxi(icells) = i
            indxj(icells) = j
         endif
      enddo
      enddo

      if (icells > 0) then

      do n = 1, ncat

      !-----------------------------------------------------------------
      ! Account for tracers important for conservation
      !-----------------------------------------------------------------

         if (tr_pond_topo) then
!DIR$ CONCURRENT !Cray
!cdir nodep      !NEC
!ocl novrec      !Fujitsu
            do ij = 1, icells
               i = indxi(ij)
               j = indxj(ij)
               xtmp = aicen(i,j,n) &
                    * trcrn(i,j,nt_apnd,n) * trcrn(i,j,nt_hpnd,n) &
                    * (aice(i,j)-c1)/aice(i,j)
               dfpond(i,j) = dfpond(i,j) - xtmp
            enddo                  ! ij
         endif

         if (tr_aero) then
!DIR$ CONCURRENT !Cray
!cdir nodep      !NEC
!ocl novrec      !Fujitsu
            do ij = 1, icells
               i = indxi(ij)
               j = indxj(ij)
               do it = 1, n_aero
                  xtmp = (vsnon(i,j,n)*(trcrn(i,j,nt_aero  +4*(it-1),n)     &
                                      + trcrn(i,j,nt_aero+1+4*(it-1),n))    &
                       +  vicen(i,j,n)*(trcrn(i,j,nt_aero+2+4*(it-1),n)     &
                                      + trcrn(i,j,nt_aero+3+4*(it-1),n)))   &
                       * (aice(i,j)-c1)/aice(i,j) / dt
                  dfaero_ocn(i,j,it) = dfaero_ocn(i,j,it) + xtmp
               enddo               ! it
            enddo                  ! ij
         endif

      !----------------------------------------------------------------- 
      ! Zap ice energy and use ocean heat to melt ice 
      !----------------------------------------------------------------- 
       
         do k = 1, nilyr 
!DIR$ CONCURRENT !Cray 
!cdir nodep      !NEC 
!ocl novrec      !Fujitsu 
            do ij = 1, icells 
               i = indxi(ij) 
               j = indxj(ij) 
 
               xtmp = trcrn(i,j,nt_qice+k-1,n) &
                    * vicen(i,j,n)/real(nilyr,kind=dbl_kind) &
                    * (aice(i,j)-c1)/aice(i,j) / dt ! < 0 
               dfhocn(i,j) = dfhocn(i,j) + xtmp 
 
            enddo               ! ij 
         enddo                  ! k 
 
      !----------------------------------------------------------------- 
      ! Zap snow energy and use ocean heat to melt snow 
      !----------------------------------------------------------------- 

         do k = 1, nslyr 
!DIR$ CONCURRENT !Cray 
!cdir nodep      !NEC 
!ocl novrec      !Fujitsu
            do ij = 1, icells
               i = indxi(ij) 
               j = indxj(ij) 
 
               xtmp = trcrn(i,j,nt_qsno+k-1,n) &
                    * vsnon(i,j,n)/real(nslyr,kind=dbl_kind) &
                    * (aice(i,j)-c1)/aice(i,j) / dt ! < 0 
               dfhocn(i,j) = dfhocn(i,j) + xtmp 
 
            enddo               ! ij
         enddo                  ! k
 
      !-----------------------------------------------------------------
      ! Zap ice and snow volume, add water and salt to ocean
      !-----------------------------------------------------------------

!DIR$ CONCURRENT !Cray 
!cdir nodep      !NEC 
!ocl novrec      !Fujitsu 
         do ij = 1, icells 
            i = indxi(ij) 
            j = indxj(ij) 
 
            xtmp = (rhoi*vicen(i,j,n) + rhos*vsnon(i,j,n)) &
                 * (aice(i,j)-c1)/aice(i,j) / dt 
            dfresh(i,j) = dfresh(i,j) + xtmp 
 
            xtmp = rhoi*vicen(i,j,n)*ice_ref_salinity*p001 &
                 * (aice(i,j)-c1)/aice(i,j) / dt
            dfsalt(i,j) = dfsalt(i,j) + xtmp 
 
            aicen(i,j,n) = aicen(i,j,n) * (c1/aice(i,j)) 
            vicen(i,j,n) = vicen(i,j,n) * (c1/aice(i,j)) 
            vsnon(i,j,n) = vsnon(i,j,n) * (c1/aice(i,j))
 
         enddo                  ! ij

      ! Note: Tracers are unchanged.

      enddo                     ! n

      !-----------------------------------------------------------------
      ! Correct aice
      !-----------------------------------------------------------------

!DIR$ CONCURRENT !Cray 
!cdir nodep      !NEC 
!ocl novrec      !Fujitsu 
      do ij = 1, icells 
         i = indxi(ij) 
         j = indxj(ij) 
         aice(i,j) = c1
         aice0(i,j) = c0
      enddo

      endif ! icells

      end subroutine zap_small_areas

!=======================================================================

      subroutine zap_snow(nx_block,   ny_block, &
                          icells,               &
                          indxi,      indxj,    &
                          dt,         ntrcr,    &
                          trcrn,      vsnon,    &
                          dfresh,     dfhocn,   &
                          dfaero_ocn, tr_aero)

      use ice_state, only: nt_qsno, nt_aero

      integer (kind=int_kind), intent(in) :: &
         nx_block, ny_block, & ! block dimensions
         icells            , & ! number of ice/ocean grid cells
         ntrcr                 ! number of tracers in use
 
      real (kind=dbl_kind), intent(in) :: &
         dt           ! time step

      integer (kind=int_kind), dimension (nx_block*ny_block), &
         intent(in) :: &
         indxi,  indxj       ! compressed i/j indices

      real (kind=dbl_kind), dimension(nx_block,ny_block), &
         intent(inout) :: &
         vsnon        ! volume per unit area of snow         (m)

      real (kind=dbl_kind), dimension (nx_block,ny_block,ntrcr), &
         intent(inout) :: &
         trcrn        ! ice tracers

      real (kind=dbl_kind), dimension (nx_block,ny_block), &
         intent(inout) :: &
         dfresh   , & ! zapped fresh water flux (kg/m^2/s)
         dfhocn       ! zapped energy flux ( W/m^2)

      real (kind=dbl_kind), dimension (nx_block,ny_block,max_aero), &
         intent(inout) :: &
         dfaero_ocn   ! zapped aerosol flux   (kg/m^2/s)

      logical (kind=log_kind), intent(in) :: &
         tr_aero      ! aerosol flag

      ! local variables

      integer (kind=int_kind) :: &
         i,j, k, it  , & ! counting indices
         ij                 ! combined i/j horizontal index

      real (kind=dbl_kind) :: xtmp

      ! aerosols
      if (tr_aero) then
!DIR$ CONCURRENT !Cray 
!cdir nodep      !NEC 
!ocl novrec      !Fujitsu
         do ij = 1, icells
            i = indxi(ij)
            j = indxj(ij)

            do it = 1, n_aero
               xtmp = (vsnon(i,j)*(trcrn(i,j,nt_aero  +4*(it-1))     &
                                 + trcrn(i,j,nt_aero+1+4*(it-1))))/dt
               dfaero_ocn(i,j,it) = dfaero_ocn(i,j,it) + xtmp
            enddo                 ! it

         enddo               ! ij

      endif ! tr_aero

      ! snow enthalpy tracer
      do k = 1, nslyr 
!DIR$ CONCURRENT !Cray 
!cdir nodep      !NEC 
!ocl novrec      !Fujitsu
         do ij = 1, icells
            i = indxi(ij)
            j = indxj(ij)

            xtmp = trcrn(i,j,nt_qsno+k-1) / dt &
                 * vsnon(i,j)/real(nslyr,kind=dbl_kind) ! < 0
            dfhocn(i,j) = dfhocn(i,j) + xtmp
            trcrn(i,j,nt_qsno+k-1) = c0

         enddo               ! ij
      enddo                  ! k

      ! snow volume
!DIR$ CONCURRENT !Cray 
!cdir nodep      !NEC 
!ocl novrec      !Fujitsu
      do ij = 1, icells
         i = indxi(ij)
         j = indxj(ij)

         xtmp = (rhos*vsnon(i,j)) / dt
         dfresh(i,j) = dfresh(i,j) + xtmp
         vsnon(i,j) = c0

      enddo               ! ij

      end subroutine zap_snow

!=======================================================================
   
      subroutine zap_snow_temperature(nx_block,   ny_block, &
                                      ilo, ihi, jlo, jhi,   &
                                      dt,         ntrcr,    &
                                      aicen,                &
                                      trcrn,      vsnon,    &
                                      dfresh,     dfhocn,   &
                                      dfaero_ocn, tr_aero)

      use ice_state, only: nt_qsno 
      use ice_therm_shared, only: heat_capacity, Tmin
      use ice_calendar, only: istep1

      integer (kind=int_kind), intent(in) :: &
         nx_block, ny_block, & ! block dimensions
         ilo,ihi,jlo,jhi   , & ! beginning and end of physical domain
         ntrcr                 ! number of tracers in use

      real (kind=dbl_kind), intent(in) :: &
         dt           ! time step

      real (kind=dbl_kind), dimension (nx_block,ny_block,ncat),  &
         intent(in) :: & 
         aicen        ! concentration of ice 

      real (kind=dbl_kind), dimension(nx_block,ny_block,ncat), &
         intent(inout) :: &
         vsnon        ! volume per unit area of snow         (m)

      real (kind=dbl_kind), dimension (nx_block,ny_block,ntrcr,ncat), &
         intent(inout) :: &
         trcrn        ! ice tracers

      real (kind=dbl_kind), dimension (nx_block,ny_block), &
         intent(inout) :: &
         dfresh   , & ! zapped fresh water flux (kg/m^2/s)
         dfhocn       ! zapped energy flux ( W/m^2)

      real (kind=dbl_kind), dimension (nx_block,ny_block,max_aero), &
         intent(inout) :: &
         dfaero_ocn   ! zapped aerosol flux   (kg/m^2/s)

      logical (kind=log_kind), intent(in) :: &
         tr_aero      ! aerosol flag

      ! local variables

      integer (kind=int_kind) :: &
         i,j, n, k, it , & ! counting indices
         icells        , & ! number of cells with ice to zap
         ij                 ! combined i/j horizontal index

      integer (kind=int_kind), dimension (nx_block*ny_block) :: &
         indxi         , & ! compressed indices for i/j directions
         indxj

      real (kind=dbl_kind) :: &
         rnslyr        , & ! real(nslyr)
         hsn           , & ! snow thickness (m)
         zqsn          , & ! snow layer enthalpy (J m-2)
         zTsn          , & ! snow layer temperature (C)
         Tmax              ! maximum allowed snow temperature

      logical :: &
         l_zap             ! logical whether zap snow

!jd-metroms
      integer (kind=int_kind) :: nzaps
      nzaps=0
!jd-metroms

      rnslyr = real(nslyr,kind=dbl_kind)
      
      do n = 1, ncat

      !-----------------------------------------------------------------
      ! Determine cells to zap
      !-----------------------------------------------------------------

         icells = 0
         do j = jlo, jhi
         do i = ilo, ihi

            l_zap = .false.

            if (aicen(i,j,n) > puny) then

            ! snow thickness
            hsn = vsnon(i,j,n) / aicen(i,j,n)

            ! check each snow layer - zap all if one is bad
            do k = 1, nslyr

               ! snow enthalpy and max temperature
               if (hsn > hs_min .and. heat_capacity) then
                  ! zqsn < 0              
                  zqsn = trcrn(i,j,nt_qsno+k-1,n)
                  Tmax = -zqsn*puny*rnslyr / &
                       (rhos*cp_ice*vsnon(i,j,n))
               else
                  zqsn = -rhos * Lfresh
                  Tmax = puny
               endif
                     
               ! snow temperature
               zTsn = (Lfresh + zqsn/rhos)/cp_ice

               ! check for zapping
               if (zTsn < Tmin .or. zTsn > Tmax) then
                  l_zap = .true.
!jd                  write(nu_diag,*) "zap_snow_temperature: temperature out of bounds!"
!jd                  write(nu_diag,*) "istep1, my_task, i, j, k:", istep1, my_task, i, j, k
!jd                  write(nu_diag,*) "zTsn:", zTsn
!jd                  write(nu_diag,*) "Tmin:", Tmin
!jd                  write(nu_diag,*) "Tmax:", Tmax
!jd                  write(nu_diag,*) "zqsn:", zqsn
               endif

            enddo ! k

            endif ! aicen > puny

            ! add cell to zap list
            if (l_zap) then
               icells = icells + 1
               indxi(icells) = i
               indxj(icells) = j
            endif ! l_zap

         enddo ! i
         enddo ! j

      !-----------------------------------------------------------------
      ! Zap the cells
      !-----------------------------------------------------------------

         if (icells > 0) &
         call zap_snow(nx_block,       ny_block,     &
                       icells,                       &
                       indxi,          indxj,        &
                       dt,             ntrcr,        &
                       trcrn(:,:,:,n), vsnon(:,:,n), &
                       dfresh,         dfhocn,       &
                       dfaero_ocn,     tr_aero)

         nzaps=nzaps+icells

        enddo ! n

        if (nzaps > 0) write(nu_diag,*) &
             'zap_snow_temperature :: ',nzaps,' points zaped at proc ',my_task

      end subroutine zap_snow_temperature

!=======================================================================
! Checks that the snow and ice energy in the zero layer thermodynamics
! model still agrees with the snow and ice volume.
! If there is an error, the model will abort.
! This subroutine is only called if heat_capacity = .false.
!
! author: Alison McLaren, Met Office
!         May 2010:  ECH replaced eicen, esnon with trcrn but did not test 
! the changes.  The loop below runs over n=1,ncat and I added loops 
! over k, making the test more stringent.

      subroutine zerolayer_check (nx_block,    ny_block,   &
                                  ntrcr,                   &
                                  icells,  indxi,   indxj, &
                                  aicen,                   &
                                  vicen,       vsnon,      &
                                  trcrn,       l_stop,     &
                                  istop,       jstop)

      use ice_state, only: nt_qice, nt_qsno

      integer (kind=int_kind), intent(in) :: & 
         nx_block, ny_block, & ! block dimensions 
         ntrcr             , & ! number of tracers in use
         icells                ! number of grid cells with ice

      integer (kind=int_kind), dimension (nx_block*ny_block), &
         intent(in) :: &
         indxi, indxj      ! compressed i/j indices
 
      real (kind=dbl_kind), dimension (nx_block,ny_block,ncat),  &
         intent(inout) :: & 
         aicen , & ! concentration of ice 
         vicen , & ! volume per unit area of ice          (m) 
         vsnon     ! volume per unit area of snow         (m) 

      real (kind=dbl_kind), dimension (nx_block,ny_block,ntrcr,ncat), &
         intent(inout) :: &
         trcrn        ! ice tracers
      
      logical (kind=log_kind), intent(out) :: &
         l_stop    ! if true, abort on return

      integer (kind=int_kind), intent(out) :: &
         istop, jstop ! indices of grid cell where model aborts

      ! local variables

      integer (kind=int_kind) :: &
         i, j, k          , & ! horizontal, vertical indices
         n                , & ! category index
         ij                   ! combined horizontal index

      real (kind=dbl_kind), parameter :: &
         max_error = puny*Lfresh*rhos ! max error in zero layer energy check
                                      ! (so max volume error = puny)

      real (kind=dbl_kind), dimension (nx_block,ny_block,ncat) :: &
         eicen     ! energy of melting for each ice layer (J/m^2) 
 
      real (kind=dbl_kind), dimension (nx_block,ny_block,ncat) :: &
         esnon     ! energy of melting for each snow layer (J/m^2) 

      logical (kind=log_kind) :: &
         ice_energy_correct  , & ! zero layer ice energy check
         snow_energy_correct     ! zero layer snow energy check

      real (kind=dbl_kind), dimension (nx_block,ny_block) :: &
         worka, workb

      !-----------------------------------------------------------------
      ! Initialize
      !-----------------------------------------------------------------

      l_stop = .false.
      istop = 0
      jstop = 0

      worka(:,:) = c0
      workb(:,:) = c0

      !----------------------------------------------------------------
      ! Calculate difference between ice and snow energies and the
      ! energy values derived from the ice and snow volumes
      !----------------------------------------------------------------

      ice_energy_correct  = .true.
      snow_energy_correct = .true.

      do n=1,ncat

         do ij=1,icells
            i=indxi(ij)
            j=indxj(ij)

            eicen(i,j,n) = c0
            do k = 1, nilyr
               eicen(i,j,n) = eicen(i,j,n) + trcrn(i,j,nt_qice+k-1,n) &
                            * vicen(i,j,n) / real(nilyr,kind=dbl_kind)
            enddo
            worka(i,j) = eicen(i,j,n) + rhoi * Lfresh * vicen(i,j,n)
            esnon(i,j,n) = c0
            do k = 1, nslyr
               esnon(i,j,n) = esnon(i,j,n) + trcrn(i,j,nt_qsno+k-1,n) &
                            * vsnon(i,j,n) / real(nslyr,kind=dbl_kind)
            enddo
            workb(i,j) = esnon(i,j,n) + rhos * Lfresh * vsnon(i,j,n)

            if(abs(worka(i,j)) > max_error) then
               ice_energy_correct = .false.
            endif

            if(abs(workb(i,j)) > max_error) then
               snow_energy_correct = .false.
            endif
         enddo

      !----------------------------------------------------------------
      ! If there is a problem, abort with error message
      !----------------------------------------------------------------

         if (.not. ice_energy_correct) then

            do ij=1,icells
               i=indxi(ij)
               j=indxj(ij)

               if(abs(worka(i,j)) > max_error) then
                  write(nu_diag,*) ' '
                  write(nu_diag,*) &
                    'zerolayer check - wrong ice energy'
                  write(nu_diag,*) 'i, j, n:', i,j,n
                  write(nu_diag,*) 'eicen =', eicen(i,j,n)
                  write(nu_diag,*) 'error=',  worka(i,j)
                  write(nu_diag,*) 'vicen =', vicen(i,j,n)
                  write(nu_diag,*) 'aicen =', aicen(i,j,n)
                  l_stop = .true.
                  istop = i
                  jstop = j
               endif
            enddo

         endif
         if (l_stop) return

         if (.not. snow_energy_correct) then

            do ij=1,icells
               i=indxi(ij)
               j=indxj(ij)

               if(abs(workb(i,j)) > max_error) then
                  write(nu_diag,*) ' '
                  write(nu_diag,*) &
                    'zerolayer_check - wrong snow energy'
                  write(nu_diag,*) 'i, j, n:', i,j,n
                  write(nu_diag,*) 'esnon =', esnon(i,j,n)
                  write(nu_diag,*) 'error=',  workb(i,j)
                  write(nu_diag,*) 'vsnon =', vsnon(i,j,n)
                  write(nu_diag,*) 'aicen =', aicen(i,j,n)
                  l_stop = .true.
                  istop = i
                  jstop = j
                  return
               endif
            enddo

         endif

      enddo  ! ncat

      end subroutine zerolayer_check

!=======================================================================

      end module ice_itd

!=======================================================================









