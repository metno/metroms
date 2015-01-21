#include "cppdefs.h"
      MODULE ocean_coupler_mod

#if defined MODEL_COUPLING && defined MCT_LIB
!
!svn $Id: ocean_coupler.F 523 2011-01-05 03:21:38Z arango $
!==================================================== John C. Warner ===
!  Copyright (c) 2002-2011 The ROMS/TOMS Group      Hernan G. Arango   !
!    Licensed under a MIT/X style license                              !
!    See License_ROMS.txt                                              !
!=======================================================================
!                                                                      !
!  This module is used to communicate and exchange data between        !
!  ROMS/TOMS and other coupled model(s)  via the Model Coupling        !
!  Toolkit (MCT), developed at the Argonne National Laboratory.        !
!                                                                      !
!=======================================================================
!
!  Component Model Registry.
!
      USE m_MCTWorld, ONLY : MCTWorld_init => init
      USE m_MCTWorld, ONLY : MCTWorld_clean => clean
!
!  Domain Decomposition Descriptor DataType and associated methods.
!
      USE m_GlobalSegMap, ONLY : GlobalSegMap
      USE m_GlobalSegMap, ONLY : GlobalSegMap_init => init
      USE m_GlobalSegMap, ONLY : GlobalSegMap_lsize => lsize
      USE m_GlobalSegMap, ONLY : GlobalSegMap_clean => clean
      USE m_GlobalSegMap, ONLY : GlobalSegMap_Ordpnts => OrderedPoints
!
!  Field Storage DataType and associated methods.
!
      USE m_AttrVect, ONLY : AttrVect
      USE m_AttrVect, ONLY : AttrVect_init => init
      USE m_AttrVect, ONLY : AttrVect_zero => zero
      USE m_AttrVect, ONLY : AttrVect_lsize => lsize
      USE m_AttrVect, ONLY : AttrVect_clean => clean
      USE m_AttrVect, ONLY : AttrVect_importRAttr => importRAttr
      USE m_AttrVect, ONLY : AttrVect_exportRAttr => exportRAttr
!
!  Intercomponent communications scheduler.
!
      USE m_Router, ONLY : Router
      USE m_Router, ONLY : Router_init => init
      USE m_Router, ONLY : Router_clean => clean
!
!  Intercomponent transfer.
!
      USE m_Transfer, ONLY: MCT_Send => send
      USE m_Transfer, ONLY: MCT_Recv => recv
!
!  Sparse Matrix DataType and associated methods.
!
      USE m_SparseMatrix, ONLY : SparseMatrix
      USE m_SparseMatrix, ONLY : SparseMatrix_init => init
      USE m_SparseMatrix, ONLY : SparseMatrix_importGRowInd =>          &
     &                           importGlobalRowIndices
      USE m_SparseMatrix, ONLY : SparseMatrix_importGColInd =>          &
     &                           importGlobalColumnIndices
      USE m_SparseMatrix, ONLY : SparseMatrix_importMatrixElts =>       &
     &                           importMatrixElements
      USE m_SparseMatrixPlus, ONLY : SparseMatrixPlus
      USE m_SparseMatrixPlus, ONLY : SparseMatrixPlus_init => init
      USE m_SparseMatrixPlus, ONLY : SparseMatrixPlus_clean => clean
!
!  Decompose matrix by row.
!
      USE m_SparseMatrixPlus, ONLY : Xonly
!
!  Matrix-Vector multiply methods.
!
      USE m_MatAttrVectMul, ONLY : MCT_MatVecMul => sMatAvMult
!
      implicit none
!
      PRIVATE

# ifdef SWAN_COUPLING
      PUBLIC :: initialize_ocn2wav_coupling
      PUBLIC :: ocn2wav_coupling
      PUBLIC :: finalize_ocn2wav_coupling
# endif
# ifdef WRF_COUPLING
      PUBLIC :: initialize_ocn2atm_coupling
      PUBLIC :: ocn2atm_coupling
      PUBLIC :: finalize_ocn2atm_coupling
# endif
# ifdef CICE_COUPLING
      PUBLIC :: initialize_ocn2cice_coupling
      PUBLIC :: ocn2cice_coupling
      PUBLIC :: finalize_ocn2cice_coupling
# endif
!
!  Declarations.
!
# ifdef SWAN_COUPLING
      TYPE(AttrVect) :: wav2ocn_AV            ! AttrVect variables
      TYPE(AttrVect) :: ocn2wav_AV
      TYPE(Router)   :: ROMStoSWAN            ! Router variables
# endif
# ifdef WRF_COUPLING
      TYPE(AttrVect) :: atm2ocn_AV            ! AttrVect variables
      TYPE(AttrVect) :: ocn2atm_AV
      TYPE(Router)   :: ROMStoWRF             ! Router variables
# endif
# ifdef CICE_COUPLING
      TYPE(AttrVect) :: cice2ocn_AV            ! AttrVect variables
      TYPE(AttrVect) :: ocn2cice_AV
      TYPE(Router)   :: ROMStoCICE             ! Router variables
# endif
      TYPE(GlobalSegMap) :: GSMapROMS         ! GloabalSegMap variables

      CONTAINS
!************************************************************************
!*  Include model specific communication routines.
!************************************************************************

# ifdef SWAN_COUPLING
#  include "mct_roms_swan.h"
# endif
# ifdef REFDIF_COUPLING
#  include "mct_roms_refdif.h"
# endif
# ifdef WRF_COUPLING
#  include "mct_roms_wrf.h"
# endif
# ifdef CICE_COUPLING
      SUBROUTINE initialize_ocn2cice_coupling (ng, tile)
      USE mod_param
      USE mod_parallel
      USE mod_coupler
      USE mod_forces
      USE mod_kinds
      USE mod_scalars
!
!  Imported variable definitions.
!
      integer, intent(in) :: ng, tile
!
!  Local variable declarations.
!
      integer :: Istr, Iend, Jstr, Jend
      integer :: IstrR, IendR, JstrR, JendR, IstrU, JstrV
      integer :: Asize, Jsize, MyError
      integer :: j, jc, nprocs

      integer, allocatable :: length(:)
      integer, allocatable :: start(:)
!
!-----------------------------------------------------------------------
!  Begin initialization phase.
!-----------------------------------------------------------------------
!
!  Get communicator local rank and size.
!
      WRITE (6,*) ' ROMS Ocean Coupler: mpi_comm_rank'
      CALL mpi_comm_rank (OCN_COMM_WORLD, MyRank, MyError)
      WRITE (6,*) ' ROMS Ocean Coupler: mpi_comm_size'
      CALL mpi_comm_size (OCN_COMM_WORLD, nprocs, MyError)
!
!  Initialize MCT coupled model registry.
!
      WRITE (6,*) ' ROMS Ocean Coupler: calling MCTWorld_init'
      CALL MCTWorld_init (Nmodels, MPI_COMM_WORLD, OCN_COMM_WORLD,      &
     &                    OCNid)
      WRITE (6,*) ' Done: ROMS Ocean Coupler: calling MCTWorld_init'
!
!-----------------------------------------------------------------------
!  Compute lower and upper bounds over a particular domain partition or
!  tile for RHO-, U-, and V-variables. Notice that "set_bounds.h" is
!  not used here because of implementation of periodicity in other
!  models.
!-----------------------------------------------------------------------
!
      Istr=BOUNDS(ng)%Istr(tile)
      Iend=BOUNDS(ng)%Iend(tile)
      Jstr=BOUNDS(ng)%Jstr(tile)
      Jend=BOUNDS(ng)%Jend(tile)
!
      IF (DOMAIN(ng)%Western_Edge(tile)) THEN
        IstrR=BOUNDS(ng)%Istr(tile)-1
      ELSE
        IstrR=BOUNDS(ng)%Istr(tile)
      END IF
      IF (DOMAIN(ng)%Eastern_Edge(tile)) THEN
        IendR=BOUNDS(ng)%Iend(tile)+1
      ELSE
        IendR=BOUNDS(ng)%Iend(tile)
      END IF
      IF (DOMAIN(ng)%Southern_Edge(tile)) THEN
        JstrR=BOUNDS(ng)%Jstr(tile)-1
      ELSE
        JstrR=BOUNDS(ng)%Jstr(tile)
      END IF
      IF (DOMAIN(ng)%Northern_Edge(tile)) THEN
        JendR=BOUNDS(ng)%Jend(tile)+1
      ELSE
        JendR=BOUNDS(ng)%Jend(tile)
      END IF
!
!  Determine start and lengths for domain decomposition.
!
      Jsize=JendR-JstrR+1
      WRITE (6,*) ' ROMS Ocean Coupler: Jsize=', Jsize
      IF (.not.allocated(start)) THEN
        allocate ( start(Jsize) )
      END IF
      IF (.not.allocated(length)) THEN
        allocate ( length(Jsize) )
      END IF
      jc=0
      DO j=JstrR,JendR
        jc=jc+1
        start (jc)=j*(Lm(ng)+2)+IstrR+1
        length(jc)=(IendR-IstrR+1)
      END DO
      WRITE (6,*) ' ROMS Ocean Coupler: calling GlobalSegMap_init'
      CALL GlobalSegMap_init (GSMapROMS, start, length, 0,              &
     &     OCN_COMM_WORLD, OCNid)
!
!  Initialize attribute vector holding the export data code strings of
!  the cice model. The Asize is the number of grid point on this
!  processor.
!
      Asize=GlobalSegMap_lsize(GSMapROMS, OCN_COMM_WORLD)

      WRITE (6,*) ' ROMS Ocean Coupler: calling AttrVect_init. Asize=', &
     &     Asize
      write(6,*) 'cice2ocn_AV, Exportlist ', trim(ExportList(Icice))
      CALL AttrVect_init (cice2ocn_AV, rList=TRIM(ExportList(Icice)),   &
     &     lsize=Asize)
!
!  Initialize attribute vector holding the export data code string of
!  the ocean model.
!

      write(6,*) 'ocn2cice_AV, Exportlist ', trim(ExportList(Iocean))
      CALL AttrVect_init (ocn2cice_AV, rList=TRIM(ExportList(Iocean)),  &
     &     lsize=Asize)
      CALL AttrVect_zero (ocn2cice_AV)

      WRITE (6,*) ' ROMS Ocean Coupler: calling Router_init'
      CALL Router_init (CICEid, GSMapROMS, OCN_COMM_WORLD, ROMStoCICE)
      WRITE (6,*) ' ROMS Ocean Coupler: calling Router_init. Done'
!
!  Deallocate working arrays.
!
      IF (allocated(start)) THEN
        deallocate (start)
      END IF
      IF (allocated(length)) THEN
        deallocate (length)
      END IF

      RETURN
      END SUBROUTINE initialize_ocn2cice_coupling

      SUBROUTINE ocn2cice_coupling (ng, tile, ncouple)
      USE mod_param
      USE mod_parallel
      USE mod_coupler
      USE mod_forces
      USE mod_ocean
      USE mod_scalars
      USE mod_stepping
      USE mod_iounits
      USE mod_grid
!      USE distribute_mod, ONLY : mp_reduce
      USE ROMS_import_mod, ONLY : ROMS_import2d
      USE ROMS_export_mod, ONLY : ROMS_export2d
#  ifdef LMD_SKPP
      USE mod_mixing
#  endif
      USE bc_2d_mod
# ifdef DISTRIBUTE
      USE mp_exchange_mod, ONLY : mp_exchange2d
# endif
      USE mod_ice
!jd
      USE frazil_ice_prod_mod, only : t_freeze
!jd

      implicit none

!
!  Imported variable declarations.
!
      integer, intent(in) :: ng, tile, ncouple
!
!  Local variable declarations.
!
      integer :: Istr, Iend, Jstr, Jend
      integer :: IstrR, IendR, JstrR, JendR, IstrU, JstrV
      integer :: Asize, Iimport, Iexport, MyError
      integer :: gtype, i, id, ifield, ij, j, status

      real(r8) :: add_offset, scale

      real(r8), pointer :: A(:)

      ! intermediates for ice stress interpolation
      real(r8) :: aice_psip, aice_psi, sap, sa 

      character (len=3 ), dimension(2) :: op_handle
      character (len=40) :: code


      integer :: nbotu, nbotv, k
      integer, dimension(:,:), allocatable :: nbot
      real(r8), dimension(:,:), allocatable :: uw
      real(r8), dimension(:,:), allocatable :: vw
      real(r8), dimension(:,:), allocatable :: uwater
      real(r8), dimension(:,:), allocatable :: vwater

      real(r8) :: mlio
      real(r8) :: dml
      real(r8) :: totml

#include "tile.h"

      allocate(nbot(IminS:ImaxS,JminS:JmaxS))
      allocate(uw(IminS:ImaxS,JminS:JmaxS))
      allocate(vw(IminS:ImaxS,JminS:JmaxS))
      allocate(uwater(LBi:UBi,LBj:UBj))
      allocate(vwater(LBi:UBi,LBj:UBj))

!      uw=0.0_r8
!      vw=0.0_r8
!      uwater=0.0_r8
!      vwater=0.0_r8


      IF (Master) THEN
          write(6,*) ''
          write(6,*) '   ***************************************** '
          write(6,*) &
          '    Ocean - CICE: coupling routine called from ROMS'
          write(6,*) &
          '    Time : ', time_code, time
          write(6,*) '   ****************************************  '

          write(6,*) ''
      END IF

!
!-----------------------------------------------------------------------
!  Compute lower and upper bounds over a particular domain partition or
!  tile for RHO-, U-, and V-variables. Notice that "set_bounds.h" is
!  not used here because of implementation of periodicity in other
!  models.
!-----------------------------------------------------------------------
!
      Istr=BOUNDS(ng)%Istr(tile)
      Iend=BOUNDS(ng)%Iend(tile)
      Jstr=BOUNDS(ng)%Jstr(tile)
      Jend=BOUNDS(ng)%Jend(tile)
      IstrU=BOUNDS(ng)%IstrU(tile)
      JstrV=BOUNDS(ng)%JstrV(tile)

!
      IF (DOMAIN(ng)%Western_Edge(tile)) THEN
        IstrR=BOUNDS(ng)%Istr(tile)-1
      ELSE
        IstrR=BOUNDS(ng)%Istr(tile)
      END IF
      IF (DOMAIN(ng)%Eastern_Edge(tile)) THEN
        IendR=BOUNDS(ng)%Iend(tile)+1
      ELSE
        IendR=BOUNDS(ng)%Iend(tile)
      END IF
      IF (DOMAIN(ng)%Southern_Edge(tile)) THEN
        JstrR=BOUNDS(ng)%Jstr(tile)-1
      ELSE
        JstrR=BOUNDS(ng)%Jstr(tile)
      END IF
      IF (DOMAIN(ng)%Northern_Edge(tile)) THEN
        JendR=BOUNDS(ng)%Jend(tile)+1
      ELSE
        JendR=BOUNDS(ng)%Jend(tile)
      END IF
!
!-----------------------------------------------------------------------
!  Allocate communications array.
!-----------------------------------------------------------------------
!
      Asize=GlobalSegMap_lsize(GSMapROMS, OCN_COMM_WORLD)
      allocate(A(Asize))
      A=0.0_r8

!
!  Schedule receiving fields from cice model.
!
      CALL mpi_comm_rank (OCN_COMM_WORLD, MyRank, MyError)

      CALL MCT_Recv (cice2ocn_AV, ROMStoCICE)
!
!  Receive fields from cice model.
!
      Iimport=0
      DO ifield=1,Nimport(Iocean)
        id=ImportID(Iocean)%val(ifield)
        code=ADJUSTL(Fields(id)%code)
        gtype=Fields(id)%GridType
        scale=Fields(id)%scale
        add_offset=Fields(id)%AddOffset

        SELECT CASE (TRIM(code))

          CASE ('iarr')
  
            CALL AttrVect_exportRAttr (cice2ocn_AV, TRIM(code), A, Asize)
            Iimport=Iimport+1
            scale=1.0_r8
            add_offset=0.0_r8
            CALL ROMS_import2d (ng, tile,                               &
     &                          id, gtype, scale, add_offset,           &
     &                          Asize, A,                               &
     &                          IstrR, IendR, JstrR, JendR,             &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          Fields(id)%ImpMin, Fields(id)%ImpMax,   &
     &                          ICE(ng)%i_arr,                          &
     &                          status)
          do i=LBi+1,UBi-1
          do j=LBj,UBj
            ICE(ng)%i_arr(i,j) = (ICE(ng)%i_arr(i-1,j)+ICE(ng)%i_arr(i+1,j))*0.5
          enddo
          enddo         
          
             CALL mp_exchange2d (ng, tile, iNLM, 1,                     &
     &            LBi, UBi, LBj, UBj,                                   &
     &            NghostPoints, EWperiodic(ng), NSperiodic(ng),         &
     &            ICE(ng)%i_arr)

          CASE ('jarr')
  
            CALL AttrVect_exportRAttr (cice2ocn_AV, TRIM(code), A, Asize)
            Iimport=Iimport+1
            scale=1.0_r8
            add_offset=0.0_r8
            CALL ROMS_import2d (ng, tile,                               &
     &                          id, gtype, scale, add_offset,           &
     &                          Asize, A,                               &
     &                          IstrR, IendR, JstrR, JendR,             &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          Fields(id)%ImpMin, Fields(id)%ImpMax,   &
     &                          ICE(ng)%j_arr,                          &
     &                          status)
          do i=LBi,UBi
          do j=LBj+1,UBj-1
            ICE(ng)%j_arr(i,j) = (ICE(ng)%j_arr(i,j-1)+ICE(ng)%j_arr(i,j+1))*0.5
          enddo
          enddo

             CALL mp_exchange2d (ng, tile, iNLM, 1,                     &
     &            LBi, UBi, LBj, UBj,                                   &
     &            NghostPoints, EWperiodic(ng), NSperiodic(ng),         &
     &            ICE(ng)%j_arr)

          CASE ('ULON')
  
            CALL AttrVect_exportRAttr (cice2ocn_AV, TRIM(code), A, Asize)
            Iimport=Iimport+1
            write(6,*) 'ROMS rank ', MyRank,                            &
     &           ' received ',trim(code),' field (max/min): ',          &
     &           maxval(A),' ',minval(A) 
            scale=1.0_r8
            add_offset=0.0_r8
            CALL ROMS_import2d (ng, tile,                               &
     &                          id, gtype, scale, add_offset,           &
     &                          Asize, A,                               &
     &                          IstrR, IendR, JstrR, JendR,             &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          Fields(id)%ImpMin, Fields(id)%ImpMax,   &
     &                          ICE(ng)%ulon,                           &
     &                          status)


          CASE ('ULAT')

            CALL AttrVect_exportRAttr (cice2ocn_AV, TRIM(code), A, Asize)
            Iimport=Iimport+1
            scale=1.0_r8
            add_offset=0.0_r8
            CALL ROMS_import2d (ng, tile,                               &
     &                          id, gtype, scale, add_offset,           &
     &                          Asize, A,                               &
     &                          IstrR, IendR, JstrR, JendR,             &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          Fields(id)%ImpMin, Fields(id)%ImpMax,   &
     &                          ICE(ng)%ulat,                           &
     &                          status)

         CASE ('AICE')         ! Sea ice concentration

            CALL AttrVect_exportRAttr (cice2ocn_AV, TRIM(code),A,Asize)
            Iimport=Iimport+1
            write(6,*) 'ROMS rank ', MyRank,                            &
     &           ' received ',trim(code),' field (max/min): ',          &
     &           maxval(A),' ',minval(A)
            scale=1.0_r8
            add_offset=0.0_r8
            CALL ROMS_import2d (ng, tile,                               &
     &                          id, gtype, scale, add_offset,           &
     &                          Asize, A,                               &
     &                          IstrR, IendR, JstrR, JendR,             &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          Fields(id)%ImpMin, Fields(id)%ImpMax,   &
     &                          ICE(ng)%a,                              &
     &                          status)


       
          CASE ('freshAI')         ! Fresh water flux

            CALL AttrVect_exportRAttr (cice2ocn_AV, TRIM(code), A, Asize)
            Iimport=Iimport+1
            write(6,*) 'ROMS rank ', MyRank,                            &
     &           ' received ',trim(code),' field (max/min): ',          &
     &           maxval(A),' ',minval(A)

          CASE ('fsaltAI')         ! Salt flux

            CALL AttrVect_exportRAttr (cice2ocn_AV, TRIM(code), A, Asize)
            Iimport=Iimport+1
            write(6,*) 'ROMS rank ', MyRank,                            &
     &           ' received ',trim(code),' field (max/min): ',          &
     &           maxval(A),' ',minval(A)

          CASE ('fhocnAI')         ! Nonradiative heat flux

            CALL AttrVect_exportRAttr (cice2ocn_AV, TRIM(code), A, Asize)
            Iimport=Iimport+1
            write(6,*) 'ROMS rank ', MyRank,                            &
     &           ' received ',trim(code),' field (max/min): ',          &
     &           maxval(A),' ',minval(A)

          CASE ('fswthruAI')         ! Radiative heat flux through ice

            CALL AttrVect_exportRAttr (cice2ocn_AV, TRIM(code), A, Asize)
            Iimport=Iimport+1
            write(6,*) 'ROMS rank ', MyRank,                            &
     &           ' received ',trim(code),' field (max/min): ',          &
     &           maxval(A),' ',minval(A)

          CASE ('strocnx')         ! ice-ocean stress x-dir

            CALL AttrVect_exportRAttr (cice2ocn_AV, TRIM(code), A, Asize)
            Iimport=Iimport+1
            write(6,*) 'ROMS rank ', MyRank,                            &
     & ' received strocnx field (max/min): ', maxval(A), ' ', minval(A)
            scale=1.0_r8
            add_offset=0.0_r8
! Had Istr etc here first, and gtype as r2dvar -> visible processor map
! IstrR and r2dvar worked better. Now trying p2dvar, i.e. psi-point.
! But there's a one grid point offset between CICE and ROMS that bugs me
! CICE U(i,j) = ROMS psi(i-1,j-1). Or so I recon at least :)
            CALL ROMS_import2d (ng, tile,                               &
     &                          id, gtype, scale, add_offset,           &
     &                          Asize, A,                               &
     &                          IstrR, IendR, JstrR, JendR,             &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          Fields(id)%ImpMin, Fields(id)%ImpMax,   &
     &                          ICE(ng)%strx,                           &
     &                          status)
           
            do j = LBj+1,UBj-1
            do i = LBi+2,UBi
! I wonder if all the omn terms are overkill.
! omn = RHO-grid area (meters2).
! ICE(ng)%a are rho-points.

             !ice fraction on u-point
             ICE(ng)%aice_u(i,j) = (ICE(ng)%a(i-1,j)*GRID(ng)%omn(i-1,j)+ICE(ng)%a(i,j)*GRID(ng)%omn(i,j)) / (GRID(ng)%omn(i-1,j)+GRID(ng)%omn(i,j))
             !ice fraction on psi-point (B-grid u(i-1,j-1) point)
             !first i,j+1 (psip), then i,j (psi)
             aice_psip =( ICE(ng)%a(i-1,j)*GRID(ng)%omn(i-1,j)          &
     &                   +ICE(ng)%a(i-1,j+1)*GRID(ng)%omn(i-1,j+1)      &
     &                   +ICE(ng)%a(i,j+1)*GRID(ng)%omn(i,j+1)          &
     &                   +ICE(ng)%a(i,j)*GRID(ng)%omn(i,j))             &
     &                  /(GRID(ng)%omn(i-1,j)+GRID(ng)%omn(i-1,j+1)     &
     &                     +GRID(ng)%omn(i,j+1)+GRID(ng)%omn(i,j))
             aice_psi =( ICE(ng)%a(i-1,j-1)*GRID(ng)%omn(i-1,j-1)       &
     &                   +ICE(ng)%a(i-1,j)*GRID(ng)%omn(i-1,j)          &
     &                   +ICE(ng)%a(i,j)*GRID(ng)%omn(i,j)              &
     &                   +ICE(ng)%a(i,j-1)*GRID(ng)%omn(i,j-1))         &
     &                  /(GRID(ng)%omn(i-1,j-1)+GRID(ng)%omn(i-1,j)     &
     &                      +GRID(ng)%omn(i,j)+GRID(ng)%omn(i,j-1))

             ! ice stress on b-grid u-point = psi(i+1,j+1), hence the offset of i-1.
             ! sap for j+1 sa for j
             sap = ICE(ng)%strx(i-1,j)*aice_psip
             sa  = ICE(ng)%strx(i-1,j-1)*aice_psi        
             
             ! finally area-wheigted stress on c-grid u-point
             if ((aice_psip+aice_psi).gt.1e-12) then
                ICE(ng)%stru(i,j) = (sap+sa)/(aice_psip+aice_psi) 
             else
                ICE(ng)%stru(i,j) = 0
             endif          

           end do
          end do
#ifdef DISTRIBUTE
             CALL mp_exchange2d (ng, tile, iNLM, 1,                     &
     &            LBi, UBi, LBj, UBj,                                   &
     &            NghostPoints, EWperiodic(ng), NSperiodic(ng),         &
     &            ICE(ng)%stru)
             CALL mp_exchange2d (ng, tile, iNLM, 1,                     &
     &            LBi, UBi, LBj, UBj,                                   &
     &            NghostPoints, EWperiodic(ng), NSperiodic(ng),         &
     &            ICE(ng)%aice_u)
#endif



          CASE ('strocny')         ! ice-ocean stress y-dir 


            CALL AttrVect_exportRAttr (cice2ocn_AV, TRIM(code), A, Asize)
            Iimport=Iimport+1
            write(6,*) 'ROMS rank ', MyRank,                            &
     &  ' received strocny field (max/min): ', maxval(A), ' ', minval(A)
            scale=1.0_r8
            add_offset=0.0_r8
            CALL ROMS_import2d (ng, tile,                               &
     &                          id, gtype, scale, add_offset,           &
     &                          Asize, A,                               &
     &                          IstrR, IendR, JstrR, JendR,             &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          Fields(id)%ImpMin, Fields(id)%ImpMax,   &
     &                          ICE(ng)%stry,                           &
     &                          status)

            do j = LBj+1,UBj
            do i = LBi+1,UBi-1

             !ice fraction on v-point
             ICE(ng)%aice_v(i,j) = (ICE(ng)%a(i,j-1)*GRID(ng)%omn(i,j-1)+ICE(ng)%a(i,j)*GRID(ng)%omn(i,j)) / (GRID(ng)%omn(i,j-1)+GRID(ng)%omn(i,j))
             !ice fraction on psi-point (B-grid u(i-1,j-1) point)
             !first j+1 (psip), then j (psi)
             aice_psip =( ICE(ng)%a(i,j-1)*GRID(ng)%omn(i,j-1)          &
     &                   +ICE(ng)%a(i,j)*GRID(ng)%omn(i,j)              &
     &                   +ICE(ng)%a(i+1,j)*GRID(ng)%omn(i+1,j)          &
     &                   +ICE(ng)%a(i+1,j-1)*GRID(ng)%omn(i+1,j-1))     &
     &                  /(GRID(ng)%omn(i,j-1)+GRID(ng)%omn(i,j)         &
     &                     +GRID(ng)%omn(i+1,j)+GRID(ng)%omn(i+1,j-1))
             aice_psi =( ICE(ng)%a(i-1,j-1)*GRID(ng)%omn(i-1,j-1)       &
     &                   +ICE(ng)%a(i-1,j)*GRID(ng)%omn(i-1,j)          &
     &                   +ICE(ng)%a(i,j)*GRID(ng)%omn(i,j)              &
     &                   +ICE(ng)%a(i,j-1)*GRID(ng)%omn(i,j-1))         &
     &                  /(GRID(ng)%omn(i-1,j-1)+GRID(ng)%omn(i-1,j)     &
                            +GRID(ng)%omn(i,j)+GRID(ng)%omn(i,j-1))
             ! ice stress on b-grid u-point = psi(i+1,j+1) -> j-1 offset
             ! sap for j+1 sa for j
             sap = ICE(ng)%stry(i,j-1)*aice_psip
             sa  = ICE(ng)%stry(i-1,j-1)*aice_psi
             
             if ((aice_psip+aice_psi).gt.1e-12) then
            ! finally area-wheigted stress on c-grid u-point
                ICE(ng)%strv(i,j) = (sap+sa)/(aice_psip+aice_psi)
             else
                ICE(ng)%strv(i,j) = 0
             endif
            

           end do
          end do
#ifdef DISTRIBUTE
             CALL mp_exchange2d (ng, tile, iNLM, 1,                     &
     &            LBi, UBi, LBj, UBj,                                   &
     &            NghostPoints, EWperiodic(ng), NSperiodic(ng),         &
     &            ICE(ng)%strv)
             CALL mp_exchange2d (ng, tile, iNLM, 1,                     &
     &            LBi, UBi, LBj, UBj,                                   &
     &            NghostPoints, EWperiodic(ng), NSperiodic(ng),         &
     &            ICE(ng)%aice_v)
#endif
        
        END SELECT
      END DO

!
!-----------------------------------------------------------------------
!  Export fields from ocean (ROMS) to sea ice (CICE) model.
!-----------------------------------------------------------------------
!

!  Prepare for depth-averaging (find k index just below mixed layer)

#  define I_RANGE MAX(Istr-2,0),MIN(Iend+2,Lm(ng)+1)
#  define J_RANGE MAX(Jstr-2,0),MIN(Jend+2,Mm(ng)+1)
      do j=J_RANGE
         do i=I_RANGE
            
#  ifdef LMD_SKPP
            mlio = min(MIXING(ng)%hsbl(i,j),-10._r8)
#  else
            mlio = -10._r8
#  endif
            nbot(i,j) = 1
            do k=N(ng),1,-1
               if(GRID(ng)%z_r(i,j,k).lt.mlio) then
                  nbot(i,j) = min(k,N(ng))
                  nbot(i,j) = max(nbot(i,j),1)
                  goto 1111
               endif
            enddo
 1111       continue
         enddo
      enddo
#undef I_RANGE
#undef J_RANGE

!  Schedule sending fields to the cice model.
!
      Iexport=0
      DO ifield=1,Nexport(Iocean)
        id=ExportID(Iocean)%val(ifield)
        code=ADJUSTL(Fields(id)%code)
        gtype=Fields(id)%GridType
        scale=Fields(id)%scale
        add_offset=Fields(id)%AddOffset

        SELECT CASE (TRIM(code))

          CASE ('SST')

            CALL ROMS_export2d (ng, tile,                               &
     &                          id, gtype, scale, add_offset,           &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          OCEAN(ng)%t(:,:,N(ng),NOUT,itemp),      &
     &                          Fields(id)%ExpMin, Fields(id)%ExpMax,   &
     &                          Asize, A,                               &
     &                          status)
            write(6,*)                                                  &
     &           'ROMS rank ', MyRank, ' sending sst field (max/min): ',&
     &           maxval(A), ' ', minval(A)
            CALL AttrVect_importRAttr(ocn2cice_AV, TRIM(code), A, Asize)
            Iexport=Iexport+1

          CASE ('SSS')

            CALL ROMS_export2d (ng, tile,                               &
     &                          id, gtype, scale, add_offset,           &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          OCEAN(ng)%t(:,:,N(ng),NOUT,isalt),      &
     &                          Fields(id)%ExpMin, Fields(id)%ExpMax,   &
     &                          Asize, A,                               &
     &                          status)
            write(6,*) &
     &           'ROMS rank ', MyRank, ' sending sss field (max/min): ',&
     &           maxval(A), ' ', minval(A)
            CALL AttrVect_importRAttr(ocn2cice_AV, TRIM(code), A, Asize)
            Iexport=Iexport+1

          CASE ('FRZMLT')
            
           do j=JstrR,JendR
            do i=IstrR,IendR
             if (OCEAN(ng)%qfraz_accum(i,j) .gt.0.0_r8) then
              OCEAN(ng)%qfraz_accum(i,j)= &
     &               OCEAN(ng)%qfraz_accum(i,j)/ncouple
             else
!jd Assume 5-meter thickness of layer. Here melt potential should be a proper 
!jd weight over a typical mixed (?)/ near-ice layer. 
             OCEAN(ng)%qfraz_accum(i,j) = rho0*Cp *                     &
     &        min(t_freeze(OCEAN(ng)%t(i,j,N(ng),NOUT,isalt),0.0_r8)    &
     &        - OCEAN(ng)%t(i,j,N(ng),NOUT,itemp), 0.0_r8 ) * 5.0_r8
             end if
            end do
           end do

            CALL ROMS_export2d (ng, tile,                               &
     &                          id, gtype, scale, add_offset,           &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          OCEAN(ng)%qfraz_accum(:,:),            &
     &                          Fields(id)%ExpMin, Fields(id)%ExpMax,   &
     &                          Asize, A,                               &
     &                          status)
            write(6,*) &
     &         'ROMS rank ', MyRank, ' sending frzmlt field (max/min):',&
     &          maxval(A), ' ', minval(A)
            CALL AttrVect_importRAttr(ocn2cice_AV, TRIM(code), A, Asize)
            Iexport=Iexport+1

!jd Reset accumulation array
            ocean(ng)%qfraz_accum(:,:) = 0


          CASE ('u')
#  define I_RANGE MAX(Istr-1,1),Iend+1             
             do j=Jstr,Jend
                do i=I_RANGE
                   nbotu = NINT(0.5_r8*(nbot(i-1,j)+nbot(i,j)))
                   nbotu = max(min(nbotu,N(ng)),1)
                   uw(i,j) = 0._r8
                   totml = 0._r8
                   do k=N(ng),nbotu,-1
                      dml = 0.5_r8*(GRID(ng)%z_w(i-1,j,k)-GRID(ng)%z_w(i-1,j,k-1) + GRID(ng)%z_w(i,j,k)-GRID(ng)%z_w(i,j,k-1))
                      uw(i,j) = uw(i,j) + OCEAN(ng)%u(i,j,k,NOUT)*dml
                      totml = totml + dml
                   enddo
                   uw(i,j) = uw(i,j)/totml
                enddo
             enddo
#  undef I_RANGE
             do j=Jstr,Jend
                do i=IstrU,Iend
                   uwater(i,j) = uw(i,j)
                enddo
             enddo
!  Apply boundary conditions.
             CALL bc_u2d_tile (ng, tile,                                &
     &            LBi, UBi, LBj, UBj,                                   &
     &            uwater)
#ifdef DISTRIBUTE
             CALL mp_exchange2d (ng, tile, iNLM, 1,                     &
     &            LBi, UBi, LBj, UBj,                                   &
     &            NghostPoints, EWperiodic(ng), NSperiodic(ng),         &
     &            uwater)
#endif
!            write(6,*) TRIM(code),id,gtype
            CALL ROMS_export2d (ng, tile,                               &
     &                          id, gtype, scale, add_offset,           &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          uwater,                                 &
!     &                          OCEAN(ng)%u(:,:,N(ng),NOUT),            &
     &                          Fields(id)%ExpMin, Fields(id)%ExpMax,   &
     &                          Asize, A,                               &
     &                          status)

            write(6,*)                                                  &
     &           'ROMS rank ',MyRank,' sending u field (max/min):',     &
     &            maxval(A), ' ', minval(A)
            CALL AttrVect_importRAttr(ocn2cice_AV, TRIM(code), A, Asize)
            Iexport=Iexport+1

          CASE ('v')
#  define J_RANGE MAX(Jstr-1,1),Jend+1 
             do j=J_RANGE
                do i=Istr,Iend
                   nbotv = NINT(0.5_r8*(nbot(i,j-1)+nbot(i,j)))
                   nbotv = max(min(nbotv,N(ng)),1)
                   vw(i,j) = 0._r8
                   totml = 0._r8
                   do k=N(ng),nbotv,-1
                      dml = 0.5_r8*(GRID(ng)%z_w(i,j-1,k)-GRID(ng)%z_w(i,j-1,k-1) + GRID(ng)%z_w(i,j,k)-GRID(ng)%z_w(i,j,k-1))
                      vw(i,j) = vw(i,j) + OCEAN(ng)%v(i,j,k,NOUT)*dml
                      totml = totml + dml
                   enddo
                   vw(i,j) = vw(i,j)/totml
                enddo
             enddo
#  undef J_RANGE
             do j=JstrV,Jend
                do i=Istr,Iend
                   vwater(i,j) = vw(i,j)
                enddo
             enddo
!  Apply boundary conditions.
             CALL bc_v2d_tile (ng, tile,                                &
     &            LBi, UBi, LBj, UBj,                                   &
     &            vwater)
#ifdef DISTRIBUTE
             CALL mp_exchange2d (ng, tile, iNLM, 1,                     &
     &            LBi, UBi, LBj, UBj,                                   &
     &            NghostPoints, EWperiodic(ng), NSperiodic(ng),         &
     &            vwater)
#endif
            CALL ROMS_export2d (ng, tile,                               &
     &                          id, gtype, scale, add_offset,           &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          vwater,                                 &
!     &                          OCEAN(ng)%v(:,:,N(ng),NOUT),            &
     &                          Fields(id)%ExpMin, Fields(id)%ExpMax,   &
     &                          Asize, A,                               &
     &                          status)
            write(6,*)                                                  &
     &           'ROMS rank ',MyRank,' sending v field (max/min):',     &
     &            maxval(A), ' ', minval(A)
            CALL AttrVect_importRAttr(ocn2cice_AV, TRIM(code), A, Asize)
            Iexport=Iexport+1

          CASE ('SSH')

            CALL ROMS_export2d (ng, tile,                               &
     &                          id, gtype, scale, add_offset,           &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          OCEAN(ng)%zeta(:,:,KOUT),               &
     &                          Fields(id)%ExpMin, Fields(id)%ExpMax,   &
     &                          Asize, A,                               &
     &                          status)
            write(6,*)                                                  &
     &           'ROMS rank ',MyRank,' sending SSH field (max/min):',   &
     &            maxval(A), ' ', minval(A)
            CALL AttrVect_importRAttr(ocn2cice_AV, TRIM(code), A, Asize)
            Iexport=Iexport+1


        END SELECT
      END DO

!
!  Send ocean fields
!
      IF (Iexport.gt.0) THEN
         if (master) write(6,*)'ROMS sends', iexport,' fields to CICE'
        CALL MCT_Send(ocn2cice_AV, ROMStoCICE)
      END IF

!
!  Deallocate communication arrays.
!
      deallocate (A,nbot,uw,vw,uwater,vwater)

      RETURN
      END SUBROUTINE ocn2cice_coupling

      SUBROUTINE finalize_ocn2cice_coupling
!
!========================================================================
!                                                                       !
!  This routine finalizes ocean and wave models coupling data streams.  !
!                                                                       !
!========================================================================
!
!  Local variable declarations.
!
      integer :: MyError
!
!-----------------------------------------------------------------------
!  Deallocate MCT environment.
!-----------------------------------------------------------------------
!
      CALL Router_clean (ROMStoCICE, MyError)
      CALL AttrVect_clean (ocn2cice_AV, MyError)
      CALL GlobalSegMap_clean (GSMapROMS, MyError)

      RETURN

      END SUBROUTINE finalize_ocn2cice_coupling

# endif

#endif
      END MODULE ocean_coupler_mod
