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
      start(1)=0
      length(1)=10
      WRITE (6,*) ' ROMS Ocean Coupler: calling GlobalSegMap_init'
      CALL GlobalSegMap_init (GSMapROMS, start, length, 0,              &
     &                        OCN_COMM_WORLD, OCNid)
!
!  Initialize attribute vector holding the export data code strings of
!  the cice model. The Asize is the number of grid point on this
!  processor.
!
      Asize=GlobalSegMap_lsize(GSMapROMS, OCN_COMM_WORLD)
      WRITE (6,*) ' ROMS Ocean Coupler: calling AttrVect_init'
      CALL AttrVect_init (cice2ocn_AV, rList=TRIM(ExportList(Icice)),   &
     &                    lsize=Asize)
!
!  Initialize attribute vector holding the export data code string of
!  the ocean model.
!
      CALL AttrVect_init (ocn2cice_AV, rList=TRIM(ExportList(Iocean)),   &
     &                    lsize=Asize)
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

      SUBROUTINE ocn2cice_coupling (ng, tile)
      USE mod_param
      USE mod_parallel
      USE mod_coupler
      USE mod_forces
      USE mod_kinds
      USE mod_scalars
!
      implicit none
!
!  Imported variable declarations.
!
      integer, intent(in) :: ng, tile
!
!  Local variable declarations.
!
      integer :: Istr, Iend, Jstr, Jend
      integer :: IstrR, IendR, JstrR, JendR, IstrU, JstrV
      integer :: Asize, Iimport, Iexport, MyError
      integer :: gtype, i, id, ifield, ij, j, status

      real(r8) :: add_offset, scale

      real(r8), pointer :: A(:)

      character (len=3 ), dimension(2) :: op_handle
      character (len=40) :: code

#include "tile.h"


      IF (Master) THEN
          write(6,*) ''
          write(6,*) '   *****************************************************  '
          write(6,*) '    Ocean - CICE: coupling routine called from ROMS'
          write(6,*) '   *****************************************************  '
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
!  Schedule receiving fields from wave model.
!
      CALL mpi_comm_rank (OCN_COMM_WORLD, MyRank, MyError)

!      CALL MCT_Recv (cice2ocn_AV, ROMStoCICE)
!
!  Receive fields from wave model.
!
      Iimport=0
      DO ifield=1,Nimport(Iocean)
        id=ImportID(Iocean)%val(ifield)
        code=ADJUSTL(Fields(id)%code)
        gtype=Fields(id)%GridType
        scale=Fields(id)%scale
        add_offset=Fields(id)%AddOffset

        SELECT CASE (TRIM(code))


          CASE ('Wubot')                  ! bottom orbital velocity

            CALL AttrVect_exportRAttr (cice2ocn_AV, TRIM(code), A, Asize)
            Iimport=Iimport+1
            DO i=1,Asize
              A(i)=MAX(0.0_r8,A(i))
            END DO
!            scale=1.0_r8
!            add_offset=0.0_r8
!            CALL ROMS_import2d (ng, tile,                               &
!     &                          id, gtype, scale, add_offset,           &
!     &                          Asize, A,                               &
!     &                          IstrR, IendR, JstrR, JendR,             &
!     &                          LBi, UBi, LBj, UBj,                     &
!     &                          Fields(id)%ImpMin, Fields(id)%ImpMax,   &
!     &                          FORCES(ng)%Ub_swan,                     &
!     &                          status)

        END SELECT
      END DO
!
!-----------------------------------------------------------------------
!  Export fields from ocean (ROMS) to sea ice (CICE) model.
!-----------------------------------------------------------------------
!
!  Schedule sending fields to the wave model.
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
            CALL AttrVect_importRAttr (ocn2cice_AV, TRIM(code), A, Asize)
            Iexport=Iexport+1
        END SELECT
      END DO
!
!  Send ocean fields
!
      IF (Iexport.gt.0) THEN
        CALL MCT_Send (ocn2cice_AV, ROMStoCICE)
      END IF

!      allocate(avdata(10))
!      avdata=0.0_r8
!      avdata(MyRank+1)=100.0 + MyRank

!      CALL AttrVect_importRAttr (ocn2cice_AV, 'SST', avdata)

!      CALL MCT_Send(ocn2cice_AV, ROMStoCICE)

!      CALL MCT_Recv(cice2ocn_AV, ROMStoCICE)

!      CALL AttrVect_exportRAttr(cice2ocn_AV, 'SST', avdata)

!      IF (Master) THEN
!          write(6,*) 'ROMS rank ', MyRank, ' received: ', avdata(MyRank+1)
!      END IF

!
!  Deallocate communication arrays.
!
      deallocate (A)

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
