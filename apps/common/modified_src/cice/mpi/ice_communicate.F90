!  SVN:$Id: ice_communicate.F90 700 2013-08-15 19:17:39Z eclare $
!|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||

 module ice_communicate

!  This module contains the necessary routines and variables for
!  communicating between processors.
!
! author: Phil Jones, LANL
! Oct. 2004: Adapted from POP version by William H. Lipscomb, LANL

   use ice_kinds_mod
!
!  Componenet model registry.
!
      USE m_MCTWorld, ONLY : MCTWorld_init => init
      USE m_MCTWorld, ONLY : MCTWorld_clean => clean
!
!  Domain decompositin descriptor datatype and assocoiated methods.
!
      USE m_GlobalSegMap, ONLY : GlobalSegMap
      USE m_GlobalSegMap, ONLY : GlobalSegMap_init => init
      USE m_GlobalSegMap, ONLY : GlobalSegMap_lsize => lsize
      USE m_GlobalSegMap, ONLY : GlobalSegMap_clean => clean
      USE m_GlobalSegMap, ONLY : GlobalSegMap_Ordpnts => OrderedPoints
!
!  Field storage data types and associated methods.
!
      USE m_AttrVect, ONLY : AttrVect
      USE m_AttrVect, ONLY : AttrVect_init => init
      USE m_AttrVect, ONLY : AttrVect_zero => zero
      USE m_AttrVect, ONLY : AttrVect_clean => clean
      USE m_AttrVect, ONLY : AttrVect_indxR => indexRA
      USE m_AttrVect, ONLY : AttrVect_importRAttr => importRAttr
      USE m_AttrVect, ONLY : AttrVect_exportRAttr => exportRAttr
!
!  Intercomponent communitcations scheduler.
!
      USE m_Router, ONLY : Router
      USE m_Router, ONLY : Router_init => init
      USE m_Router, ONLY : Router_clean => clean
!
!  Intercomponent transfer.
!
      USE m_Transfer, ONLY : MCT_Send => send
      USE m_Transfer, ONLY : MCT_Recv => recv
!


   implicit none
   private
   save

   public  :: init_communicate,          &
              get_num_procs,             &
              create_communicator

   integer (int_kind), public :: &
      MPI_COMM_ICE,             &! MPI communicator for ice comms
      ice_comm,                 &
      nprocs,                   &
      CICEid,                   &
      OCNid,                    &
      Nmodels,                  &
      mpiR16,                   &! MPI type for r16_kind
      mpiR8,                    &! MPI type for dbl_kind
      mpiR4,                    &! MPI type for real_kind
      my_task,                  &! MPI task number for this task
      master_task                ! task number of master task

   integer (int_kind), parameter, public :: &
      mpitagHalo            = 1,    &! MPI tags for various
      mpitag_gs             = 1000   ! communication patterns

   type(GlobalSegMap) :: GSMapCICE         ! GloabalSegMap variables

   type(AttrVect) :: cice2ocn_AV            ! AttrVect variables
   type(AttrVect) :: ocn2cice_AV
   type(Router)   :: CICEtoROMS            ! Router variables

!***********************************************************************

 contains

!***********************************************************************

 subroutine init_communicate

!  This routine sets up MPI environment and defines ice
!  communicator.

!-----------------------------------------------------------------------
!
!  local variables
!
!-----------------------------------------------------------------------

   include 'mpif.h'   ! MPI Fortran include file

   integer (int_kind) :: ierr  ! MPI error flag
   integer, pointer :: start(:), length(:)
   integer :: gsmsize
   character (len=240) :: exportList

!-----------------------------------------------------------------------
!
!  initiate mpi environment and create communicator for internal
!  ice communications
!
!-----------------------------------------------------------------------

      WRITE (6,*) ' CICE: init_communicate  '

   CALL mpi_comm_rank (MPI_COMM_ICE, my_task, ierr)
      WRITE (6,*) ' CICE: init_communicate my_task=',my_task
   CALL mpi_comm_size (MPI_COMM_ICE, nprocs, ierr)
      WRITE (6,*) ' CICE: init_communicate nprocs=',nprocs
!
!  Initialize MCT coupled model registry.
!
   CALL MCTWorld_init (Nmodels, MPI_COMM_WORLD, MPI_COMM_ICE, CICEid)
      WRITE (6,*) ' CICE: MCTWorld_init called'

   ice_comm = MPI_COMM_ICE
   call MPI_BARRIER (ice_comm, ierr)
   call MPI_COMM_DUP(ice_comm, MPI_COMM_ICE, ierr)

   master_task = 0
   call MPI_COMM_RANK  (MPI_COMM_ICE, my_task, ierr)

   mpiR16 = MPI_REAL16
   mpiR8  = MPI_REAL8
   mpiR4  = MPI_REAL4
   allocate(start(1))
   allocate(length(1))
   gsmsize=0
   exportList=''
      WRITE (6,*) ' CICE: GlobalSegMap_init'
   call GlobalSegMap_init (GSMapCICE, start, length, 0, MPI_COMM_ICE, CICEid)
      WRITE (6,*) ' CICE: AttrVect_init'
   call AttrVect_init (cice2ocn_AV, rlist=exportList, lsize=gsmsize)
   call AttrVect_zero (cice2ocn_AV)
   call AttrVect_init (ocn2cice_AV, rList=exportList, lsize=gsmsize)
   call AttrVect_zero (ocn2cice_AV)
      WRITE (6,*) ' CICE: Router_init'
   call Router_init (OCNid, GSMapCICE, MPI_COMM_ICE, CICEtoROMS)
      WRITE (6,*) ' CICE: Router_init. Done.'


!-----------------------------------------------------------------------

 end subroutine init_communicate

!***********************************************************************

 function get_num_procs()

!  This function returns the number of processor assigned to
!  MPI_COMM_ICE

   integer (int_kind) :: get_num_procs

!-----------------------------------------------------------------------
!
!  local variables
!
!-----------------------------------------------------------------------

   integer (int_kind) :: ierr

!-----------------------------------------------------------------------

   call MPI_COMM_SIZE(MPI_COMM_ICE, get_num_procs, ierr)

!-----------------------------------------------------------------------

 end function get_num_procs

!***********************************************************************

 subroutine create_communicator(new_comm, num_procs)

!  This routine creates a separate communicator for a subset of
!  processors under default ice communicator.
!
!  this routine should be called from init_domain1 when the
!  domain configuration (e.g. nprocs_btrop) has been determined

   include 'mpif.h'

   integer (int_kind), intent(in) :: &
      num_procs         ! num of procs in new distribution

   integer (int_kind), intent(out) :: &
      new_comm          ! new communicator for this distribution

!-----------------------------------------------------------------------
!
!  local variables
!
!-----------------------------------------------------------------------

   integer (int_kind) :: &
     MPI_GROUP_ICE,         &! group of processors assigned to ice
     MPI_GROUP_NEW           ! group of processors assigned to new dist

   integer (int_kind) :: &
     ierr                    ! error flag for MPI comms

   integer (int_kind), dimension(3) :: &
     range                   ! range of tasks assigned to new dist
                             !  (assumed 0,num_procs-1)

!-----------------------------------------------------------------------
!
!  determine group of processes assigned to distribution
!
!-----------------------------------------------------------------------

   call MPI_COMM_GROUP (MPI_COMM_ICE, MPI_GROUP_ICE, ierr)

   range(1) = 0
   range(2) = num_procs-1
   range(3) = 1

!-----------------------------------------------------------------------
!
!  create subroup and communicator for new distribution
!  note: MPI_COMM_CREATE must be called by all procs in MPI_COMM_ICE
!
!-----------------------------------------------------------------------

   call MPI_GROUP_RANGE_INCL(MPI_GROUP_ICE, 1, range, &
                             MPI_GROUP_NEW, ierr)

   call MPI_COMM_CREATE (MPI_COMM_ICE, MPI_GROUP_NEW,  &
                         new_comm, ierr)

!-----------------------------------------------------------------------

 end subroutine create_communicator

!***********************************************************************

 end module ice_communicate

!|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
