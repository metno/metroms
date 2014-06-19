      module CICE_MCT

      use ice_kinds_mod
      use ice_blocks, only : block, get_block, nx_block, ny_block
      use ice_domain, only : nblocks, blocks_ice
      use ice_domain_size, only : nx_global, ny_global !, block_size_x, block_size_y, max_blocks
      use ice_flux, only: sst

!  MCT framework for ROMS coupling
!
!  Componenent model registry.
!
      USE m_MCTWorld, ONLY : MCTWorld_init => init
      USE m_MCTWorld, ONLY : MCTWorld_clean => clean
!
!  Domain decomposition descriptor datatype and associated methods.
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

      USE m_Transfer, ONLY : MCT_Send => send
      USE m_Transfer, ONLY : MCT_Recv => recv

      USE ice_communicate, ONLY: MPI_COMM_ICE, nprocs, my_task, master_task

!

      implicit none
      private
      public  :: init_mct,                  &
                 CICE_MCT_coupling,         &
                 GSMapCICE,                 &
                 cice2ocn_AV,               &
                 ocn2cice_AV,               &
                 CICEtoROMS

      save

      real (kind=dbl_kind) ::   TimeInterval = 7200.0
      real (kind=dbl_kind) ::   tcoupling = 0.0

   integer (int_kind), public :: &
      CICEid,                   &
      OCNid,                    &
      Nmodels

!  MCT coupling variables
   type(GlobalSegMap) :: GSMapCICE         ! GloabalSegMap variables
   type(AttrVect)     :: cice2ocn_AV       ! AttrVect variables
   type(AttrVect)     :: ocn2cice_AV
   type(Router)       :: CICEtoROMS        ! Router variables

!=======================================================================

      contains

 subroutine init_mct
!
!  MCT interface initialization
!
   include 'mpif.h'   ! MPI Fortran include file

   integer, pointer :: start(:), length(:)
   integer :: Asize,Istr,Jstr !,j
   character (len=240) :: importList, exportList


    integer     :: ilo_glob, j_glob
    integer     :: i, j, iblk, n, gi
    integer     :: lsize,gsize
    integer     :: ier
    integer     :: ilo, ihi, jlo, jhi ! beginning and end of physical domain
    type(block) :: this_block         ! block information for current block

!
!  Initialize MCT coupled model registry.
!
   CALL MCTWorld_init (Nmodels, MPI_COMM_WORLD, MPI_COMM_ICE, CICEid)
   WRITE (6,*) ' CICE: MCTWorld_init called'


!-------------------------------------------------------------------

! Build the CICE grid numbering for MCT
! NOTE:  Numbering scheme is: West to East and South to North
! starting at south pole.  Should be the same as what's used
! in SCRIP

! number the local grid

    n=0
    do iblk = 1, nblocks
       this_block = get_block(blocks_ice(iblk),iblk)
       ilo = this_block%ilo
       ihi = this_block%ihi
       jlo = this_block%jlo
       jhi = this_block%jhi
       n = n + (1+jhi-jlo)
    enddo
    lsize = n
    WRITE (6,*) ' CICE: lsize=', lsize
    allocate(start(lsize))
    allocate(length(lsize))
    n=0
    do iblk = 1, nblocks
       this_block = get_block(blocks_ice(iblk),iblk)
       ilo = this_block%ilo
       ihi = this_block%ihi
       jlo = this_block%jlo
       jhi = this_block%jhi
       do j = jlo, jhi
          n = n+1
          ilo_glob = this_block%i_glob(ilo)
          j_glob = this_block%j_glob(j)
          start(n) = (j_glob-1)*nx_global + ilo_glob
          length(n) = 1+ihi-ilo
       enddo
    enddo

!  Use grid decomposition to initialize global segmentation map
   WRITE (6,*) ' CICE: GlobalSegMap_init'
   call GlobalSegMap_init(GSMapCICE, start, length, 0, MPI_COMM_ICE, CICEid)
   Asize=GlobalSegMap_lsize(GSMapCICE, MPI_COMM_ICE)


!  Initialize import/export attribute vectors
   importList='SST'
   exportList=''

   WRITE (6,*) ' CICE: AttrVect_init, Asize=', Asize
   call AttrVect_init(ocn2cice_AV, rList=importList, lsize=Asize)
   call AttrVect_zero(ocn2cice_AV)
   call AttrVect_init(cice2ocn_AV, rlist=exportList, lsize=Asize)
   call AttrVect_zero(cice2ocn_AV)


!  Initialize router to ROMS
   WRITE (6,*) ' CICE: Router_init'
   call Router_init (OCNid, GSMapCICE, MPI_COMM_ICE, CICEtoROMS)
   WRITE (6,*) ' CICE: Router_init. Done.'

 end subroutine init_mct

!***********************************************************************


      subroutine CICE_MCT_coupling(time,dt)
         real(kind=dbl_kind), intent(in) :: time,dt
         real(kind=dbl_kind), pointer :: avdata(:)
         integer     :: ilo, ihi, jlo, jhi ! beginning and end of physical domain
         type(block) :: this_block         ! block information for current block
         integer     :: i,j,Asize,iblk,n

!        ***********************************
!             ROMS coupling
!        ***********************************
!
         tcoupling = tcoupling + dt
         IF (tcoupling >= TimeInterval) THEN
            IF (my_task == master_task) THEN
                write(6,*) '*****************************************************'
                write(6,*) 'CICE - Ocean: coupling routine called from CICE'
                write(6,*) 'time = ', time
                write(6,*) 'dt = ', dt
                write(6,*) '*****************************************************'
            END IF
            CALL MCT_Recv(ocn2cice_AV, CICEtoROMS)
            write(6,*) 'CICE - Ocean: CICE Received data'
!
            Asize=GlobalSegMap_lsize(GSMapCICE, MPI_COMM_ICE)
            allocate(avdata(Asize))
            avdata=0.0
!
            CALL AttrVect_exportRAttr(ocn2cice_AV, 'SST', avdata)

            write(6,*) 'CICE rank ', my_task, ' setting the sst field (max/min): ', maxval(avdata), ' ', minval(avdata)
            n = 0
            do iblk = 1, nblocks
               this_block = get_block(blocks_ice(iblk),iblk)
               ilo = this_block%ilo
               ihi = this_block%ihi
               jlo = this_block%jlo
               jhi = this_block%jhi
               do j = jlo, jhi
                  do i = ilo, ihi
                      n = n+1
                      sst(i,j,iblk)=avdata(n)
                  enddo
               enddo
            enddo
            tcoupling = 0.0
         END IF


!        ***********************************

      end subroutine

      end module
