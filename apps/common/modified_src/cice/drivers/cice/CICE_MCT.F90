      module CICE_MCT

      use ice_kinds_mod

!     MCT couling modules
      USE m_GlobalSegMap, ONLY : GlobalSegMap
      USE m_AttrVect, ONLY : AttrVect
      USE m_Router,   ONLY : Router
      USE m_Transfer, ONLY : MCT_Send => send
      USE m_Transfer, ONLY : MCT_Recv => recv
      USE m_AttrVect, ONLY : AttrVect_importRAttr => importRAttr
      USE m_AttrVect, ONLY : AttrVect_exportRAttr => exportRAttr

      USE ice_communicate, ONLY: GSMapCICE, cice2ocn_AV, ocn2cice_AV, CICEtoROMS, my_task, master_task

!

      implicit none
      private
      public :: CICE_MCT_coupling
      save

      real (kind=dbl_kind) ::   TimeInterval = 7200.0
      real (kind=dbl_kind) ::   tcoupling = 0.0

!=======================================================================

      contains

      subroutine CICE_MCT_coupling(time,dt)
         real(kind=dbl_kind), intent(in) :: time,dt
         real(kind=dbl_kind), pointer :: avdata(:)

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
            allocate(avdata(19481))
            avdata=0.0
!
            CALL AttrVect_exportRAttr(ocn2cice_AV, 'SST', avdata)

            IF (my_task == master_task) THEN
                write(6,*) 'CICE rank ', my_task, ' received: ', avdata
            END IF

            tcoupling = 0.0
         END IF


!        ***********************************

      end subroutine

      end module
