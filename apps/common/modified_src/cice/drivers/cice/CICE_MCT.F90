module CICE_MCT

  use ice_kinds_mod
  use ice_blocks, only : block, get_block, nx_block, ny_block
  use ice_constants, only: field_loc_center, field_type_scalar, c0
  use ice_domain, only : nblocks, blocks_ice, halo_info
  use ice_domain_size, only : nx_global, ny_global, max_blocks !, block_size_x, block_size_y
  use ice_flux, only: sst, uocn, vocn, zeta, ss_tltx, ss_tlty,&
       sss,frzmlt
  use ice_boundary, only: ice_HaloUpdate
  use ice_fileunits, only: ice_stdout, ice_stderr ! these might be the same

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

!jd Coupling fields and indexes
  integer, parameter :: nfields = 7,  &
       idaice=1, &
       idfresh=2, &
       idfsalt=3, &
       idfhocn=4, &
       idfswthru=5, &
       idstrocnx=6, &
       idstrocny=7

!jd Time-accumulation of coupling fields. 
  real (kind=dbl_kind), dimension (nx_block,ny_block,max_blocks,nfields)::&
       accum_i2o_fields ! Time accumulation of fields sendt to ROMS
  

!jd      real (kind=dbl_kind) ::   TimeInterval = 7200.0
  real (kind=dbl_kind) ::   TimeInterval = 3600.0
  real (kind=dbl_kind) ::   tcoupling = 0.0
  
  character (len=240) :: &
       importList = 'SST:SSS:FRZMLT:u:v:SSH', &
       exportList = &
       'AICE:freshAI:fsaltAI:fhocnAI:fswthruAI:strocnx:strocny'

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
  
  subroutine init_mct()
!
!  MCT interface initialization
   !
    include 'mpif.h'   ! MPI Fortran include file

    integer, pointer :: start(:), length(:)
    integer :: Asize,Istr,Jstr !,j
    
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
    WRITE (ice_stdout,*) ' CICE: MCTWorld_init called'


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
    WRITE (ice_stdout,*) ' CICE: lsize=', lsize
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
    WRITE (ice_stdout,*) ' CICE: GlobalSegMap_init'
    call GlobalSegMap_init(GSMapCICE, start, length, 0, MPI_COMM_ICE, CICEid)
    Asize=GlobalSegMap_lsize(GSMapCICE, MPI_COMM_ICE)


!  Initialize import/export attribute vectors

!jd   importList='SST:u:v:SSH'

    WRITE (ice_stdout,*) ' CICE: AttrVect_init, Asize=', Asize
    call AttrVect_init(ocn2cice_AV, rList=importList, lsize=Asize)
    call AttrVect_zero(ocn2cice_AV)
    call AttrVect_init(cice2ocn_AV, rlist=exportList, lsize=Asize)
    call AttrVect_zero(cice2ocn_AV)
    

!  Initialize router to ROMS
    WRITE (ice_stdout,*) ' CICE: Router_init'
    call Router_init (OCNid, GSMapCICE, MPI_COMM_ICE, CICEtoROMS)
    WRITE (ice_stdout,*) ' CICE: Router_init. Done.'
    
    deallocate(start,length)

    call CICE_MCT_coupling
!jd    call zero_i2o_fields

  end subroutine init_mct

!***********************************************************************


!jd  subroutine CICE_MCT_coupling(time,dt)
  subroutine CICE_MCT_coupling()
    use ice_grid, only: HTN, HTE, dxu, dyu, dxt, dyt
    use ice_calendar, only: dt, time, write_ic 
!jd    real(kind=dbl_kind), intent(in) :: time,dt
    real(kind=dbl_kind), pointer :: avdata(:)
    integer     :: ilo, ihi, jlo, jhi ! beginning and end of physical domain
    type(block) :: this_block         ! block information for current block
    integer     :: i,j,Asize,iblk,n

!        ***********************************
!             ROMS coupling
!        ***********************************
!

    tcoupling = tcoupling + dt
    call accumulate_i2o_fields(dt)
    
    IF (tcoupling >= TimeInterval) THEN
       IF (my_task == master_task) THEN
          write(ice_stdout,*) '*********************************************'
          
          write(ice_stdout,*) 'CICE - Ocean: coupling routine called from CICE'
          write(ice_stdout,*) 'time = ', time
          write(ice_stdout,*) 'dt = ', dt
          write(ice_stdout,*) '*********************************************'
       END IF
       Asize=GlobalSegMap_lsize(GSMapCICE, MPI_COMM_ICE)
       allocate(avdata(Asize))
       avdata=0.0

!jd 
       call mean_i2o_fields(tcoupling)
!
! Exporting aice
       call ice2ocn_send_field(accum_i2o_fields(:,:,:,idaice),'AICE')
! Exporting fresh_ai
       call ice2ocn_send_field(accum_i2o_fields(:,:,:,idfresh),'freshAI')
! Exporting fsalt_ai
       call ice2ocn_send_field(accum_i2o_fields(:,:,:,idfsalt),'fsaltAI')
! Exporting fhocn_ai
       call ice2ocn_send_field(accum_i2o_fields(:,:,:,idfhocn),'fhocnAI')
! Exporting fswthru_ai
       call ice2ocn_send_field(accum_i2o_fields(:,:,:,idfswthru),'fswthruAI')
! Export stress vector (These are on the velocity point (Ugrid)
! Change of sign here as the stress on the ocean acts in opposite
! directon as the stress on the ice.
       call ice2ocn_send_field(-accum_i2o_fields(:,:,:,idstrocnx),'strocnx')
       call ice2ocn_send_field(-accum_i2o_fields(:,:,:,idstrocny),'strocny')

! Transfere data to ocean
       CALL MCT_Send(cice2ocn_AV, CICEtoROMS)


! Recive data from ocean
       CALL MCT_Recv(ocn2cice_AV, CICEtoROMS)
       write(ice_stdout,*) 'CICE - Ocean: CICE Received data'
!
! SST
!
       CALL AttrVect_exportRAttr(ocn2cice_AV, 'SST', avdata)

       write(ice_stdout,*) 'CICE rank ',my_task,  &
            ' setting the sst field (max/min): ', &
            maxval(avdata), ' ', minval(avdata)

       call avec2field(avdata,sst)
       call ice_HaloUpdate (sst, halo_info, &
            field_loc_center, field_type_scalar)

! Salinity
       CALL AttrVect_exportRAttr(ocn2cice_AV, 'SSS', avdata)
       
       write(ice_stdout,*) 'CICE rank ',my_task,  &
            ' setting the sss field (max/min): ', &
            maxval(avdata), ' ', minval(avdata)

       call avec2field(avdata,sss)
       call ice_HaloUpdate (sss, halo_info, &
            field_loc_center, field_type_scalar)

! Melt freeze potential
       CALL AttrVect_exportRAttr(ocn2cice_AV, 'FRZMLT', avdata)

       write(ice_stdout,*) 'CICE rank ',my_task,  &
            ' setting the frzmlt field (max/min): ', &
            maxval(avdata), ' ', minval(avdata)

       call avec2field(avdata,frzmlt)
       call ice_HaloUpdate (frzmlt, halo_info, &
            field_loc_center, field_type_scalar)

!
! recieve ocean currents and interpolate to B grid
!
!
! Uocn
       !
       CALL AttrVect_exportRAttr(ocn2cice_AV, 'u', avdata)

       write(ice_stdout,*) 'CICE rank ', my_task,    &
            ' setting the U (uocn) field(max/min): ',&
            maxval(avdata), ' ', minval(avdata)

       call avec2field(avdata,uocn)
      ! unfortunately need to cal ice_HaloUpdate twice for the
      ! interpolations sake (ihi+1)
       call ice_HaloUpdate (uocn, halo_info, &
            field_loc_center, field_type_scalar)
            
! this should really be a function I think.
! HTN - length of northern side of T-cell
! dxu - width through the middle of U-cell (B-grid)
! dxu(i,j) = 0.5*( HTN(i,j)+HTN(i+1,j), at least on the test grid.
       do iblk = 1, nblocks
          this_block = get_block(blocks_ice(iblk),iblk)
          ilo = this_block%ilo
          ihi = this_block%ihi
          jlo = this_block%jlo
          jhi = this_block%jhi
          do j = jlo, jhi
             do i = ilo, ihi
! there is an annoying offset here stemming from ROMS vs CICE grid indexing.
! CICE - u/v leads rho, CICE - rho leads u/v
                uocn(i,j,iblk) =                            &     
                     0.5*(uocn(i+1,j,iblk)*HTE(i,j,iblk)        &
                     +uocn(i+1,j+1,iblk)*HTE(i,j+1,iblk)) &
                     /dyu(i,j,iblk)
             enddo
          enddo
       enddo

       call ice_HaloUpdate (uocn, halo_info, &
            field_loc_center, field_type_scalar)

! Vocn

       CALL AttrVect_exportRAttr(ocn2cice_AV, 'v', avdata)
       
       write(ice_stdout,*) 'CICE rank ', my_task, &
            ' setting the v (vocn) field(max/min): ', &
            maxval(avdata), ' ', minval(avdata)
       
       call avec2field(avdata,vocn)
       call ice_HaloUpdate (vocn, halo_info, &
            field_loc_center, field_type_scalar)
       
       do iblk = 1, nblocks
          this_block = get_block(blocks_ice(iblk),iblk)
          ilo = this_block%ilo
          ihi = this_block%ihi
          jlo = this_block%jlo
          jhi = this_block%jhi
          do j = jlo, jhi
             do i = ilo, ihi
                vocn(i,j,iblk) =                                 &
                     0.5*(vocn(i,j+1,iblk)*HTN(i,j,iblk)        &
                     +vocn(i+1,j+1,iblk)*HTN(i+1,j,iblk))  &
                     /dxu(i,j,iblk)
             enddo
          enddo
       enddo
       
       call ice_HaloUpdate (vocn, halo_info, &
            field_loc_center, field_type_scalar)
       
       !
       ! SSH
!
       CALL AttrVect_exportRAttr(ocn2cice_AV, 'SSH', avdata)

       write(ice_stdout,*) 'CICE rank ', my_task, &
            ' setting the SSH field(max/min): ', &
            maxval(avdata), ' ', minval(avdata)

       call avec2field(avdata,zeta)
       call ice_HaloUpdate (zeta, halo_info, &
            field_loc_center, field_type_scalar)
       
       do iblk = 1, nblocks
          this_block = get_block(blocks_ice(iblk),iblk)
          ilo = this_block%ilo
          ihi = this_block%ihi
          jlo = this_block%jlo
          jhi = this_block%jhi
          do j = jlo, jhi-1
             do i = ilo, ihi-1
!jd zeta is on the T-point here, while ss_tltx is at the velocity point. 
!jd Should also include zeta(i+1,j+1)-zeta(i,j+1) type of difference. 
                ss_tltx(i,j,iblk) =                              &
                     (zeta(i+1,j,iblk)-zeta(i,j,iblk))/dxt(i,j,iblk)
                ss_tlty(i,j,iblk) =                              &
                     (zeta(i,j+1,iblk)-zeta(i,j,iblk))/dyt(i,j,iblk)
             enddo
          enddo
       enddo
       
       call ice_HaloUpdate (ss_tltx, halo_info, &
            field_loc_center, field_type_scalar)
       call ice_HaloUpdate (ss_tlty, halo_info, &
            field_loc_center, field_type_scalar)
       

      
       tcoupling = 0.0
       call zero_i2o_fields
       
       deallocate(avdata)
    END IF


!        ***********************************
  contains
    subroutine ice2ocn_send_field(field,fieldname)
!jd     use ice_domain_size, only: max_blocks
      real (kind=dbl_kind), dimension (nx_block,ny_block,max_blocks), &
           intent(in) :: field
      character(len=*),intent(in) :: fieldname

      call field2avec(field,avdata)
      
      write(ice_stdout,*) 'CICE rank ', my_task, &
           ' sending ',trim(fieldname),' field (max/min): ', &
           maxval(avdata), ' ', minval(avdata)
      
      CALL AttrVect_importRAttr(cice2ocn_AV, trim(fieldname), avdata)
      
    end subroutine ice2ocn_send_field
    
    subroutine avec2field(avec, field)
!jd     use ice_domain_size, only: max_blocks
      real(kind=dbl_kind), pointer :: avec(:)
      real (kind=dbl_kind), dimension (nx_block,ny_block,max_blocks), &
           intent(out) :: field
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
               field(i,j,iblk)=avec(n)
            enddo
         enddo
      enddo
    end subroutine avec2field
    
    subroutine field2avec(field, avec)
!jd     use ice_domain_size, only: max_blocks
      real(kind=dbl_kind), pointer :: avec(:)
      real (kind=dbl_kind), dimension (nx_block,ny_block,max_blocks), &
           intent(in):: field
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
               avec(n) = field(i,j,iblk)
            enddo
         enddo
      enddo
    end subroutine field2avec
    
  end subroutine CICE_MCT_coupling
  
  
  subroutine accumulate_i2o_fields(dt)
    use ice_state, only: aice
    use ice_flux, only: fresh_ai, fsalt_ai,&
         fhocn_ai,fswthru_ai, strocnx, strocny
    
    real(kind=dbl_kind), intent(in) :: dt
    
    call accum_field(idaice, aice)
    call accum_field(idfresh, fresh_ai)
    call accum_field(idfsalt, fsalt_ai)
    call accum_field(idfhocn, fhocn_ai)
    call accum_field(idfswthru, fswthru_ai)
    call accum_field(idstrocnx, strocnx)
    call accum_field(idstrocny, strocny)
    
  contains 
    
    subroutine accum_field(id,field)
      integer,intent(in) :: id
      real (kind=dbl_kind), dimension (nx_block,ny_block,max_blocks), &
           intent(in):: field
      accum_i2o_fields(:,:,:,id) = accum_i2o_fields(:,:,:,id) &
           + dt*field(:,:,:)
    end subroutine accum_field
    
  end subroutine accumulate_i2o_fields
  
  subroutine mean_i2o_fields(dtcouple)
    real(kind=dbl_kind), intent(in) :: dtcouple
    accum_i2o_fields(:,:,:,:) = accum_i2o_fields(:,:,:,:) / dtcouple
  end subroutine mean_i2o_fields
  
  subroutine zero_i2o_fields
    accum_i2o_fields(:,:,:,:) = c0
  end subroutine zero_i2o_fields
  
  
end module CICE_MCT
