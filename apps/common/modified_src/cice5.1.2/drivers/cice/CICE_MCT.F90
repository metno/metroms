#ifdef ROMSCOUPLED
#undef REPORT_ALL
module CICE_MCT

  use ice_kinds_mod
  use ice_blocks, only : block, get_block, nx_block, ny_block
  use ice_constants, only: c0,p5, field_loc_center, field_type_scalar,&
       field_loc_NEcorner, field_type_vector
  use ice_global_reductions, only: global_minval, global_maxval
  use ice_domain, only : nblocks, blocks_ice, halo_info, distrb_info
  use ice_domain_size, only : nx_global, ny_global, max_blocks !, block_size_x, block_size_y
  use ice_flux, only: sst, uocn, vocn, zeta, ss_tltx, ss_tlty,&
       sss,frzmlt
  use ice_boundary, only: ice_HaloUpdate
  use ice_fileunits, only: ice_stdout, ice_stderr ! these might be the same

  use ice_accum_shared, only: idaice, idfresh, idfsalt, idfhocn, idfswthru, &
       idstrocnx, idstrocny, accum_time
  use ice_accum_fields, only: accum_i2o_fields, mean_i2o_fields, zero_i2o_fields
  use ice_timers, only: ice_timer_start, ice_timer_stop,ice_timer_print,&
       timer_cplrecv, timer_rcvsnd, timer_cplsend,timer_sndrcv,timer_tmp

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
!jd  USE m_GlobalSegMap, ONLY : GlobalSegMap_Ordpnts => OrderedPoints
!
!  Field storage data types and associated methods.
!
  USE m_AttrVect, ONLY : AttrVect
  USE m_AttrVect, ONLY : AttrVect_init => init
  USE m_AttrVect, ONLY : AttrVect_zero => zero
  USE m_AttrVect, ONLY : AttrVect_clean => clean
!jd  USE m_AttrVect, ONLY : AttrVect_indxR => indexRA
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
  
  USE ice_communicate, ONLY: MPI_COMM_ICE, my_task, master_task

!

  implicit none
  private
  logical :: initial_call 
  real (kind=dbl_kind) ::   TimeInterval = 3600.0
  
  logical :: report_cpl = .true.


  public  :: init_mct,                  &
       CICE_MCT_coupling,         &
       finalize_mct_coupling,     &
       GSMapCICE,                 &
       cice2ocn_AV,               &
       ocn2cice_AV,               &
       CICEtoROMS,                &
       TimeInterval

  save


 ! real (kind=dbl_kind) ::   tcoupling = 0.0
  
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
    call ice_timer_start(timer_tmp)
    CALL MCTWorld_init (Nmodels, MPI_COMM_WORLD, MPI_COMM_ICE, CICEid)
#ifdef REPORT_ALL
    WRITE (ice_stdout,*) ' CICE: MCTWorld_init called'
#endif

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
#ifdef REPORT_ALL
    WRITE (ice_stdout,*) ' CICE: lsize=', lsize
#endif
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
#ifdef REPORT_ALL
    WRITE (ice_stdout,*) ' CICE: GlobalSegMap_init'
#endif
    call GlobalSegMap_init(GSMapCICE, start, length, 0, MPI_COMM_ICE, CICEid)
    Asize=GlobalSegMap_lsize(GSMapCICE, MPI_COMM_ICE)


!  Initialize import/export attribute vectors

#ifdef REPORT_ALL
    WRITE (ice_stdout,*) ' CICE: AttrVect_init, Asize=', Asize
#endif
    call AttrVect_init(ocn2cice_AV, rList=importList, lsize=Asize)
    call AttrVect_zero(ocn2cice_AV)
    call AttrVect_init(cice2ocn_AV, rlist=exportList, lsize=Asize)
    call AttrVect_zero(cice2ocn_AV)
    

!  Initialize router to ROMS
#ifdef REPORT_ALL
    WRITE (ice_stdout,*) ' CICE: Router_init'
#endif
    call Router_init (OCNid, GSMapCICE, MPI_COMM_ICE, CICEtoROMS)
#ifdef REPORT_ALL
    WRITE (ice_stdout,*) ' CICE: Router_init. Done.'
#endif
    
    deallocate(start,length)
    initial_call = .true.

    call ice_timer_stop(timer_tmp)
    call ice_timer_print(timer_tmp,.false.)

!jd    call CICE_MCT_coupling
!jd    initial_call = .false.

  end subroutine init_mct

!***********************************************************************


  subroutine CICE_MCT_coupling()
    use ice_grid, only: HTN, HTE, dxu, dyu, dxt, dyt, tmask,umask,&
         t2ugrid_vector
    use ice_calendar, only: dt, time, write_ic ,istep, istep1

    real(kind=dbl_kind), pointer :: avdata(:)
    integer     :: ilo, ihi, jlo, jhi ! beginning and end of physical domain
    type(block) :: this_block         ! block information for current block
    integer     :: i,j,Asize,iblk,n

    real(kind=dbl_kind) :: fmin,fmax

!        ***********************************
!             ROMS coupling
!        ***********************************
!

!   update of accumulated time since last coupling (accum_time) and accumulation
!   of flux fields are done from RunMod (calling functions in ice_accum_fields).
    IF (accum_time >= TimeInterval .or. initial_call) THEN
       call ice_timer_start(timer_sndrcv)
       IF (my_task == master_task) THEN
          write(ice_stdout,*) '*********************************************'
          
          write(ice_stdout,*) 'CICE - Ocean: coupling routine called from CICE'
          write(ice_stdout,*) 'time = ', time
          write(ice_stdout,*) 'dt = ', dt
          write(ice_stdout,*) 'istep ', istep
          write(ice_stdout,*) 'istep1 ', istep1
          write(ice_stdout,*) '*********************************************'
          call flush(ice_stdout)
       END IF
       Asize=GlobalSegMap_lsize(GSMapCICE, MPI_COMM_ICE)
       allocate(avdata(Asize))
       avdata=0.0

!jd 
       call mean_i2o_fields()


! Exporting aice
       call ice2ocn_send_field(accum_i2o_fields(:,:,idaice,:),'AICE')
! Exporting fresh_ai
       call ice2ocn_send_field(accum_i2o_fields(:,:,idfresh,:),'freshAI')
! Exporting fsalt_ai
       call ice2ocn_send_field(accum_i2o_fields(:,:,idfsalt,:),'fsaltAI')
! Exporting fhocn_ai
       call ice2ocn_send_field(accum_i2o_fields(:,:,idfhocn,:),'fhocnAI')
! Exporting fswthru_ai
       call ice2ocn_send_field(accum_i2o_fields(:,:,idfswthru,:),'fswthruAI')
! Export stress vector. Allready converted to T-cell and scaled with aice
! Change of sign here as the stress on the ocean acts in opposite
! directon as the stress on the ice.

       call ice2ocn_send_field(-accum_i2o_fields(:,:,idstrocnx,:),'strocnx')
       call ice2ocn_send_field(-accum_i2o_fields(:,:,idstrocny,:),'strocny')

       call ice_timer_stop(timer_sndrcv)

! Transfere data to ocean
       call ice_timer_start(timer_cplsend)
       CALL MCT_Send(cice2ocn_AV, CICEtoROMS)
       call ice_timer_stop(timer_cplsend)
       if (initial_call) then
          call ice_timer_print(timer_cplsend,.false.)
       endif

       call ice_timer_start(timer_cplrecv)
! Recive data from ocean
       CALL MCT_Recv(ocn2cice_AV, CICEtoROMS)
       call ice_timer_stop(timer_cplrecv)

       call ice_timer_start(timer_rcvsnd)
#ifdef REPORT_ALL
       write(ice_stdout,*) 'CICE - Ocean: CICE Received data'
#endif

!
! SST
!
       CALL AttrVect_exportRAttr(ocn2cice_AV, 'SST', avdata)

#ifdef REPORT_ALL
       write(ice_stdout,*) 'CICE rank ',my_task,  &
            ' setting the sst field (max/min): ', &
            maxval(avdata), ' ', minval(avdata)
#endif

       call avec2field(avdata,sst)
       call ice_HaloUpdate (sst, halo_info, &
            field_loc_center, field_type_scalar)

       if (report_cpl) call o2i_report(sst,'SST',tmask)

! Salinity
       CALL AttrVect_exportRAttr(ocn2cice_AV, 'SSS', avdata)
       
#ifdef REPORT_ALL
       write(ice_stdout,*) 'CICE rank ',my_task,  &
            ' setting the sss field (max/min): ', &
            maxval(avdata), ' ', minval(avdata)
#endif

       call avec2field(avdata,sss)

       if (minval(avdata) < c0) then
          write(ice_stdout,*) 'CICE rank ',my_task,  &
               ' correcting invalid sss ', minval(avdata)
          
          do iblk = 1, nblocks
             this_block = get_block(blocks_ice(iblk),iblk)
             ilo = this_block%ilo
             ihi = this_block%ihi
             jlo = this_block%jlo
             jhi = this_block%jhi
             do j = jlo, jhi
                do i = ilo, ihi
                   sss(i,j,iblk)=max(sss(i,j,iblk),c0)
                end do
             end do
          end do
       endif
       call ice_HaloUpdate (sss, halo_info, &
            field_loc_center, field_type_scalar)
       if (report_cpl) call o2i_report(sss,'SSS',tmask)

! Melt freeze potential
       CALL AttrVect_exportRAttr(ocn2cice_AV, 'FRZMLT', avdata)

#ifdef REPORT_ALL
       write(ice_stdout,*) 'CICE rank ',my_task,  &
            ' setting the frzmlt field (max/min): ', &
            maxval(avdata), ' ', minval(avdata)
#endif

       call avec2field(avdata,frzmlt)
       call ice_HaloUpdate (frzmlt, halo_info, &
            field_loc_center, field_type_scalar)
       if (report_cpl) call o2i_report(frzmlt,'FRZMLT',tmask)
!
! recieve ocean currents and interpolate to B grid
!
!
! Uocn
       !
       CALL AttrVect_exportRAttr(ocn2cice_AV, 'u', avdata)

#ifdef REPORT_ALL
       write(ice_stdout,*) 'CICE rank ', my_task,    &
            ' setting the U (uocn) field(max/min): ',&
            maxval(avdata), ' ', minval(avdata)
#endif

       call avec2field(avdata,uocn)

       call t2ugrid_vector(uocn)
            
       call ice_HaloUpdate (uocn, halo_info, &
            field_loc_NEcorner, field_type_vector)

       if (report_cpl) call o2i_report(uocn,'u',umask)
! Vocn

       CALL AttrVect_exportRAttr(ocn2cice_AV, 'v', avdata)
       
#ifdef REPORT_ALL 
      write(ice_stdout,*) 'CICE rank ', my_task, &
            ' setting the v (vocn) field(max/min): ', &
            maxval(avdata), ' ', minval(avdata)
#endif
       
       call avec2field(avdata,vocn)
       call ice_HaloUpdate (vocn, halo_info, &
            field_loc_center, field_type_scalar)
       
       call t2ugrid_vector(vocn)
       
       call ice_HaloUpdate (vocn, halo_info, &
            field_loc_NEcorner, field_type_vector)
       if (report_cpl) call o2i_report(vocn,'v',umask)
       
       !
       ! SSH
!
       CALL AttrVect_exportRAttr(ocn2cice_AV, 'SSH', avdata)

#ifdef REPORT_ALL
       write(ice_stdout,*) 'CICE rank ', my_task, &
            ' setting the SSH field(max/min): ', &
            maxval(avdata), ' ', minval(avdata)
#endif

       call avec2field(avdata,zeta)
       call ice_HaloUpdate (zeta, halo_info, &
            field_loc_center, field_type_scalar)
       if (report_cpl) call o2i_report(zeta,'zeta',tmask)

       do iblk = 1, nblocks
          this_block = get_block(blocks_ice(iblk),iblk)
          ilo = this_block%ilo
          ihi = this_block%ihi
          jlo = this_block%jlo
          jhi = this_block%jhi
          do j = jlo, jhi
             do i = ilo, ihi
                ss_tltx(i,j,iblk) =                                  &
                     p5*((zeta(i+1,j,iblk) + zeta(i+1,j+1,iblk)) -   &
                     (zeta(i,j,iblk) + zeta(i,j+1,iblk))) /dxu(i,j,iblk)
                ss_tlty(i,j,iblk) =                                  &
                     p5*((zeta(i,j+1,iblk) + zeta(i+1,j+1,iblk)) -   &
                     (zeta(i,j,iblk) + zeta(i+1,j,iblk))) /dyu(i,j,iblk)
             enddo
          enddo
       enddo
       
       call ice_HaloUpdate (ss_tltx, halo_info, &
            field_loc_NEcorner, field_type_vector)
       call ice_HaloUpdate (ss_tlty, halo_info, &
            field_loc_NEcorner, field_type_vector)
       
      
       call zero_i2o_fields ! also accum_time is zeroed
       
       deallocate(avdata)
       call ice_timer_stop(timer_rcvsnd)

    END IF
    initial_call=.false.

!        ***********************************
  contains

    subroutine o2i_report(field,fieldname,mask)
      real (kind=dbl_kind), dimension (nx_block,ny_block,max_blocks), &
           intent(in) :: field
      logical (kind=log_kind), &
         dimension (nx_block,ny_block,max_blocks),intent(in) :: mask
      character(len=*),intent(in) :: fieldname

      fmin = global_minval(field,distrb_info,mask)
      fmax = global_maxval(field,distrb_info,mask)
      if (my_task.eq.master_task)  &
           write (ice_stdout,*) 'o2i: ',trim(fieldname),' min/max', fmin,fmax 

    end subroutine o2i_report

    subroutine ice2ocn_send_field(field,fieldname)

      real (kind=dbl_kind), dimension (nx_block,ny_block,max_blocks), &
           intent(in) :: field
      character(len=*),intent(in) :: fieldname

      fmin = global_minval(field,distrb_info,tmask)
      fmax = global_maxval(field,distrb_info,tmask)
      if (my_task.eq.master_task)  &
           write (ice_stdout,*) 'i2o: ',trim(fieldname),' min/max', fmin,fmax 


      call field2avec(field,avdata)
      
#ifdef REPORT_ALL
      write(ice_stdout,*) 'CICE rank ', my_task, &
           ' sending ',trim(fieldname),' field (max/min): ', &
           maxval(avdata), ' ', minval(avdata)
#endif
      

      CALL AttrVect_importRAttr(cice2ocn_AV, trim(fieldname), avdata)
      
    end subroutine ice2ocn_send_field
    
    subroutine avec2field(avec, field)
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

  subroutine finalize_mct_coupling
    implicit none
    integer :: err
!-----------------------------------------------------------------------
!  Deallocate MCT environment.
!-----------------------------------------------------------------------
!
      CALL Router_clean (CICEtoROMS, err)
      CALL AttrVect_clean (ocn2cice_AV, err)
      CALL AttrVect_clean (cice2ocn_AV, err)
      CALL GlobalSegMap_clean (GSMapCICE, err)

  end subroutine finalize_mct_coupling
  
end module CICE_MCT
#endif
