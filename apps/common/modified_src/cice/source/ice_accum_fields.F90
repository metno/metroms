
      module ice_accum_fields

      use ice_accum_shared
      use ice_blocks, only : nx_block, ny_block
      use ice_domain_size, only :  max_blocks
      use ice_kinds_mod
      use ice_constants
      use seb_write_mod
      use ice_fileunits, only: ice_stdout
      implicit none

      private
      public :: init_accum_fields, accumulate_i2o_fields,               &
                write_restart_accum_fields, read_restart_accum_fields,  &
                update_accum_clock, mean_i2o_fields, zero_i2o_fields,   &
                idaice, idfresh, idfsalt, idfhocn, idfswthru,           &
                idstrocnx, idstrocny, accum_i2o_fields

!jd Time-accumulation of coupling fields. 
       real (kind=dbl_kind),  &
            dimension (nx_block,ny_block,nfields,max_blocks) ::       &
           accum_i2o_fields ! Time accumulation of fields sendt to ROMS


!=======================================================================

      contains

!=======================================================================

!  Initialize ice age tracer (call prior to reading restart data)

      subroutine init_accum_fields()

      accum_i2o_fields(:,:,:,:) = c0
!      accum_time = c0

      end subroutine init_accum_fields


!======================================================================

! Increase accum time clock

      subroutine update_accum_clock(dt)
      
      real (kind=dbl_kind), intent(in) :: &
         dt

      accum_time = accum_time + dt

      end subroutine update_accum_clock

!======================================================================

! reset

      subroutine zero_i2o_fields()

      accum_i2o_fields = c0
      accum_time = 0

      end subroutine zero_i2o_fields


!======================================================================

      subroutine accumulate_i2o_fields(dt)
      use ice_state, only: aice
      use ice_flux, only: fresh_ai, fsalt_ai,&
         fhocn_ai,fswthru_ai, strocnx, strocny

      real(kind=dbl_kind), intent(in) :: dt

      call accum_field(idaice, aice, dt)
      call accum_field(idfresh, fresh_ai, dt)
      call accum_field(idfsalt, fsalt_ai, dt)
      call accum_field(idfhocn, fhocn_ai, dt)
      call accum_field(idfswthru, fswthru_ai, dt)
      call accum_field(idstrocnx, strocnx, dt)
      call accum_field(idstrocny, strocny, dt)

      contains 

! Accumulate field
        subroutine accum_field(id,field,dt)
          integer,intent(in) :: id
          real (kind=dbl_kind), dimension (nx_block,ny_block,max_blocks), &
               intent(in):: field
          real(kind=dbl_kind), intent(in) :: dt

          accum_i2o_fields(:,:,id,:) = accum_i2o_fields(:,:,id,:) &
                + dt*field(:,:,:)

        end subroutine accum_field

      end subroutine accumulate_i2o_fields

!=======================================================================

! calculate mean

      subroutine mean_i2o_fields()

           accum_i2o_fields(:,:,:,:) = accum_i2o_fields(:,:,:,:) / accum_time

      end subroutine mean_i2o_fields

!=======================================================================

! Dumps all values needed for restarting
! author Elizabeth C. Hunke, LANL

      subroutine write_restart_accum_fields()

      use ice_communicate, only: my_task, master_task
      use ice_domain_size, only: ncat
      use ice_fileunits, only: nu_diag, nu_dump_accum
      use ice_state, only: trcrn, nt_iage
      use ice_restart ,only: write_restart_field

      ! local variables

      logical (kind=log_kind) :: diag
      character (len=200) :: my_string
      integer :: i
      diag = .true.

      !-----------------------------------------------------------------

      do i = 1,7
         write(my_string,*) 'min max i20 for var: ', i, &
             minval(accum_i2o_fields(:,:,i,:)/accum_time), &
             maxval(accum_i2o_fields(:,:,i,:)/accum_time), &
             accum_time
         call seb_write(my_string)
      enddo
      call write_restart_field(nu_dump_accum,0, &
                  accum_i2o_fields(:,:,idaice,:),'ruf8', & 
                  'accum_aice',1,diag)
      call write_restart_field(nu_dump_accum,0, &
                  accum_i2o_fields(:,:,idfresh,:),'ruf8', & 
                  'accum_fresh',1,diag)
      call write_restart_field(nu_dump_accum,0, &
                  accum_i2o_fields(:,:,idfsalt,:),'ruf8', & 
                  'accum_fsalt',1,diag)
      call write_restart_field(nu_dump_accum,0, &
                  accum_i2o_fields(:,:,idfhocn,:),'ruf8', & 
                  'accum_fhocn',1,diag)
      call write_restart_field(nu_dump_accum,0, &
                  accum_i2o_fields(:,:,idfswthru,:),'ruf8', & 
                  'accum_fswthru',1,diag)
      call write_restart_field(nu_dump_accum,0, &
                  accum_i2o_fields(:,:,idstrocnx,:),'ruf8', & 
                  'accum_strocnx',1,diag)
      call write_restart_field(nu_dump_accum,0, &
                  accum_i2o_fields(:,:,idstrocny,:),'ruf8', & 
                  'accum_strocny',1,diag)

      end subroutine write_restart_accum_fields

!=======================================================================

! Reads all values needed for an ice age restart
! author Elizabeth C. Hunke, LANL

      subroutine read_restart_accum_fields()

      use ice_communicate, only: my_task, master_task
      use ice_domain_size, only: ncat
      use ice_fileunits, only: nu_diag, nu_restart_accum
      use ice_state, only: trcrn, nt_iage
      use ice_restart ,only: read_restart_field

      ! local variables

      logical (kind=log_kind) :: &
         diag
      character (len=200) :: my_string
      integer :: i
      diag = .true.

      call seb_write('read_restart_field')
      call read_restart_field(nu_restart_accum,0, &
                  accum_i2o_fields(:,:,idaice,:),'ruf8', & 
                  'accum_aice',1,diag)
      call read_restart_field(nu_restart_accum,0, &
                  accum_i2o_fields(:,:,idfresh,:),'ruf8', & 
                  'accum_fresh',1,diag)
      call read_restart_field(nu_restart_accum,0, &
                  accum_i2o_fields(:,:,idfsalt,:),'ruf8', & 
                  'accum_fsalt',1,diag)
      call read_restart_field(nu_restart_accum,0, &
                  accum_i2o_fields(:,:,idfhocn,:),'ruf8', & 
                  'accum_fhocn',1,diag)
      call read_restart_field(nu_restart_accum,0, &
                  accum_i2o_fields(:,:,idfswthru,:),'ruf8', & 
                  'accum_fswthru',1,diag)
      call read_restart_field(nu_restart_accum,0, &
                  accum_i2o_fields(:,:,idstrocnx,:),'ruf8', & 
                  'accum_strocnx',1,diag)
      call read_restart_field(nu_restart_accum,0, &
                  accum_i2o_fields(:,:,idstrocny,:),'ruf8', & 
                  'accum_strocny',1,diag)
      do i = 1,7
         write(my_string,*) 'min max i20 for var: ', i, &
             minval(accum_i2o_fields(:,:,i,:)/accum_time), &
             maxval(accum_i2o_fields(:,:,i,:)/accum_time), &
             accum_time
         call seb_write(my_string)
      enddo

      end subroutine read_restart_accum_fields

!=======================================================================

      end module ice_accum_fields

!=======================================================================
