#ifdef ROMSCOUPLED
! Stuff ice accum needs to share with other modules so to avoid circular
! dependencies

      module ice_accum_shared
      use ice_kinds_mod
      implicit none

      logical (kind=log_kind), public :: &
               bool_accum_write = .true., &  ! if .true., write age tracer restartfile
               bool_accum_read = .false.     ! same but for reading

      real (kind=dbl_kind), public :: accum_time

!jd Coupling fields and indexes
      integer, parameter, public :: nfields = 7,  &
         idaice=1, &
         idfresh=2, &
         idfsalt=3, &
         idfhocn=4, &
         idfswthru=5, &
         idstrocnx=6, &
         idstrocny=7 

      end module ice_accum_shared
#endif
