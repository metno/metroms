! Ugly hack to make sure we get all the text written by the master_task.
! Other tasks may fuck everything up if there are more than 2 tasks,
! which there isn't in my version :D :D

      module seb_write_mod

      use ice_fileunits, only: ice_stdout, ice_altout, &
          stdout_file, altout_file, ice_altstd,altstd_file
      use ice_communicate, only: my_task, master_task

      implicit none
      private
      public :: seb_write

      contains

      subroutine seb_write(string)
      character (len=*), intent(in) :: string

      integer :: err
      
      if (my_task == master_task) then
         write(ice_altstd,*) string
      !   close(UNIT=ice_altstd)
      !   open(UNIT=ice_altstd,FILE=stdout_file,ACCESS='append',IOSTAT=err)
      !   if (err/=0) then
      !       print*, 'IOSTAT ERROR in seb_write, err: ', err, &
      !               ' My_task= ', my_task
      !   endif
       else
         write(ice_altout,*) string
       !  close(UNIT=ice_altout)
       !  open(UNIT=ice_altout,FILE=altout_file,ACCESS='append',IOSTAT=err)
       !  if (err/=0) then
       !     print*, 'IOSTAT ERROR in seb_write, err: ', err, &
       !              'My_tast= ', my_task, ' is not master task :P'
       !  endif
       endif
       

      end subroutine seb_write
      end module seb_write_mod

       
