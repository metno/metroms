subroutine getvar(ncid,varName,itime,varArray)
use netcdf
implicit none
character(len=4), intent(in)  :: varName
integer          , intent(in)  :: itime,ncid
real   		 , intent(inout) :: varArray
integer                        :: status,VarId

! status = nf90_open(trim(avgfile),nf90_nowrite,ncid)
! if(status /= nf90_NoErr) call handle_err(status)
write(*,*) varName
status = nf90_inq_varid(ncid,trim(varName),VarId)
if(status /= nf90_NoErr) call handle_err(status)
status = nf90_get_var(ncid,VarId,varArray,start=(/1,1,1,itime/))
if(status /= nf90_NoErr) call handle_err(status)

! status = nf90_close(ncid)
! if(status /= nf90_NoErr) call handle_err(status)


end subroutine getvar