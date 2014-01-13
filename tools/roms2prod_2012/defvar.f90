subroutine defvar(varname,ncidin,ncidout)
!subroutine defvar(varname,ncidin,ncidout,proj_name)
!********************************************************
! Subroutine for defining variables in outputfiles      !
! Attributes are given as specified in netcdf_atts.f90  !
!********************************************************
  use dimensions
  use netcdf_atts

  implicit none

  character (len=40) :: varname
  integer :: ncidin, ncidout
  integer :: varid, status, varidin 
  character (len=100) :: text
  !character (len=80) :: proj_name
  integer :: i, irec

! First find position of variable in var_info
  do i=1,maxvar
     if (verify(trim(varname),trim(var_info(i)%rname)).eq.0) then
       print *, "Varname: ",trim(varname), " varinfo: ",trim(var_info(i)%rname)
       irec=i
       exit
     end if
  end do
  print *, "Adding definition of variable ",trim(varname), " to output file"
  ! add definition of variable to file

  if (var_info(irec)%num_dim.eq.3) then
     status = nf_def_var(ncidout,trim(var_info(irec)%vname),NF_SHORT,var_info(irec)%num_dim,(/ XID, YID, outtimeID /),varid); call check_err(status)
  elseif (var_info(irec)%num_dim.eq.4) then
     status = nf_def_var(ncidout,trim(var_info(irec)%vname),NF_SHORT,var_info(irec)%num_dim,(/ XID, YID, nzID, outtimeID /),varid); call check_err(status)
  end if
  ! and its attributes
  status = nf_put_att_text(ncidout,varid,"standard_name",len_trim(var_info(irec)%standard_name),trim(var_info(irec)%standard_name)); call check_err(status)
  status = nf_put_att_text(ncidout,varid,"long_name",len_trim(var_info(irec)%long_name),trim(var_info(irec)%long_name)); call check_err(status)
  status = nf_put_att_text(ncidout,varid,"units",len_trim(var_info(irec)%units),trim(var_info(irec)%units)); call check_err(status)
  status = nf_put_att_text(ncidout,varid,"time",len_trim(var_info(irec)%time),trim(var_info(irec)%time)); call check_err(status)
  status = nf_put_att_text(ncidout,varid,"coordinates",len_trim(var_info(irec)%coordinates),trim(var_info(irec)%coordinates)); call check_err(status)
  status = nf_put_att_text(ncidout,varid,"grid_mapping",len_trim(var_info(irec)%grid_mapping),trim(var_info(irec)%grid_mapping)); call check_err(status)
  status = nf_put_att_int2(ncidout,varid,"_FillValue",NF_SHORT,1,var_info(irec)%fillvalue); call check_err(status)  
  status = nf_put_att_real(ncidout,varid,"add_offset",NF_REAL,1,var_info(irec)%add_offset); call check_err(status)
  status = nf_put_att_real(ncidout,varid,"scale_factor",NF_REAL,1,var_info(irec)%scale_factor); call check_err(status)


end subroutine defvar
