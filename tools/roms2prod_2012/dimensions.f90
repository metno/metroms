module dimensions
! This module reads in dimensions from a ROMS netcdf file (get_infile_dims)
! and writes dimensions to the output file (set_outfile_dims)
  implicit none
  include "netcdf.inc"

  integer :: xi_rhoID, xi_uID, xi_vID
  integer :: eta_rhoID, eta_uID, eta_vID
  integer :: s_rhoID, s_wID, timeID 

  integer :: xi_rho, xi_u, xi_v
  integer :: eta_rho, eta_u, eta_v
  integer :: s_rho, s_w, time

  integer :: XID, YID, nzID, outtimeID
  integer :: X, Y, nz, outtime

  contains

    subroutine get_infile_dims(ncid)
      integer,intent(in) :: ncid
      integer :: status

      status = nf_inq_dimid(ncid,'xi_rho', xi_rhoID);   call check_err(status)
      status = nf_inq_dimlen(ncid,xi_rhoID,xi_rho);     call check_err(status)
      status = nf_inq_dimid(ncid,'eta_rho', eta_rhoID); call check_err(status)
      status = nf_inq_dimlen(ncid,eta_rhoID,eta_rho);   call check_err(status)
      status = nf_inq_dimid(ncid,'xi_u', xi_uID);       call check_err(status)
      status = nf_inq_dimlen(ncid,xi_uID,xi_u);         call check_err(status)
      status = nf_inq_dimid(ncid,'eta_u', eta_uID);     call check_err(status)
      status = nf_inq_dimlen(ncid,eta_uID,eta_u);       call check_err(status)
      status = nf_inq_dimid(ncid,'xi_v', xi_vID);       call check_err(status)
      status = nf_inq_dimlen(ncid,xi_vID,xi_v);         call check_err(status)
      status = nf_inq_dimid(ncid,'eta_v', eta_vID);     call check_err(status)
      status = nf_inq_dimlen(ncid,eta_vID,eta_v);       call check_err(status)
      status = nf_inq_dimid(ncid,'s_rho', s_rhoID);     call check_err(status)
      status = nf_inq_dimlen(ncid,s_rhoID,s_rho);       call check_err(status)
      status = nf_inq_dimid(ncid,'s_w', s_wID);         call check_err(status)
      status = nf_inq_dimlen(ncid,s_wID,s_w);           call check_err(status)
      status = nf_inq_dimid(ncid,'ocean_time', timeID); call check_err(status)
      status = nf_inq_dimlen(ncid,timeID,time);         call check_err(status)


    end subroutine get_infile_dims

    subroutine set_outfile_dims(ncid)
      integer,intent(in) :: ncid
      integer :: status

      status = nf_def_dim(ncid,"X",X, XID); call check_err(status)
      status = nf_def_dim(ncid,"Y",Y, YID); call check_err(status)
      status = nf_def_dim(ncid,"depth",nz, nzID); call check_err(status)
      status = nf_def_dim(ncid,"time",outtime, outtimeID); call check_err(status)


    end subroutine set_outfile_dims


end module dimensions
