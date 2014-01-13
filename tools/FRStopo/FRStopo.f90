program FRStopo
! nilsmk@met.no
 
  use netcdf
  implicit none

  real              :: cff1,cff2,pi
  integer           :: ncid,dim_x,dim_y,statusi,statuso,varid,Iwrk
  integer           :: X,Y,TIME,i,j
  character(len=99) :: gridFileOuter,gridFileInner,x_dimname,y_dimname
  
  real, dimension(:,:), allocatable     ::  hOuter,hInner_raw,hInner_smooth,wrk

  pi=3.1416

!  call getarg(1,gridFileOuter)
!  call getarg(2,gridFileInner)
!  call getarg(3,Iwrk)
   read(5,'(a)') gridFileOuter
   read(5,'(a)') gridFileInner
   read(5,*) Iwrk

  
  ! Read bottom matrix of outer gridfile
  statusi = nf90_open(trim(gridFileOuter),nf90_nowrite,ncid)
  statusi = nf90_inq_dimid(ncid,'xi_rho',dim_x)
  if (statusi /= nf90_noerr) statusi = nf90_inq_dimid(ncid,'lat',dim_x)
  statusi = nf90_inq_dimid(ncid,'eta_rho',dim_y)
  if (statusi /= nf90_noerr) statusi = nf90_inq_dimid(ncid,'lon',dim_y)
  statusi = nf90_Inquire_Dimension(ncid,dim_x,x_dimname,X)
  statusi = nf90_Inquire_Dimension(ncid,dim_y,y_dimname,Y)
  write(*,*) 'Outer X, Y = ',X,Y
  allocate(hOuter(X,Y))
  statusi = nf90_inq_varid(ncid,'h',varid)
  statusi = nf90_get_var(ncid,varid,hOuter)
  statusi = nf90_close(ncid)
  where (hOuter > 100000) hOuter=10

  ! Read bottom matrix of inner gridfile
  statusi = nf90_open(trim(gridFileInner),nf90_write,ncid)
  statusi = nf90_inq_dimid(ncid,'xi_rho',dim_x)
  statusi = nf90_inq_dimid(ncid,'eta_rho',dim_y)
  statusi = nf90_Inquire_Dimension(ncid,dim_x,x_dimname,X)
  statusi = nf90_Inquire_Dimension(ncid,dim_y,y_dimname,Y)
  write(*,*) 'Inner X, Y = ',X,Y
  allocate(hInner_raw(X,Y))
  allocate(hInner_smooth(X,Y))
  allocate(wrk(X,Y))
  statusi = nf90_inq_varid(ncid,'h',varid)
  statusi = nf90_get_var(ncid,varid,hInner_raw)
  !statusi = nf90_close(ncid)

  cff1=1.0
  cff2=0.0
  DO j=1,Y
     DO i=1,X
        wrk(i,j)=cff2
     END DO
  END DO

  DO j=1,Y
     ! East
     !write(*,*) 'East'  
     DO i=(X-Iwrk),X
        wrk(i,j)=cff1*(1.0+COS(pi*(X-i)/Iwrk))/2
     END DO
     ! West
     !write(*,*) 'West'  
     DO i=1,(Iwrk+1)
        wrk(i,j)=cff1*(1.0+COS(pi*(i-1)/Iwrk))/2
     END DO
  END DO
  ! North
  !write(*,*) 'North'  
  DO j=(Y-Iwrk),Y
     DO i=1,X
        wrk(i,j)=MAX(wrk(i,j),cff1*(1.0+COS(pi*(Y-j)/Iwrk))/2)
     END DO
  END DO
  ! South
  !write(*,*) 'South'  
  DO j=1,(Iwrk+1)
     DO i=1,X
        wrk(i,j)=MAX(wrk(i,j),cff1*(1.0+COS(pi*j/Iwrk))/2)
     ENDDO
  ENDDO

  hInner_smooth=(wrk*hOuter)+((1-wrk)*hInner_raw)
  

  ! Write "smoothed" bottom matrix back to gridfile
  statuso = nf90_inq_varid(ncid,'wrk',varid)
  statuso = nf90_put_var(ncid,varid,wrk)
  statuso = nf90_inq_varid(ncid,'h',varid)
  statuso = nf90_put_var(ncid,varid,hInner_smooth)

 
  statusi = nf90_close(ncid)

end program FRStopo
