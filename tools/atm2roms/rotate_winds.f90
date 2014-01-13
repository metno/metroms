!Program for rotating winds from feltfile
!Compile: gfortran -o rotate_winds rotate_winds.f90 grv2grv.f mer2sph.f pol2sph.f sph2rot.f uvconvert.f -L/fou/hav/felles/lib/libmi_pgi/lib/libmi_pgi.a -lmi 

program rotate_winds
  implicit none
  integer              igtypein,igtypeot,ierror,icall,it,undef
  real                 gridin(6),gridot(6)
  parameter    (undef= -32767)


  INTEGER              grid,len,NLAT,NLON,PROD,ntimes,length
  PARAMETER            (grid=2115,len=66,NLAT=580,NLON=315,PROD=88,ntimes=len)
  
  
  CHARACTER*50         infile
  CHARACTER*50         outfile
  
  
  !     felt variables:
  INTEGER              maxsiz, ldata,ihour
  INTEGER              vlevel
  PARAMETER            (maxsiz=NLON*NLAT)
  PARAMETER            (ldata=20+maxsiz+50)
  PARAMETER            (vlevel=1000)
  INTEGER*2            in(1:16),idata(1:ldata)
  
  REAL                 rr_field(NLON,NLAT),uwind(NLON,NLAT),vwind(NLON,NLAT),vturn(4,NLAT*NLON)

!--------------------------------------------------------------
  in(1) = PROD
  in(2) = grid
  in(3) = -32767
  in(4) = -32767
  in(5) = -32767
  in(9) = 3
  in(11)= -32767
  in(13)= vlevel
  in(14)= 0
  
  
  icall=2

! Definisjon av grid in oog ut
  igtypein=3
  gridin(1)=10.505
  gridin(2)=-6.895
  gridin(3)=0.036
  gridin(4)=0.036
  gridin(5)=-24
  gridin(6)=66.5
  igtypeot=5
  gridot(1)=gridin(1)
  gridot(2)=gridin(2)
  gridot(3)=4
  gridot(4)=4
  gridot(5)=66.5
  gridot(6)=0
  
!All defs done!!

  call getarg(1,infile)
  call getarg(2,outfile)

  do it=0,ntimes,1            
     in(10)=it           !each r            
     IF(it.gt.0) THEN
        in(9)=2
     END IF
     ! read u component of wind 
     !write(*,*) 'reading u' 
     in(12)=33              !u componet of the wind
     CALL mrfelt(0,infile,20,in,1,maxsiz,rr_field,1.,ldata,idata,ierror)
     IF (ierror.NE.0) THEN
        WRITE(*,*) 'Error reading file: ',infile
        STOP
     END IF
     uwind=rr_field
     
     ! read v component of wind         
     !write(*,*) 'reading v' 
     in(12)=34              !v componet of the wind
     CALL mrfelt(0,infile,20,in,1,maxsiz,rr_field,1.,ldata,idata,ierror)        
     IF (ierror.NE.0) THEN
        WRITE(*,*) 'Error reading file: ',infile
        STOP
     END IF
     vwind=rr_field
 
     !call gintuvp(0,undef,uwind,vwind,igtypein,gridin,igtypeot,gridot,ierror)
     call grv2grv(icall,igtypein,gridin,igtypeot,gridot,uwind,vwind,NLON,NLAT,vturn,undef,ierror)
     IF (ierror.NE.0) STOP 'ierror .ne. 0 '
     !write(*,*) 'rotating done...'
     
     idata(6)=33
     idata(8)=0
     ! write  u componet of the wind (file previously opened by using nyfelt)
     !write(*,*) 'writing u' 
     CALL mwfelt(0,outfile,10,1,maxsiz,uwind,1.,ldata, idata,ierror)
     IF (ierror.NE.0) STOP 'ierror .ne. 0 '
     
     idata(6)=34
     idata(8)=0
     ! write  v componet of the wind
     !write(*,*) 'writing v' 
     CALL mwfelt(0,outfile,10,1,maxsiz,vwind,1.,ldata, idata,ierror)
     IF (ierror.NE.0) STOP 'ierror .ne. 0 '  
     
     vwind=0.0
     uwind=0.0
  end do

end program rotate_winds
