
module fill_mod
  implicit none

  integer :: npn, npo, ntotal, n, nm1

  integer, allocatable, dimension(:,:) :: iind, jind
  
  real, dimension(-1:1,-1:1) :: mwg 
  real, dimension(-1:1,-1:1) :: wg 

  real, allocatable, dimension(:,:) :: rmask
  real, allocatable, dimension(:)   :: fval
  integer, allocatable, dimension(:)   :: imask
  

  logical :: notready = .true.

contains



  subroutine fill_init(ilo,ihi,jlo,jhi,mask) 
    integer, intent(in) :: ilo,ihi,jlo,jhi
    real, intent(in),  dimension(ilo:ihi,jlo:jhi) :: mask

    integer :: i,j,k,l

    if (notready) then
       ntotal = (ihi-ilo + 1)*(jhi-jlo+1)
       allocate(rmask(ilo:ihi,jlo:jhi))
       allocate(iind(ntotal,2))
       allocate(jind(ntotal,2))
       allocate(fval(ntotal))
       allocate(imask(ntotal))
       notready = .false.
    end if

    rmask = mask
    imask = 1.0
    iind = -1
    jind = -1

    nm1=1
    n=2

    npn = 0
    do j=jlo+1,jhi-1
       do i=ilo+1,ihi-1
          if (mask(i,j) < 0.5 ) then
             npn = npn + 1
             rmask(i,j) = 0.0
             iind(npn,n) = i
             jind(npn,n) = j
          end if
       end do
    end do

    wg = 0.0
    do k=-1,0
       wg(:,k) = wg(:,k) + 1
       wg(:,k+1) = wg(:,k+1) + 1
    end do
    wg(0,:) = wg(-1,:) + wg(1,:)  

!    write(*,*) wg


  end subroutine fill_init

end module fill_mod
