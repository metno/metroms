        MODULE mod_ice
!
!   temporary storage for ice data
!
!   ice_strx - ice stress in i-direction on B-grid U-point
!   ice_stry - ice stress in j-direction, B-grid U-point
!   aice - ice fraction on rho-point

!  CICE B-grid
!
!   U(i-1,j) --------------U(i,j)
!        |                    |
!        |                    |
!        |      T(i,j)        |
!        |                    |
!        |                    |
!   U(i-1,j-1)-------------U(i,j-1)
!
!
! ROMS C-grid
!
!         ------V(i,j+1)------
!        |                    |
!        |                    |
!     U(i,j)    T(i,j)   U(i+1,j)
!        |                    |
!        |                    |
!         ------V(i,j)--------
! 
!  Notice the difference in boundary condition at i=0 or j=0 

        USE mod_kinds

        implicit none

        TYPE T_ICE 
          real(r8), pointer :: strx(:,:)
          real(r8), pointer :: stry(:,:)
          real(r8), pointer :: stru(:,:)
          real(r8), pointer :: strv(:,:)
          real(r8), pointer :: a(:,:)
          real(r8), pointer :: aice_u(:,:)
          real(r8), pointer :: aice_v(:,:)
        END TYPE T_ICE

        TYPE (T_ICE), allocatable :: ICE(:)

        CONTAINS

        SUBROUTINE allocate_ice (ng, LBi, UBi, LBj, UBj)

        USE mod_param
      
        integer, intent(in) :: ng, LBi, UBi, LBj, UBj

        if (ng.eq.1) allocate ( ICE(Ngrids) )
        allocate ( ICE(ng)%strx(LBi:UBi,LBj:UBj) )
        allocate ( ICE(ng)%stry(LBi:UBi,LBj:UBj) )
        allocate ( ICE(ng)%stru(LBi:UBi,LBj:UBj) )
        allocate ( ICE(ng)%strv(LBi:UBi,LBj:UBj) )
        allocate ( ICE(ng)%a(LBi:UBi,LBj:UBj) )
        allocate ( ICE(ng)%aice_u(LBi:UBi,LBj:UBj) )
        allocate ( ICE(ng)%aice_v(LBi:UBi,LBj:UBj) )
        
        ICE(ng)%strx = 0
        ICE(ng)%stry = 0
        ICE(ng)%stru = 0
        ICE(ng)%strv = 0
        ICE(ng)%a = 0
        ICE(ng)%aice_u = 0
        ICE(ng)%aice_v = 0

        RETURN
        END SUBROUTINE allocate_ice
        END MODULE mod_ice
  
  
