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
          real(r8), pointer :: aice(:,:)      ! ice concentration
          real(r8), pointer :: aice_u(:,:)    ! ice concentration on u-points
          real(r8), pointer :: aice_v(:,:)    ! ice concentration on v-points
          real(r8), pointer :: fhocnAI(:,:)   ! nonradiative heat flux under ice
          real(r8), pointer :: fswthruAI(:,:) ! radiative heat flux under ice
          real(r8), pointer :: freshAI(:,:)   ! freshwater flux ice to ocean (kg/s/m**2)
          real(r8), pointer :: fsaltAI(:,:)   ! salt flux ice to ocean (kg/s/m**2)
          real(r8), pointer :: strx(:,:)      ! x-stress on CICE psi-points
          real(r8), pointer :: stry(:,:)      ! y-stress on CICE psi-points
          real(r8), pointer :: stru(:,:)      ! x-stress on ROMS u-points
          real(r8), pointer :: strv(:,:)      ! y-stress on ROMS v-points
        END TYPE T_ICE

! Note that all fluxes come multiplied by the (CICE) ice concentration, so e.g. strx = a*strx

        TYPE (T_ICE), allocatable :: ICE(:)

        CONTAINS

        SUBROUTINE allocate_ice (ng, LBi, UBi, LBj, UBj)

        USE mod_param
      
        integer, intent(in) :: ng, LBi, UBi, LBj, UBj

        if (ng.eq.1) allocate ( ICE(Ngrids) )
        allocate ( ICE(ng)%aice(LBi:UBi,LBj:UBj) )
        allocate ( ICE(ng)%aice_u(LBi:UBi,LBj:UBj) )
        allocate ( ICE(ng)%aice_v(LBi:UBi,LBj:UBj) )
        allocate ( ICE(ng)%fhocnAI(LBi:UBi,LBj:UBj) )
        allocate ( ICE(ng)%fswthruAI(LBi:UBi,LBj:UBj) )
        allocate ( ICE(ng)%freshAI(LBi:UBi,LBj:UBj) )
        allocate ( ICE(ng)%fsaltAI(LBi:UBi,LBj:UBj) )
        allocate ( ICE(ng)%strx(LBi:UBi,LBj:UBj) ) ! Could be local variable in ocean_coupler? 
        allocate ( ICE(ng)%stry(LBi:UBi,LBj:UBj) ) ! Could be local variable in ocean_coupler?
        allocate ( ICE(ng)%stru(LBi:UBi,LBj:UBj) )
        allocate ( ICE(ng)%strv(LBi:UBi,LBj:UBj) )
        
        ICE(ng)%aice = 0
        ICE(ng)%aice_u = 0
        ICE(ng)%aice_v = 0
        ICE(ng)%fhocnAI = 0
        ICE(ng)%freshAI = 0
        ICE(ng)%fsaltAI = 0
        ICE(ng)%fswthruAI = 0
        ICE(ng)%strx = 0
        ICE(ng)%stry = 0
        ICE(ng)%stru = 0
        ICE(ng)%strv = 0
        
        RETURN
        END SUBROUTINE allocate_ice
        END MODULE mod_ice
  
  
