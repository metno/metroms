#include "cppdefs.h"
      MODULE frazil_ice_prod_mod
#if defined CICE_COUPLING
      use mod_kinds, only: r8
!
!jd- MET.no                                                                     
!
!  This routine computes the frazil ice growth in the water when the
!  water temperature gets below freezing by taking account of the energy 
!  difference between under-cooled water, and water at the freezing point.
!  Temperature is then set back to the freezing-temperature. 
!  This version takes implicitly account of the change in salinity when 
!  frazil-ice is formed, and the change in salinity is estimated before the 
!  Available energy for freezing is calculated, and stored. 
!
!  No known reference at present. Based on calculation by Jens Debernard
!          inspired by Steele et al. (1989). JPO, 19, 139-147, 
!          but without taking account change in ocean enthaply due to mass 
!          exchange between ocean model and ice model. The present 
!          version of ROMS, and the coupling is not consistent with a exchange
!          of mass.
!                                                                      !
!=======================================================================
!
      implicit none

      PRIVATE
      PUBLIC frazil_ice_prod, t_freeze

      CONTAINS

      real(r8) function t_freeze(s1,z1)
      real(r8), intent(in) :: s1,z1
!  Inline functions
!  Freezing temperature (Gill, 1982)
!     t_freeze(s1,z1) = -0.0575*s1 + 1.710523d-3*sqrt(s1)**3
!    &       - 2.154996d-4*s1*s1 + 0.000753*z1
!  Freezing temperature (Steele et al. 1989)
      t_freeze = -0.0543*s1 + 0.000759*z1
      return
      end function t_freeze

      SUBROUTINE frazil_ice_prod (ng, tile)

      USE mod_param
      USE mod_grid
      USE mod_ocean
      USE mod_stepping

      integer, intent(in) :: ng, tile
!
# include "tile.h"
!
# ifdef PROFILE
      CALL wclock_on (ng, iNLM, 44)
# endif
!
      CALL frazil_ice_prod_tile (ng, tile,                                   &
     &                      LBi, UBi, LBj, UBj,                         &
     &                      IminS, ImaxS, JminS, JmaxS,                 &
     &                      nnew(ng),                                   &
# ifdef MASKING
     &                      GRID(ng) % rmask,                           &
# endif
# ifdef WET_DRY
     &                      GRID(ng) % rmask_wet,                       &
# endif
     &                      GRID(ng) % Hz,                              &
     &                      GRID(ng) % z_r,                             &
     &                      OCEAN(ng) % t,                              &
     &                      Ocean(ng) % qfraz,                          &
     &                      Ocean(ng) % qfraz_accum)
# ifdef PROFILE
      CALL wclock_off (ng, iNLM, 44)
# endif
      RETURN
      END SUBROUTINE frazil_ice_prod
!
!***********************************************************************
      subroutine frazil_ice_prod_tile (ng, tile,                        &
     &                            LBi, UBi, LBj, UBj,                   &
     &                            IminS, ImaxS, JminS, JmaxS,           &
     &                            nnew,                                 &
# ifdef MASKING
     &                            rmask,                                &
# endif
# ifdef WET_DRY
     &                            rmask_wet,                            &
# endif
     &                            Hz, z_r, t, qfraz, qfraz_accum)
!***********************************************************************
!
      USE mod_param
      USE mod_scalars
!
      USE bc_2d_mod, ONLY : bc_r2d_tile
#if defined EW_PERIODIC || defined NS_PERIODIC
      USE exchange_3d_mod, ONLY : exchange_r3d_tile
#endif
# ifdef DISTRIBUTE
      USE mp_exchange_mod
# endif
!
      implicit none
!
!  Imported variable declarations.
!
      integer, intent(in) :: ng, tile
      integer, intent(in) :: LBi, UBi, LBj, UBj
      integer, intent(in) :: IminS, ImaxS, JminS, JmaxS
      integer, intent(in) :: nnew

# ifdef ASSUMED_SHAPE
#  ifdef MASKING
      real(r8), intent(in) :: rmask(LBi:,LBj:)
#  endif
#  ifdef WET_DRY
      real(r8), intent(in) :: rmask_wet(LBi:,LBj:)
#  endif
      real(r8), intent(in) :: Hz(LBi:,LBj:,:)
      real(r8), intent(in) :: z_r(LBi:,LBj:,:)
      real(r8), intent(inout) :: t(LBi:,LBj:,:,:,:)
      real(r8), intent(out) :: qfraz(LBi:,LBj:)
      real(r8), intent(out) :: qfraz_accum(LBi:,LBj:)
# else
#  ifdef MASKING
      real(r8), intent(in) :: rmask(LBi:UBi,LBj:UBj)
#  endif
#  ifdef WET_DRY
      real(r8), intent(in) :: rmask_wet(LBi:UBi,LBj:UBj)
#  endif
      real(r8), intent(in) :: Hz(LBi:UBi,LBj:UBj,N(ng))
      real(r8), intent(in) :: z_r(LBi:UBi,LBj:UBj,N(ng))
      real(r8), intent(inout) :: t(LBi:UBi,LBj:UBj,N(ng),3,NT(ng))
      real(r8), intent(out) :: qfraz(LBi:UBi,LBj:UBj)
      real(r8), intent(out) :: qfraz_accum(LBi:UBi,LBj:UBj)
# endif
!
! Local variable definitions
!

      integer :: i, j, k, itrc

      real(r8), parameter :: Lh_ice = 334000.0_r8

!jd      real(r8), parameter :: Lhat = 79.2_r8
!jd      real(r8), parameter :: r = 0.5_r8

      real(r8) :: rhocp
      real(r8) :: cpw_hat
      real(r8) :: tsuper  ! jd Amount of supercooling

      real(r8) :: t_fr

# include "set_bounds.h"

      cpw_hat= Cp/Lh_ice
      rhocp = rho0*Cp

      DO j=Jstr,Jend
        DO i=Istr,Iend
          qfraz(i,j) = 0.0_r8
        ENDDO
      ENDDO
      DO j=Jstr,Jend
        DO i=Istr,Iend
          DO k=1,N(ng)
# ifdef MASKING
            IF (rmask(i,j) .ne. 0.0_r8) THEN
# endif
# ifdef WET_DRY
            IF (rmask_wet(i,j) .ne. 0.0_r8) THEN
# endif
              t_fr = t_freeze(t(i,j,k,nnew,isalt),z_r(i,j,k))
              tsuper= max(t_fr - t(i,j,k,nnew,itemp),0.0_r8)

              t(i,j,k,nnew,isalt) = t(i,j,k,nnew,isalt) *               &
     &                                  (1.0_r8 + cpw_hat*tsuper)

              qfraz(i,j) = qfraz(i,j) + rhocp*tsuper*Hz(i,j,k)

              t(i,j,k,nnew,itemp) =  max(t(i,j,k,nnew,itemp),           &
     &             t_freeze(t(i,j,k,nnew,isalt),z_r(i,j,k)))              
# ifdef WET_DRY
            END IF
# endif
# ifdef MASKING
            END IF
# endif
          END DO
          qfraz(i,j) = qfraz(i,j)/dt(ng)
          IF (qfraz(i,j) .lt. 0.0_r8) THEN
            print *, 'trouble in frazil_ice_mod', i, j,                 &
     &         t(i,j,N(ng),nnew,itemp), t(i,j,N(ng),nnew,isalt),        &
     &         qfraz(i,j), Hz(i,j,N(ng))
          END IF
        END DO
      END DO
      CALL bc_r2d_tile (ng, tile,                                       &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          qfraz)
#ifdef DISTRIBUTE
      CALL mp_exchange2d (ng, tile, iNLM, 1,                            &
     &                    LBi, UBi, LBj, UBj,                           &
     &                    NghostPoints, EWperiodic(ng), NSperiodic(ng), &
     &                    qfraz)
#endif

      DO j=Jstr,Jend
        DO i=Istr,Iend
          qfraz_accum(i,j) = qfraz_accum(i,j) + qfraz(i,j)
        ENDDO
      ENDDO

      CALL bc_r2d_tile (ng, tile,                                       &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          qfraz_accum)
#ifdef DISTRIBUTE
      CALL mp_exchange2d (ng, tile, iNLM, 1,                            &
     &                    LBi, UBi, LBj, UBj,                           &
     &                    NghostPoints, EWperiodic(ng), NSperiodic(ng), &
     &                    qfraz_accum)
#endif

# if defined EW_PERIODIC || defined NS_PERIODIC
!
!  Apply periodic boundary conditions.
!
      DO itrc=1,NT(ng)
        CALL exchange_r3d_tile (ng, tile,                               &
     &                          LBi, UBi, LBj, UBj, 1, N(ng),           &
     &                          t(:,:,:,nnew,itrc))
      END DO
# endif
# ifdef DISTRIBUTE
      DO itrc=1,NT(ng)
        CALL mp_exchange3d (ng, tile, iNLM, 1,                          &
     &                    LBi, UBi, LBj, UBj, 1, N(ng),                 &
     &                    NghostPoints, EWperiodic(ng), NSperiodic(ng), &
     &                    t(:,:,:,nnew,itrc))
      END DO
# endif
      RETURN
      END SUBROUTINE frazil_ice_prod_tile

#endif
      END MODULE frazil_ice_prod_mod

