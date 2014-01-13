subroutine spec_vert_grid(Lp,Mp,N,h,Zt_avg1,Tcline,theta_b,theta_s,Vtransform,Vstretching,z_r)
implicit none
integer, intent(in)	:: Lp,Mp,N,Vtransform,Vstretching
real, intent(in)   	:: h(Lp,Mp),Tcline,theta_b,theta_s,Zt_avg1(Lp,Mp)
real, intent(out)   	:: z_r(Lp,Mp,N)
real			:: z_w(Lp,Mp,N)
real			:: Cs_r(N),Cs_w(N),hmin,hc,cff_r,cff1_r,cff2_r,cff_w,cff1_w,cff2_w,hwater,hinv
!real                	:: sc_r(N),Cs_r(N),sc_w(N),Cs_w(N),hmin,hc,cff1,cff2,cff_r,cff1_r,cff2_r,cff
integer			:: i,j,k
real	 		:: Aweight, Bweight, Cweight, Cbot, Csur, Hscale
real	 		:: ds, exp_bot, exp_sur, sc_r(N), sc_w(N)
real	 		:: cff1, cff2
real	 		:: z_w0, z_r0, sc_wi, sc_ri


! Vertical grid specification 
hmin = 1.0e+23
do j=1,Mp
do i=1,Lp
  hmin = min(hmin,h(i,j))
enddo
enddo
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!The following code is copied from 'set_scoord.F' and 'set_depth.F' in the official ROMS code.
!There are minor alterations (pointers removed, some vars renamed and types altered)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!-----------------------------------------------------------------------
!  Set thickness controlling vertical coordinate stretching.
!-----------------------------------------------------------------------
!
!  Set hc <= hmin, in the original formulation (Vtransform=1) to avoid
!  [h(x,y)-hc] to be negative which results in dz/ds to be negative.
!  Notice that this restriction is REMOVED in the new transformation
!  (Vtransform=2): hc can be any value. It works for both hc < hmin
!  and hc > hmin.
!
      IF (Vtransform.eq.1) THEN
        hc=MIN(hmin,Tcline)
      ELSE IF (Vtransform.eq.2) THEN
        hc=Tcline
      END IF
!
!-----------------------------------------------------------------------
!  Original vertical strectching function, Song and Haidvogel (1994).
!-----------------------------------------------------------------------
!
      IF (Vstretching.eq.1) THEN
!
!  This vertical stretching function is defined as:
!
!      C(s) = (1 - b) * [SINH(s * a) / SINH(a)] +
!
!             b * [-0.5 + 0.5 * TANH(a * (s + 0.5)) / TANH(0.5 * a)]
!
!  where the stretching parameters (a, b) are specify at input:
!
!         a = theta_s               0 <  theta_s <= 8
!         b = theta_b               0 <= theta_b <= 1
!
!  If theta_b=0, the refinement is surface intensified as theta_s is
!  increased.
!  If theta_b=1, the refinement is both bottom ans surface intensified
!  as theta_s is increased.
!
        IF (theta_s.ne.0.0) THEN
          cff1=1.0/SINH(theta_s)
          cff2=0.5/TANH(0.5*theta_s)
        END IF
        !sc_w(0)=-1.0
        !Cs_w(0)=-1.0
        ds=1.0/REAL(N)
        DO k=1,N
          sc_w(k)=ds*REAL(k-N)
          sc_r(k)=ds*(REAL(k-N)-0.5)
          IF (theta_s.ne.0.0) THEN
            Cs_w(k)=(1.0-theta_b)*cff1*SINH(theta_s*sc_w(k))+theta_b*(cff2*TANH(theta_s*(sc_w(k)+0.5))-0.5)
            Cs_r(k)=(1.0-theta_b)*cff1*SINH(theta_s*sc_r(k))+theta_b*(cff2*TANH(theta_s*(sc_r(k)+0.5))-0.5)
          ELSE
            Cs_w(k)=sc_w(k)
            Cs_r(k)=sc_r(k)
          END IF
        END DO
!
!-----------------------------------------------------------------------
!  A. Shchepetkin vertical stretching function. This function was
!  improved further to allow bottom refiment (see Vstretching=4).
!-----------------------------------------------------------------------
!
      ELSE IF (Vstretching.eq.2) THEN
!
!  This vertical stretching function is defined, in the simplest form,
!  as:
!
!      C(s) = [1.0 - COSH(theta_s * s)] / [COSH(theta_s) - 1.0]
!
!  it is similar in meaning to the original vertical stretcing function
!  (Song and Haidvogel, 1994), but note that hyperbolic functions are
!  COSH, and not SINH.
!
!  Note that the above definition results in
!
!         -1 <= C(s) <= 0
!
!  as long as
!
!         -1 <= s <= 0
!
!  and, unlike in any previous definition
!
!         d[C(s)]/ds  -->  0      if  s -->  0
!
!  For the purpose of bottom boundary layer C(s) is further modified
!  to allow near-bottom refinement.  This is done by blending it with
!  another function.
!
        Aweight=1.0
        Bweight=1.0
        ds=1.0/REAL(N)
!
        sc_w(N)=0.0
        Cs_w(N)=0.0
        DO k=N-1,1,-1
          sc_wi=ds*REAL(k-N)
          sc_w(k)=sc_wi
          IF (theta_s.gt.0.0) THEN
            Csur=(1.0-COSH(theta_s*sc_wi))/(COSH(theta_s)-1.0)
            IF (theta_b.gt.0.0) THEN
              Cbot=SINH(theta_b*(sc_wi+1.0))/SINH(theta_b)-1.0
              Cweight=(sc_wi+1.0)**Aweight*(1.0+(Aweight/Bweight)*(1.0-(sc_wi+1.0)**Bweight))
              Cs_w(k)=Cweight*Csur+(1.0-Cweight)*Cbot
            ELSE
              Cs_w(k)=Csur
            END IF
          ELSE
            Cs_w(k)=sc_wi
          END IF
        END DO
        !sc_w(0)=-1.0
        !Cs_w(0)=-1.0
!
        DO k=1,N
          sc_ri=ds*(REAL(k-N)-0.5)
          sc_r(k)=sc_ri
          IF (theta_s.gt.0.0) THEN
            Csur=(1.0-COSH(theta_s*sc_ri))/(COSH(theta_s)-1.0)
            IF (theta_b.gt.0.0) THEN
              Cbot=SINH(theta_b*(sc_ri+1.0))/SINH(theta_b)-1.0
              Cweight=(sc_ri+1.0)**Aweight*(1.0+(Aweight/Bweight)*(1.0-(sc_ri+1.0)**Bweight))
              Cs_r(k)=Cweight*Csur+(1.0-Cweight)*Cbot
            ELSE
              Cs_r(k)=Csur
            END IF
          ELSE
            Cs_r(k)=sc_ri
          END IF
        END DO
!
!-----------------------------------------------------------------------
!  R. Geyer stretching function for high bottom boundary layer
!  resolution.
!-----------------------------------------------------------------------
!
      ELSE IF (Vstretching.eq.3) THEN
!
!  This stretching function is intended for very shallow coastal
!  applications, like gravity sediment flows.
!
!  At the surface, C(s=0)=0
!
!      C(s) = - LOG(COSH(Hscale * ABS(s) ** alpha)) /
!               LOG(COSH(Hscale))
!
!  At the bottom, C(s=-1)=-1
!
!      C(s) = LOG(COSH(Hscale * (s + 1) ** beta)) /
!             LOG(COSH(Hscale)) - 1
!
!  where
!
!       Hscale : scale value for all hyperbolic functions
!                  Hscale = 3.0    set internally here
!        alpha : surface stretching exponent
!                  alpha = 0.65   minimal increase of surface resolution
!                          1.0    significant amplification
!         beta : bottoom stretching exponent
!                  beta  = 0.58   no amplification
!                          1.0    significant amplification
!                          3.0    super-high bottom resolution
!            s : stretched vertical coordinate, -1 <= s <= 0
!                  s(k) = (k-N)/N       k=0:N,    W-points  (s_w)
!                  s(k) = (k-N-0.5)/N   k=1:N,  RHO-points  (s_rho)
!
!  The stretching exponents (alpha, beta) are specify at input:
!
!         alpha = theta_s
!         beta  = theta_b
!
        exp_sur=theta_s
        exp_bot=theta_b
        Hscale=3.0
        ds=1.0/REAL(N)
!
        sc_w(N)=0.0
        Cs_w(N)=0.0
        DO k=N-1,1,-1
          sc_wi=ds*REAL(k-N)
          sc_w(k)=sc_wi
          Cbot= LOG(COSH(Hscale*(sc_wi+1.0)**exp_bot))/               &
     &          LOG(COSH(Hscale))-1.0
          Csur=-LOG(COSH(Hscale*ABS(sc_wi)**exp_sur))/                   &
     &          LOG(COSH(Hscale))
          Cweight=0.5*(1.0-TANH(Hscale*(sc_wi+0.5)))
          Cs_w(k)=Cweight*Cbot+(1.0-Cweight)*Csur
        END DO
        !sc_w(0)=-1.0
        !Cs_w(0)=-1.0
!
        DO k=1,N
          sc_ri=ds*(REAL(k-N)-0.5)
          sc_r(k)=sc_ri
          Cbot= LOG(COSH(Hscale*(sc_ri+1.0)**exp_bot))/               &
     &          LOG(COSH(Hscale))-1.0
          Csur=-LOG(COSH(Hscale*ABS(sc_ri)**exp_sur))/                   &
     &          LOG(COSH(Hscale))
          Cweight=0.5*(1.0-TANH(Hscale*(sc_ri+0.5)))
          Cs_r(k)=Cweight*Cbot+(1.0-Cweight)*Csur
        END DO
!
!-----------------------------------------------------------------------
!  A. Shchepetkin improved double vertical stretching functions with
!  bottom refiment.
!-----------------------------------------------------------------------
!
      ELSE IF (Vstretching.eq.4) THEN
!
!  The range of meaningful values for the control parameters are:
!
!       0 <  theta_s <= 10.0
!       0 <= theta_b <=  3.0
!
!  Users need to pay attention to extreme r-factor (rx1) values near
!  the bottom.
!
!  This vertical stretching function is defined, in the simplest form,
!  as:
!
!      C(s) = [1.0 - COSH(theta_s * s)] / [COSH(theta_s) - 1.0]
!
!  it is similar in meaning to the original vertical stretcing function
!  (Song and Haidvogel, 1994), but note that hyperbolic functions are
!  COSH, and not SINH.
!
!  Note that the above definition results in
!
!         -1 <= C(s) <= 0
!
!  as long as
!
!         -1 <= s <= 0
!
!  and
!
!         d[C(s)]/ds  -->  0      if  s -->  0
!
!  For the purpose of bottom boundary layer C(s) is further modified
!  to allow near-bottom refinement by using a continuous, second
!  stretching function
!
!         C(s) = [EXP(theta_b * C(s)) - 1.0] / [1.0 - EXP(-theta_b)]
!
!  This double transformation is continuous with respect to "theta_s"
!  and "theta_b", as both values approach to zero.
!
        ds=1.0/REAL(N)
!
        sc_w(N)=0.0
        Cs_w(N)=0.0
        DO k=N-1,1,-1
          sc_wi=ds*REAL(k-N)
          sc_w(k)=sc_wi
          IF (theta_s.gt.0.0) THEN
            Csur=(1.0-COSH(theta_s*sc_wi))/                       &
     &           (COSH(theta_s)-1.0)
          ELSE
            Csur=-sc_wi**2
          END IF
          IF (theta_b.gt.0.0) THEN
            Cbot=(EXP(theta_b*Csur)-1.0)/                        &
     &           (1.0-EXP(-theta_b))
            Cs_w(k)=Cbot
          ELSE
            Cs_w(k)=Csur
          END IF
        END DO
        !sc_w(0)=-1.0
        !Cs_w(0)=-1.0
!
        DO k=1,N
          sc_ri=ds*(REAL(k-N)-0.5)
          sc_r(k)=sc_ri
          IF (theta_s.gt.0.0) THEN
            Csur=(1.0-COSH(theta_s*sc_ri))/                       &
     &           (COSH(theta_s)-1.0)
          ELSE
            Csur=-sc_ri**2
          END IF
          IF (theta_b.gt.0.0) THEN
            Cbot=(EXP(theta_b*Csur)-1.0)/                        &
     &           (1.0-EXP(-theta_b))
            Cs_r(k)=Cbot
          ELSE
            Cs_r(k)=Csur
          END IF
        END DO
   END IF
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!-----------------------------------------------------------------------
!  Original formulation: Compute vertical depths (meters, negative) at
!                        RHO- and W-points, and vertical grid
!  thicknesses. Various stretching functions are possible.
!
!         z_w(x,y,s,t) = Zo_w + zeta(x,y,t) * [1.0 + Zo_w / h(x,y)]
!
!                 Zo_w = hc * [s(k) - C(k)] + C(k) * h(x,y)
!
!-----------------------------------------------------------------------
!
      IF (Vtransform.eq.1) THEN	
        DO j=1,Mp
          DO i=1,Lp
            z_w(i,j,1)=-h(i,j)   
          END DO
          DO k=1,N !standard er k=1, blir prob at z_w refer til k=0!!
            cff_r=hc*(sc_r(k)-Cs_r(k))
            cff_w=hc*(sc_w(k)-Cs_w(k))
            cff1_r=Cs_r(k)
            cff1_w=Cs_w(k)
            DO i=1,Lp  !antar zeta=0
              hwater=h(i,j)
              hinv=1.0/hwater
              z_w0=cff_w+cff1_w*hwater
              z_w(i,j,k)=z_w0+Zt_avg1(i,j)*(1.0+z_w0*hinv) !håper Zt_avg1 er samme som zeta... sjekke!!
              z_r0=cff_r+cff1_r*hwater
              z_r(i,j,k)=z_r0+Zt_avg1(i,j)*(1.0+z_r0*hinv)
              !Hz(i,j,k)=z_w(i,j,k)-z_w(i,j,k-1)
          END DO
        END DO
      END DO
!
!-----------------------------------------------------------------------
!  New formulation: Compute vertical depths (meters, negative) at
!                   RHO- and W-points, and vertical grid thicknesses.
!  Various stretching functions are possible.
!
!         z_w(x,y,s,t) = zeta(x,y,t) + [zeta(x,y,t)+ h(x,y)] * Zo_w
!
!                 Zo_w = [hc * s(k) + C(k) * h(x,y)] / [hc + h(x,y)]
!
!-----------------------------------------------------------------------
!
      ELSE IF (Vtransform.eq.2) THEN
        DO j=1,Mp
          DO i=1,Lp
            z_w(i,j,1)=-h(i,j)
          END DO
          DO k=1,N
            cff_r=hc*sc_r(k)
            cff_w=hc*sc_w(k)
            cff1_r=Cs_r(k)
            cff1_w=Cs_w(k)
            DO i=1,Lp
              hwater=h(i,j)
              hinv=1.0/(hc+hwater)
              cff2_r=(cff_r+cff1_r*hwater)*hinv
              cff2_w=(cff_w+cff1_w*hwater)*hinv
              z_w(i,j,k)=Zt_avg1(i,j)+(Zt_avg1(i,j)+hwater)*cff2_w
              z_r(i,j,k)=Zt_avg1(i,j)+(Zt_avg1(i,j)+hwater)*cff2_r
	      !antar zeta=0
!               z_w(i,j,k)=hwater*cff2_w
!               z_r(i,j,k)=hwater*cff2_r
              !Hz(i,j,k)=z_w(i,j,k)-z_w(i,j,k-1)
            END DO
          END DO
        END DO
      END IF
!      write(*,*) '#####################################'
      write(*,*) 'h:', -h(80,80), 'zeta:', Zt_avg1(80,80)
      write(*,*) z_r(80,80,:)
!      write(*,*) '#####################################'
end subroutine spec_vert_grid
