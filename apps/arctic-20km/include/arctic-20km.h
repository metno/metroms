/*
** svn $Id: basin.h 8 2007-02-06 19:00:29Z arango $
*******************************************************************************
** Copyright (c) 2002-2007 The ROMS/TOMS Group
**
**   Licensed under a MIT/X style license
**
**   See License_ROMS.txt
**
*******************************************************************************
**
**  Options for ROMS
*/

#undef  RST_SINGLE         /* define if single precision restart fields */
#undef  PERFECT_RESTART    /* use to include perfect restart variables */
#define CURVGRID           /* define if using  curvilinear coordinate grid*/

#define UV_ADV             /* turn ON or OFF advection terms */
#define UV_COR             /* turn ON or OFF Coriolis term */
#undef  UV_VIS2            /* turn ON or OFF Laplacian horizontal mixing, Frode off */
#undef  UV_VIS4            /* turn ON or OFF biharmonic horizontal mixing */
#define UV_SADVECTION      /* turn ON or OFF splines vertical advection */
#define UV_QDRAG           /* turn ON or OFF quadratic bottom friction */

#undef  VISC_GRID          /* viscosity coefficient scaled by grid size */
#define NONLIN_EOS         /* define if using nonlinear equation of state */
#undef  MIX_GEO_UV         /* mixing on geopotential (constant Z) surfaces */
#undef  WJ_GRADP           /* Weighted density Jacobian (Song, 1998) */
#define DJ_GRADPS          /* Splines density  Jacobian (Shchepetkin, 2000) */
#undef  DIFF_GRID          /* diffusion coefficient scaled by grid size */

#undef  TS_DIF2            /* turn ON or OFF Laplacian horizontal mixing */
#undef  TS_DIF4            /* turn ON or OFF biharmonic horizontal mixing */
#define TS_U3HADVECTION    /* define if 3rd-order upstream horiz. advection */
#undef  TS_A4HADVECTION    /* define if 4th-order Akima horiz. advection */
#undef  TS_C4HADVECTION    /* define if 4th-order centered horizontal advection */

#undef  TS_MPDATA          /* define if recursive MPDATA 3D advection */

#undef  TS_A4VADVECTION    /* define if 4th-order Akima vertical advection */
#define TS_SVADVECTION     /* define if splines vertical advection */
#undef  TS_C4VADVECTION    /* define if 4th-order centered vertical advection */
#undef  TS_SMAGORINSKY     /* define if Smagorinsky-like diffusion */

#define MIX_GEO_TS         /* mixing on geopotential (constant Z) surfaces, Frode undef */
#undef  MIX_S_UV           /* mixing along constant S-surfaces,   Frode -*/

#define SALINITY           /* define if using salinity */
#define SOLVE3D            /* define if solving 3D primitive equations */
#undef  BODYFORCE          /* define if applying stresses as bodyforces */
#define SPLINES            /* turn ON or OFF parabolic splines reconstruction */
#define MASKING            /* define if there is land in the domain */
#define AVERAGES           /* define if writing out time-averaged data */
#undef  AVERAGES_DETIDE    /*use if writing out NLM time-averaged detided fields*/
#undef  AVERAGES_AKT       /*undef Frode, define if writing out time-averaged AKt*/
#undef  AVERAGES_AKS       /*undef Frode, define if writing out time-averaged AKs*/
#undef  AVERAGES_AKV       /* define if writing out time-averaged AKv */

#undef  STATIONS           /* define if writing out station data */
#undef  STATIONS_CGRID     /* define if extracting data at native C-grid */

#undef  BVF_MIXING         /* define if Brunt_Vaisala frequency mixing */
#define LMD_MIXING         /* define if Large et al. (1994) interior closure */
#undef  MY25_MIXING        /* define if Mellor/Yamada level-2.5 mixing */
#undef  GLS_MIXING         /* Activate Generic Length-Scale mixing */

#ifdef GLS_MIXING
# define N2S2_HORAVG       /* Activate horizontal smoothing of buoyancy/shear */
#endif
#ifdef LMD_MIXING
# define  LMD_BKPP          /* use if bottom boundary layer KPP mixing */
# define  LMD_CONVEC        /* use to add convective mixing due to shear instability */
# define  LMD_DDMIX         /* use to add double-diffusive mixing */
# define  LMD_NONLOCAL      /* use if nonlocal transport */
# define  LMD_RIMIX         /* use to add diffusivity due to shear instability */
# define  LMD_SHAPIRO       /* use if Shapiro filtering boundary layer depth */
# define  LMD_SKPP          /* use if surface boundary layer KPP mixing */
#endif

#define ANA_BSFLUX         /* analytical bottom salinity flux */
#define ANA_BTFLUX         /* analytical bottom temperature flux */
#undef  ANA_GRID           /* analytical grid set-up */
#undef  ANA_INITIAL        /* analytical initial conditions */
#undef  ANA_MASK           /* analytical land/sea masking */
#undef  ANA_MEANRHO         
#undef  ANA_SSFLUX         /* analytical surface salinity flux */
#undef  ANA_STFLUX         /* analytical surface temperature flux */
#undef  ANA_SMFLUX         /* analytical surface momentum stress */

/* CLIMATOLOGY */
#define M2CLIMATOLOGY      /* define 2D momentum climatology */
#define M3CLIMATOLOGY      /* define 3D momentum climatology */
#define TCLIMATOLOGY       /* define tracers climatology */
#undef  M2CLM_NUDGING
#define M3CLM_NUDGING      /* nudging 3D momentum climatology */
#define TCLM_NUDGING       /* nudging tracers climatology */
#define OBC_NUDGING

#undef  WRF_COUPLING       /* coupling to WRF atmospheric model */

/* ATMOSPHERIC FORCING */
#define BULK_FLUXES        /* turn ON or OFF bulk fluxes computation */
#ifdef  BULK_FLUXES
# undef  ANA_RAIN          /* analytical rain fall rate */
# undef  ANA_PAIR          /* analytical surface air pressure */
# undef  ANA_HUMIDITY      /* analytical surface air humidity */
# undef  ANA_CLOUD         /* analytical cloud fraction */
# undef  ANA_TAIR          /* analytical surface air temperature */
# undef  ANA_WINDS         /* analytical surface winds */
# define EMINUSP           /* turn ON internal calculation of E-P */
# define ANA_SRFLUX        /* analytical surface shortwave radiation flux */
# define ALBEDO            /* use albedo equation for shortwave radiation */
# undef  CLOUDS      
# undef  LONGWAVE_OUT      /* compute outgoing longwave radiation */
# define LONGWAVE          /* Compute net longwave radiation internally */
# define COOL_SKIN         /* turn ON or OFF cool skin correction *//* Ikke def hos Frode*/
# undef  SHORTWAVE
#endif

#define ATM_PRESS          /* use to impose atmospheric pressure onto sea surface */
#define SOLAR_SOURCE       /* define solar radiation source term */
#define SPECIFIC_HUMIDITY  /* if input is specific humidity in kg/kg */

/* TIDES */
#define SSH_TIDES          /* turn on computation of tidal elevation, default define */
#define UV_TIDES           /* turn on computation of tidal currents */
#define ADD_FSOBC          /* Add tidal elevation to processed OBC data, default define */
#define ADD_M2OBC          /* Add tidal currents  to processed OBC data */
#define ADD_FS_INV_BARO
#define RAMP_TIDES         /* Spin up tidal forcing */


/* ELVER */
#define UV_PSOURCE         /* turn ON or OFF point Sources/Sinks */
#define TS_PSOURCE         /* turn ON or OFF point Sources/Sinks */
/* --------------------------- */

