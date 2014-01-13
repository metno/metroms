      subroutine earthr(rearth)
c
c  NAME:
c     earthr
c
c  PURPOSE:
c     Return earth radius in unit meter,
c     i.e. the DNMI standard value.
c
c  SYNOPSIS:
c     subroutine earthr(rearth)
c     real rearth
c
c  OUTPUT:
c     rearth  - earth radius i unit meter
c
c-----------------------------------------------------------------------
c  DNMI/FoU  xx.xx.19xx  ............ ... rearth = 6368.00 km in models
c  DNMI/FoU  25.08.1995  Anstein Foss ... rearth = 6371.22 km
c  DNMI/FoU  21.03.1996  Anstein Foss ... rearth = 6371.00 km
c-----------------------------------------------------------------------
c
      real rearth
c
      rearth = 6371000.
c
      return
      end
