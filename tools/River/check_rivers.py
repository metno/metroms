#!/usr/bin/env python
# -*- coding: utf-8 -*-

# Skript for å sjekke plassering av elver i ROMS oppsett
# Versjon som virker mot river-filen
#
# Bjørn Ådlandsvik <bjorn@imr.no>
# 2010-06-02

import numpy as np
from netCDF4 import Dataset

# -------------
# Settings
# --------------

grid_file = "../../Input/norkyst_800m_grid.nc"
river_file = "../../Input/norkyst_800m_river.nc"

# -----------------
# Read grid file
# -----------------

f = Dataset(grid_file)
H = f.variables['h'][:]
M = f.variables['mask_rho'][:]
f.close()

# ------------------
# Read river file
# ------------------

f = Dataset(river_file)

river = len(f.dimensions['river'])

#print "Number of rivers = ", river

I = f.variables['river_Xposition'][:]
J = f.variables['river_Eposition'][:]
dir = f.variables['river_direction'][:]
sign = np.sign(f.variables['river_transport'][0,:])

f.close()

# --------
# Check
# --------

for n in xrange(river):

    i = int(I[n])
    j = int(J[n])

    if dir[n] == 0: # U point
        if sign[n] > 0: # from left
            # Må ha (i-1,j) på land og (i,j) = sjø¸
            if M[j,i-1]:   print "Error with river, source", n+1, "from sea cell"
            if not M[j,i]: print "Error with river, source", n+1, "to land cell"
            print "grid cell: %4d %5d %5d %8.1f %4d %4d" % ( n+1, i, j, H[j,i], dir[n], sign[n])
        else:
            # Må ha (i,j) på land og (i-1,j) på sjø¸
            if M[j,i]:       print "Error with river, source", n+1, "from sea cell"
            if not M[j,i-1]: print "Error with river, source", n+1, "to land cell"
            print "grid cell: %4d %5d %5d %8.1f %4d %4d" % ( n+1, i-1, j, H[j,i-1], dir[n], sign[n])

    else: # V-point
        if sign[n] > 0: # "up"-wards
            # Må ha (i,j-1) på land og (i,j) på sjø¸
            if M[j-1,i]:   print "Error with river, source", n+1, "from sea cell"
            if not M[j,i]: print "Error with river, source", n+1, "to land cell"
            print "grid cell: %4d %5d %5d %8.1f %4d %4d" % (n+1, i, j, H[j,i], dir[n], sign[n])
        else:
            # Må ha (i,j) på land og (i,j-1) på sjø¸
            if M[j,i]:       print "Error with river, source", n+1, "from sea cell"
            if not M[j-1,i]: print "Error with river, source", n+1, "to land cell"
            print "grid cell: %4d %5d %5d %8.1f %4d %4d" % (n+1, i, j-1, H[j-1,i], dir[n], sign[n])

