
# Script to generate a CICE restart file based on a ROMS ini (clm) file.
# Assumes an existing CICE restart to fill values into (to avoid having to
# make netcdf vartiables an attributes manually (should probably add support
# for this in the future, though, to make it more general).
#
# Was made when TOPAZ4 was the clm file data so a "hack" is used for most
# ice variables to use the limited information from the TOPAZ4 ice fields (it
# does not have ice categories, vertical temp/salt profiles and much more).

import datetime
import netCDF4
import numpy as np
import sys

def set_to_val(nc,variable,icemask,val=0.0,n=-999):
    var    = nc.variables[variable]
    if n < 0:
        var[:] = val*icemask
    else:
        var[n,:] = val*icemask

# handle command line arguments
if len(sys.argv) == 7:
    date = datetime.datetime.strptime(sys.argv[4], "%Y%m%d")  # date to use for filling global attributes
    start_year = int(sys.argv[5])                             # start year for CICE (year_init in ice_in namelist)
    time_step = float(sys.argv[6])                            # planned model time step for filling global attributes
elif len(sys.argv) != 4:
    raise ValueError("Usage: python roms_ini_to_cice.py <romsifile> <romsgrid> <ciceofile> [<date>] [start_year] [<time_step>]")

romsfile = sys.argv[1]  # some existing roms ini file
romsgrid = sys.argv[2]  # grid file for model
cicefile = sys.argv[3]  # some existing CICE restart file (to be filled below)

# categories (from CICE) and variables to set to zero everywhere
NCAT = [0, 0.6445072, 1.391433, 2.470179, 4.567288, 1e+08] # upper limits of ice categories
varlist2d_null = ['uvel','vvel','scale_factor','swvdr','swvdf','swidr','swidf','strocnxT','strocnyT',
                  'stressp_1','stressp_2','stressp_3','stressp_4','stressm_1','stressm_2','stressm_3',
                  'stressm_4','stress12_1','stress12_2','stress12_3','stress12_4','iceumask','fsnow']
varlist3d_null = ['vsnon','iage','apnd','hpnd','ipnd','dhs','ffrac','qsno001']

# open roms and CICE ini file
nc_roms   = netCDF4.Dataset(romsfile)
grid_roms = netCDF4.Dataset(romsgrid)
nc_cice   = netCDF4.Dataset(cicefile,'r+')

# key CICE variables
aicen_c   = nc_cice.variables['aicen']  # ice concentration per category in grid cell
vicen_c   = nc_cice.variables['vicen']  # volume per unit area of ice (in category n)

# key ROMS/clm variables
mask_r    = grid_roms.variables['mask_rho']
aice_r    = nc_roms.variables['aice']   # mean ice concentration for grid cell
hice_r    = nc_roms.variables['hice']

icemask = np.where((aice_r[:]*mask_r[:]) > 0.1, 1, 0)

# loop over categories and fill CICE restart file
for n in range(len(NCAT[1:])):
    print("\nUpper limit: {}".format(NCAT[n+1]))
    
    # get ice concentration, volume and snow volume from roms ini file
    cat_mask = np.logical_and( hice_r[:,:] > NCAT[n], hice_r[:,:] <= NCAT[n+1] )
    aicen_c[n,:,:] = np.where(cat_mask, aice_r[:,:], 0.0) * mask_r[:,:]
    vicen_c[n,:,:] = np.where(cat_mask, hice_r[:,:]*aice_r[:,:], 0.0) * mask_r[:,:]
    
    # Make sure no negative values exist:
    aicen_c[n,:,:] = np.where(aicen_c[n,:,:] < 0.0, 0, aicen_c[n,:,:])
    vicen_c[n,:,:] = np.where(vicen_c[n,:,:] < 0.0, 0, vicen_c[n,:,:])
    
    print("Setting 3D-vars to zero...")
    for s in varlist3d_null:
        print(s, n)
        set_to_val(nc_cice,s,icemask,0.0,n)
    
    # some "best guesses" from some variables
    set_to_val(nc_cice,'sice001',icemask,5.0,n)
    set_to_val(nc_cice,'sice002',icemask,5.0,n)
    set_to_val(nc_cice,'sice003',icemask,5.0,n)
    set_to_val(nc_cice,'sice004',icemask,5.0,n)
    set_to_val(nc_cice,'sice005',icemask,5.0,n)
    set_to_val(nc_cice,'sice006',icemask,5.0,n)
    set_to_val(nc_cice,'sice007',icemask,5.0,n)
    set_to_val(nc_cice,'Tsfcn',icemask,-5.0,n)
    set_to_val(nc_cice,'qice001',icemask,-2.0e8,n)
    set_to_val(nc_cice,'qice002',icemask,-2.0e8,n)
    set_to_val(nc_cice,'qice003',icemask,-2.0e8,n)
    set_to_val(nc_cice,'qice004',icemask,-2.0e8,n)
    set_to_val(nc_cice,'qice005',icemask,-2.0e8,n)
    set_to_val(nc_cice,'qice006',icemask,-2.0e8,n)
    set_to_val(nc_cice,'qice007',icemask,-2.0e8,n)
    set_to_val(nc_cice,'alvl',icemask,1.0,n)
    set_to_val(nc_cice,'vlvl',icemask,1.0,n)

print("\nSetting 2D-vars to zero...")
for s in varlist2d_null:
    print(s)
    set_to_val(nc_cice,s,icemask,0.0)

# finally update CICE restart file global attributes
print("\nUpdating global attributes...")

if len(sys.argv) == 7:
    date_startyear = datetime.datetime(start_year, 1, 1)
    time = float((date - date_startyear).total_seconds())

    if time_step == 0:
        nc_cice.istep1 = 0  # "undefined"
    elif time % time_step != 0:
        print("WARNING: time_step {} doesn't an integer times between {} and {}".format(date_startyear, date))
        nc_cice.istep1 = int(round(time/time_step))
    else:
        nc_cice.istep1 = int(time/time_step)
        
    nc_cice.time = time
    nc_cice.time_forc = time
    nc_cice.nyr = date.year - (start_year - 1)
    nc_cice.month = date.month
    nc_cice.mday = date.day
    nc_cice.sec = date.second
    nc_cice.accum_time = time_step

else:
    nc_cice.istep1 = 0
    nc_cice.time = 0.0
    nc_cice.time_forc = 0.0
    nc_cice.nyr = 1
    nc_cice.month = 1
    nc_cice.mday = 1
    nc_cice.sec = 0.0

nc_cice.close()
print("done")
