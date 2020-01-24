#!/usr/bin/python

import os, sys
import numpy as np
import subprocess as sp
from datetime import datetime, timedelta
try:
   from Scientific.IO.NetCDF import NetCDFFile as nc
except:
   from netCDF4 import Dataset as nc

#===========================================================
def set_to_val(f,variable,icemask,val=0.0,n=-999):
    var    = f.variables[variable]
    if n < 0:
        var[:] = val*icemask
    else:
        var[n,:] = val*icemask


#===========================================================
# Make initialization of new cice-file...
romsgrd = os.environ.get('ROMSGRD')
workdir = os.environ.get('WORKDIR')
initdir = os.environ.get('INITDIR') 
app     = os.environ.get('app')

romsfile = initdir + '/ocean_ini.nc'
romsgrid = romsgrd + '/' + app + '_grd.nc'

ncat = 5
nilyr = 7
nslyr = 1

chi = str(nilyr)
chs = str(nslyr)

#===========================================================
print romsfile

t0 = nc(romsfile).variables['ocean_time'][:][0]
t1  = datetime(1970,1,1) + timedelta(t0)
cicefile = initdir + '/iced.' + t1.strftime('%Y-%m-%d-') + str(100000 + t1.hour*3600)[1:] + '.nc'
print cicefile

sp.call(['cp',initdir+'/iced_i' + chi + '_s' + chs + '.nc',cicefile])

f = open(initdir+'/ice.restart_file','w')
f.write(cicefile)
f.close()

#===========================================================
NCAT = [0, 0.6445072, 1.391433, 2.470179, 4.567288, 1e+08] #upper limits of ice categories
varlist2d_null = ['uvel','vvel','scale_factor','swvdr','swvdf','swidr','swidf','strocnxT','strocnyT',
                  'stressp_1','stressp_2','stressp_3','stressp_4','stressm_1','stressm_2','stressm_3',
                  'stressm_4','stress12_1','stress12_2','stress12_3','stress12_4','iceumask','fsnow']
varlist3d_null = ['apnd','hpnd','ipnd','dhs','ffrac']

nc_roms   = nc(romsfile)
grid_roms = nc(romsgrid)
nc_cice   = nc(cicefile,'r+')

aice_r    = nc_roms.variables['aice'][0,:,:]   # mean ice concentration for grid cell
hice_r    = nc_roms.variables['hice'][0,:,:]
hsno_r    = nc_roms.variables['snow_thick'][0,:,:]
age_r	  = nc_roms.variables['ageice'][0,:,:]
mask_r    = grid_roms.variables['mask_rho'][:]
icemask   = np.where((aice_r*mask_r) > 0.01, 1, 0)
aice_r    = aice_r * mask_r
hice_r    = hice_r * mask_r
age_r     = age_r * mask_r

aicen_c   = nc_cice.variables['aicen']  # ice concentration per category in grid cell
vicen_c   = nc_cice.variables['vicen']  # volume per unit area of ice (in category n)
vsnon_c   = nc_cice.variables['vsnon']
agen_c    = nc_cice.variables['iage']
Tsfcn_c   = nc_cice.variables['Tsfcn']  # ice surface temperature


for n in range(len(NCAT[1:])):
    print 'Upper limit: '+str(NCAT[n+1])

    cat_mask = np.logical_and( hice_r[:,:] > NCAT[n], hice_r[:,:] <= NCAT[n+1] )

    aicen_c[n,:,:] = np.where(cat_mask, aice_r, 0.0) * mask_r
    vicen_c[n,:,:] = np.where(cat_mask, hice_r, 0.0) * aicen_c[n,:,:] * mask_r
    vsnon_c[n,:,:] = np.where(cat_mask, hsno_r, 0.0) * aicen_c[n,:,:] * mask_r
    agen_c[n,:,:]  = np.where(cat_mask, age_r,  0.0) * mask_r

    # Make sure no negative values exist:
    aicen_c[n,:,:] = np.where(aicen_c[n,:,:] >    1, 0, aicen_c[n,:,:])
    aicen_c[n,:,:] = np.where(aicen_c[n,:,:] < 0.01, 0, aicen_c[n,:,:])
    vicen_c[n,:,:] = np.where(vicen_c[n,:,:] < 0.01, 0, vicen_c[n,:,:])
    aicen_c[n,:,:] = np.where(vicen_c[n,:,:] < 0.01, 0, aicen_c[n,:,:])
    vsnon_c[n,:,:] = np.where(vsnon_c[n,:,:] < 0.01, 0, vsnon_c[n,:,:])
    agen_c[n,:,:]  = np.where(vicen_c[n,:,:] < 0.01, 0, agen_c[n,:,:])
    Tsfcn_c[n,:,:] = -1.8 - vicen_c[n,:,:]*10.0

    for s in varlist3d_null:
        print s
        set_to_val(nc_cice,s,icemask,0.0,n)

    for k in range(nilyr):
        ch = str(101+k)[1:]
        set_to_val(nc_cice,'sice0'+ch,icemask,5.0,n)
        set_to_val(nc_cice,'qice0'+ch,icemask,-3.0e8,n)
    for k in range(nslyr):
        ch = str(101+k)[1:]
        set_to_val(nc_cice,'qsno0'+ch,icemask,-1.5e8,n)

    set_to_val(nc_cice,'alvl',icemask,1.0,n)
    set_to_val(nc_cice,'vlvl',icemask,1.0,n)

for s in varlist2d_null:
    print s
    set_to_val(nc_cice,s,icemask,0.0)
    
nc_cice.istep1 = 0
nc_cice.time = 0
nc_cice.time_forc = 0
nc_cice.nyr = 1
nc_cice.month = 1
nc_cice.mday = 1
nc_cice.sec = 0


nc_cice.close()
print 'done'
