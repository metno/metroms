import netCDF4
import numpy as np

def set_to_val(nc,variable,icemask,val=0.0,n=-999):
    var    = nc.variables[variable]
    if n < 0:
        var[:] = val*icemask
    else:
        var[n,:] = val*icemask

# Make initialization of new cice-file...

romsfile = '/disk1/CICE_ini/ocean_ini.nc'
romsgrid = '/disk1/MODELS/bunnmatriser/A20_grd_openBering.nc'
cicefile = '/disk1/CICE_ini/iced.1997-01-01-00000.nc'

NCAT = [0, 0.6445072, 1.391433, 2.470179, 4.567288, 1e+08] #upper limits of ice categories
varlist2d_null = ['uvel','vvel','scale_factor','swvdr','swvdf','swidr','swidf','strocnxT','strocnyT',
                  'stressp_1','stressp_2','stressp_3','stressp_4','stressm_1','stressm_2','stressm_3',
                  'stressm_4','stress12_1','stress12_2','stress12_3','stress12_4','iceumask','fsnow']
varlist3d_null = ['vsnon','iage','apnd','hpnd','ipnd','dhs','ffrac','qsno001']

nc_roms   = netCDF4.Dataset(romsfile)
grid_roms = netCDF4.Dataset(romsgrid)
nc_cice   = netCDF4.Dataset(cicefile,'r+')

aice_r    = nc_roms.variables['aice']   # mean ice concentration for grid cell
aicen_c   = nc_cice.variables['aicen']  # ice concentration per category in grid cell
vicen_c   = nc_cice.variables['vicen']  # volume per unit area of ice (in category n)

mask_r    = grid_roms.variables['mask_rho']

hice_r    = nc_roms.variables['hice']

icemask = np.where((aice_r[:]*mask_r[:]) > 0.1, 1, 0)
print icemask

for n in range(len(NCAT[1:])):
    print 'Upper limit: '+str(NCAT[n+1])
    aicen_c[n,:,:] = np.where(np.logical_and( hice_r[:,:] > NCAT[n], hice_r[:,:] < NCAT[n+1] ),
                              aice_r[:,:],0.0) * mask_r[:,:]
    vicen_c[n,:,:] = np.where(np.logical_and( hice_r[:,:] > NCAT[n], hice_r[:,:] < NCAT[n+1] ),
                              hice_r[:,:]*aice_r[:,:],0.0) * mask_r[:,:]
    # Make sure no negative values exist:
    aicen_c[n,:,:] = np.where(aicen_c[n,:,:] < 0.0, 0, aicen_c[n,:,:])
    vicen_c[n,:,:] = np.where(vicen_c[n,:,:] < 0.0, 0, vicen_c[n,:,:])
    for s in varlist3d_null:
        print s
        set_to_val(nc_cice,s,icemask,0.0,n)
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