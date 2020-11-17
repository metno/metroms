# This was written by YvonneG (MET)
# Minor changes by NilsMK (MET) for operational use
# 16.nov 2020: NilsMK - cleanup and fix for rotating vectors
# if using rotate: use *-el7.q and "module load pyromstools/0.3"
# qlogin -q research-el7.q -l h_vmem=10G
# E.g. run:
# python3 /home/nilsmk/metroms/tools/windStressProgram.py
#   /home/nilsmk/metroms_apps/nordic-4km_stormsurge2d/grid/nordic-4km_grd.nc
#   ocean_force2.nc ocean_force.nc
################################################################################
import numpy as np
from netCDF4 import Dataset
import datetime
import sys
#
#----------------------------
# USER INPUT:
#-----------------------------
FileGRID = sys.argv[1]
File     = sys.argv[2]
FileOUT  = sys.argv[3]
#
def ocnstress(u, v, rot=False, angle=None):
    t0   = datetime.datetime.now()
    # Stress beregnet paa samme
    # maate som i Hirlam modellen
    #
    # If uwind/vwind is in east/north; rotate:
    if rot:
        # rot here
        from pyromstools import ROMStools
        u, v = ROMStools.rotate_vectors_togrid(angle,u,v)
    zref  = 10.0
    kappa = 0.4
    z0min = 1.5e-5
    beta  = 0.018
    g     = 9.8
    rhoa  = 1.2923 # Reference density of dry air
    B     = beta/g
    taux  = []
    tauy  = []
    for i in range(0,u.shape[0]):
        vind   = ( u[i,:,:]**2 + v[i,:,:]**2)**0.5
        us2    = np.ones_like(vind)*0.1
        z0     = np.ones((2,vind.shape[0],vind.shape[1]),dtype=np.float)*z0min
        us2min = 1e-3
        us2old = us2*0.0
        n = 0
        while ( max( abs(np.min(us2 - us2old)),abs(np.max( us2 - us2old ))) > us2min):
            n+=1
            us2old    = us2
            z0[0,:,:] = B*us2
            us2       = ((kappa*vind)/(np.log(zref/np.max(z0,0))))**2
        vind = np.where(vind<0.1, 0.1, vind)
        #
        taux.append(rhoa*us2*u[i,:,:]/vind)
        tauy.append(rhoa*us2*v[i,:,:]/vind)
    print('Calc stress in {} seconds'.format(np.int((datetime.datetime.now()-t0).total_seconds())))
    return(taux,tauy)
#-------------------------------
# Read Forcing files:
#-------------------------------
t0   = datetime.datetime.now()
try:
    f0 = Dataset(File)
except Exception as e :
    print("ERROR opening file %s for reading (%s)" % (File, e))
    sys.exit()
# ------------------------
# Read grid file:
# -------------------------
try:
    f1 = Dataset(FileGRID)
except Exception as e :
    print("ERROR opening file %s for reading (%s)" % (File, e))
    sys.exit()
#------------------------------------------
# Calculate wind stress for u and v
#-------------------------------------------
try:
    Tx,Ty = ocnstress(f0.variables['Uwind'][:], f0.variables['Vwind'][:])
except:
    Tx,Ty = ocnstress(f0.variables['x_wind_10m'][:], f0.variables['y_wind_10m'][:], rot=True, angle=f1.variables['angle'][:])
timeIN    = f0.variables['time'][:]
try:
    f_Reftime = f0.variables['forecast_reference_time'][:]
except:
    f_Reftime = None
try:
    Pair  = f0.variables['Pair'][:]
except:
    Pair  = f0.variables['surface_air_pressure'][:]
Tx = np.array(Tx)
Ty = np.array(Ty)
#
etarho   = f1.dimensions['eta_rho']
xirho    = f1.dimensions['xi_rho']
etau     = f1.dimensions['eta_u']
xiu      = f1.dimensions['xi_u']
etav     = f1.dimensions['eta_v']
xiv      = f1.dimensions['xi_v']
lon_u    = 0.5*(f1.variables['lon_rho'][:,1:]+f1.variables['lon_rho'][:,:-1])
lat_u    = 0.5*(f1.variables['lat_rho'][:,1:]+f1.variables['lat_rho'][:,:-1])
lon_v    = 0.5*(f1.variables['lon_rho'][1:,:]+f1.variables['lon_rho'][:-1,:])
lat_v    = 0.5*(f1.variables['lat_rho'][1:,:]+f1.variables['lat_rho'][:-1,:])
ref_time = datetime.date(1970, 1, 1)
print('Read and calc in {} seconds'.format(np.int((datetime.datetime.now()-t0).total_seconds())))
#------------------------------------
# Write to netCDF file:
#------------------------------------
print("Write to NetCDF")
t1   = datetime.datetime.now()
root = Dataset(FileOUT, 'w', format='NETCDF3_CLASSIC')
#
root.createDimension('time', None)
root.createDimension('xi_rho', len(xirho))
root.createDimension('xi_u', len(xiu))
root.createDimension('xi_v', len(xiv))
root.createDimension('eta_rho', len(etarho))
root.createDimension('eta_u', len(etau))
root.createDimension('eta_v', len(etav))
#
v           = root.createVariable('lon_u', 'f', ('eta_u','xi_u',))
v.long_name = "longitude of U-points"
v.units     = "degree_east"
v[:,:]      = lon_u
root.sync()
#
v           = root.createVariable('lat_u', 'f', ('eta_u','xi_u',))
v.long_name = "latitude of U-points"
v.units     = "degree_north"
v[:,:]      = lat_u
root.sync()
#
v           = root.createVariable('lon_v', 'f', ('eta_v','xi_v',))
v.long_name = "longitude of V-points"
v.units     = "degree_east"
v[:,:]      = lon_v
root.sync()
#
v           = root.createVariable('lat_v', 'f', ('eta_v','xi_v',))
v.long_name = "latitude of V-points"
v.units     = "degree_north"
v[:,:]      = lat_v
root.sync()
#
v           = root.createVariable('time', 'f', ('time',))
v.long_name = "surface forcing time"
v.units     ="days since %s 00:00:00" % ref_time.isoformat()
v.field     = "time, scalar, series"
v[:]        = timeIN[:]
root.sync()
#
if f_Reftime is not None:
    v           = root.createVariable('forecast_reference_time', 'f')
    v.long_name = "surface forcing refrence time"
    v.units     ="days since %s 00:00:00" % ref_time.isoformat()
    v.field     = "time, scalar, series"
    v[:]        = f_Reftime
    root.sync()
#
v             = root.createVariable('sustr', 'f',('time', 'eta_u', 'xi_u',))
v.long_name   = "surface u-momentum stress"
v.units       = "newton meter-2"
v.field       = "surface u-momentum stress, scalar, series"
v.coordinates = "lon_u lat_u"
v.time        = "time"
v[:,:,:]      = Tx[:,:,1:]
root.sync()
#
v             = root.createVariable('svstr', 'f',('time', 'eta_v', 'xi_v',))
v.long_name   = "surface v-momentum stress"
v.units       = "newton meter-2"
v.field       = "surface v-momentum stress, scalar, series"
v.coordinates = "lon_v lat_v"
v.time        = "time"
v[:,:,:]      = Ty[:,1:,:]
root.sync()
#
v             = root.createVariable('Pair', 'f',('time', 'eta_rho', 'xi_rho',))
v.long_name   = "surface air pressure"
v.units       = "Pascal"
v.field       = "Pair, scalar, series"
v.coordinates = "lon_rho lat_rho"
v.time        = "time"
v[:,:,:]      = Pair[:,:,:]
root.sync()
#
f0.close()
f1.close()
root.close()
print('Wrote file in {} seconds'.format(np.int((datetime.datetime.now()-t1).total_seconds())))
print('Script ran in {} seconds'.format(np.int((datetime.datetime.now()-t0).total_seconds())))
print('---------------------------------------------')
