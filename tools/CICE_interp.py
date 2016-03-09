import netCDF4
from datetime import datetime, timedelta
import ROMStools
import numpy as np
from scipy.interpolate import griddata
from scipy.ndimage import gaussian_filter
import time


def get_var(nc,var,index):
    nc = netCDF4.Dataset(nc)
    for i in range(len(var)):
        if var[i] in nc.variables.keys():
            if ( len(nc.variables[var[i]].shape) == 3 ):
                yd, xd = nc.variables[var[i]][0,:,:].shape
                varo = np.zeros([1,yd,xd])
                varo[0,:,:]=nc.variables[var[i]][index,:,:]
            elif ( len(nc.variables[var[i]].shape) == 4 ):
                sd, yd, xd = nc.variables[var[i]][0,:,:,:].shape
                varo = np.zeros([1,sd,yd,xd])
                varo[0,:,:,:] = nc.variables[var[i]][index,:,:,:]            
    nc.close()
    return varo

def fill(var):
    fillvalue = var.min()
    varmasked = np.ma.masked_values(var,fillvalue)
    if ( len(var.shape) == 4 ):
        for s in range(len(var[0,:,0,0])):
            for t in range(len(var[:,0,0,0])):
                var[t,s,:,:] = np.where((var[t,s,:,:] == fillvalue), varmasked[t,s,:,:].mean(), var[t,s,:,:])
                var[t,s,:,:] = gaussian_filter(var[t,s,:,:],sigma=1)
    elif ( len(var.shape) == 3 ):
        for t in range(len(var[:,0,0])):
            var[t,:,:] = np.where((var[t,:,:] == fillvalue), varmasked[t,:,:].mean(), var[t,:,:])
            var[t,:,:] = gaussian_filter(var[t,:,:],sigma=1)
    return var

# Generate netcdf-file:
# ncdump /disk1/nilsmk/tmproms/run/arctic-20km/cice/rundir/restart/iced.1997-01-16-00000_Coldstart.nc > ncfile
# ncgen -k 2 -o iced.1997-01-16-00000_Coldstart.nc ncfile

### MAIN ####
ifile = '/disk1/nilsmk/tmproms/run/arctic-20km/cice/rundir/restart/iced.1997-01-16-00000_Coldstart.nc'
ofile = '/disk1/nilsmk/tmproms/run/arctic-4km/cice/rundir/restart/iced.1997-01-16-00000_Coldstart.nc'
igfile = '/disk1/nilsmk/tmproms/run/arctic-20km/cice/rundir/cice.grid.nc'
ogfile = '/disk1/nilsmk/tmproms/run/arctic-4km/cice/rundir/cice.grid.nc'

latro, lonro = ROMStools.read_grid_info(ogfile)
latri, lonri = ROMStools.read_grid_info(igfile)
     
# Get variables
nci=netCDF4.Dataset(ifile)
nco=netCDF4.Dataset(ofile,'r+')
for var in nci.variables:
    print var
    nco.variables[var][:] = ROMStools.hor_interp(latri, lonri, latro, lonro, nci.variables[var][:])
    nco.sync()
nco.close()
print 'done'