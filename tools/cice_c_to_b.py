# This script creates the CICE grid files based on a ROMS gridfile
# Created by Sebastian Maartensson around 2014/2015
# Edit by nilsmk@met.no 21.08.2019

import sys
import os
import netCDF4 as nc


def get_2dvar(ROMSGrid=None,varname=None):
    """General function for converting from C-grid to B-grid for 2d-variable."""
    ROMS = ROMSGrid.variables[varname][:]
    CICE = ROMS.copy()
    CICE[:-1,:-1] = 0.25*(ROMS[:-1,:-1] + ROMS[:-1,1:] + ROMS[1:,:-1] + ROMS[1:,1:])
    CICE[:,-1] = (ROMS[:,-1]) + 0.5*((ROMS[:,-1])-(ROMS[:,-2]))
    CICE[-1,:] = (ROMS[-1,:]) + 0.5*((ROMS[-1,:])-(ROMS[-2,:]))
    return CICE

def get_xi_u(ROMSGrid=None):
    """Here we do interpolation again"""
    xi_u = ROMSGrid.variables['xi_rho'][:]
    xi_u_roms = ROMSGrid.variables['xi_u'][:]
    xi_u[:-1] = 0.5*(xi_u_roms + xi_u_roms)
    xi_u[-1] = xi_u_roms[-1]+ (xi_u_roms[-1] - xi_u_roms[-2])
    return xi_u

def get_eta_u(ROMSGrid=None):
    """Here we do interpolation again"""
    #eta_u = ROMSGrid.variables['eta_rho'][:]
    eta_u_roms = ROMSGrid.variables['eta_u'][:]
    eta_u = 0.5*(eta_u_roms + eta_u_roms)
    #eta_u[-1] = eta_u_roms[-1] + (eta_u_roms[-1]-eta_u_roms[-2])
    return eta_u

def get_dims(ROMSGrid=None):
    dims = ROMSGrid.dimensions
    return len(dims['xi_rho']), len(dims['eta_rho'])


if __name__ == "__main__":
    from numpy import pi
    if not len(sys.argv) == 2:
        print("Incorrect number of arguments. Only path should be given\n {} arguments was given".format(len(sys.argv)))
        sys.exit()
    ROMS_grid_path = sys.argv[1]
    if not os.path.exists(ROMS_grid_path):
        print("Path {} does not exist!".format(ROMS_grid_path))
        sys.exit()
    # Sanity checks done

    ROMSGrid = nc.Dataset(ROMS_grid_path)

    new_grid = nc.Dataset('new_cice.grid.nc', 'w', format='NETCDF3_CLASSIC')
    dim_xi_t,dim_eta_t = get_dims(ROMSGrid = ROMSGrid)
    xi_t = new_grid.createDimension("xi_t", dim_xi_t)
    eta_t = new_grid.createDimension("eta_t", dim_eta_t)
    xi_u = new_grid.createDimension("xi_u", dim_xi_t)
    eta_u = new_grid.createDimension("eta_u", dim_eta_t)

    xi_t_var = new_grid.createVariable('xi_t', 'f4', ('xi_t',))
    xi_u_var = new_grid.createVariable('xi_u', 'f4', ('xi_u',))
    eta_t_var = new_grid.createVariable('eta_t', 'f4', ('eta_t',))
    eta_u_var = new_grid.createVariable('eta_u', 'f4', ('eta_u',))

    xi_t_var[:] = ROMSGrid.variables['xi_rho']
    eta_t_var[:] = ROMSGrid.variables['eta_rho']
    xi_u_var[:] = get_xi_u(ROMSGrid = ROMSGrid)
    eta_u_var[:] = get_eta_u(ROMSGrid = ROMSGrid)

    ulat = new_grid.createVariable('ulat', 'f4', ('eta_u','xi_u'))
    ulon = new_grid.createVariable('ulon', 'f4', ('eta_u','xi_u'))
    angle = new_grid.createVariable('angle','f4', ('eta_u','xi_u'))
    HTN = new_grid.createVariable('HTN','f4', ('eta_u','xi_u'))
    HTN.units = "m"
    HTE = new_grid.createVariable('HTE','f4', ('eta_u','xi_u'))
    HTE.units = "m"
    dxt = new_grid.createVariable('dxt','f4', ('eta_u','xi_u'))
    dxt.units = "m"
    dyt = new_grid.createVariable('dyt','f4', ('eta_u','xi_u'))
    dyt.units = "m"
    dxu = new_grid.createVariable('dxu','f4', ('eta_u','xi_u'))
    dxu.units = "m"
    dyu = new_grid.createVariable('dyu','f4', ('eta_u','xi_u'))
    dyu.units = "m"

    ulat[:] = get_2dvar(ROMSGrid = ROMSGrid, varname = 'lat_rho')
    ulon[:] = get_2dvar(ROMSGrid = ROMSGrid, varname = 'lon_rho')
    angle[:] = get_2dvar(ROMSGrid = ROMSGrid, varname = 'angle')*180/pi
    HTN[:] = 1./get_2dvar(ROMSGrid = ROMSGrid, varname = 'pm')
    HTE[:] = 1./get_2dvar(ROMSGrid = ROMSGrid, varname = 'pn')
    dxt[:] = HTE[:]
    dxu[:] = HTE[:]
    dyt[:] = HTN[:]
    dyu[:] = HTN[:]

    new_grid.type = 'cice grid information'
    new_grid.grid = 'on T points'

    new_grid.close()

    kmt_grid = nc.Dataset('new_cice.kmt.nc', 'w', format='NETCDF3_CLASSIC')
    xi_ha = kmt_grid.createDimension('xi_ha', dim_xi_t)
    eta_ha = kmt_grid.createDimension('eta_ha', dim_eta_t)

    xi_ha_var = kmt_grid.createVariable('xi_ha', 'f4', ('xi_ha',))
    eta_ha_var = kmt_grid.createVariable('eta_ha', 'f4', ('eta_ha',))
    kmt = kmt_grid.createVariable('kmt', 'f4', ('eta_ha','xi_ha'))

    xi_ha_var[:] = ROMSGrid.variables['xi_rho']
    eta_ha_var[:] = ROMSGrid.variables['eta_rho']
    kmt[:] = ROMSGrid.variables['mask_rho']

    kmt_grid.close()
