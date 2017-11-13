import sys
import os
import netCDF4 as nc


def get_rho_mask(ROMSGrid=None):
    return ROMSGrid.variables['mask_rho'][:]
    
def get_uv_mask(ROMSGrid=None):
    """ROMS psi points are located at a b-grid u/v point. However, index is shifted so that, psi(1,1) = u/v(0,0) etc"""
    psi_mask = ROMSGrid.variables['mask_psi'][:]
    
    #The rho mask has the same shape as the u/v mask, the psi-mask is 1 smaller. 
    rho_mask = ROMSGrid.variables['mask_rho'][:]
    rho_mask = rho_mask*0
    rho_mask[:-1,:-1] = psi_mask[:,:]
    return rho_mask
    
def get_angle(ROMSGrid=None):
    """Looks like we should convert from radians to degrees, even though precent cice grid says radians. Meh.
        Angles are located on T-points in ROMS and u-points in cice."""
    ROMS_angle = ROMSGrid.variables['angle'][:]
    angle = ROMS_angle.copy()
    angle[:-1,:-1] = 0.25*(ROMS_angle[:-1,:-1] + ROMS_angle[:-1,1:] + ROMS_angle[1:,:-1] + ROMS_angle[1:,1:])
    angle[:,-1] = ROMS_angle[:,-1] + 0.5*(ROMS_angle[:,-1]-ROMS_angle[:,-2])
    angle[-1,:] = ROMS_angle[-1,:] + 0.5*(ROMS_angle[-1,:]-ROMS_angle[-2,:])
    
    return angle
        

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
    
def get_lat_u(ROMSGrid=None):
    lat_rho = ROMSGrid.variables['lat_rho'][:]
    lat_u = lat_rho.copy()
    lat_u[:-1,:-1] = 0.25*(lat_rho[:-1,:-1] + lat_rho[:-1,1:] + lat_rho[1:,:-1] + lat_rho[1:,1:])
    lat_u[:,-1] = lat_rho[:,-1] + 0.5*(lat_rho[:,-1]-lat_rho[:,-2])
    lat_u[-1,:] = lat_rho[-1,:] + 0.5*(lat_rho[-1,:]-lat_rho[-2,:])
    
    return lat_u
    
    
def get_lon_u(ROMSGrid=None):
    lon_rho = ROMSGrid.variables['lon_rho'][:]
    lon_u = lon_rho.copy()
    lon_u[:-1,:-1] = 0.25*(lon_rho[:-1,:-1] + lon_rho[:-1,1:] + lon_rho[1:,:-1] + lon_rho[1:,1:])
    lon_u[:,-1] = lon_rho[:,-1] + 0.5*(lon_rho[:,-1]-lon_rho[:,-2])
    lon_u[-1,:] = lon_rho[-1,:] + 0.5*(lon_rho[-1,:]-lon_rho[-2,:])
    
    return lon_u
    
def get_dims(ROMSGrid=None):
    dims = ROMSGrid.dimensions
    return dims['xi_rho'].size, dims['eta_rho'].size
    

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
    
    rho_mask = get_rho_mask(ROMSGrid)
    uv_mask = get_uv_mask(ROMSGrid)
    angle = get_angle(ROMSGrid)
    
    new_grid = nc.Dataset('new_cice_grid.nc', 'w', format='NETCDF3_CLASSIC')
    dim_xi_t,dim_eta_t = get_dims(ROMSGrid=ROMSGrid)
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
  
    ulat[:] = get_lat_u(ROMSGrid=ROMSGrid)
    ulon[:] = get_lon_u(ROMSGrid=ROMSGrid)
    angle[:] = get_angle(ROMSGrid=ROMSGrid)*180/pi
  
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
    
    
    
    
    
