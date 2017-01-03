import netCDF4
import sys


cice_file = sys.argv[1]
roms_file = sys.argv[2]

nc_cice = netCDF4.Dataset(cice_file, 'r+')
nc_roms = netCDF4.Dataset(roms_file)

kmt      = nc_cice.variables['kmt']
mask_rho = nc_roms.variables['mask_rho']

kmt[:]   = mask_rho[:]

nc_cice.sync()
nc_cice.close()
nc_roms.close()
