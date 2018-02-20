import numpy as np
import sys
import netCDF4


def spec2hum(qair, tair, pair):
    es = 6.112 * np.exp((17.67 * tair ) / ( tair + 243.5))
    e  = qair * pair / ((0.378 * qair) + 0.622)
    rh = e / es
    rh = np.where(rh > 1, 1, rh)
    rh = np.where(rh < 0, 0, rh)
    return rh


ifile = sys.argv[1]
nc    = netCDF4.Dataset(ifile, 'r+')

qair  = nc.variables['Qair']
tair  = nc.variables['Tair']
pair  = nc.variables['Pair']

rh = spec2hum(qair[:], tair[:], pair[:])
qair[:] = rh
nc.sync()
nc.close()

