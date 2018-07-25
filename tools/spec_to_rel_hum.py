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
time  = nc.variables['time']

for t in range(len(time[:])):
    if qair[t,0,0] < 0.1:
        if pair[t,0,0] > 2000:
            print 'convert from spec to rel percent to frac'
            rh = spec2hum(qair[t,:], tair[t,:], pair[t,:]/100)
        else:
            print 'convert from spec to rel'
            rh = spec2hum(qair[t,:], tair[t,:], pair[t,:])
        qair[t,:] = rh
        nc.sync()
    else:
        print 'dont need to convert qair'
#nc.close()
print 'done'
