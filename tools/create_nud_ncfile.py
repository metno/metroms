from netCDF4 import Dataset
import numpy as np
import sys

if len(sys.argv) < 3:
    print 'Too few args'
    sys.exit()

# Set input/output NetCDF files.

GRDname = sys.argv[1]
NUDname = sys.argv[2]

 
# Get grid structure.

ng = Dataset(GRDname)
ngv = ng.variables
[Lr,Mr] = ngv['h'][:].shape
Nr = int(sys.argv[3])
mask_rho = ngv['mask_rho']

# Set switches for state variables to nudge.

LnudgeM2CLM    = False           # nudging 2D momentum
LnudgeM3CLM    = False           # nudging 3D momentum
LnudgeTCLM     = True            # nudging tracers (T-S)
LnudgeTgeneric = True            # nudging generic tracers

CorrectForMask = True

NudgeWest  = True
NudgeEast  = False
NudgeNorth = False
NudgeSouth = False

VerticalNudge = True

# Set NetCDF variables to process. Nudging in ROMS is:
#
#      F(...,new) = F(...,new) +
#                   dt * F_nudgcoef * (Fclm - F(...,new))

ncname = NUDname
print(['** Creating NetCDF file: ', ncname,' **'])
nf = Dataset(NUDname,mode='w',format='NETCDF4_CLASSIC')
s_dim = nf.createDimension('s_rho', Nr)
xi_dim = nf.createDimension('xi_rho', Mr)
eta_dim = nf.createDimension('eta_rho', Lr)
spherical = nf.createVariable('spherical','i4')
spherical.long_name = "grid type logical switch"
spherical.flag_values = "0, 1"
spherical.flag_meanings = "Cartesian spherical"
M2_NudgeCoef = nf.createVariable('M2_NudgeCoef','f8',('eta_rho','xi_rho'),fill_value=1e+37) 
M2_NudgeCoef.long_name = "2D momentum inverse nudging coefficients"
M2_NudgeCoef.units = "day-1"
M2_NudgeCoef.coordinates = "xi_rho eta_rho "
M3_NudgeCoef = nf.createVariable('M3_NudgeCoef','f8',('s_rho','eta_rho','xi_rho'),fill_value=1e+37) 
M3_NudgeCoef.long_name = "3D momentum inverse nudging coefficients"
M3_NudgeCoef.units = "day-1"
M3_NudgeCoef.coordinates = "xi_rho eta_rho s_rho "
temp_NudgeCoef = nf.createVariable('temp_NudgeCoef','f8',('s_rho','eta_rho','xi_rho'),fill_value=1e+37) 
temp_NudgeCoef.long_name = "temp inverse nudging coefficients"
temp_NudgeCoef.units = "day-1"
temp_NudgeCoef.coordinates = "xi_rho eta_rho s_rho "
salt_NudgeCoef = nf.createVariable('salt_NudgeCoef','f8',('s_rho','eta_rho','xi_rho'),fill_value=1e+37) 
salt_NudgeCoef.long_name = "salt inverse nudging coefficients"
salt_NudgeCoef.units = "day-1"
salt_NudgeCoef.coordinates = "xi_rho eta_rho s_rho "
tracer_NudgeCoef = nf.createVariable('tracer_NudgeCoef','f8',('s_rho','eta_rho','xi_rho'),fill_value=1e+37)
tracer_NudgeCoef.long_name = "generic tracer inverse nudging coefficients"
tracer_NudgeCoef.units = "day-1"
tracer_NudgeCoef.coordinates = "xi_rho eta_rho s_rho "
lon_rho = nf.createVariable('lon_rho','f8',('eta_rho','xi_rho'))
lon_rho.long_name = "longitude of RHO-points"
lon_rho.units = "degree_east"
lon_rho.standard_name = "longitude"
lat_rho = nf.createVariable('lat_rho','f8',('eta_rho','xi_rho'))
lat_rho.long_name = "latitude of RHO-points"
lat_rho.units = "degree_north"
lat_rho.standard_name = "latitude"


nfv = nf.variables

nfv['M2_NudgeCoef'][:] = np.zeros((Lr,Mr))                # RHO-points
nfv['M3_NudgeCoef'][:] = np.zeros((Nr,Lr,Mr))             # RHO-points
nfv['temp_NudgeCoef'][:] = np.zeros((Nr,Lr,Mr))
nfv['salt_NudgeCoef'][:] = np.zeros((Nr,Lr,Mr))
nfv['tracer_NudgeCoef'][:] = np.zeros((Nr,Lr,Mr))
nfv['lat_rho'][:] = ngv['lat_rho'][:]
nfv['lon_rho'][:] = ngv['lon_rho'][:]

JstrR = 0
JendR = Lr-1 
IstrR = 0
IendR = Mr-1

#--------------------------------------------------------------------------
# Set inverse time scales.
#--------------------------------------------------------------------------

# Nudging is done in the
# domain edges over n-point 
# nudging scales of inner to outer days.

inner   = 1./360.                   # <inner> days at interior limit
inner3D = 1./180.                 # nudging in the depth in the interior
outer   = 1./30.                    # <outer> days at boundary
width   = 200.                       # <width> points
w_idx   = int(width)
work    = np.zeros((Nr,Lr,Mr))
work[:] = inner

if (NudgeWest):
    for i in range(w_idx):                   # Western boundary
    #    work[:,w_idx:JendR-w_idx,i] = outer*(1.+np.cos(np.pi*(np.float64(i)/width)))#inner + (width - i) * (outer - inner) / width
        work[:,:,i] = outer*(1.+np.cos(np.pi*(np.float64(i)/width)))#inner + (width - i) * (outer - inner) / width
if (NudgeEast):
    for i in range(w_idx):
    #    work[:,w_idx:JendR-w_idx,IendR-w_idx+i] = work[:,w_idx:JendR-w_idx,w_idx-i]
        work[:,:,IendR-w_idx+i] = np.maximum(work[:,:,IendR-w_idx+i], outer*(1.+np.cos(np.pi*(np.float64(np.abs(i-width))/width))))
if (NudgeSouth):
    for j in range(w_idx):                   # Southern boundary
    #    work[:,j,:] = outer*(1.+np.cos(np.pi*(np.float64(j)/width)))
        work[:,j,:] = np.maximum(work, outer*(1.+np.cos(np.pi*(np.float64(j)/width)))) #inner + (width - j) * (outer - inner) / width
if (NudgeNorth):
    for j in range(w_idx):
    #    work[:,JendR-w_idx+j,:] = work[:,w_idx-j,::-1]
        work[:,JendR+j,:] = np.maximum(work[:,JendR+j,:], outer*(1.+np.cos(np.pi*(np.float64(np.abs(j-width))/width))))

work[:,JendR,:] = work[:,0,:]
work[:,:,IendR] = work[:,:,0]

if (CorrectForMask):
    work = work[:] * mask_rho[:]

if (VerticalNudge):
    for n in range(Nr):
        work[n,:] = np.maximum(work[n,:], inner3D*(1.+np.cos(np.pi*(np.float64(np.abs(n))/Nr))))

#for k in range(w_idx): 
#    work[:,k:w_idx+1,k]=work[:,(k+2*w_idx):3*w_idx+1,k]
#    work[:,JendR-w_idx:JendR-k,k]=work[:,w_idx:2*w_idx-k,k]
#    work[:,k:w_idx+1,IendR-k]=work[:,k+2*w_idx:3*w_idx+1,IendR-k]
#    work[:,JendR-w_idx:JendR-k,IendR-k]=work[:,w_idx:2*w_idx-k,IendR-k]

if (LnudgeM2CLM):
    nfv['M2_NudgeCoef'][:] = work[0,:]
    # nfv['M2_NudgeCoef'][:] = M2_NudgeCoef.squeeze()
    nf.sync()

if (LnudgeM3CLM):  
    nfv['M3_NudgeCoef'][:] = work
    nf.sync()

if (LnudgeTCLM):
    nfv['temp_NudgeCoef'][:] = work
    nfv['salt_NudgeCoef'][:] = work
    nf.sync()

if (LnudgeTgeneric):
    nfv['tracer_NudgeCoef'][:]  = work
    nf.sync()

nf.close()
ng.close()
