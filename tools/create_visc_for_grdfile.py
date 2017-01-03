from netCDF4 import Dataset
import numpy as np
import sys
# Set input/output NetCDF files.

GRDname = sys.argv[1]
NUDname = sys.argv[2]
 
# Get grid structure.

ng = Dataset(GRDname)
ngv = ng.variables
[Lr,Mr] = ngv['h'][:].shape

ncname = NUDname
print(['** Creating NetCDF file: ', ncname,' **'])
nf = Dataset(NUDname,mode='w',format='NETCDF4_CLASSIC')
xi_dim = nf.createDimension('xi_rho', Mr)
eta_dim = nf.createDimension('eta_rho', Lr)
#--------------------------------------------------------------------------
# Set inverse time scales.
#--------------------------------------------------------------------------

JstrR = 0
JendR = Lr-1 
IstrR = 0
IendR = Mr-1

# Increased diffusion and viscosity is applied in the nudging zone, <width> points. 
# 

inner_d = 1.                        # inner value at interior limit
outer = 10.                         #  outer value at boundary
width = 50.                         #  width points
inner = 0.
i_idx = 1
o_idx = 10
w_idx = 50

varis = ['visc_factor','diff_factor']

for var in varis:
	nf.createVariable(var,'f8',('eta_rho','xi_rho'),fill_value=1e+37) 
	nfv = nf.variables

	if var=='visc_factor':
		nfv[var][:] = np.zeros((Lr,Mr))                # RHO-points
		work  = np.zeros((Lr,Mr))
		inner = inner	
	elif var=='diff_factor':
		nfv[var][:] = np.zeros((Lr,Mr))
		work  = np.ones((Lr,Mr))
		inner = inner_d

	for i in range(w_idx):                   # Southern boundary
	    work[w_idx:JendR-w_idx,i+1] = inner + (width - i) * (outer - inner) / width

	for i in range(IendR-w_idx,IendR,1):             # Northern boundary
	    work[w_idx:JendR-w_idx,i+1] = outer + (IendR - i) * (inner - outer) / width

	for j in range(w_idx):                   # Western boundary
	    work[j+1,:] = inner + (width - j) * (outer - inner) / width

	for j in range(JendR-w_idx,JendR,1):             # Eastern boundary
	    work[j+1,:] = outer + (JendR - j) * (inner - outer) / width

	for k in range(w_idx): 
		work[k:w_idx+1,k]=work[(k+2*w_idx):3*w_idx+1,k]
		work[JendR-w_idx:JendR-k,k]=work[w_idx:2*w_idx-k,k]
		work[k:w_idx+1,IendR-k]=work[k+w_idx:2*w_idx+1,IendR-k]
		work[JendR-w_idx:JendR-k,IendR-k]=work[w_idx:2*w_idx-k,IendR-k]
	# Load values into structure

	nfv[var][:] = work
	nf.sync()

nf.close()
ng.close()
