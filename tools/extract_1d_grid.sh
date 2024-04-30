# Make "1D"-grid centered around gridpoint:
x=42
y=593
nx=20 # Must be even number
ny=22 # Must be even number
#
i0=$(($x-$(($nx/2))))
i1=$(($x+$(($nx/2))-1))
j0=$(($y-$(($ny/2))))
j1=$(($y+$(($ny/2))-1))
ncks -O -d xi_rho,$i0,$i1 -d eta_rho,$j0,$j1 -d xi_u,$i0,$(($i1-1)) -d eta_u,$j0,$j1 -d xi_v,$i0,$i1 -d eta_v,$j0,$(($j1-1)) -d xi_psi,$i0,$(($i1-1)) -d eta_psi,$j0,$(($j1-1)) -d xi_vert,$i0,$i1 -d eta_vert,$j0,$j1 FjordOs_grd_v9.nc 1d_grid.nc
python set_h_and_mask.py 1d_grid.nc