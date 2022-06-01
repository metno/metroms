# Make cut of roms-file:
# Usage: ./cut_roms_file.sh <ifile> <ofile> <x1> <x2> <y1> <y2>
ifile=$1
ofile=$2
i0=$3
i1=$4
j0=$5
j1=$6
# Gridfile:
#ncks -O -d xi_rho,$i0,$i1 -d eta_rho,$j0,$j1 -d xi_u,$i0,$(($i1-1)) -d eta_u,$j0,$j1 -d xi_v,$i0,$i1 -d eta_v,$j0,$(($j1-1)) -d xi_psi,$i0,$(($i1-1)) -d eta_psi,$j0,$(($j1-1)) -d xi_vert,$i0,$i1 -d eta_vert,$j0,$j1 $ifile $ofile
# Inifile:
#ncks -O -d xi_rho,$i0,$i1 -d eta_rho,$j0,$j1 -d xi_u,$i0,$(($i1-1)) -d eta_v,$j0,$(($j1-1)) $ifile $ofile
# Tide-file:
ncks -O -d xi_rho,$i0,$i1 -d eta_rho,$j0,$j1 $ifile $ofile
