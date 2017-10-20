#Make ini from clm on Vilje
#USAGE: ./roms_ini_from_clm.sh <climfile> <inifile>

module load netcdf
module load nco

ncks -d clim_time,4 ${1} ${2}
ncrename -O -d clim_time,ocean_time ${2}

for var in zeta ubar vbar u v salt temp; do
   ncatted -O -a time,${var},c,c,"ocean_time" ${2}
done

ncrename -v clim_time,ocean_time ${2}

