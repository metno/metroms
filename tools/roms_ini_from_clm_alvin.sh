#Make ini from clm on Alvin
#USAGE: ./roms_ini_from_clm.sh <climfile> <inifile> <time>

module load netcdf/4.3.2-i1501-hdf5-1.8.14
module load nco/4.4.7-i1501-netcdf-4.3.2-hdf5-1.8.14

ncks -d clim_time,${3} ${1} ${2}
ncrename -O -d clim_time,ocean_time ${2}

for var in zeta ubar vbar u v salt temp; do
   ncatted -O -a time,${var},c,c,"ocean_time" ${2}
done

ncrename -v clim_time,ocean_time ${2}

