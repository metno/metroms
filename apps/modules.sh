#!/bin/bash
echo "loading $METROMS_MYHOST modules"
if [ "$METROMS_MYHOST" == "metlocal" ]; then
    echo '..'
elif [ "$METROMS_MYHOST" == "met_ppi" ]; then
  echo "Linux distro is `lsb_release -sc`"
  if [ `lsb_release -sc` == 'xenial' ]; then
    module load compiler/intelPE2018
    module load netcdf/4.5.0intel18
    module load OpenMPI/3.0.0intel18
    module load hdf5/1.10.1intel18
  elif [ `lsb_release -sc` == 'Core' ]; then
    module load compiler/intelPE2018
    module load hdf5/1.10.5-intel2018
    module load netcdf/4.7.0-intel2018
    module load openmpi/3.1.4-intel2018
    module load nco/4.7.9-intel2018
  elif [ `lsb_release -sc` == 'Ootpa' ]; then
    module use /modules/MET/rhel8/user-modules/ /modules/MET/rhel8/IT-modules
    module add compiler/Intel2022
    module add IB-R8-A/netcdf/4.6.1-IB-i22-2023
    module add IB-R8-A/openmpi/3.1.4-IB-i22-2023
  else
    echo "Undefined linux distro for met_ppi"
  fi
elif [ "$METROMS_MYHOST" == "nebula" ] || [ "$METROMS_MYHOST" == "stratus" ]; then
    module load buildenv-intel/2018a-eb
    module load netCDF/4.3.2-HDF5-1.8.12-nsc1-intel-2018.u1-bare
    module load NCO/4.6.3-nsc1
    module load Python/2.7.15-anaconda-5.3.0-extras-nsc1
elif [ "$METROMS_MYHOST" == "nebula2" ] ; then
    module load buildenv-intel/2023a-eb
    module load netCDF-HDF5-utils/4.9.2-1.12.2-hpc1-intel-2023a-eb
elif [ "$METROMS_MYHOST" == "fram" ]; then
    module load intel/2018b
    module load netCDF-Fortran/4.4.4-intel-2018b
    module load Python/2.7.15-intel-2018b
    module load netcdf4-python/1.4.1-intel-2018b-Python-2.7.15
else
    echo "Undefined METROMS_MYHOST ", $METROMS_MYHOST
fi
