#!/bin/bash
echo "loading $METROMS_MYHOST modules"
if [ "$METROMS_MYHOST" == "metlocal" ]; then
    echo '..'
elif [ "$METROMS_MYHOST" == "vilje" ]; then
    source /etc/profile.d/modules.sh
    module purge
    module load mpt/2.10
    module load intelcomp/15.0.1
    module load netcdf/4.3.2
    module load hdf5/1.8.14-mpi
    module load python/2.7.9
elif [ "$METROMS_MYHOST" == "alvin" ]; then
    module load intel/15.0.1.133
    module load netcdf/4.3.2-i1501-hdf5-1.8.14
    module load impi/5.0.2.044
    module load nco/4.4.7-i1501-netcdf-4.3.2-hdf5-1.8.14
    module load fimex/0.63.7
    module load python/2.7.9-smhi-1
else
    echo "Undefined METROMS_MYHOST ", $METROMS_MYHOST
fi
