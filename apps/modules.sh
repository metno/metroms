#!/bin/bash
source /etc/profile.d/modules.sh

#module purge
#module load mpt/2.06 intelcomp/12.0.5.220 netcdf/4.1.3 python/2.7.2

module purge
module load mpt/2.10
module load intelcomp/15.0.1
module load netcdf/4.3.2
module load hdf5/1.8.14-mpi
module load python/2.7.9
