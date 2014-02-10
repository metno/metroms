#!/bin/bash
set -x

workingdir=${PWD} 
cd ../
metroms_base=${PWD} 
cd ../
tup=${PWD}

# Build ESMF
cd ${tup}/tmproms
tar -xf ${metroms_base}/static_libs/esmf_5_2_0rp3_src.tar.gz
export ESMF_DIR=${tup}/tmproms/esmf/
cd $ESMF_DIR
ESMF_COMM=mpiuni
ESMF_COMPILER=gfortran
make

# Clean up unpacked static code:
#cd  ${MY_PROJECT_DIR}
#rm -rf ${MY_ROMS_SRC}


set +x