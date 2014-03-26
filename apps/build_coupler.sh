#!/bin/bash
set -x

workingdir=${PWD} 
cd ../
metroms_base=${PWD} 
cd ../
tup=${PWD}

# Build ESMF
export BUILD_DIR=${tup}/tmproms
cd ${BUILD_DIR}/
cp -rf ${metroms_base}/apps/coupler . 
export COUPLER_DIR=${tup}/tmproms/coupler/
cd $COUPLER_DIR
ESMF_COMM=mpiuni
ESMF_COMPILER=gfortran
make

set +x