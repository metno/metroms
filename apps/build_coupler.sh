#!/bin/bash
set -x

workingdir=${PWD} 
cd ../
metroms_base=${PWD} 
cd ../
tup=${PWD}

# Build Coupler
export BUILD_DIR=${tup}/tmproms
cd ${BUILD_DIR}/
cp -rf ${metroms_base}/apps/coupler . 
export COUPLER_DIR=${tup}/tmproms/coupler/
cd $COUPLER_DIR
make

set +x