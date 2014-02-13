#!/bin/bash
set -x

workingdir=${PWD} 
cd ../
metroms_base=${PWD} 
cd ../
tup=${PWD}

# Build CICE
cd ${tup}/tmproms
mkdir cice
export CICE_DIR=${tup}/tmproms/cice/
cd $CICE_DIR

# Unpack standard source files
tar -xf ${metroms_base}/static_libs/cice_4_1.tar.gz

# Copy modified source files
cp -auv $workingdir/common/modified_src/cice ${tup}/tmproms

./comp_ice

set +x