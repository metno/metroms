#!/bin/bash
set -x

workingdir=${PWD} 
cd ../
metroms_base=${PWD} 
cd ../
tup=${PWD}

# Build CICE
mkdir -p ${tup}/tmproms
cd ${tup}/tmproms
# Unpack standard source files
tar -xf ${metroms_base}/static_libs/cice5.tar.gz
export CICE_DIR=${tup}/tmproms/
cd $CICE_DIR/cice


# Copy modified source files
cp -auv $workingdir/common/modified_src/cice ${tup}/tmproms/cice

./comp_ice

# Build a library (for use in the ROMS build)
cd $CICE_DIR/rundir/compile
ar rcv libcice.a *.o

set +x