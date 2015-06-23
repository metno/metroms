#!/bin/bash
set -x

workingdir=${PWD} 
cd ../
metroms_base=${PWD} 
cd ../
#tup=/work/$USER
tup=${PWD}

# Build MCT
mkdir -p ${tup}/tmproms
cd ${tup}/tmproms
# Unpack standard source files
tar -xf ${metroms_base}/static_libs/mct-2.8.tar.gz
export MCT_DIR=${tup}/tmproms/MCT
cd $MCT_DIR

#./configure --prefix=$MCT_DIR
#./configure FC=gfortran --prefix=$MCT_DIR
./configure FC=ifort --prefix=$MCT_DIR
make install

set +x
