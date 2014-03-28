#!/bin/bash
set -x

workingdir=${PWD} 
cd ../
metroms_base=${PWD} 
cd ../
tup=${PWD}

# Build MCT
cd ${tup}/tmproms
tar -xf ${metroms_base}/static_libs/mct.tar.gz
export MCT_DIR=${tup}/tmproms/MCT/
cd $MCT_DIR
./configure --enable-mpiserial 
make

# Clean up unpacked static code:
#cd  ${MY_PROJECT_DIR}
#rm -rf ${MY_ROMS_SRC}


set +x