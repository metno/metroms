#!/bin/bash
set -x

if [ $# -ne 2 ]
then
    echo "Usage: $0 NPX NPY"
    exit 1
fi 

export workingdir=${PWD} 
cd ../
metroms_base=${PWD} 
cd ../
#tup=/global/work/sebastm #${PWD}
tup=${PWD}

# Build CICE
mkdir -p ${tup}/tmproms
cd ${tup}/tmproms
# Unpack standard source files
tar -xf ${metroms_base}/static_libs/cice5.tar.gz
export CICE_DIR=${tup}/tmproms/cice
cd $CICE_DIR

export MCT_INCDIR=${tup}/tmproms/MCT/include
export MCT_LIBDIR=${tup}/tmproms/MCT/lib


# Copy modified source files
cp -auv $workingdir/common/modified_src/cice ${tup}/tmproms

# Remove old binaries
rm -rf ${tup}/tmproms/cice/rundir/compile

#
# NB! Compile flags needed on Vilje
# -O2 -w -convert big_endian -assume byterecl
#

./comp_ice $1 $2

# Build a library (for use in the ROMS build)
cd $CICE_DIR/rundir/compile
ar rcv libcice.a *.o


set +x
