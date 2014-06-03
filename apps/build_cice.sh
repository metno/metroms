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

./comp_ice

# Build a library (for use in the ROMS build)
cd $CICE_DIR/rundir/compile
ar rcv libcice.a *.o

sed "s#<cicedir>#$CICE_DIR#" $workingdir/common/modified_src/cice/input_templates/ice_in > $CICE_DIR/rundir/ice_in

set +x