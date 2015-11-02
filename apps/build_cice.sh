#!/bin/bash
set -x
#export CICEVERSION=cice5.0
export CICEVERSION=cice5.1.2

NPX=1; NPY=1
if [ "${METROMS_MYHOST}" == "metlocal" ]; then
    NPX=1  
    NPY=2
elif [ "${METROMS_MYHOST}" == "vilje" ]; then
    NPX=1  
    NPY=2
fi

if [ $# -ge 2 ]; then
    NPX=$1
    NPY=$2
fi

echo "NPX = $NPX, NPY = $NPY"

#if [ $# -ne 2 ]
#then
#    echo "Usage: $0 NPX NPY"
#    exit 1
#fi 

export workingdir=${PWD} 
cd ../
metroms_base=${PWD} 
cd ../
if [ "$METROMS_TMPDIR" == "" ]; then
    tup=${PWD}
else
    tup=${METROMS_TMPDIR}
    if [ ! -d $tup ] ; then
	echo "$tup not defined, set environment variable METROMS_TMPDIR to "
	echo "override default behaviour"
	exit 
    fi
fi

# Build CICE
mkdir -p ${tup}/tmproms
cd ${tup}/tmproms
# Unpack standard source files
tar -xf ${metroms_base}/static_libs/$CICEVERSION.tar.gz
export CICE_DIR=${tup}/tmproms/cice
cd $CICE_DIR

export MCT_INCDIR=${tup}/tmproms/MCT/include
export MCT_LIBDIR=${tup}/tmproms/MCT/lib


# Copy modified source files
mkdir -p ${tup}/tmproms/cice
cp -a $workingdir/common/modified_src/$CICEVERSION/* ${tup}/tmproms/cice/.
cp -auv $workingdir/common/cice_input_grids/a20 ${tup}/tmproms/cice/input_templates
# Remove old binaries
rm -f $CICE_DIR/rundir/cice

#rm -rf ${tup}/tmproms/cice/rundir/compile

#
# NB! Compile flags needed on Vilje
# -O2 -w -convert big_endian -assume byterecl
#

./comp_ice $NPX $NPY

# Test if compilation and linking was successfull

if [ ! -f $CICE_DIR/rundir/cice ]; then
    echo "$CICE_DIR/rundir/cice not found"
    echo "Error with compilation "
    exit -1
fi

# Build a library (for use in the ROMS build)
cd $CICE_DIR/rundir/compile
ar rcv libcice.a *.o

rm -f $CICE_DIR/rundir/cice

#cd $CICE_DIR


set +x
