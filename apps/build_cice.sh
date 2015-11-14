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

if [ $# -lt 1 ]
  then
  echo "Usage: $0 modelname <xcpu> <ycpu>"
  echo "<xcpu> <ycpu> are optional arguments"
  exit
fi
export ROMS_APPLICATION=$1

if [ $# -ge 3 ]; then
    NPX=$2
    NPY=$3
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
export CICE_DIR=${tup}/tmproms/run/$ROMS_APPLICATION/cice
mkdir -p $CICE_DIR/rundir
cd ${tup}/tmproms/run/$ROMS_APPLICATION
# Unpack standard source files
echo $PWD
tar -xvf ${metroms_base}/static_libs/$CICEVERSION.tar.gz
cd $CICE_DIR

export MCT_INCDIR=${tup}/tmproms/MCT/include
export MCT_LIBDIR=${tup}/tmproms/MCT/lib


# Copy modified source files
#mkdir -p ${tup}/tmproms/cice
cp -a $workingdir/common/modified_src/$CICEVERSION/* $CICE_DIR
cp -auv $workingdir/common/cice_input_grids/$ROMS_APPLICATION $CICE_DIR/input_templates
# Remove old binaries
rm -f $CICE_DIR/rundir/cice

#rm -rf ${tup}/tmproms/cice/rundir/compile

#
# NB! Compile flags needed on Vilje
# -O2 -w -convert big_endian -assume byterecl
#

echo $PWD
./comp_ice $ROMS_APPLICATION $NPX $NPY

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
