#!/bin/bash
set -x

workingdir=${PWD} 
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

# Build MCT
mkdir -p ${tup}/tmproms
cd ${tup}/tmproms
# Unpack standard source files
#tar -xf ${metroms_base}/static_libs/mct-2.8.tar.gz
export MCT_DIR=${tup}/tmproms/MCT
cd $MCT_DIR

if [ ${METROMS_MYHOST} == "metlocal" ]; then
    FORT=mpif90
elif [ ${METROMS_MYHOST} == "vilje" ]; then
    FORT=ifort
else
    echo " Computer not defined set environment variable METROMS_MYHOST= metlocal, vilje .."
    exit
fi

./configure FC=$FORT --prefix=$MCT_DIR
make install

set +x
