#!/bin/bash
set -x

if [ ! -d ${METROMS_TMPDIR} ] ; then
    echo "METROMS_TMPDIR not defined, set environment variable METROMS_TMPDIR"
    exit 
fi
if [ ! -d ${METROMS_BASEDIR} ] ; then
    echo "METROMS_BASEDIR not defined, set environment variable METROMS_TMPDIR"
    exit 
fi

# Build MCT
cd ${METROMS_TMPDIR}
# Unpack standard source files
tar -xvf ${METROMS_BASEDIR}/static_libs/mct-2.9.tar.gz
export MCT_DIR=${METROMS_TMPDIR}/MCT
cd $MCT_DIR

if [ ${METROMS_MYHOST} == "metlocal" ] || [ "${METROMS_MYHOST}" == "met_ppi" ]; then
    FORT=mpif90
elif [ ${METROMS_MYHOST} == "vilje" ] || [ ${METROMS_MYHOST} == "fram" ]; then
    FORT=ifort
else
    echo " Computer not defined set environment variable METROMS_MYHOST= metlocal, vilje .."
    exit
fi

./configure FC=$FORT --prefix=$MCT_DIR
make install
make clean

set +x
