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

cd $MCT_DIR
./configure FC=$FORT --prefix=$MCT_DIR
make install
make clean

set +x
