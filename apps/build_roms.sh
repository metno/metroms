#!/bin/bash
#
# svn $Id: build.bash 474 2010-06-25 20:19:44Z arango $
#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Copyright (c) 2002-2010 The ROMS/TOMS Group                           :::
#   Licensed under a MIT/X style license                                :::
#   See License_ROMS.txt                                                :::
#::::::::::::::::::::::::::::::::::::::::::::::::::::: Hernan G. Arango :::
#                                                                       :::
# ROMS/TOMS Compiling Script                                            :::
#                                                                       :::
# Script to compile an user application where the application-specific  :::
# files are kept separate from the ROMS source code.                    :::
#                                                                       :::
# Q: How/why does this script work?                                     :::
#                                                                       :::
# A: The ROMS makefile configures user-defined options with a set of    :::
#    flags such as ROMS_APPLICATION. Browse the makefile to see these.  :::
#    If an option in the makefile uses the syntax ?= in setting the     :::
#    default, this means that make will check whether an environment    :::
#    variable by that name is set in the shell that calls make. If so   :::
#    the environment variable value overrides the default (and the      :::
#    user need not maintain separate makefiles, or frequently edit      :::
#    the makefile, to run separate applications).                       :::
#                                                                       :::
# Usage:                                                                :::
#                                                                       :::
#    ./build.bash [options]                                             :::
#                                                                       :::
# Options:                                                              :::
#                                                                       :::
#    -j [N]      Compile in parallel using N CPUs                       :::
#                  omit argument for all available CPUs                 :::
#    -noclean    Do not clean already compiled objects                  :::
#                                                                       :::
# Notice that sometimes the parallel compilation fail to find MPI       :::
# include file "mpif.h".                                                :::
#                                                                       :::
#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# 08/01/2014: Big rewrite by nilsmk@met.no to make build-script more general
# 03/11/2020: reorganize the directory of the modified ROMS
set -x

if [ $# -lt 1 ]
  then
  echo "Usage: $0 modelname -j 4"
  echo "Or specify more kernels than 4 for compilation if you have them available"
  exit
fi

#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Users can/should/must change things between here...
#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
#
# Setting up things, like compilers etc:
export ROMS_APPLICATION=$1
#export roms_ver="roms-3.6"
export roms_ver="roms-trunk"

export USE_MPI=on
export USE_MPIF90=on
export which_MPI=openmpi
export USE_OpenMP=
#export USE_LARGE=on
#export USE_DEBUG=on
export USE_NETCDF4=on
#export USE_PARALLEL_IO=on
export USE_CICE=on

#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ... and here.
#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

if [ ! -d ${METROMS_TMPDIR} ] ; then
    echo "METROMS_TMPDIR not defined, set environment variable METROMS_TMPDIR"
    exit 
fi
if [ ! -d ${METROMS_BLDDIR} ] ; then
    echo "METROMS_BLDDIR not defined, set environment variable METROMS_BLDDIR"
    exit
fi
if [ ! -d ${METROMS_BASEDIR} ] ; then
    echo "METROMS_BASEDIR not defined, set environment variable METROMS_TMPDIR"
    exit 
fi

# set environment variables
export MY_ROOT_DIR=${METROMS_APPDIR}/${ROMS_APPLICATION}/
export MY_PROJECT_DIR=${METROMS_APPDIR}/${ROMS_APPLICATION}/
export SCRATCH_DIR=${METROMS_BLDDIR}/build

# unpack original source code
export MY_ROMS_SRC=${METROMS_BLDDIR}/roms_src
mkdir -p ${MY_ROMS_SRC}
cd ${MY_ROMS_SRC}
tar -xf ${METROMS_BASEDIR}/static_libs/${roms_ver}.tar.gz
rm -rf User

# copy modified source code
export MODIFIED_SRC_FOLDER=${METROMS_BASEDIR}/apps/common/modified_src/${roms_ver}
cp -a ${MODIFIED_SRC_FOLDER}/* ${MY_ROMS_SRC}

export COMPILERS=${MY_ROMS_SRC}/Compilers
cp ${COMPILERS}/Linux-${FORT}.mk_$SITE ${COMPILERS}/Linux-${FORT}.mk

# Set number of nested/composed/mosaic grids.  Currently, only one grid
# is supported.

export NestedGrids=1

if [ -n "${USE_CICE:+1}" ]; then
	export USE_MCT=on
	export MY_CPP_FLAGS="${MY_CPP_FLAGS} -DNO_LBC_ATT -DMODEL_COUPLING -DUSE_MCT -DMCT_COUPLING -DMCT_LIB -DCICE_COUPLING -DCICE_OCEAN"
fi

#
if [ -n "${USE_NETCDF4:+1}" ]; then
 export USE_DAP=on
fi

export MY_HEADER_DIR=${MY_PROJECT_DIR}/include
export MY_ANALYTICAL_DIR=${MY_HEADER_DIR}

# Build ROMS
# Put the binary to execute in the following directory.
export BINDIR=${METROMS_TMPDIR}/${ROMS_APPLICATION}
mkdir -p $BINDIR

# set compiler parameters
parallel=1
clean=1

while [ $# -gt 1 ]
do
  case "$2" in
    -j )
shift
parallel=1
test=`echo $2 | grep -P '^\d+$'`
if [ "$test" != "" ]; then
  NCPUS="-j $2"
  shift
else
  NCPUS="-j"
fi
;;
esac
done

cd ${MY_ROMS_SRC}
if [ $clean -eq 1 ]; then
  make clean
fi

# Compile (the binary will go to BINDIR set above).
if [ $parallel -eq 1 ]; then
  make $NCPUS
else
  make
fi

if [ -n "${USE_CICE:+1}" ]; then
	cp ${MODIFIED_SRC_FOLDER}/coupling.dat $BINDIR/
fi

# Clean up unpacked static code:
cd  ${MY_PROJECT_DIR}
#rm -rf ${MY_ROMS_SRC}

set +x
