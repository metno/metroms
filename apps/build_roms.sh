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
set -x

if [ $# -le 1 ]
then
  echo "Usage: $0 modelname -j 4"
  echo "Or specify more kernels than 4 for compilation if you have them available"
  exit
fi

export ROMS_APPLICATION=$1

export USE_MPI=on
export USE_MPIF90=on
export FORT=gfortran
#export FORT=ifort
#export USE_OpenMP=on
export USE_LARGE=on

#export USE_DEBUG=on
#export USE_NETCDF4=on

workingdir=${PWD} 
cd ../
metroms_base=${PWD} 
cd ../
tup=${PWD}

tmpdir=tmproms

export MY_ROMS_SRC=${tup}/${tmpdir}/roms_src
mkdir -p ${MY_ROMS_SRC}
cd ${MY_ROMS_SRC}
tar -xf ${metroms_base}/static_libs/roms-3.6.tar.gz

# Set path of the directory containing makefile configuration (*.mk) files.
# The user has the option to specify a customized version of these files
# in a different directory than the one distributed with the source code,
# ${MY_ROMS_SCR}/Compilers. If this is the case, the you need to keep
# these configurations files up-to-date.

export COMPILERS=${MY_ROMS_SRC}/Compilers

cd ${workingdir}

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

cd ${ROMS_APPLICATION}

# Set number of nested/composed/mosaic grids.  Currently, only one grid
# is supported.

export NestedGrids=1

export MY_ROOT_DIR=${workingdir}/${ROMS_APPLICATION}/
export MY_PROJECT_DIR=${workingdir}/${ROMS_APPLICATION}/
export SCRATCH_DIR=${tup}/${tmpdir}/build

cd ${MY_PROJECT_DIR}

# # KHC - 20110209
# # Check if we have any modified source files

if [ -s modified_src ]; then
    cd modified_src
    gotModifiedSource=`ls *.F *.h *.mk *.in`
    cd ..
fi

# Replace the original files with the modifications
if [ "$gotModifiedSource" != "" ]; then

    # Copy locally modified source to main ROMS directory
    for ModSrc in $gotModifiedSource; do

        # Check where original resides
        origFile=`find $MY_ROMS_SRC -name $ModSrc`

        if [ -f "$origFile" ]; then

            # Moving original and copying user-modifed source code
            # first checking if the original already exists with
            # the .orig extension
            if [ ! -f "$origFile.orig" ]; then
                mv $origFile $origFile.orig
                echo "Moving $origFile to $origFile.orig"
            fi

            # Copying from local source directory to repository
            cp modified_src/$ModSrc $origFile
            echo "Copying modified_src/$ModSrc to $origFile"

            if [ ! -f USER_MODIFIED_CODE_IN_REPO ]; then

                # Touch file to notify that user modified code has been
                # placed in the repository
                touch USER_MODIFIED_CODE_IN_REPO

            fi
        else

            # No such file in repository, quit script
            echo "No source code file $ModSrc in repository, exiting."
            exit 3

        fi
    done
fi

# Removing user modified source code in repository
# KHC - 20110209
# NMK - 2013
rollback() {
    cd $MY_ROOT_DIR
    
    if [ -f USER_MODIFIED_CODE_IN_REPO ]; then
	
    # Find source code files with ".orig"-ending and
    # remove ending
	filelist=`find "$MY_ROMS_SRC" -name *.orig`
	
	if [ "$filelist" != "" ]; then
	    
	    for oldFileName in $filelist; do
		
	    # extract basename
		newFileName=`basename $oldFileName .orig`
		fileDirectory=`dirname $oldFileName`
		mv $oldFileName  $fileDirectory/$newFileName
		
		echo "Moved $oldFileName  to $fileDirectory/$newFileName"
		
	    done
	    
	else # Empty filelist, no such files in repository
	    
	    echo "Did not find any .orig-files in the repository, empty file deleted"
	    
	fi
	
    # Remove empty file
	rm -f USER_MODIFIED_CODE_IN_REPO
	
    fi
}
trap 'rollback; exit 99' 0

export USE_MCT=on
export USE_CICE=on
export MY_CPP_FLAGS="${MY_CPP_FLAGS} -DNO_LBC_ATT -DMODEL_COUPLING -DUSE_MCT -DMCT_COUPLING -DMCT_LIB -DCICE_COUPLING -DCICE_OCEAN"
export USE_MY_LIBS=on

if [ -n "${USE_NETCDF4:+1}" ]; then
 export USE_DAP=on
 export PATH=/usr/bin:$PATH
fi

export MY_HEADER_DIR=${MY_PROJECT_DIR}/include
export MY_ANALYTICAL_DIR=${MY_HEADER_DIR}

# Build ROMS
# Put the binary to execute in the following directory.
export BINDIR=${tup}/${tmpdir}/run/${ROMS_APPLICATION}
mkdir -p $BINDIR

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

# Clean up unpacked static code:
cd  ${MY_PROJECT_DIR}
#rm -rf ${MY_ROMS_SRC}

set +x