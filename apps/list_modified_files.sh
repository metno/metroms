#!/bin/bash
#set -x


#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Users can/should/must change things between here...
#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
#
# Setting up things, like compilers etc:
export ROMS_APPLICATION=$1
#export roms_ver="roms-3.6"
export roms_ver="roms-trunk"


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

tmpdir=tmproms

export MY_ROMS_SRC=${tup}/${tmpdir}/roms_src
mkdir -p ${MY_ROMS_SRC}
cd ${MY_ROMS_SRC}
tar -xf ${metroms_base}/static_libs/${roms_ver}.tar.gz

# JD : Added temporary to have place for a new file
touch $MY_ROMS_SRC/ROMS/Nonlinear/frazil_ice_prod_mod.F
# JD end

#SM: Same here, added temporary for new file
touch $MY_ROMS_SRC/ROMS/Modules/mod_ice.F

# Set number of nested/composed/mosaic grids.  Currently, only one grid
# is supported.

export MY_ROOT_DIR=${workingdir}/${ROMS_APPLICATION}/
export MY_PROJECT_DIR=${workingdir}/${ROMS_APPLICATION}/
export SCRATCH_DIR=${tup}/${tmpdir}/build

cd ${MY_PROJECT_DIR}

# # NMK - 20151030
# # Check if we have any common modified source files
export MODIFIED_SRC_FOLDER=${workingdir}/common/modified_src/${roms_ver}
if [ -s $MODIFIED_SRC_FOLDER ]; then
  cd $MODIFIED_SRC_FOLDER
  gotModifiedSourceCOMMON=`ls *.F *.h *.mk *.in`
  cd ${MY_PROJECT_DIR}
fi

# # KHC - 20110209
# # Check if we have any modified source files
if [ -s modified_src ]; then
  cd modified_src
  gotModifiedSourceAPP=`ls *.F *.h *.mk *.in`
  cd ..
fi

# Replace the original files with the modifications
if [ "$gotModifiedSourceAPP" != "" ] || [ "$gotModifiedSourceCOMMON" != "" ]; then
  echo $gotModifiedSourceAPP
  echo $gotModifiedSourceCOMMON
  echo "!!!!!!!!!!!!Found modified src...!!!!!!!!!!!!!!!!!!!!!"
  echo "COMMON:"
    # Copy common modified source to ROMS app-directory
    for ModSrc in $gotModifiedSourceCOMMON; do
        # Check where original resides
        origFile=`find ${MY_ROMS_SRC} -name $ModSrc`
        #echo "*****************************************************************************"
	echo $origFile
	#echo "< MODIFIED vs ORIG >"
        #echo "*****************************************************************************"
	#diff $MODIFIED_SRC_FOLDER/$ModSrc $origFile
    done
    # Copy locally modified source to main ROMS directory
  echo "APP:"
    for ModSrc in $gotModifiedSourceAPP; do
        # Check where original resides
        origFile=`find ${MY_ROMS_SRC} -name $ModSrc`
        #echo "*****************************************************************************"
        echo $origFile
	#echo "< MODIFIED vs ORIG >"
        #echo "*****************************************************************************"
	#diff modified_src/$ModSrc $origFile
    done
fi


#set +x
