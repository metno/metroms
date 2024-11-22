#!/bin/bash

# Check if at least one argument was provided
if [ $# -eq 0 ]; then
    echo "Error: Please provide at least one argument."
    exit 1
fi
# Always assign the first argument to METROMS_MYHOST
METROMS_MYHOST=$1

# If METROMS_MYHOST is met_ppi, check if second argument METROMS_COMPUTENODE is provided
if [ "$METROMS_MYHOST" == "met_ppi" ]; then
    # Check if a second argument was provided
    if [ $# -eq 2 ]; then
        METROMS_COMPUTENODE=$2
    else
        # If not, set the METROMS_COMPUTENODE variable to r8_b for met_ppi
        METROMS_COMPUTENODE="r8_b"
        echo "No login node option provided, loading $METROMS_COMPUTENODE login node"
    fi
fi

# Export the variables
export METROMS_MYHOST
export METROMS_COMPUTENODE
echo "Loading $METROMS_MYHOST paths"
echo "Loading $METROMS_COMPUTENODE login node if METROMS_MYHOST is met_ppi"

if [ "$METROMS_MYHOST" == "met_ppi" ]; then
    if [ "$USER" == "havis" ]; then
        export METROMS_BASEDIR=$HOME/sea/ROMS/metroms
        export METROMS_TMPDIR=$HOME/run
        export METROMS_BLDDIR=$HOME/work/sea/ROMS/metroms
        export METROMS_APPDIR=$HOME/sea/ROMS/metroms_apps
    else
    	export METROMS_BASEDIR=$HOME/metroms
    if [ -d "/lustre/storeB/users/$USER" ]; then
        echo "using storeB"
	    export METROMS_TMPDIR=/lustre/storeB/users/$USER/metroms_run
    elif [ -d "/lustre/storeA/users/$USER" ]; then
        echo "using storeA"
        export METROMS_TMPDIR=/lustre/storeA/users/$USER/metroms_run
    else
        echo "ERROR: NO STORE FOUND!!"
        exit 99
    fi
	export METROMS_BLDDIR=$METROMS_TMPDIR
	export METROMS_APPDIR=$HOME/metroms_apps
    fi
elif [ "$METROMS_MYHOST" == "nebula" ] || [ "$METROMS_MYHOST" == "nebula2" ]; then
    export METROMS_BASEDIR=$HOME/metroms
    export METROMS_TMPDIR=/nobackup/forsk/$USER/metroms_run
    export METROMS_BLDDIR=$METROMS_TMPDIR
    export METROMS_APPDIR=$HOME/metroms_apps
elif [ "$METROMS_MYHOST" == "stratus" ]; then
    if [ "$USER" == "metno_op" ]; then
        export METROMS_BASEDIR=$HOME/sea/ROMS/metroms
        export METROMS_TMPDIR=$HOME/run
        export METROMS_BLDDIR=$HOME/work/sea/ROMS/metroms
        export METROMS_APPDIR=$HOME/sea/ROMS/metroms_apps
    else
	echo 'not defined yet'
    fi
elif [ "$METROMS_MYHOST" == "fram" ]; then
    export METROMS_BASEDIR=$HOME/metroms
    export METROMS_TMPDIR=/cluster/work/users/$USER/metroms_run
    export METROMS_BLDDIR=$METROMS_TMPDIR
    export METROMS_APPDIR=$HOME/metroms_apps
else
    echo "Undefined METROMS_MYHOST ", $METROMS_MYHOST
fi


export PYTHONPATH=$PYTHONPATH:$METROMS_BASEDIR/apps/common/python/
source ${METROMS_BASEDIR}/apps/modules.sh
