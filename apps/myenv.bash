#!/bin/bash

export METROMS_MYHOST=$1
echo "loading $METROMS_MYHOST paths"

if [ "$METROMS_MYHOST" == "metlocal" ]; then
    export METROMS_BASEDIR=/disk1/$USER
    export METROMS_TMPDIR=/disk1/$USER
elif [ "$METROMS_MYHOST" == "vilje" ]; then
    if [ "$USER" == "forecast" ]; then
	export METROMS_BASEDIR=$HOME/sea/ROMS/metroms
        export METROMS_TMPDIR=$HOME/run
	export METROMS_BLDDIR=$METROMS_TMPDIR
        export METROMS_APPDIR=$HOME/sea/ROMS/metroms_apps
    else
	export METROMS_BASEDIR=$HOME/metroms
	export METROMS_TMPDIR=/work/$USER/tmproms/run
	export METROMS_BLDDIR=$METROMS_TMPDIR
	export METROMS_APPDIR=$HOME/metroms_apps
    fi
elif [ "$METROMS_MYHOST" == "alvin" ]; then
    if [ "$USER" == "metno_op" ]; then
	export METROMS_BASEDIR=$HOME/sea/ROMS/metroms
	export METROMS_TMPDIR=$HOME/run
	export METROMS_BLDDIR=$HOME/work/sea/ROMS/metroms
	export METROMS_APPDIR=$HOME/sea/ROMS/metroms_apps
    else
	echo 'not defined yet'
    fi
elif [ "$METROMS_MYHOST" == "met_ppi" ]; then
    if [ "$USER" == "havis" ]; then
	export METROMS_BASEDIR=$HOME/sea/ROMS/metroms
	export METROMS_TMPDIR=$HOME/run
	export METROMS_BLDDIR=$HOME/work/sea/ROMS/metroms
	export METROMS_APPDIR=$HOME/sea/ROMS/metroms_apps
    else
	export METROMS_BASEDIR=$HOME/metroms
	export METROMS_TMPDIR=/lustre/storeB/users/$USER/metroms_run
	export METROMS_BLDDIR=$METROMS_TMPDIR
	export METROMS_APPDIR=$HOME/metroms_apps
    fi
elif [ "$METROMS_MYHOST" == "elvis" ] || [ "$METROMS_MYHOST" == "nebula" ]; then
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
else
    echo "Undefined METROMS_MYHOST ", $METROMS_MYHOST
fi

export PYTHONPATH=$PYTHONPATH:$METROMS_BASEDIR/apps/common/python/

#=====================================================================
export MCT_DIR=$METROMS_TMPDIR/MCT

export SITE=$METROMS_MYHOST
export FORT=ifort
if [ "$METROMS_MYHOST" == "metlocal" ]; then
   export SITE=Ubuntu
   export FORT=gfortran
fi

