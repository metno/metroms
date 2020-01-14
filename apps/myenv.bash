#!/bin/bash
# 08/01/2020 by keguangw@met.no to include more environment variables

set -x

if [ $# -eq 2 ]; then
   export METROMS_MYHOST=$1
   export app=$2
   echo "loading $METROMS_MYHOST path for $app"
elif [ $# -eq 1 ]; then
   export METROMS_MYHOST=$1
   echo "loading $METROMS_MYHOST paths"
else
   echo "Undefined METROMS_MYHOST/APPLICATION !!! "
   return
fi

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
elif [ "$METROMS_MYHOST" == "alvin" ] || [ "$METROMS_MYHOST" == "elvis" ]; then
    export METROMS_BASEDIR=$HOME/metroms
    export METROMS_TMPDIR=/nobackup/forsk/$USER/metroms_run
    export METROMS_BLDDIR=$METROMS_TMPDIR
    export METROMS_APPDIR=$HOME/metroms_apps
elif [ "$METROMS_MYHOST" == "nebula" ] ; then
    export METROMS_BASEDIR=$HOME/metroms
    export METROMS_TMPDIR=/nobackup/forsk/$USER/metroms_run
    export METROMS_BLDDIR=$METROMS_TMPDIR
    export METROMS_APPDIR=$HOME/metroms_apps
    export WORKDIR=$METROMS_TMPDIR/..
    export FORCDIR=$WORKDIR/Forcing/$app
    export INITDIR=$WORKDIR/Initial/$app
    export DATADIR=$WORKDIR/Data/$app
    export RUNDIR=$METROMS_TMPDIR/$app/run
    mkdir -p $RUNDIR
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

export ROMSGRD=$METROMS_APPDIR/grid
export CICEGRD=$METROMS_APPDIR/cice_input_grid
export MCT_DIR=$METROMS_TMPDIR/MCT
export METROMS_PYTHON=$METROMS_BASEDIR/apps/common/python
export PYTHONPATH=$PYTHONPATH:$METROMS_PYTHON

if [ "$METROMS_MYHOST" == "metlocal" ]; then
    export FORT=gfortran
    export SITE=Ubuntu
elif [ "$METROMS_MYHOST" == "met_ppi" ]; then
    export FORT=ifort
    export SITE=MET_PPI
elif [ "$METROMS_MYHOST" == "vilje" ]; then
    export FORT=ifort
    export SITE=NTNU.vilje
else
    export FORT=ifort
    export SITE=$METROMS_MYHOST
fi

set +x

