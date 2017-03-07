#!/bin/bash

# if [ $# -le 1 ]
# then
#   echo "Usage: $0 [vilje|metlocal]"
#   echo "Choose architecture..."
#   exit
# fi

#export METROMS_MYHOST=metlocal
export METROMS_MYHOST=vilje
#export METROMS_MYHOST=$1

#export METROMS_MYARCH=Linux

if [ "$METROMS_MYHOST" == "metlocal" ]; then
    export METROMS_BASEDIR=/disk1/$USER
    export METROMS_TMPDIR=/disk1/$USER
elif [ "$METROMS_MYHOST" == "vilje" ]; then
    export METROMS_BASEDIR=$HOME/metroms
    export METROMS_TMPDIR=/work/$USER/tmproms
    export METROMS_APPDIR=$HOME/metroms_apps
else
    echo "Undefined METROMS_MYHOST ", $METROMS_MYHOST
fi


export PYTHONPATH=$PYTHONPATH:$METROMS_BASEDIR/apps/common/python/
