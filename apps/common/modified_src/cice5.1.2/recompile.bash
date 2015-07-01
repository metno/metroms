#!/bin/bash

clean=1
PX=-1
PY=-1
BX=-1
BY=-1
NPX=0
NPY=0
BPX=0
BPY=0
if [ $# -ne 4 ] ; then
    clean=-1
#    echo 'Must give number of processors, blocks in x and y direction, NPX NPY BPX BPY'
else
    PX=$1
    PY=$2
    BX=$3
    BY=$4
fi
if [ -f $SRCDIR/cice_pes.bash ]; then source $SRCDIR/cice_pes.bash; fi

if [ $PX -eq $NPX ] && [ $PY -eq $NPY ] && [ $BX -eq $BPX ] && [ $BY -eq $BPY ]; then
    clean=0
fi

echo $clean
