#!/bin/bash -x 


if [ $# -eq 0 ]; then
    if [ -x $SRCDIR/cice_conf.bash ] ; then 
	source $SRCDIR/cice_conf.bash
    else
	echo No cice_conf.bash found
	exit
    fi
fi


if [ ! -d $EXEDIR ]; then
    echo $EXEDIR not found
    exit
fi

cd $OBJDIR
pwd

# Clean compile directory
MAKE=$(which make)
if [[ $MAKE == '' ]]; then
    echo 'make command not found '
    exit
fi

$MAKE -f $CBLD/Makefile clean  || exit 2


