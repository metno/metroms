#!/bin/bash

script=$(readlink -f $0)
scriptpath=$(dirname $script)

if [ ! -x $scriptpath/cice_baseconf.sh ]; then
    echo "Could not source $scriptpath/cice_baseconf.sh. Are you sure "
    echo "you have configures cice according to setup?" 
    echo 
    echo "Use local_mods/.cice_baseconf.sh as template"
else
    source $scriptpath/cice_baseconf.sh
fi

res=gx3
casename=test


if [ $# -lt 2 ]; then
    echo "You should give name and domain for your case (and optional site) "
    echo Uses default-name $casename at the $res domain, configured for $site
else
    casename=$1
    res=$2
    if [ $# -ge 3 ]; then
	site=$3
    fi
    echo Configures $casename at the $res domain, using $site specific files.
fi



if [ -d $casename ]; then
    echo "$casename allready defined. (R)emove or rename old directory and try again, or Update (U) with old setup."
    read val
    [ $val == 'U' ] || exit
fi

mkdir -p $casename 


cd $casename

tar xzf $CICERELEASE

[ -d cice ] || exit Something wrong with $casename/cice

rsync -auv $CICELOCAL/* cice/.

cat cice/comp_ice.bash | sed -e s/CASENAME/$casename/ | sed -e s/CASEDOMAIN/$res/ > cice/comp_ice
chmod u+x cice/comp_ice

CICEDIR=$(pwd)/cice

if [ ! -d $CICEDIR/input_templates/$res ]; then
    echo $CICEDIR/input_templates/$res not found. 
    echo Could not find information about the domain $res
    exit 
fi


cp -p $CICEDIR/input_templates/run_ice.$SITE .
cp $scriptpath/cice_baseconf.sh .
chmod u+x cice_baseconf.sh
echo "export CICEDIR=$CICEDIR" >> ./cice_baseconf.sh

