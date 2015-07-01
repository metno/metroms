#!/bin/bash

set -x 

CASEHOME=$(pwd)

NRES=CASEDOMAIN
casename=CASENAME

source $CASEHOME/cice_baseconf.sh

### Set SRCDIR and EXEDIR to your own paths!
export SRCDIR=$CASEHOME/cice

if [ "$mach" == "vilje" ]; then
    source /etc/profile.d/modules.sh
#module load mpt/2.06 netcdf intelcomp
    module load mpt netcdf intelcomp
fi

ncomp=4  # Processors used for compilation


if [ "$NRES" == 'CASE''DOMAIN' ]; then
    NRES=gx3
fi

GRID=GRID
GRID=$(echo $GRID | tr [:upper:] [:lower:])

case $NRES in 
    a20) GRID=322x242;;
    col) GRID=5x5;;
    gx3) GRID=100x116;;
#    'gx1') GRID=320x384;;
#    'tx1')  GRID=360x240;;
esac

if [ $GRID == "grid" ]; then echo $NRES not implemented; exit; fi


if [ $casename == 'CASE''NAME' ]; then
    casename=test.$NRES
fi

restart='.true.'
if [ $NRES == 'a20' ];  then restart='.false.'; fi

comp=1

# Standard configuration in CICE5
export NICELYR=7       # number of vertical layers in the ice
export NSNWLYR=1       # number of vertical layers in the snow
export NICECAT=5       # number of ice thickness categories

# Optional Tracers

### Tracers              # match ice_in tracer_nml to conserve memory
export TRAGE=1           # set to 1 for ice age tracer
export TRFY=1            # set to 1 for first-year ice area tracer
export TRLVL=1           # set to 1 for level and deformed ice tracers
export TRPND=1           # set to 1 for melt pond tracers
export NTRAERO=0         # number of aerosol tracers 
                         # (up to max_aero in ice_domain_size.F90) 
                         # CESM uses 3 aerosol tracers
export TRBRI=0           # set to 1 for brine height tracer
export NBGCLYR=7         # number of zbgc layers
export TRBGCS=0          # number of skeletal layer bgc tracers 
                         # TRBGCS=0 or 2<=TRBGCS<=9)

### Change these to your own site and user directory! 
### You will need to create a Makefile Macro in bld/ and a run_ice script 
### in input_templates/.

#export SITE=NTNU.vilje
#export SITE=Ubuntu



### SYSTEM_USERDIR is predefined on ORNL machines

export SYSTEM_USERDIR=$WORKDIR/CICE$vers  

if [ -e $SRCDIR/cice_pes.bash ]; then
    source $SRCDIR/cice_pes.bash
else
    ((NPX=0))
    ((NPY=0))
    ((BPX=1))
    ((BPY=1))
    ((NPY=1))
    ((NTASK=NPX*NPY))
fi
ORES=$RES
((NPXO=NPX))
((NPYO=NPY))
((BPXO=BPXO))
((NPYO=BPYO))



if [ ! $ORES == $RES ]; then
    echo Area has changed 
    ((NPX=0))
    ((NPY=0))
    ((BPX=1))
    ((BPY=1))
    ((NTASK=NPX*NPY))
fi
RES=$NRES

# Recommendations:
#   NTASK equals nprocs in ice_in 
#   use processor_shape = slenderX1 or slenderX2 in ice_in
#   one per processor with distribution_type='cartesian' or
#   squarish blocks with distribution_type='rake'
# If BLCKX (BLCKY) does not divide NXGLOB (NYGLOB) evenly, padding 
# will be used on the right (top) of the grid.

if [ $# -ge 2 ]; then
    NPX=$1   # number of processors in x direction
    NPY=$2   # number of processors in y direction
fi
if [ $# -ge 4 ]; then
    BPX=$3   # number of blocks per processor in x direction
    BPY=$4   # number of blocks per processor in y direction
fi
### Specialty code
export CAM_ICE=no        # set to yes for CAM runs (single column) 
export SHRDIR=csm_share  # location of CCSM shared code
export IO_TYPE=netcdf       # set to no if netcdf library is unavailable
                         # set to pio for parallel netcdf
export DITTO=no          # reproducible diagnostics
export BARRIERS=no       # prevent MPI buffer overflow during gather/scatter 
export THRD=no           # set to yes for OpenMP threading (not working yet)

if [ $THRD == 'yes' ]; then  
    export compile_threaded=true
    export OMP_NUM_THREADS=2 ; 
fi # positive integer 

### File unit numbers
export NUMIN=11           # minimum file unit number
export NUMAX=99           # maximum file unit number





export EXEDIR=$SYSTEM_USERDIR/$NRES/$casename
      [ -d $EXEDIR ] || mkdir -p $EXEDIR
export  CBLD=$SRCDIR/bld
export OBJDIR=$EXEDIR/compile           ; [ -d $OBJDIR ] ||  mkdir -p $OBJDIR
export RSTDIR=$EXEDIR/restart           ; [ -d $RSTDIR ] ||  mkdir -p $RSTDIR
export HSTDIR=$EXEDIR/history           ; [ -d $HSTDIR ] ||  mkdir -p $HSTDIR

#export ARCH=`uname -s`

#   cp -f $CBLD/Makefile.std $CBLD/Makefile
   cp -f $CBLD/Makefile.$ARCH $CBLD/Makefile

#export ARCH=$ARCH.$SITE

cd $SRCDIR/source

cd $EXEDIR

if [ $RES == 'a20' ]; then
    [ -e cice.grid.nc ] || cp $SRCDIR/input_templates/$RES/cice_$GRID.grid.nc cice.grid.nc
    [ -e cice.kmt.nc ] || cp $SRCDIR/input_templates/$RES/cice_$GRID.kmt.nc cice.kmt.nc
else
    if [ ! $RES == 'col' ]; then
	[ -e grid.nc ] || cp $SRCDIR/input_templates/$RES/global_$RES.grid.nc grid.nc
	[ -e kmt.nc ] || cp $SRCDIR/input_templates/$RES/global_$RES.kmt.nc kmt.nc
    fi
fi

cd $RSTDIR

if [ ! $RES == 'a20' ];   then
    cp $SRCDIR/input_templates/$RES/iced_$RES* .
    [ -e ice.restart_file ] || cp $SRCDIR/input_templates/$RES/ice.restart_file .
fi

### Calculate processor tiling
((ntask=NPX*NPY)); NTASK=$ntask
if [ $NTASK -eq 0 ]; then
    echo Too few processors NPX = $NPX, NPY = $NPY, NTASK = $NTASK
    exit
fi
NXGLOB=$(echo $GRID | sed s/x.\*//)
NYGLOB=$(echo $GRID | sed s/.\*x//)
### x grid decomposition
((a=NXGLOB/NPX)); ((rem1=NXGLOB % NPX)); ((b=a+1))
if [ $rem1 -eq 0 ]; then ((BLCKX=a)); else ((BLCKX=b)); fi
((a=BLCKX/BPX)); ((rem2=BLCKX % BPX)); ((b=a+1))
if [ $rem2 -eq 0 ]; then ((BLCKX=a)); else ((BLCKX=b)); fi

### y grid decomposition
((a=NYGLOB/NPY)); ((rem1=NYGLOB % NPY)); ((b=a+1))
if [ $rem1 -eq 0 ]; then ((BLCKY=a)); else ((BLCKY=b)); fi
((a=BLCKY/BPY)); ((rem2=BLCKY % BPY)); ((b=a+1))
if [ $rem2 -eq 0 ]; then ((BLCKY=a)); else ((BLCKY=b)); fi

### max blocks
((m=BPX * BPY)); ((MXBLCKS=m))

# may need to increase MXBLCKS with rake distribution or padding
# export MXBOCKS=37 # if necessary (code will print proper value)

# Alternative from original script, should give same answere. 
## may need to increase MXBLCKS with rake distribution or padding
#@ a = $NXGLOB * $NYGLOB ; @ b = $BLCKX * $BLCKY * $NTASK  
#@ m = $a / $b ; setenv MXBLCKS $m; if ($MXBLCKS == 0) setenv MXBLCKS 1
##setenv MXBLCKS 37 # if necessary (code will print proper value)

cd $SRCDIR
clean=$($SRCDIR/recompile.bash $NPX $NPY $BPX $BPY )

if [  $clean -ne 0 ]; then 
    echo Cleans build directory
    $SRCDIR/clean_ice.bash def  
fi


cd $EXEDIR
cat $SRCDIR/input_templates/$RES/ice_in | sed -e s/NTASK/$NTASK/ | sed -e s/RESTART/$restart/ > ice_in

ln -sf $CICEDATADIR data

cd $OBJDIR

if [ $NTASK -eq 1 ]; then
   export COMMDIR=serial
else
   export COMMDIR=mpi 
fi

export DRVDIR=cice  # Stand-alone model

if [ $IO_TYPE == 'netcdf' ];  then
    export IODIR=io_netcdf
elif [ $IO_TYPE == 'pio' ];  then
    export IODIR=io_pio
else
    export IODIR=io_binary
fi


### List of source code directories (in order of importance).
cat > Filepath << EOF
$SRCDIR/drivers/$DRVDIR
$SRCDIR/source
$SRCDIR/$COMMDIR
$SRCDIR/$IODIR
$SRCDIR/$SHRDIR
EOF


MAKE=$(which gmake)

if [[ $MAKE == '' ]]; then
    MAKE=$(which make)
fi

if [[ $MAKE == '' ]]; then
    echo 'make command not found '
    exit
fi

if [ $comp -eq 1 ]; then
    cc -o makdep $CBLD/makdep.c                      || exit 2

    $MAKE -j $ncomp VPFILE=Filepath EXEC=$EXEDIR/cice \
        NXGLOB=$NXGLOB NYGLOB=$NYGLOB \
        BLCKX=$BLCKX BLCKY=$BLCKY MXBLCKS=$MXBLCKS \
	-f  $CBLD/Makefile MACFILE=$CBLD/Macros.$SITE || exit 2
#	-f  $CBLD/Makefile MACFILE=$CBLD/Macros.$ARCH || exit 2
fi

cd ..
pwd                                         
echo NTASK = $NTASK
echo "global N, N procs, N blocks/proc, block_size (requested)" 
echo "x    $NXGLOB,    $NPX,           $BPX,          $BLCKX"
echo "y    $NYGLOB,    $NPY,           $BPY,          $BLCKY"
echo max_blocks = $MXBLCKS


cat > $SRCDIR/cice_pes.bash <<EOF
NTASK=$NTASK 
NPX=$NPX 
NPY=$NPY 
BPX=$BPX
BPY=$BPY
RES=$RES 

MXBLCKS=$MXBLCKS
TRAGE=$TRAGE
TRFY=$TRFY
TRPND=$TRPND
NTRAERO=$NTRAERO # number of aerosol tracers
TRBRI=$TRBRI     # brine height tracer
NBGCLYR=$NBGCLYR # number of bio grid layers
TRBGCS=$TRBGCS   #  number of BGC tracers

THRD=$THRD
if [ $THRD == 'yes' ]; then  
    OMP_NUM_THREADS=2 ; 
fi # positive integer 
EOF

cat > $SRCDIR/cice_conf.bash <<EOF
SRCDIR=$SRCDIR
CASEHOME=$CASEHOME
SYSTEM_USERDIR=$SYSTEM_USERDIR
EXEDIR=$EXEDIR
CBLD=$CBLD
OBJDIR=$OBJDIR
vers=$vers
release=$release
EOF

cd $SRCDIR
chmod u+x cice_pes.bash cice_conf.bash


