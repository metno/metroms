#!/sw/sdev/Modules/python/python-2.7.2/bin/python
#PBS -N arctic-20km
#PBS -A mifa01hi
#PBS -l walltime=12:00:00
#PBS -l select=1:ncpus=32:mpiprocs=16:ompthreads=16:mem=29gb
#PBS -o /work/keguangw/tmproms/run/arctic-20km/prep_o.log
#PBS -j oe
#PBS -V

import numpy as np
import os
import Constants

from GlobalParams import *
from Params import *
from ModelRun import *
########################################################################
# Note: Variable 'fclen' is forecast length in hours
########################################################################
# Set cpus for ROMS:
xcpu=4
ycpu=3
# Set cpus for CICE:
icecpu=2
# Choose a predifnes ROMS-application:
app='a20' # Arctic-20km

a20params=Params(app,xcpu,ycpu,fclen=8760,irestart=0,cicecpu=icecpu)
#a20params.RUNPATH="/disk1/tmp"
#a20params.ROMSINFILE=a20params.RUNPATH+"/roms___.in"

modelrun=ModelRun(a20params,Constants.FELT,Constants.FELT)

modelrun.preprocess()
modelrun.run_roms(Constants.MPI,Constants.NODEBUG,Constants.MET64) #24h hindcast
modelrun.postprocess()

