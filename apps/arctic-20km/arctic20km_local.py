#!/usr/bin/python
import numpy as np
import os
import Constants

from GlobalParams import *
from Params import *
from ModelRun import *
########################################################################

rundir="/disk1/keguangw/tmproms/run/arctic-20km"

# Set cpus for ROMS:
xcpu=3
ycpu=2
# Set cpus for CICE:
icecpu=2

a20params=Params(rundir,xcpu,ycpu,tsteps=720,irestart=0,cicecpu=icecpu)
a20params.ROMSINFILE=a20params.RUNPATH+"/roms.in"

modelrun=ModelRun(a20params,Constants.FELT,Constants.FELT)

modelrun.preprocess()
modelrun.run_roms(Constants.MPI,Constants.NODEBUG,Constants.MET64) #24h hindcast
modelrun.postprocess()

