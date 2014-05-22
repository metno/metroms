#!/usr/bin/python
import numpy as np
import os
import Constants

from GlobalParams import *
from Params import *
from ModelRun import *
########################################################################

rundir="/disk1/tmproms/run/arctic-20km"

xcpu=2
ycpu=4

a20params=Params(rundir,xcpu,ycpu,tsteps=720,irestart=0)
a20params.ROMSINFILE=a20params.RUNPATH+"/roms.in"

modelrun=ModelRun(a20params,Constants.FELT,Constants.FELT)

modelrun.preprocess()
modelrun.run_roms(Constants.OPENMP,Constants.NODEBUG,Constants.MET64) #24h hindcast
modelrun.postprocess()