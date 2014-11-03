#!/usr/bin/python
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
xcpu=3
ycpu=2
# Set cpus for CICE:
icecpu=2
# Choose a predifnes ROMS-application:
app='a20' # Arctic-20km

a20params=Params(app,xcpu,ycpu,fclen=240,irestart=0,cicecpu=icecpu)
#a20params.RUNPATH="/disk1/tmp"
a20params.ROMSINFILE=a20params.RUNPATH+"/roms___.in"

modelrun=ModelRun(a20params,Constants.FELT,Constants.FELT)

modelrun.preprocess()
modelrun.run_roms(Constants.DRY,Constants.NODEBUG,Constants.MET64) #24h hindcast
modelrun.postprocess()

