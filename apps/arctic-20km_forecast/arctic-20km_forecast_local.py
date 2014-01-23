#!/usr/bin/python
import numpy as np
import os
from ModelRun import *
from Constants import *
from GlobalParams import *
from params import Params
########################################################################
rundir="/disk1/tmproms/run/arctic-20km"

a20hparams=Params(rundir)
a20hparams.ROMSINFILE=a20hparams.RUNPATH+"/roms_hindcast.in"
a20hparams.set_run_params(tsteps=720,irestart=-1)

a20fparams=Params(rundir)
a20fparams.ROMSINFILE=a20fparams.RUNPATH+"/roms_forecast.in"
a20fparams.set_run_params(tsteps=7200,irestart=1)
    

modelrun_forecast=ModelRun(a20fparams,FELT,FELT)
modelrun_hindcast=ModelRun(a20hparams,FELT,FELT)
modelrun_forecast.preprocess()
modelrun_hindcast.run_roms(DRY,NODEBUG,MET64) #24h hindcast
modelrun_forecast.run_roms(DRY,NODEBUG,MET64) #240h forecast
modelrun_forecast.postprocess()
