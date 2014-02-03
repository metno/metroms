#!/usr/bin/python
import numpy as np
import os
from ModelRun import *
from Constants import *
from GlobalParams import *
from Params import Params
########################################################################
rundir="/disk1/tmproms/run/arctic-20km"

xcpu=2
ycpu=2

a20hparams=Params(rundir,xcpu,ycpu,tsteps=720,irestart=-1)
a20hparams.ROMSINFILE=a20hparams.RUNPATH+"/roms_hindcast.in"
#a20hparams.set_run_params(xcpu,ycpu,tsteps=720,irestart=-1)
a20hparams.change_run_param('RSTSTEP',72)

a20fparams=Params(rundir,xcpu,ycpu,tsteps=7200,irestart=-1)
a20fparams.ROMSINFILE=a20fparams.RUNPATH+"/roms_forecast.in"
#a20fparams.set_run_params(xcpu,ycpu,tsteps=7200,irestart=1)
    

modelrun_forecast=ModelRun(a20fparams,Constants.FELT,Constants.FELT)
modelrun_hindcast=ModelRun(a20hparams,Constants.FELT,Constants.FELT)

modelrun_forecast.preprocess()
modelrun_hindcast.run_roms(Constants.DRY,Constants.NODEBUG,Constants.MET64) #24h hindcast
modelrun_forecast.run_roms(Constants.DRY,Constants.NODEBUG,Constants.MET64) #240h forecast
modelrun_forecast.postprocess()
