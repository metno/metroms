#!/usr/bin/python
import numpy as np
import os
from ModelRun import *
from Constants import *
from GlobalParams import *
from n4_2dParams import Params
########################################################################
rundir="/disk1/tmproms/run/nordic-4km2d-stormsurge"

xcpu=2
ycpu=2
dt=10 #Timestep in seconds
fclen=120 #Forecast length in hours

n4_2d_surge_params=Params(rundir,xcpu,ycpu,tsteps=(fclen*3600)/dt,deltat=dt,irestart=-1)
n4_2d_surge_params.ROMSINFILE=n4_2d_surge_params.RUNPATH+"/roms.in"
#n4_2d_surge_params.set_run_params(xcpu,ycpu,tsteps=720,irestart=-1)
#n4_2d_surge_params.change_run_param('RSTSTEP',72)

    

modelrun=ModelRun(n4_2d_surge_params,Constants.FELT,Constants.FELT)

modelrun.preprocess()
modelrun.run_roms(Constants.DRY,Constants.NODEBUG,Constants.MET64) #24h hindcast
modelrun.postprocess()
