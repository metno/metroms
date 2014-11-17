#!/usr/bin/python
import numpy as np
import os
import Constants

from ModelRun import *
from GlobalParams import *
from Params import *
########################################################################
xcpu=2
ycpu=2
fclen=120 #Forecast length in hours
app='n4_2dss'

n4_2d_surge_params=Params(app,xcpu,ycpu,fclen,irestart=-1)
#n4_2d_surge_params.ROMSINFILE=n4_2d_surge_params.RUNPATH+"/roms.in"
#n4_2d_surge_params.set_run_params(xcpu,ycpu,tsteps=720,irestart=-1)
#n4_2d_surge_params.change_run_param('RSTSTEP',72)

    

modelrun=ModelRun(n4_2d_surge_params,Constants.FELT,Constants.FELT)

modelrun.preprocess()
modelrun.run_roms(Constants.DRY,Constants.NODEBUG,Constants.MET64) #24h hindcast
modelrun.postprocess()
