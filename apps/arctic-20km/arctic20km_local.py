#!/usr/bin/python
import numpy as np
import os
import Constants
from datetime import datetime, timedelta

from GlobalParams import *
from Params import *
from ModelRun import *
########################################################################
########################################################################
# Set cpus for ROMS:
xcpu=3
ycpu=2
# Set cpus for CICE:
icecpu=2
# Choose a predefined ROMS-application:
app='a20' # Arctic-20km

start_date = datetime(1997,01,16,12)
end_date   = datetime(1997,02,01,12)


#NMK: If restart, set irestart=
a20params=Params(app,xcpu,ycpu,start_date,end_date,nrrec=0,cicecpu=icecpu,restart=True)
#a20params=Params(app,xcpu,ycpu,fclen=(744*12),irestart=0,cicecpu=icecpu)
#a20params.RUNPATH="/disk1/tmp"
#a20params.ROMSINFILE=a20params.RUNPATH+"/roms___.in"

modelrun=ModelRun(a20params)

print GlobalParams.RUNDIR
print GlobalParams.COMMONPATH

modelrun.preprocess()
#modelrun.run_roms(Constants.MPI,Constants.NODEBUG,Constants.MET64) #24h hindcast
modelrun.run_roms(Constants.DRY,Constants.NODEBUG,Constants.MET64) #24h hindcast
modelrun.postprocess()

