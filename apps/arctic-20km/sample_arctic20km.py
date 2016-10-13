########################################################################
# Python-modules:
########################################################################
import numpy as np
import os
import Constants
from datetime import datetime, timedelta
########################################################################
# METROMS-modules:
########################################################################
from GlobalParams import *
from Params import *
from ModelRun import *
########################################################################
########################################################################
# Set cpus for ROMS:
xcpu=3
ycpu=8
# Set cpus for CICE:
icecpu=8
# Choose a predefined ROMS-application:
app='arctic-20km' # Arctic-20km

start_date = datetime(1993,01,17,00)
end_date   = datetime(1997,01,01,00)


a20params=Params(app,xcpu,ycpu,start_date,end_date,nrrec=-1,cicecpu=icecpu,restart=False)

modelrun=ModelRun(a20params)

print GlobalParams.RUNDIR
print GlobalParams.COMMONPATH

modelrun.preprocess()
modelrun.run_roms(Constants.MPI,Constants.NODEBUG,Constants.MET64) #24h hindcast
#modelrun.run_roms(Constants.DRY,Constants.NODEBUG,Constants.MET64) #24h hindcast
modelrun.postprocess()

