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
import time
time.sleep(20)
########################################################################
# Set cpus for ROMS:
xcpu=6
ycpu=6
# Set cpus for CICE:
icecpu=12
# Choose a predefined ROMS-application:
app='arctic-4km' # Arctic-4km

start_date = datetime(1997,01,16,12)
end_date   = datetime(1997,01,17,12)


a4params=Params(app,xcpu,ycpu,start_date,end_date,nrrec=-1,cicecpu=icecpu,restart=False)

modelrun=ModelRun(a4params)

print GlobalParams.RUNDIR
print GlobalParams.COMMONPATH

modelrun.preprocess()
modelrun.run_roms(Constants.MPI,Constants.DEBUG,Constants.VILJE) #24h hindcast
#modelrun.run_roms(Constants.DRY,Constants.NODEBUG,Constants.MET64) #24h hindcast
modelrun.postprocess()

