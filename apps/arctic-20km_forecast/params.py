import numpy as np
from GlobalParams import *
########################################################################
# Name of roms.in keyword-file:
########################################################################
keywordfile="roms_keyword.in"
keywordpath="/disk1/METROMS/apps/common/origfiles"
########################################################################
# List of keywords:
########################################################################
apptitle=['APPTITLE',"ROMS 3.5 - Arctic-20km - Operational Run"]
myappcpp=['MYAPPCPPNAME',"ARCTIC20KM"]
varfile=['VARFILE',"/disk1/METROMS/apps/common/include/varinfo.dat"]
xpoints=['XPOINTS',"320"]
ypoints=['YPOINTS',"240"]
nlevels=['NLEVELS',"35"]
xcpu=['XCPU',"2"]
ycpu=['YCPU',"2"]
tsteps=['TSTEPS',"72"]
dt=['DELTAT',"300"]
ndtfast=['RATIO',"30"]
nrrec=['IRESTART',"-1"]
nrst=['RSTSTEP',"8640"]
nsta=['STASTEP',"12"]
ninfo=['INFOSTEP',"12"]
nhis=['HISSTEPP',"12"]
ndefhis=['DEFHISSTEP',"0"]     #if 0; all output in one his-file
navg=['AVGSTEPP',"72"]
ntsavg=['STARTAVG',"0"]
ndefavg=['DEFAVGSTEP',"0"]     #if 0; all output in one avg-file
dstart=['STARTTIME',"8506.0"]
tide_start=['TIDEREF',"8506.0"]
time_ref=['TIMEREF',"19700101.00"]
obcfac=['OBCFAKTOR',"120.0"]
grdfile=['GRDFILE',"/disk1/METROMS/apps/common/grid/A20_grd_openBering.nc"]
rundir=['RUNDIR',"/disk1/tmproms/run/arctic-20km"]
tidedir=['TIDEDIR',rundir[1]]
atmdir=['ATMDIR',rundir[1]]
riverfile=['RIVERFILE',"/disk1/METROMS/apps/common/rivers/A20_rivers_openBering35.nc"]
nffiles=['FORCEFILES',"4"]
########################################################################
romsinfile=rundir[1]+"/roms.in"
########################################################################
keywordlist=np.array([apptitle,myappcpp,varfile,xpoints,ypoints,nlevels,
                      xcpu,ycpu,tsteps,dt,ndtfast,nrrec,nrst,nsta,ninfo,
                      nhis,ndefhis,navg,ntsavg,ndefavg,dstart,tide_start,
                      time_ref,obcfac,grdfile,rundir,tidedir,atmdir,
                      riverfile,nffiles])
########################################################################
