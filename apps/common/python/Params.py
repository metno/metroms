from GlobalParams import *
from Constants import *
from Utils import *

class Params(object):
    RUNPATH=None
    KEYWORDFILE=None
    ROMSINFILE=None
    FELT_CLMFILE=None
    KEYWORDLIST=None
    XCPU=None
    YCPU=None
    TSTEPS=None
    IRESTART=None
    
    def __init__(self,runpath,xcpu,ycpu,tsteps,irestart):
        ########################################################################
        # Name of roms.in keyword-file:
        ########################################################################
        self.RUNPATH=runpath
        self.KEYWORDFILE=GlobalParams.COMMONORIGPATH+"/roms_keyword.in"
        self.ROMSINFILE=self.RUNPATH+"/roms.in"
        self.FELT_CLMFILE=self.RUNPATH+"/FOAM.felt"
        self.XCPU=xcpu
        self.YCPU=ycpu
        self.TSTEPS=tsteps
        self.IRESTART=irestart
        #self.ROMSINIFILE=self.RUNPATH+"/"+INIFILE
        ########################################################################
        # List of keywords:
        ########################################################################
        self.KEYWORDLIST=[
        ['APPTITLE',"ROMS 3.5 - Arctic-20km - Operational Run"],
        ['MYAPPCPPNAME',"ARCTIC20KM"],
        ['VARFILE',GlobalParams.COMMONPATH+"/include/varinfo.dat"],
        ['XPOINTS',"320"],  #Could read from grd-file?
        ['YPOINTS',"240"],  #Could read from grd-file?
        ['NLEVELS',"35"],  #Could read from grd-file?
        ['XCPU',str(self.XCPU)],
        ['YCPU',str(self.YCPU)],
        ['TSTEPS',str(self.TSTEPS)],
        ['DELTAT',"300"],
        ['RATIO',"30"],
        ['IRESTART',str(self.IRESTART)],
        ['RSTSTEP',"8640"],
        ['STASTEP',"12"],
        ['INFOSTEP',"12"],
        ['HISSTEPP',"12"],
        ['DEFHISSTEP',"0"],     #if 0; all output in one his-file
        ['AVGSTEPP',"72"],
        ['STARTAVG',"0"],
        ['DEFAVGSTEP',"0"],     #if 0; all output in one avg-file
        ['STARTTIME',"8506.0"],
        ['TIDEREF',"8506.0"],
        ['TIMEREF',"19700101.00"],
        ['OBCFAKTOR',"120.0"],
        ['GRDFILE',GlobalParams.COMMONPATH+"/grid/A20_grd_openBering.nc"],
        ['RUNDIR',self.RUNPATH],
        ['TIDEDIR',self.RUNPATH],
        ['ATMDIR',self.RUNPATH],
        ['RIVERFILE',GlobalParams.COMMONPATH+"/rivers/A20_rivers_openBering35.nc"],
        ['FORCEFILES',"4"],
        ['ROMS/External/coupling.dat', GlobalParams.COMMONPATH + "/../../../tmproms/roms_src/ROMS/External/coupling.dat"]
        ['ocean.in', self.ROMSINFILE ]
        ['cice.in', GlobalParams.COMMONPATH + "/../../../tmproms/cice/rundir/ice_in" ]
        ]
        ########################################################################
        ########################################################################

#     def set_run_params(self,xcpu,ycpu,tsteps,irestart):
#         for keyvalue in self.KEYWORDLIST:
#             if keyvalue[0]=='TSTEPS':
#                 keyvalue[1]=str(tsteps)
#             if keyvalue[0]=='IRESTART':
#                 keyvalue[1]=str(irestart)
#             if keyvalue[0]=='STARTTIME':
#                 keyvalue[1]=read_time_from_nc(INIFILE,irestart)
#             if keyvalue[0]=='XCPU':
#                 keyvalue[1]=str(xcpu)
#             if keyvalue[0]=='YCPU':
#                 keyvalue[1]=str(ycpu)

    def change_run_param(self,keyword,value):
        for keyvalue in self.KEYWORDLIST:
            if keyvalue[0]==keyword:
                keyvalue[1]=str(value)
