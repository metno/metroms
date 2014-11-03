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
    
    def __init__(self,runpath,xcpu,ycpu,tsteps,deltat,irestart,cicecpu=0):
        ########################################################################
        # Name of roms.in keyword-file:
        ########################################################################
        self.RUNPATH=runpath
        self.KEYWORDFILE=GlobalParams.COMMONORIGPATH+"/roms_keyword.in"
        self.ROMSINFILE=self.RUNPATH+"/roms.in"
        self.FELT_CLMFILE=None
        self.XCPU=xcpu
        self.YCPU=ycpu
        self.CICECPU=cicecpu
        self.TSTEPS=tsteps
        self.DELTAT=deltat
        self.IRESTART=irestart
        #self.ROMSINIFILE=self.RUNPATH+"/"+INIFILE
        ########################################################################
        # List of keywords:
        ########################################################################
        self.KEYWORDLIST=[
        ['APPTITLE',"ROMS 3.6 - Nordic-4km 2D stormsurge"],
        ['MYAPPCPPNAME',"NORDIC42DSURGE"],
        ['VARFILE',GlobalParams.COMMONPATH+"/include/varinfo.dat"],
        ['XPOINTS',"1022"],  #Could read from grd-file?
        ['YPOINTS',"578"],  #Could read from grd-file?
        ['NLEVELS',"35"],  #Could read from grd-file?
        ['XCPU',str(self.XCPU)],
        ['YCPU',str(self.YCPU)],
        ['TSTEPS',str(self.TSTEPS)],
        ['DELTAT',str(self.DELTAT)],
        ['RATIO',"1"],
        ['IRESTART',str(self.IRESTART)],
        ['RSTSTEP',str(24*int(self.DELTAT))],
        ['STASTEP',str(1*int(self.DELTAT))],
        ['INFOSTEP',str(1*int(self.DELTAT))],
        ['HISSTEPP',str(1*int(self.DELTAT))],
        ['DEFHISSTEP',"0"],     #if 0; all output in one his-file
        ['AVGSTEPP',str(24*int(self.DELTAT))],
        ['STARTAVG',"0"],
        ['DEFAVGSTEP',"0"],     #if 0; all output in one avg-file
        ['STARTTIME',"9877.5"], #Must be read from restartfile
        ['TIDEREF',"9877.5"], #Hardcoded, but not really used in 2D stormsurge
        ['TIMEREF',"19700101.00"],
        ['OBCFAKTOR',"1"],
        ['GRDFILE',GlobalParams.COMMONPATH+"/grid/nordic-4km_grd.nc"],
        ['RUNDIR',self.RUNPATH],
        ['TIDEDIR',self.RUNPATH],
        ['ATMDIR',self.RUNPATH],
        ['RIVERFILE',None],
        ['FORCEFILES',"1"],
        ['ROMS/External/coupling.dat', GlobalParams.COMMONPATH + "/../../../tmproms/roms_src/ROMS/External/coupling.dat"],
        ['ROMSINFILE', self.ROMSINFILE ],
        ['CICEINFILE', GlobalParams.COMMONPATH + "/../../../tmproms/cice/rundir/ice_in" ],
        ['NUMROMSCORES',str(int(self.XCPU)*int(self.YCPU))],
        ['NUMCICECORES',str(int(self.CICECPU))]
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
