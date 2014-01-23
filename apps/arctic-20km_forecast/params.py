from GlobalParams import *
from Constants import *
from Utils import *

class Params(object):
    RUNPATH=None
    KEYWORDFILE=None
    ROMSINFILE=None
    FELT_CLMFILE=None
    KEYWORDLIST=None
    
    def __init__(self,runpath):
        ########################################################################
        # Name of roms.in keyword-file:
        ########################################################################
        self.RUNPATH=runpath
        self.KEYWORDFILE=COMMONORIGPATH+"/roms_keyword.in"
        self.ROMSINFILE=self.RUNPATH+"/roms.in"
        self.FELT_CLMFILE=self.RUNPATH+"/FOAM.felt"
        #self.ROMSINIFILE=self.RUNPATH+"/"+INIFILE
        ########################################################################
        # List of keywords:
        ########################################################################
        self.KEYWORDLIST=[
        ['APPTITLE',"ROMS 3.5 - Arctic-20km - Operational Run"],
        ['MYAPPCPPNAME',"ARCTIC20KM"],
        ['VARFILE',COMMONPATH+"/include/varinfo.dat"],
        ['XPOINTS',"320"],
        ['YPOINTS',"240"],
        ['NLEVELS',"35"],
        ['XCPU',"2"],
        ['YCPU',"2"],
        ['TSTEPS',"72"],
        ['DELTAT',"300"],
        ['RATIO',"30"],
        ['IRESTART',None],
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
        ['GRDFILE',COMMONPATH+"/grid/A20_grd_openBering.nc"],
        ['RUNDIR',self.RUNPATH],
        ['TIDEDIR',self.RUNPATH],
        ['ATMDIR',self.RUNPATH],
        ['RIVERFILE',COMMONPATH+"/rivers/A20_rivers_openBering35.nc"],
        ['FORCEFILES',"4"]
        ]
        ########################################################################
        ########################################################################

    def set_run_params(self,tsteps,irestart):
        for keyvalue in self.KEYWORDLIST:
            if keyvalue[0]=='TSTEPS':
                keyvalue[1]=str(tsteps)
            if keyvalue[0]=='IRESTART':
                keyvalue[1]=str(irestart)
            if keyvalue[0]=='STARTTIME':
                keyvalue[1]=read_time_from_nc(INIFILE,irestart)
