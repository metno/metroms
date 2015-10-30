from GlobalParams import *
from Constants import *
from Utils import *
import sys
from datetime import datetime, timedelta

class Params(object):
    RUNPATH=None
    KEYWORDFILE=None
    ROMSINFILE=None
    FELT_CLMFILE=None
    KEYWORDLIST=None
    XCPU=None
    YCPU=None
    TSTEPS=None
    NRREC=None
    
    def __init__(self,app,xcpu,ycpu,start_date,end_date,nrrec=-1,cicecpu=0,restart=False):
        self.KEYWORDFILE=GlobalParams.COMMONORIGPATH+"/roms_keyword.in"
        #self.ROMSINFILE=GlobalParams.RUNDIR+"/roms.in"
        self.XCPU=xcpu
        self.YCPU=ycpu
        self.CICECPU=cicecpu
        self.FCLEN=(end_date-start_date).total_seconds()
        self.NRREC=nrrec
        self.TIMEREF=datetime(1970,01,01,00)
        self.RESTART=restart
        if app=='a20':
            ########################################################################
            # Name of roms.in keyword-file:
            ########################################################################
            self.RUNPATH=GlobalParams.RUNDIR+"/arctic-20km"
            self.ROMSINFILE=self.RUNPATH+"/roms.in"
            self.FELT_CLMFILE=self.RUNPATH+"/FOAM.felt"
            self.DELTAT=1200 
#            self.DELTAT=600 
#            self.DELTAT=300 
            #self.ROMSINIFILE=self.RUNPATH+"/"+INIFILE
            ########################################################################
            # List of keywords:
            ########################################################################
            self.KEYWORDLIST=[
            ['APPTITLE',"ROMS 3.6 - Arctic-20km - Coupled ROMS-CICE"],
            ['MYAPPCPPNAME',"ARCTIC20KM"],
            ['VARFILE',GlobalParams.COMMONPATH+"/include/varinfo.dat"],
            ['XPOINTS',"320"],  #Could read from grd-file?
            ['YPOINTS',"240"],  #Could read from grd-file?
            ['NLEVELS',"35"],  #Could read from grd-file?
            ['XCPU',str(self.XCPU)],
            ['YCPU',str(self.YCPU)],
            ['TSTEPS',str(self.FCLEN)],
            ['DELTAT',str(self.DELTAT)],
            ['RATIO',"20"], #['RATIO',"30"],
            ['IRESTART',str(self.NRREC)],
            ['RSTSTEP',str(24*3600/int(self.DELTAT))],
            ['STASTEP',str(1*3600/int(self.DELTAT))],
            ['INFOSTEP',str(1*3600/int(self.DELTAT))],
            ['HISSTEPP',str(1*3600/int(self.DELTAT))],
            ['DEFHISSTEP',str(720*3600/int(self.DELTAT))],  #if 0; all output in one his-file
            ['AVGSTEPP',str(24*3600/int(self.DELTAT))],
            ['STARTAVG',"0"],
            ['DEFAVGSTEP',str(720*3600/int(self.DELTAT))],  #if 0; all output in one avg-file
            ['STARTTIME',str((start_date-self.TIMEREF).total_seconds()/86400)],
            ['TIDEREF',str((start_date-self.TIMEREF).total_seconds()/86400)],
            ['TIMEREF',self.TIMEREF.strftime("%Y%m%d.00")],
            ['OBCFAKTOR',"120.0"],
            ['GRDFILE',GlobalParams.COMMONPATH+"/grid/A20_grd_openBering.nc"],
            ['RUNDIR',self.RUNPATH],
            ['TIDEDIR',self.RUNPATH],
            ['ATMDIR',self.RUNPATH],
            ['RIVERFILE',GlobalParams.COMMONPATH+"/rivers/newA20_rivers_mitya.nc"],
            ['FORCEFILES',"4"],
            ['ROMS/External/coupling.dat', GlobalParams.COMMONPATH + "/../../../tmproms/roms_src/ROMS/External/coupling.dat"],
            ['ROMSINFILE', self.ROMSINFILE ],
            ['CICEINFILE', GlobalParams.COMMONPATH + "/../../../tmproms/cice/rundir/ice_in" ],
            ['NUMROMSCORES',str(int(self.XCPU)*int(self.YCPU))],
            ['NUMCICECORES',str(int(self.CICECPU))]
            ]
            ########################################################################
            ########################################################################
        elif app=='n4_2dss':
            ########################################################################
            # Name of roms.in keyword-file:
            ########################################################################
            self.RUNPATH=GlobalParams.RUNDIR+"/nordic-4km2d-stormsurge"
            self.ROMSINFILE=self.RUNPATH+"/roms.in"
            self.FELT_CLMFILE=None
            self.DELTAT=10
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
            ['TSTEPS',str(self.FCLEN)],
            ['DELTAT',str(self.DELTAT)],
            ['RATIO',"1"],
            ['IRESTART',str(self.IRESTART)],
            ['RSTSTEP',str(24*3600/int(self.DELTAT))],
            ['STASTEP',str(1*3600/int(self.DELTAT))],
            ['INFOSTEP',str(1*3600/int(self.DELTAT))],
            ['HISSTEPP',str(1*3600/int(self.DELTAT))],
            ['DEFHISSTEP',"0"],     #if 0; all output in one his-file
            ['AVGSTEPP',str(24*3600/int(self.DELTAT))],
            ['STARTAVG',"0"],
            ['DEFAVGSTEP',"0"],     #if 0; all output in one avg-file
            ['STARTTIME',str((start_date-self.TIMEREF).total_seconds()/86400)], #Must be read from restartfile
            ['TIDEREF',str((start_date-self.TIMEREF).total_seconds()/86400)], #Hardcoded, but not really used in 2D stormsurge
            ['TIMEREF',self.TIMEREF.strftime("%Y%m%d.00")],
            ['OBCFAKTOR',"1"],
            ['GRDFILE',GlobalParams.COMMONPATH+"/grid/nordic-4km_grd.nc"],
            ['RUNDIR',self.RUNPATH],
            ['TIDEDIR',self.RUNPATH],
            ['ATMDIR',self.RUNPATH],
            ['RIVERFILE',"."],
            ['FORCEFILES',"1"],
            ['ROMS/External/coupling.dat', GlobalParams.COMMONPATH + "/../../../tmproms/roms_src/ROMS/External/coupling.dat"],
            ['ROMSINFILE', self.ROMSINFILE ],
            ['CICEINFILE', GlobalParams.COMMONPATH + "/../../../tmproms/cice/rundir/ice_in" ],
            ['NUMROMSCORES',str(int(self.XCPU)*int(self.YCPU))],
            ['NUMCICECORES',str(int(self.CICECPU))]
            ]
            ########################################################################
            ########################################################################
        else:
            print 'Unknown application, will exit now'
            sys.exit()


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
