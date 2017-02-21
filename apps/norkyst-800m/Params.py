from GlobalParams import *
from Constants import *
from Utils import *
import sys
from datetime import datetime, timedelta
import netCDF4

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
#        self.KEYWORDFILE=GlobalParams.COMMONORIGPATH+"/roms_keyword.in_roms-trunk"
        self.KEYWORDFILE=GlobalParams.METROMSDIR+"/apps/"+app+"/origfiles/roms_keyword.in_roms-trunk"
        self.XCPU=xcpu
        self.YCPU=ycpu
        self.CICECPU=cicecpu
        self.START_DATE=start_date
        self.END_DATE=end_date
        self.FCLEN=(self.END_DATE-self.START_DATE).total_seconds()
        self.NRREC=nrrec
        self.TIMEREF=datetime(1970,01,01,00)
        self.RESTART=restart
        if app=='norkyst-800m':
            ########################################################################
            # Name of roms.in keyword-file:
            ########################################################################
            self.RUNPATH=GlobalParams.RUNDIR+"/"+app
            self.ATMFILE=self.RUNPATH+"/stripe/ocean_force.nc"
            self.ROMSINFILE=self.RUNPATH+"/roms.in"
            self.CICERUNDIR=self.RUNPATH+'/cice/rundir'
            self.CICEINFILE=self.RUNPATH + "/ice_in"
            self.CICEKEYWORDFILE=self.CICERUNDIR + "/ice_in"
            self.DELTAT=60 
            self.CICEDELTAT=600
            self.COUPLINGTIME_I2O=600
            # Find restart-time of CICE:
            if self.CICECPU > 0:
                cice_start_step = (start_date-datetime(start_date.year,01,01,00)).total_seconds()/self.CICEDELTAT
                if restart == True:
                    f = open(self.CICERUNDIR+'/restart/ice.restart_file', 'r')
                    cice_restartfile = f.readline().strip()
                    cice_rst_time = netCDF4.Dataset(cice_restartfile).istep1
                    cicerst_truefalse = ".true."
                else:
                    cice_rst_time = cice_start_step
                    cicerst_truefalse = ".false."
            ########################################################################
            # List of keywords:
            ########################################################################
            self.KEYWORDLIST=[
            ['APPTITLE',"NorKyst-800m - ROMS"],
            ['MYAPPCPPNAME',"NORKYST800M"],
            #['VARFILE',"/prod/forecast/sea/ROMS/trunk/Apps/Common/Include/varinfo.dat_NCatm"],
            ['VARFILE',GlobalParams.COMMONPATH+"/include/varinfo.dat_NCatm"],
            ['XPOINTS',"2600"],  #Could read from grd-file?
            ['YPOINTS',"900"],  #Could read from grd-file?
            ['NLEVELS',"35"],  #Could read from grd-file?
            ['GRDTHETAS',"8.0d0"],
            ['GRDTHETAB',"0.1d0"],
            ['GRDTCLINE',"20.0d0"],            
            ['_TNU2_',"2*1.0d2"],
            ['_TNU4_',"1.6d8"],
            ['_VISC2_',"1.0d2"],
            ['_VISC4_',"1.6d8"],
            ['XCPU',str(self.XCPU)],
            ['YCPU',str(self.YCPU)],
            ['TSTEPS',str(self.FCLEN/self.DELTAT)],
            ['DELTAT',str(self.DELTAT)],
            ['RATIO',str(self.DELTAT)], #['RATIO',"30"],
            ['IRESTART',str(self.NRREC)],
            ['RSTSTEP',str(1*24*3600/int(self.DELTAT))],
            ['STASTEP',str(1*3600/int(self.DELTAT))],
            ['INFOSTEP',str(1*3600/int(self.DELTAT))],
            ['HISSTEPP',str(1*3600/int(self.DELTAT))],
            #['HISSTEPP',str(60/int(self.DELTAT))],
            ['DEFHISSTEP','0'],  #if 0; all output in one his-file
            ['AVGSTEPP',str(1*24*3600/int(self.DELTAT))],
            ['STARTAVG',"0"],
            ['DEFAVGSTEP','0'],  #if 0; all output in one avg-file
            ['STARTTIME',str((self.START_DATE-self.TIMEREF).total_seconds()/86400)],
            ['TIDEREF',str((datetime(2017,1,1)-self.TIMEREF).total_seconds()/86400)],
            ['TIMEREF',self.TIMEREF.strftime("%Y%m%d.00")],
            ['V_TRANS',"2"],
            ['V_STRETCH',"2"],
            ['_TNUDG_',"15.0d0 15.0d0"],
            ['OBCFAKTOR',"1.0"],
            ['NUDGZONEWIDTH',"15"],
            ['GRDFILE',GlobalParams.METROMSDIR+"/apps/"+app+"/grid/NK800_grd.nc"],
            ['RUNDIR',self.RUNPATH],
            ['_CLMNAME_',self.RUNPATH+"/stripe/norkyst_clm.nc"],
            ['_BRYNAME_',self.RUNPATH+'/stripe/norkyst_bry2d.nc \\ \n'+self.RUNPATH+'/stripe/norkyst_bry.nc'],
            ['_NUDNAME_',self.RUNPATH+'/ocean_nud.nc'],
            ['TIDEDIR',self.RUNPATH],
            ['ATMDIR',self.ATMFILE],
            ['RIVERFILE',GlobalParams.METROMSDIR+"/apps/"+app+"/rivers/norkyst_800m_river.nc"],
            ['FORCEFILES',"2"], 
            ['COUPLINGTIMEI2O',str(self.COUPLINGTIME_I2O)],
            ['ROMSINFILE', self.ROMSINFILE ],
            ['CICEINFILE', self.CICEINFILE ],
            ['NUMROMSCORES',str(int(self.XCPU)*int(self.YCPU))],
            ['NUMCICECORES',str(int(self.CICECPU))]
            ]
            ########################################################################
            # List of CICE keywords:
            ########################################################################
            if self.CICECPU > 0:
                self.CICEKEYWORDLIST=[
                    ['CICEYEARSTART',start_date.strftime("%Y")],
                    ['CICESTARTSTEP',str(int(cice_start_step))],  #number of hours after 00:00 Jan 1st
                    ['CICEDELTAT',str(self.CICEDELTAT)],
                    ['CICENPT',str(int((self.FCLEN/self.CICEDELTAT)-(cice_rst_time - cice_start_step)))],   # minus diff restart og start_date
                    ['CICERUNTYPE',"'continue'"],
                    ['CICEIC',"'default'"],
                    ['CICEREST',".true."],
                    ['CICERSTTIME',cicerst_truefalse],
                    ]
            ########################################################################
        else:
            print 'Unknown application, will exit now'
            sys.exit()


    def change_run_param(self,keyword,value):
        for keyvalue in self.KEYWORDLIST:
            if keyvalue[0]==keyword:
                keyvalue[1]=str(value)
