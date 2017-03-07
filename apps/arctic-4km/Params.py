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
        #self.KEYWORDFILE=GlobalParams.COMMONORIGPATH+"/roms_keyword.in_roms-trunk"
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
        if app=='arctic-4km':
            ########################################################################
            # Name of roms.in keyword-file:
            ########################################################################
            self.RUNPATH=GlobalParams.RUNDIR+"/arctic-4km"
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
            ['APPTITLE',"ROMS 3.6 - Arctic-4km - Coupled ROMS-CICE"],
            ['MYAPPCPPNAME',"ARCTIC4KM"],
            ['VARFILE',GlobalParams.COMMONPATH+"/include/varinfo.dat"],
            ['XPOINTS',"1600"],  #Could read from grd-file?
            ['YPOINTS',"1200"],  #Could read from grd-file?
            ['NLEVELS',"35"],  #Could read from grd-file?
            ['GRDTHETAS',"6.0d0"],
            ['GRDTHETAB',"0.1d0"],
            ['GRDTCLINE',"100.0d0"],            
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
            ['RSTSTEP',str(30*24*3600/int(self.DELTAT))],
            ['STASTEP',str(120*3600/int(self.DELTAT))],
            #['INFOSTEP',str(1)],
            ['INFOSTEP',str(1*3600/int(self.DELTAT))],
            #['HISSTEPP',str(1)],
            ['HISSTEPP',str(30*24*3600/int(self.DELTAT))],
            ['DEFHISSTEP',str(360*24*3600/int(self.DELTAT))],  #if 0; all output in one his-file
            ['AVGSTEPP',str(30*24*3600/int(self.DELTAT))],
            ['STARTAVG',"0"],
            ['DEFAVGSTEP',str(360*24*3600/int(self.DELTAT))],  #if 0; all output in one avg-file
            ['STARTTIME',str((start_date-self.TIMEREF).total_seconds()/86400)],
            ['TIDEREF',str((start_date-self.TIMEREF).total_seconds()/86400)],
            ['TIMEREF',self.TIMEREF.strftime("%Y%m%d.00")],
            ['V_TRANS',"2"],
            ['_TNUDG_',"15.0d0 15.0d0"],
            ['V_STRETCH',"2"],
            ['OBCFAKTOR',"1.0"],
            ['NUDGZONEWIDTH',"15"],
            ['GRDFILE',GlobalParams.COMMONPATH+"/grid/arctic4km_grd.nc"],
            ['RUNDIR',self.RUNPATH],
            ['TIDEDIR',self.RUNPATH],
            ['_CLMNAME_',self.RUNPATH+"/ocean_1993_clm.nc | \n"+self.RUNPATH+"/ocean_1994_clm.nc | \n"+self.RUNPATH+"/ocean_1995_clm.nc | \n"+self.RUNPATH+"/ocean_1996_clm.nc | \n"+self.RUNPATH+"/ocean_1997_clm.nc"],
            ['_BRYNAME_',self.RUNPATH+"/ocean_1993_bry.nc | \n"+self.RUNPATH+"/ocean_1994_bry.nc | \n"+self.RUNPATH+"/ocean_1995_bry.nc | \n"+self.RUNPATH+"/ocean_1996_bry.nc | \n"+self.RUNPATH+"/ocean_1997_bry.nc"],
            #['ATMDIR',self.RUNPATH+"/AN_1997_unlim.nc \ "+self.RUNPATH+"/FC_1997_unlim.nc"],
            ['ATMDIR',self.RUNPATH+"/AN_1993_unlim.nc | \n"+self.RUNPATH+"/AN_1994_unlim.nc | \n"+self.RUNPATH+"/AN_1995_unlim.nc | \n" +self.RUNPATH+"/AN_1996_unlim.nc | \n" +self.RUNPATH+"/AN_1997_unlim.nc \ \n"+self.RUNPATH+"/FC_1993_unlim.nc | \n"+self.RUNPATH+"/FC_1994_unlim.nc | \n"+self.RUNPATH+"/FC_1995_unlim.nc | \n"+self.RUNPATH+"/FC_1996_unlim.nc | \n"+self.RUNPATH+"/FC_1997_unlim.nc"],
            ['RIVERFILE',GlobalParams.COMMONPATH+"/rivers/newA4_rivers_mitya.nc"],
            ['FORCEFILES',"3"], # The files should be specified here as well
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
