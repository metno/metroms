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
        self.KEYWORDFILE=GlobalParams.COMMONORIGPATH+"/roms_keyword.in_roms-trunk"
        self.XCPU=xcpu
        self.YCPU=ycpu
        self.CICECPU=cicecpu
        self.FCLEN=(end_date-start_date).total_seconds()
        self.NRREC=nrrec
        self.TIMEREF=datetime(1970,01,01,00)
        self.RESTART=restart
        if app=='arctic-20km':
            ########################################################################
            # Name of roms.in keyword-file:
            ########################################################################
            self.RUNPATH=GlobalParams.RUNDIR+"/arctic-20km"
            self.ROMSINFILE=self.RUNPATH+"/roms.in"
            #self.CICEKEYWORDFILE=self.RUNPATH + "/ice_in_keyword"
            #self.CICEINFILE=GlobalParams.CICERUNDIR + "/ice_in"
            self.CICERUNDIR=self.RUNPATH+'/cice/rundir'
            self.CICEINFILE=self.RUNPATH + "/ice_in"
            self.CICEKEYWORDFILE=self.CICERUNDIR + "/ice_in"
            self.FELT_CLMFILE=self.RUNPATH+"/FOAM.felt"
            self.DELTAT=1200 
            self.CICEDELTAT=3600.0
            self.COUPLINGTIME_I2O=3600.0
            #self.ROMSINIFILE=self.RUNPATH+"/"+INIFILE
            # Find restart-time of CICE:
            cice_start_step = (start_date-datetime(start_date.year,01,01,00)).total_seconds()/self.CICEDELTAT
            if restart == True:
                f = open(self.CICERUNDIR+'/restart/ice.restart_file', 'r')
                cice_restartfile = f.readline().strip()
                cice_rst_time = netCDF4.Dataset(cice_restartfile).istep1
                #cice_rst_day = netCDF4.Dataset(cice_restartfile).mday
                cicerst_truefalse = ".true."
            else:
                cice_rst_time = cice_start_step
                #cice_rst_day = start_date.day
                cicerst_truefalse = ".false."
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
            ['RATIO',"20"], #['RATIO',"30"],
            ['IRESTART',str(self.NRREC)],
            ['RSTSTEP',str(30*24*3600/int(self.DELTAT))],
            ['STASTEP',str(24*3600/int(self.DELTAT))],
            ['INFOSTEP', str(1*3600/int(self.DELTAT))],
            ['HISSTEPP', str(1*3600/int(self.DELTAT))],
            ['DEFHISSTEP',str(30*24*3600/int(self.DELTAT))],  #if 0; all output in one his-file
            ['AVGSTEPP',str(1*24*3600/int(self.DELTAT))],
            ['STARTAVG',"1"],
            ['DEFAVGSTEP',str(30*24*3600/int(self.DELTAT))],  #if 0; all output in one avg-file
            ['STARTTIME',str((start_date-self.TIMEREF).total_seconds()/86400)],
            ['TIDEREF',str((start_date-self.TIMEREF).total_seconds()/86400)],
            ['TIMEREF',self.TIMEREF.strftime("%Y%m%d.00")],
            ['V_TRANS',"2"],
            ['V_STRETCH',"2"],
            ['_TNUDG_',"15.0d0 15.0d0"],
            ['OBCFAKTOR',"1.0"],
            ['NUDGZONEWIDTH',"10"],
            ['GRDFILE',GlobalParams.COMMONPATH+"/grid/A20_grd_openBering.nc"],
            ['RUNDIR',self.RUNPATH],
            ['_CLMNAME_',self.RUNPATH+"/ocean_1993_clm.nc | \n"+self.RUNPATH+"/ocean_1994_clm.nc | \n"+self.RUNPATH+"/ocean_1995_clm.nc | \n"+self.RUNPATH+"/ocean_1996_clm.nc | \n"+self.RUNPATH+"/ocean_1997_clm.nc"],
            ['_BRYNAME_',self.RUNPATH+"/ocean_1993_bry.nc | \n"+self.RUNPATH+"/ocean_1994_bry.nc | \n"+self.RUNPATH+"/ocean_1995_bry.nc | \n"+self.RUNPATH+"/ocean_1996_bry.nc | \n"+self.RUNPATH+"/ocean_1997_bry.nc"],
            ['TIDEDIR',self.RUNPATH],
            ['ATMDIR',self.RUNPATH+"/AN_1993_unlim.nc | \n"+self.RUNPATH+"/AN_1994_unlim.nc | \n"+self.RUNPATH+"/AN_1995_unlim.nc | \n" +self.RUNPATH+"/AN_1996_unlim.nc | \n" +self.RUNPATH+"/AN_1997_unlim.nc \ \n"+self.RUNPATH+"/FC_1993_unlim.nc | \n"+self.RUNPATH+"/FC_1994_unlim.nc | \n"+self.RUNPATH+"/FC_1995_unlim.nc | \n"+self.RUNPATH+"/FC_1996_unlim.nc | \n"+self.RUNPATH+"/FC_1997_unlim.nc"],
            ['RIVERFILE',GlobalParams.COMMONPATH+"/rivers/newA20_rivers_mitya.nc"],
            ['FORCEFILES',"3"], # The files should be specified here as well
            #['ROMS/External/coupling.dat', self.RUNPATH + "/coupling.dat"],
            ['COUPLINGTIMEI2O',str(self.COUPLINGTIME_I2O)],
            ['ROMSINFILE', self.ROMSINFILE ],
            ['CICEINFILE', self.CICEINFILE ],
            ['NUMROMSCORES',str(int(self.XCPU)*int(self.YCPU))],
            ['NUMCICECORES',str(int(self.CICECPU))]
            ]
            ########################################################################
            # List of CICE keywords:
            ########################################################################
            #if (cice_rst_day == start_date.day):
            # if (restart == True):
            #     cicerst_truefalse = ".true."
            # else:
            #     cicerst_truefalse = ".false."
            self.CICEKEYWORDLIST=[
            ['CICEYEARSTART',start_date.strftime("%Y")],
            ['CICESTARTSTEP',str(int(cice_start_step))],  #number of hours after 00:00 Jan 1st
            ['CICEDELTAT',str(self.CICEDELTAT)],
            ['CICENPT',str(int((self.FCLEN/self.CICEDELTAT)-(cice_rst_time - cice_start_step)))],   # minus diff restart og start_date
            ['CICERUNTYPE',"'continue'"],
            ['CICEIC',"'default'"],
            ['CICEREST',".true."],
            ['CICERSTTIME',cicerst_truefalse],
            #['<cicedir>',GlobalParams.COMMONPATH + "/../../../tmproms/cice"]
            ]
            ########################################################################
            ########################################################################
        elif app=='arctic-4km':
            ########################################################################
            # Name of roms.in keyword-file:
            ########################################################################
            self.RUNPATH=GlobalParams.RUNDIR+"/arctic-4km"
            self.ROMSINFILE=self.RUNPATH+"/roms.in"
            self.CICERUNDIR=self.RUNPATH+'/cice/rundir'
            self.CICEINFILE=self.RUNPATH + "/ice_in"
            self.CICEKEYWORDFILE=self.CICERUNDIR + "/ice_in"
            self.FELT_CLMFILE=self.RUNPATH+"/FOAM.felt"
            self.DELTAT=60 
            self.CICEDELTAT=600
            self.COUPLINGTIME_I2O=600
            # Find restart-time of CICE:
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
            ['RSTSTEP',str(7*24*3600/int(self.DELTAT))],
            ['STASTEP',str(120*3600/int(self.DELTAT))],
            #['INFOSTEP',str(1)],
            ['INFOSTEP',str(1*3600/int(self.DELTAT))],
            #['HISSTEPP',str(1)],
            ['HISSTEPP',str(1*3600/int(self.DELTAT))],
            ['DEFHISSTEP',str(720*3600/int(self.DELTAT))],  #if 0; all output in one his-file
            ['AVGSTEPP',str(24*3600/int(self.DELTAT))],
            ['STARTAVG',"0"],
            ['DEFAVGSTEP',str(720*3600/int(self.DELTAT))],  #if 0; all output in one avg-file
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
            ['ATMDIR',self.RUNPATH+"/AN_1997_unlim.nc \ "+self.RUNPATH+"/FC_1997_unlim.nc"],
            ['RIVERFILE',GlobalParams.COMMONPATH+"/rivers/newA4_rivers_mitya.nc"],
            ['FORCEFILES',"4"], # The files should be specified here as well
            ['COUPLINGTIMEI2O',str(self.COUPLINGTIME_I2O)],
            ['ROMSINFILE', self.ROMSINFILE ],
            ['CICEINFILE', self.CICEINFILE ],
            ['NUMROMSCORES',str(int(self.XCPU)*int(self.YCPU))],
            ['NUMCICECORES',str(int(self.CICECPU))]
            ]
            ########################################################################
            # List of CICE keywords:
            ########################################################################
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
            ########################################################################
        elif app=='n4_2dss':
            ########################################################################
            # Name of roms.in keyword-file:
            ########################################################################
            self.RUNPATH=GlobalParams.RUNDIR+"/nordic-4km2d-stormsurge"
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
            ['_TNUDG_',"15.0d0 15.0d0"],
            ['OBCFAKTOR',"1"],
            ['NUDGZONEWIDTH',"15"],
            ['GRDFILE',GlobalParams.COMMONPATH+"/grid/nordic-4km_grd.nc"],
            ['RUNDIR',self.RUNPATH],
            ['TIDEDIR',self.RUNPATH],
            ['ATMDIR',self.RUNPATH+"/AN_1997_unlim.nc \ "+self.RUNPATH+"/FC_1997_unlim.nc"],
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
        elif app=='arctic-20km_nordic':
            ########################################################################
            # Name of roms.in keyword-file:
            ########################################################################
            self.RUNPATH=GlobalParams.RUNDIR+"/arctic-20km_nordic"
            self.ROMSINFILE=self.RUNPATH+"/roms.in"
            #self.CICEKEYWORDFILE=self.RUNPATH + "/ice_in_keyword"
            #self.CICEINFILE=GlobalParams.CICERUNDIR + "/ice_in"
            self.CICERUNDIR=self.RUNPATH+'/cice/rundir'
            self.CICEINFILE=self.RUNPATH + "/ice_in"
            self.CICEKEYWORDFILE=self.CICERUNDIR + "/ice_in"
            self.DELTAT=1200 
            self.CICEDELTAT=3600.0
            self.COUPLINGTIME_I2O=3600.0
            #self.ROMSINIFILE=self.RUNPATH+"/"+INIFILE
            # Find restart-time of CICE:
            cice_start_step = (start_date-datetime(start_date.year,01,01)).total_seconds()/self.CICEDELTAT
            if restart == True:
                f = open(self.CICERUNDIR+'/restart/ice.restart_file', 'r')
                cice_restartfile = f.readline().strip()
                cice_rst_time = netCDF4.Dataset(cice_restartfile).istep1
                #cice_rst_day = netCDF4.Dataset(cice_restartfile).mday
                cicerst_truefalse = ".true."
            else:
                cice_rst_time = cice_start_step
                #cice_rst_day = start_date.day
                cicerst_truefalse = ".false."
            ########################################################################
            # List of keywords:
            ########################################################################
            self.KEYWORDLIST=[
            ['APPTITLE',"ROMS 3.6 - Arctic-20km - Coupled ROMS-CICE"],
            ['MYAPPCPPNAME',"ARCTIC20KM"],
            ['VARFILE',GlobalParams.COMMONPATH+"/include/varinfo.dat"],
            ['XPOINTS',"171"],  #Could read from grd-file?
            ['YPOINTS',"141"],  #Could read from grd-file?
            ['NLEVELS',"35"],  #Could read from grd-file?
            ['GRDTHETAS',"6.0d0"],
            ['GRDTHETAB',"0.1d0"],
            ['GRDTCLINE',"100.0d0"],            
            ['_TNU2_',"2*1.0d3"],
            ['_TNU4_',"1.6d8"],
            ['_VISC2_',"1.0d2"],
            ['_VISC4_',"1.6d8"],
            ['XCPU',str(self.XCPU)],
            ['YCPU',str(self.YCPU)],
            ['TSTEPS',str(self.FCLEN/self.DELTAT)],
            ['DELTAT',str(self.DELTAT)],
            ['RATIO',"20"], #['RATIO',"30"],
            ['IRESTART',str(self.NRREC)],
            ['RSTSTEP',str(30*24*3600/int(self.DELTAT))],
            ['STASTEP',str(1*3600/int(self.DELTAT))],
            ['INFOSTEP',str(1*3600/int(self.DELTAT))],
            ['HISSTEPP',str(30*24*3600/int(self.DELTAT))],
            ['DEFHISSTEP',str(360*24*3600/int(self.DELTAT))],  #if 0; all output in one his-file
            ['AVGSTEPP',str(30*24*3600/int(self.DELTAT))],
            ['STARTAVG',"0"],
            ['DEFAVGSTEP',str(360*24*3600/int(self.DELTAT))],  #if 0; all output in one avg-file
            ['STARTTIME',str((start_date-self.TIMEREF).total_seconds()/86400)],
            ['TIDEREF',str((start_date-self.TIMEREF).total_seconds()/86400)],
            ['TIMEREF',self.TIMEREF.strftime("%Y%m%d.00")],
            ['V_TRANS',"2"],
            ['V_STRETCH',"2"],
            ['_TNUDG_',"15.0d0 15.0d0"],
            ['OBCFAKTOR',"120.0"],
            ['NUDGZONEWIDTH',"50"],
            ['GRDFILE',GlobalParams.COMMONPATH+"/grid/A20_grd_NordicSeas.nc"],
            ['RUNDIR',self.RUNPATH],
            ['_CLMNAME_',self.RUNPATH+"/ocean_1993_clm.nc | \n"+self.RUNPATH+"/ocean_1994_clm.nc | \n"+self.RUNPATH+"/ocean_1995_clm.nc | \n"+self.RUNPATH+"/ocean_1996_clm.nc | \n"+self.RUNPATH+"/ocean_1997_clm.nc"],
            ['_BRYNAME_',self.RUNPATH+"/ocean_1993_bry.nc | \n"+self.RUNPATH+"/ocean_1994_bry.nc | \n"+self.RUNPATH+"/ocean_1995_bry.nc | \n"+self.RUNPATH+"/ocean_1996_bry.nc | \n"+self.RUNPATH+"/ocean_1997_bry.nc"],
            ['TIDEDIR',self.RUNPATH],
            ['ATMDIR',self.RUNPATH+"/AN_1993_unlim.nc | \n"+self.RUNPATH+"/AN_1994_unlim.nc | \n"+self.RUNPATH+"/AN_1995_unlim.nc | \n" +self.RUNPATH+"/AN_1996_unlim.nc | \n" +self.RUNPATH+"/AN_1997_unlim.nc \ \n"+self.RUNPATH+"/FC_1993_unlim.nc | \n"+self.RUNPATH+"/FC_1994_unlim.nc | \n"+self.RUNPATH+"/FC_1995_unlim.nc | \n"+self.RUNPATH+"/FC_1996_unlim.nc | \n"+self.RUNPATH+"/FC_1997_unlim.nc"],
            ['RIVERFILE',GlobalParams.COMMONPATH+"/rivers/newA20_rivers_mitya.nc"],
            ['FORCEFILES',"4"], # The files should be specified here as well
            #['ROMS/External/coupling.dat', self.RUNPATH + "/coupling.dat"],
            ['COUPLINGTIMEI2O',str(self.COUPLINGTIME_I2O)],
            ['ROMSINFILE', self.ROMSINFILE ],
            ['CICEINFILE', self.CICEINFILE ],
            ['NUMROMSCORES',str(int(self.XCPU)*int(self.YCPU))],
            ['NUMCICECORES',str(int(self.CICECPU))]
            ]
            ########################################################################
            # List of CICE keywords:
            ########################################################################
            #if (cice_rst_day == start_date.day):
            # if (restart == True):
            #     cicerst_truefalse = ".true."
            # else:
            #     cicerst_truefalse = ".false."
            self.CICEKEYWORDLIST=[
            ['CICEYEARSTART',start_date.strftime("%Y")],
            ['CICESTARTSTEP',str(int(cice_start_step))],  #number of hours after 00:00 Jan 1st
            ['CICEDELTAT',str(self.CICEDELTAT)],
            ['CICENPT',str(int((self.FCLEN/self.CICEDELTAT)-(cice_rst_time - cice_start_step)))],   # minus diff restart og start_date
            ['CICERUNTYPE',"'continue'"],
            ['CICEIC',"'default'"],
            ['CICEREST',".true."],
            ['CICERSTTIME',cicerst_truefalse],
            #['<cicedir>',GlobalParams.COMMONPATH + "/../../../tmproms/cice"]
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
