class GlobalParams(object):
    import getpass
    username=getpass.getuser()
    COMMONPATH="/disk1/"+username+"/metroms/apps/common"
    COMMONORIGPATH=COMMONPATH+"/origfiles"
    RUNDIR="/disk1/"+username+"/tmproms/run"
    ########################################################################
    # Internal files:
    ########################################################################
    FELT2NC_CONFIG=COMMONORIGPATH+"/felt2nc.xml"
    IN_CLMFILE="ocean_clm_in.nc"
    CLMFILE="ocean_clm.nc"
    INIFILE="ocean_ini.nc"
