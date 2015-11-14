class GlobalParams(object):
    import getpass
    import os
    import sys

    username=getpass.getuser()

    MYHOST=os.environ.get('METROMS_MYHOST','metlocal')

    if MYHOST=='metlocal':
        METROMSDIR=os.environ.get('METROMS_SRCDIR','/disk1/'+username+'/metroms')
        tmpdir=os.environ.get('METROMS_TMPDIR','/disk1/'+username)
        RUNDIR=tmpdir+'/tmproms/run'
    elif MYHOST=='vilje':
        HOME=os.environ.get('HOME')
        if HOME=='None':
            print "Environment variable HOME not found in configuration on vilje"
            sys.exit(1)

        METROMSDIR=os.environ.get('METROMS_SRCDIR',HOME+'/metroms')

        tmpdir=os.environ.get('METROMS_TMPDIR','/work/'+username)
        RUNDIR=tmpdir+'/tmproms/run'
    else:
        print 'Environment variable MYHOST not defined (metlocal,vilje,)'
        sys.exit(1)

    
#    COMMONPATH="/disk1/"+username+"/metroms/apps/common"
    COMMONPATH=METROMSDIR+"/apps/common"
    COMMONORIGPATH=COMMONPATH+"/origfiles"

    ########################################################################
    # Internal files:
    ########################################################################
    FELT2NC_CONFIG=COMMONORIGPATH+"/felt2nc.xml"
    IN_CLMFILE="ocean_clm_in.nc"
    CLMFILE="ocean_clm.nc"
    INIFILE="ocean_ini.nc"
