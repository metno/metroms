class GlobalParams(object):
    import getpass
    import os
    import sys

    username=getpass.getuser()

    MYHOST=os.environ.get('METROMS_MYHOST')

    if MYHOST=='metlocal' or 'vilje' or MYHOST=='alvin' or MYHOST=='met_ppi' or MYHOST=='elvis' or MYHOST=='nebula' or MYHOST=='stratus':
        HOME=os.environ.get('HOME')
        if HOME=='None':
            print "Environment variable HOME not found in configuration"
            sys.exit(1)

        METROMSDIR   =os.environ.get('METROMS_BASEDIR',HOME+'/metroms')
        METROMSAPPDIR=os.environ.get('METROMS_APPDIR', HOME+'/metroms_apps')
        METROMSTMPDIR=os.environ.get('METROMS_TMPDIR','/work/'+username)
    else:
        print 'Environment variable MYHOST not defined or unknown (metlocal,vilje,)'
        sys.exit(1)

    COMMONPATH=METROMSDIR+"/apps/common"
    COMMONORIGPATH=COMMONPATH+"/origfiles"

    ########################################################################
    # Internal files:
    ########################################################################
