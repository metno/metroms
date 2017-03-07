class GlobalParams(object):
    import getpass
    import os
    import sys

    username=getpass.getuser()

    MYHOST=os.environ.get('METROMS_MYHOST','metlocal')

    if MYHOST=='metlocal':
        METROMSDIR=os.environ.get('METROMS_BASEDIR','/disk1/'+username+'/metroms')
        tmpdir=os.environ.get('METROMS_TMPDIR','/disk1/'+username)
        RUNDIR=tmpdir+'/run'
    elif MYHOST=='vilje':
        HOME=os.environ.get('HOME')
        if HOME=='None':
            print "Environment variable HOME not found in configuration on vilje"
            sys.exit(1)

        METROMSDIR=os.environ.get('METROMS_BASEDIR',HOME+'/metroms')

        tmpdir=os.environ.get('METROMS_TMPDIR','/work/'+username)
        RUNDIR=tmpdir+'/run'
        METROMSAPPDIR=os.environ.get('METROMS_APPDIR',HOME+'/metroms_apps')
    else:
        print 'Environment variable MYHOST not defined (metlocal,vilje,)'
        sys.exit(1)

    
#    COMMONPATH="/disk1/"+username+"/metroms/apps/common"
    COMMONPATH=METROMSDIR+"/apps/common"
    COMMONORIGPATH=COMMONPATH+"/origfiles"

    ########################################################################
    # Internal files:
    ########################################################################

