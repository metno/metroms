import netCDF4
import sys
import numpy as np
import datetime
import os

def create_inifile(ofile,varnames,varlist):
    if ( len(varnames) != len(varlist) ):
        print 'check list of varnames and vars'
        sys.exit()
    # Save data to netCDF-file:
    try:
        if os.path.exists(ofile):
            print 'Outfile exists, will delete it'
            os.remove(ofile)
        print 'Making ofile'
        r = netCDF4.Dataset(ofile,'w',type='NETCDF4-CLASSIC')
        for i in range(len(varlist)):
            #print i
            if ( i == 0 ):
                #Add dimensions
                print varnames[i]
                #print len(varlist[i].shape)
                if len(varlist[i].shape) < 3:
                    ydim, xdim = varlist[i].shape
                    tdim = 1
                else:
                    tdim, ydim, xdim = varlist[i].shape
                # 4 dimensions, time is unlimited
                dt  = r.createDimension('ocean_time',None)
                dxr = r.createDimension('xi_rho',xdim)
                dyr = r.createDimension('eta_rho',ydim)
                dxr = r.createDimension('xi_u',xdim-1)
                dyr = r.createDimension('eta_v',ydim-1)
                # one global attribute
                r.title = "Made by nilsmk@met.no"
            #Add variable
            add_var_to_ncobj(r,varnames[i],varlist[i])
        r.close()
    except Exception as ex:
        print "Could not prepare netCDF file for writing (%s)" % (ex,)
    return

def add_var_to_ncobj(r,varname,var):
    print len(var.shape)
    if ( len(var.shape) == 1 ):
        print 'Assume time dim'
        if ( varname == 'ocean_time' ):
            vvar = r.createVariable('ocean_time','double',('ocean_time'))
        elif ( varname == 's_rho' ):
            vvar = r.createVariable('s_rho','double',('s_rho'))
    elif ( len(var.shape) == 2 ):
        print 'Assume x,y dims'
        if ( varname == 'u' or varname == 'ubar'):
            vvar = r.createVariable(varname,'d',('eta_rho','xi_u'),fill_value=1e+37)
        elif ( varname == 'v' or varname == 'vbar'):
            vvar = r.createVariable(varname,'d',('eta_v','xi_rho'),fill_value=1e+37)
        else:
            vvar = r.createVariable(varname,'d',('eta_rho','xi_rho'),fill_value=1e+37)
    elif ( len(var.shape) == 3 ):
        print 'Assume x,y,time dims'
        if ( varname == 'u' or varname == 'ubar'):
            vvar = r.createVariable(varname,'d',('ocean_time','eta_rho','xi_u'),fill_value=1e+37)
        elif ( varname == 'v' or varname == 'vbar'):
            vvar = r.createVariable(varname,'d',('ocean_time','eta_v','xi_rho'),fill_value=1e+37)
        else:
            vvar = r.createVariable(varname,'d',('ocean_time','eta_rho','xi_rho'),fill_value=1e+37)
    elif ( len(var.shape) == 4 ):
        print 'Assume x,y,s,time dims'
        if ( varname == 'u' or varname == 'ubar'):
            vvar = r.createVariable(varname,'d',('ocean_time','s_rho','eta_rho','xi_u'),fill_value=1e+37)
        elif ( varname == 'v' or varname == 'vbar'):
            vvar = r.createVariable(varname,'d',('ocean_time','s_rho','eta_v','xi_rho'),fill_value=1e+37)
        else:
            vvar = r.createVariable(varname,'d',('ocean_time','s_rho','eta_rho','xi_rho'),fill_value=1e+37)
    else:
        print 'error!'
    vvar[:] = var
    r.sync()

gridfile = sys.argv[1]
ofile    = sys.argv[2]

print gridfile
print ofile

gnc = netCDF4.Dataset(gridfile)
h = gnc.variables['h']
print h
print h[:].shape
y, x = h.shape

create_inifile(ofile=ofile,varnames=['zeta','ubar','vbar'],varlist=[np.zeros((1,y,x)),np.zeros((1,y,x-1)),np.zeros((1,y-1,x))])


onc = netCDF4.Dataset(ofile,'r+')
vdt = onc.createVariable('ocean_time','f',('ocean_time'))
print vdt
vdt.units='seconds since 1970-01-01 00:00:00' #time.units
vdt[0] = (datetime.datetime(2016,9,1)-datetime.datetime(1970,1,1)).total_seconds()
onc.sync()
onc.close()
