import os
import netCDF4
import Constants
from GlobalParams import *
from datetime import datetime
import bisect

class ModelRun(object):
    _params=None
    _clmfileoption=None
    _atmfileoption=None

    def __init__(self,params,clmoption=Constants.NC,atmoption=Constants.NC):
        self._params=params
        self._clmfileoption=clmoption
        self._atmfileoption=atmoption

    def run_roms(self,runoption=Constants.SERIAL,debugoption=Constants.NODEBUG,
                 architecture=Constants.MET64):
        """
        About this...
        """
        print "Running ROMS in directory: "+self._params.RUNPATH+"\n\n"
        #print (int(self._params.XCPU)*int(self._params.YCPU))+int(self._params.CICECPU)
        os.chdir(self._params.RUNPATH)
        # Prepare roms input-file, replace keywords:
        if self._params.CICECPU > 0:
            self._replace_keywords_cice_in()
        # Check to see if roms ini- and cice ini-file have same timestamp:
        if (self._params.RESTART == True):
            self._check_starttime()
        self._replace_keywords_roms_in()
        print "running roms for this timespan:"
        print self._params.START_DATE, self._params.END_DATE
        print self._params.FCLEN
        self._run(runoption,debugoption,architecture)
        # Should check output-files to verify a successful run?
        # Output to std.out that model has finished:
        print "\nROMS run finished"

    def preprocess(self):
        """
        Contains collection of preprocessors common for all models.
        """
        os.chdir(self._params.RUNPATH)
        if (self._params.RESTART == True):
            print "Model is restarting from previuos solution..."
            self._cycle_rst_ini()

    def postprocess(self):
        """
        """
        os.chdir(self._params.RUNPATH)
        #self._add_forecast_ref_time()

########################################################################
# PRIVATE METHODS:
########################################################################

    def _replace_keywords_roms_in(self):
        """
        This function will replace the keywords in the given
        keyword.in-file.
        """
        with open(self._params.KEYWORDFILE, 'r') as f:
            newlines = f.read()
        for key,value in self._params.KEYWORDLIST:
            newlines = newlines.replace(key,value)
        with open(self._params.ROMSINFILE, 'w') as f:
            f.write(newlines)

    def _replace_keywords_cice_in(self):
        """
        This function will replace the keywords in the given
        cice-keyword.in-file.
        """
        with open(self._params.CICEKEYWORDFILE, 'r') as f:
            newlines = f.read()
        for key,value in self._params.CICEKEYWORDLIST:
            newlines = newlines.replace(key,value)
        with open(self._params.CICEINFILE, 'w') as f:
            f.write(newlines)
        #os.system('cp -a '+self._params.CICEINFILE+' '+self._params.RUNPATH+'/') #should not need to do this

    def _execute_roms_mpi(self,ncpus,infile,debugoption=Constants.NODEBUG,architecture=Constants.MET64):
        """
        Execute the ROMS model itself using MPI.
        """
        if debugoption==Constants.DEBUG:
            executable="oceanG"
        else:
            executable="oceanM"

        if architecture==Constants.VILJE:
            if debugoption==Constants.PROFILE:
#                os.system("make-profiler-libraries")
#                os.system("perf-report --mpi=\"SGI MPT (batch)\" --processes="+str(ncpus)+" "+executable+" "+infile)
                print "Profiling not working yet on "+architecture
                exit(1)

            else:
#                os.environ["MPI_BUFS_PER_PROC"] = str(128)
                result = os.system("mpiexec_mpt -np "+str(ncpus)+" "+executable+" "+infile)
                if result != 0: os.system('cat cice_stderr')
        if architecture==Constants.ALVIN:
            if debugoption==Constants.PROFILE:
#                os.system("make-profiler-libraries")
#                os.system("perf-report --mpi=\"SGI MPT (batch)\" --processes="+str(ncpus)+" "+executable+" "+infile)
                print "Profiling not working yet on "+architecture
                exit(1)

            else:
#                os.environ["MPI_BUFS_PER_PROC"] = str(128)
                result = os.system("mpprun -np "+str(ncpus)+" "+executable+" "+infile)
                if result != 0: os.system('cat cice_stderr')
        else:
            result = os.system("mpirun -np "+str(ncpus)+" "+executable+" "+infile)
            if result != 0: os.system('cat cice_stderr')


    def _execute_roms_openmp(self,ncpus,infile,debugoption=Constants.NODEBUG):
        """
        Execute the ROMS model itself using OpenMP.
        """
        executable="oceanO" if debugoption==Constants.NODEBUG else "oceanG"
        os.environ['OMP_NUM_THREADS'] = str(ncpus)
        os.system("./"+executable+" < "+infile)

    def _execute_roms_serial(self,infile,debugoption=Constants.NODEBUG):
        """
        Execute the ROMS model itself in serial mode.
        """
        executable="oceanS" if debugoption==Constants.NODEBUG else "oceanG"
        os.system("./"+executable+" < "+infile)

    def _fimex_hor_interp(self,ncfilein,ncfileout):
        """
        """
        #Denne funksjonen maa ha info om input og ouput grid
        #og configfiler..?
        print "fimex_hor_interp"

    def _fimex_felt2nc(self,infile,outfile,configfile):
        """
        """
        #fimex --input.file=fil.flt --input.type=felt --output.file=fil.nc --input.config=config.cfg
        print "fimex_felt2nc"

    def _verticalinterp(self,ncfilein,ncfileout):
        """
        """
        print "verticalinterp"

    def _bry_from_clm(self,clmfile,bryfile):
        print "bry_from_clm"

    def _ini_from_clm(self,clmfile,inifile):
        print "ini_from_clm"

    def _tpxo2romstide(self,grdfile,tpxofilein,tidefileout):
        """
        Create tide.nc-file from any TPXO nc-file.
        """
        print "tpxo2romstide"

    def _dew2spec(self,ncfile):
        print "dew2spec"

    def _make_atm_force(self):  #tHIS METHOD CAN VARY BETWEEN MODELS...
        print "make_atm_force start"
        self._fimex_felt2nc(None,None,None)
        self._dew2spec(None)
        print "make_atm_force end"

    def _make_OBC(self):
        self._verticalinterp(self._get_clmfile(),GlobalParams.CLMFILE)
        self._bry_from_clm(GlobalParams.CLMFILE,None)
        self._ini_from_clm(GlobalParams.CLMFILE,None)

    def _get_clmfile(self):
        if self._clmfileoption==Constants.FELT:
            self._fimex_felt2nc(self._params.FELT_CLMFILE,GlobalParams.IN_CLMFILE,GlobalParams.FELT2NC_CONFIG)
        elif self._clmfileoption==Constants.NC:
            self._fimex_hor_interp(None, None)
        else:
            print "Illegal clmfileoption."
            exit(1)
        return GlobalParams.IN_CLMFILE

    def _run(self,runoption=Constants.SERIAL,debugoption=Constants.NODEBUG,architecture=Constants.MET64):
        """
        """
        # Run the ROMS model:
        if architecture==Constants.MET64 :
            if runoption==Constants.MPI:
                self._execute_roms_mpi((int(self._params.XCPU)*int(self._params.YCPU))+
                                       int(self._params.CICECPU),
                                       self._params.ROMSINFILE,debugoption,architecture)
            elif runoption==Constants.OPENMP:
                if self._params.CICECPU != 0:
                    print "MetROMS is currently not handling CICE coupling in OpenMP"
                    exit(1)
                self._execute_roms_openmp(int(self._params.XCPU)*int(self._params.XCPU),
                                          self._params.ROMSINFILE,debugoption)
            elif runoption==Constants.SERIAL:
                if self._params.CICECPU != 0:
                    print "MetROMS is currently not handling CICE coupling in serial"
                    exit(1)
                self._execute_roms_serial(self._params.ROMSINFILE,debugoption)
            elif runoption==Constants.DRY:
                print "Dry-run ok"
            else:
                print "No valid runoption!"
                exit(1)

        elif architecture==Constants.VILJE or architecture==Constants.ALVIN:
            if runoption==Constants.MPI:
                self._execute_roms_mpi((int(self._params.XCPU)*int(self._params.YCPU))+
                                       int(self._params.CICECPU),
                                       self._params.ROMSINFILE,debugoption,architecture)
            elif runoption==Constants.DRY:
                pass
            else:
                print "No valid runoption!"
                exit(1)

        elif architecture in (Constants.MET32,Constants.BYVIND):
            print "Currently unsupported architecture..."
            exit(1)
        else:
            print "Unsupported architecture..."
            exit(1)

    def _cycle_rst_ini(self, backup=True):
        #Cycle ocean_rst.nc to ocean_ini.nc
        _ini = self._params.RUNPATH+"/ocean_ini.nc"
        _rst = self._params.RUNPATH+"/ocean_rst.nc"
        try:
            nc_ini = netCDF4.Dataset(_ini)
        except:
            print "error finding ini-file"
            #pass
        try:
            nc_rst = netCDF4.Dataset(_rst)
        except:
            print "error finding rst-file, will use old ini-file..."
            os.system('cp -av '+_ini+' '+_rst)
            nc_rst = netCDF4.Dataset(_rst)
            #pass
        if self._params.NRREC > 0:
            nrrec = self._params.NRREC - 1
        else:
            nrrec = self._params.NRREC
        if (self._params.START_DATE == netCDF4.num2date(nc_ini.variables['ocean_time'][nrrec],nc_ini.variables['ocean_time'].units)):
            print "No need to cycle restart-files"
        else:
            if os.path.isfile(_rst):
                # Check if START_DATE is on rst-file:
                nrrec2 = bisect.bisect(netCDF4.num2date(nc_rst.variables['ocean_time'][:],nc_rst.variables['ocean_time'].units), self._params.START_DATE) - 1
                print nrrec, nrrec2
                if nrrec2 >= len(nc_rst.variables['ocean_time'][:]): nrrec2 = len(nc_rst.variables['ocean_time'][:])-1
                print nrrec, nrrec2
                if nrrec != nrrec2:
                    print "I should not restart from specified nrrec!"
                    if (self._params.START_DATE == netCDF4.num2date(nc_rst.variables['ocean_time'][nrrec2],nc_rst.variables['ocean_time'].units)):
                        nrrec = nrrec2
                        self._params.NRREC = nrrec
                        # Must update keywords!
                        self._params.change_run_param('IRESTART',str(nrrec))
                        print "Changes nrrec and keyword IRESTART"
                if (self._params.START_DATE == netCDF4.num2date(nc_rst.variables['ocean_time'][nrrec],nc_rst.variables['ocean_time'].units)):
                    os.rename(_ini, self._params.RUNPATH+netCDF4.num2date(nc_ini.variables['ocean_time'][0],nc_ini.variables['ocean_time'].units).strftime("/ocean_ini.nc_%Y%m%d-%H%M"))
                    os.rename(_rst, _ini)
                    print "Cycled restart files"
                elif (self._params.START_DATE == netCDF4.num2date(nc_ini.variables['ocean_time'][nrrec],nc_ini.variables['ocean_time'].units)):
                    print "No need to cycle restart-files"
                    # If model hasn't been run last day:
                else:
                    self._params.START_DATE = netCDF4.num2date(nc_rst.variables['ocean_time'][nrrec],nc_rst.variables['ocean_time'].units)
                    self._params.FCLEN = (self._params.END_DATE-self._params.START_DATE).total_seconds()
                    # Must update keywords!
                    self._params.change_run_param('TSTEPS',str(self._params.FCLEN/self._params.DELTAT))
                    self._params.change_run_param('STARTTIME',str((self._params.START_DATE-self._params.TIMEREF).total_seconds()/86400))
                    os.rename(_ini, self._params.RUNPATH+netCDF4.num2date(nc_ini.variables['ocean_time'][nrrec],nc_ini.variables['ocean_time'].units).strftime("/ocean_ini.nc_%Y%m%d-%H%M"))
                    os.rename(_rst, _ini)
                    print "Cycled restart and changed START_DATE and FCLEN, icluding keywords"
            else:
                print netCDF4.num2date(nc_ini.variables['ocean_time'][nrrec],nc_ini.variables['ocean_time'].units)
                print "Restartfile not found!! Will exit"
                exit(1)

    def _check_starttime(self):
        try:
            roms_ini = netCDF4.num2date(netCDF4.Dataset(self._params.RUNPATH+"/ocean_ini.nc").variables['ocean_time'][:],
                                        netCDF4.Dataset(self._params.RUNPATH+"/ocean_ini.nc").variables['ocean_time'].units)
        except:
            roms_ini = netCDF4.num2date(netCDF4.Dataset(self._params.RUNPATH+"/ocean_ini.nc").variables['ocean_time'],
                                        netCDF4.Dataset(self._params.RUNPATH+"/ocean_ini.nc").variables['ocean_time'].units)
        if self._params.CICECPU > 0:
            f = open(self._params.CICERUNDIR+'/restart/ice.restart_file', 'r')
            cice_restartfile = f.readline().strip()
            cice_rst_day = netCDF4.Dataset(cice_restartfile).mday
            if (cice_rst_day == roms_ini[self._params.NRREC].day) :
                # Dates seem ok
                pass
            else:
                if (self._params.RESTART == False):
                    print "There seems to be a problem with matching dates in ROMS and CICE, but will continue since this is not a restart"
                else:
                    print "There seems to be a problem with matching dates in ROMS and CICE. Will exit..."
                    print "ROMS: "+str(roms_ini[self._params.NRREC].day)
                    print "CICE: "+str(cice_rst_day)
                    exit(1)

#     def _add_forecast_ref_time(self):
#         os.system('module load nco; ncks -A -v forecast_reference_time '+self._params.ATMFILE+' '+self._params.RUNPATH+'/ocean_his.nc')
#         print 'added forecast ref time'
