import os
import netCDF4
import Constants
from GlobalParams import *
from datetime import datetime, timedelta
import bisect
import sys

class ModelRun(object):
    """
    Class for managing a run with the METROMS model (coupled ROMS-CICE). Contains a selection of
    methods related to the setup of the run, starting the actual run and post-processing (potentially
    in the future). The class revolve around the existence of a Params object containing info about
    the run configuration. Main tasks of the class include

     - verifying the specified model start time with the existing initial/restart files for both
       ROMS and CICE and handling file logic to setup the appropriate initial/restart files accordingly
     - replacing keywords in ROMs and CICE input config/parameter files based on the Params object
     - initiating the METROMS model run on the specified computer architecture (several differet supported)
       with MPI, OPENMP or a SERIAL run
     - post-processing (maybe to come in the future)
     """
    _params=None
    _clmfileoption=None
    _atmfileoption=None

    def __init__(self,params,clmoption=Constants.NC,atmoption=Constants.NC):
        """
        Constructor function setting defining attributes and makes sure the CICE
        time step is a multiple of the ROMS time step (to make coupling work).

        Args:
            params (Params) : An object of the Params class specific to the
                              METROMS application that is being used
            clmoption (int) : File type of climatology file
                              (choose an option from the Constants module)
            clmoption (int) : File type of atmosphere file
                              (choose an option from the Constants module)
        """
        self._params=params
        self._clmfileoption=clmoption
        self._atmfileoption=atmoption
        # Only check this if CICECPU is larger than 0:
        if self._params.CICECPU > 0:
            # Check if delta_t for the models match up:
            if not params.CICEDELTAT%params.DELTAT == 0:
                print "delta_t's dont match up!!"
                sys.exit()

    def run_roms(self,runoption=Constants.SERIAL,debugoption=Constants.NODEBUG,
                 architecture=Constants.MET64):
        """
        Function that changes working directory to the run path (dir with the executable),
        replaces keywords in the CICE and ROMS in-files (as specified by the self._params
        attribute object) and finally calls the self._run() function to start the METROMS run.

        Args:
            runoption (int)    : Either run to run with MPI, OPENMP, SERIAL or DRY run
                                 (choose an option from the Contants module)
            debugoption (int)  : Wether or not METROMS is run in debug mode or not,
                                 (choose an option from the Contants module)
            architecture (int) : What architecture to run METROMS on
                                 (choose an option from the Contants module)
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

            if self._params.CICECPU > 0:
                self._verify_cice_rst_file()

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
        Function that executes the ROMS model itself using MPI. Depending on the specified architecture,
        the ROMS executable is run using different binaries/commands.

        Args:
            ncpus (int)        : Total number of CPUs to run with
            infile (str)       : Filename of the roms.in parameter/config file
            debugoption (int)  : Wether or not METROMS is run in debug mode or not
                                 (choose an option from the Contants module)
            architecture (int) : What architecture to run METROMS on
                                 (choose an option from the Contants module)
        """
        if debugoption==Constants.DEBUG:
            executable="oceanG"
        else:
            executable="oceanM"

        if architecture==Constants.VILJE:
            print 'running on vilje:'
            if debugoption==Constants.PROFILE:
#                os.system("make-profiler-libraries")
#                os.system("perf-report --mpi=\"SGI MPT (batch)\" --processes="+str(ncpus)+" "+executable+" "+infile)
                print "Profiling not working yet on "+architecture
                exit(1)
            else:
#                os.environ["MPI_BUFS_PER_PROC"] = str(128)
                result = os.system("mpiexec_mpt -np "+str(ncpus)+" "+executable+" "+infile)
                if result != 0: os.system('cat cice_stderr')

        elif architecture==Constants.ALVIN or architecture==Constants.ELVIS or architecture==Constants.NEBULA or architecture==Constants.STRATUS:
            print 'running on NSC HPC:'
            if debugoption==Constants.PROFILE:
                print "Profiling not working yet on "+architecture
                exit(1)
            else:
                print "mpprun -np "+str(ncpus)+" "+executable+" "+infile
                result = os.system("mpprun -np "+str(ncpus)+" "+executable+" "+infile)
                if result != 0: os.system('cat cice_stderr')

        elif architecture==Constants.MET_PPI_IBX:
            print 'running on MET PPI:'
            if debugoption==Constants.PROFILE:
                print "Profiling not working yet on "+architecture
                exit(1)
            else:
                #os.environ["MPI_BUFS_PER_PROC"] = str(128)
                result = os.system("/modules/xenial/OPENMPI/3.0.0intel18/bin/mpiexec --mca btl openib,self -bind-to core " + executable + " " + infile)
                if result != 0: os.system('cat cice_stderr')

        elif architecture==Constants.MET_PPI_OPATH:
            print 'running on MET PPI:'
            if debugoption==Constants.PROFILE:
                print "Profiling not working yet on "+architecture
                exit(1)
            else:
                #os.environ["MPI_BUFS_PER_PROC"] = str(128)
                result = os.system("/modules/centos7/OPENMPI/3.1.3-intel2018/bin/mpiexec --mca mtl psm2 " + executable + " " + infile)
                if result != 0: os.system('cat cice_stderr')
        else:
            print "Unrecognized architecture!"
            #result = os.system("mpirun -np "+str(ncpus)+" "+executable+" "+infile)
            #if result != 0: os.system('cat cice_stderr')


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
        Function that calls other functions for running METROMS depending on the specified
        architecture and and runoption.

        Args:
            runoption (int)    : Either run to run with MPI, OPENMP, SERIAL or DRY run
                                 (choose an option from the Contants module)
            debugoption (int)  : Wether or not METROMS is run in debug mode or not
                                 (choose an option from the Contants module)
            architecture (int) : What architecture to run METROMS on
                                 (choose an option from the Contants module)
        """
        # if statement for backwards compatibility (remove in the future)
        # ---------------------------------------------------------------------------------------------------
        if architecture == Constants.MET_PPI:
            print("\nWarning: {0}={1} is deprecated! Use {0}={2} or {0}={3} instead. Defaulting to {2}.".format(
                  "architecture", "Constants.MET_PPI", "Constants.MET_PPI_IBX", "Constants.MET_PPI_OPATH"))
            architecture = Constants.MET_PPI_IBX
        # ---------------------------------------------------------------------------------------------------

        # Run the ROMS model:
        if architecture==Constants.MET64:
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

        elif architecture==Constants.VILJE or architecture==Constants.ALVIN or architecture==Constants.ELVIS or \
             architecture==Constants.NEBULA or architecture==Constants.STRATUS or architecture==Constants.MET_PPI_IBX or \
             architecture==Constants.MET_PPI_OPATH:
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
        print 'Start date is: '+self._params.START_DATE.strftime('%Y%m%d-%H:%M')
        print 'Ini date is: '+netCDF4.num2date(nc_ini.variables['ocean_time'][nrrec],nc_ini.variables['ocean_time'].units).strftime('%Y%m%d-%H:%M')
        print 'Rst date is: '+netCDF4.num2date(nc_rst.variables['ocean_time'][nrrec],nc_rst.variables['ocean_time'].units).strftime('%Y%m%d-%H:%M')
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
                        self._params.NRREC = nrrec + 1
                        # Must update keywords!
                        self._params.change_run_param('IRESTART',str(self._params.NRREC))
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

    def _verify_cice_rst_file(self):
        """Function checking if the specified model start time is found in the
        CICE restart file located in cice restart pointer file, if not, it checks
        if model start time is found in any other restart files in the restart
        directory. All restart files are assumed to contain only one time and
        if the model start time is not found, an IOError is raised."""

        def date_in_cice_rst(date, filename, time_name="time"):
            """Function checking for match between <date> and time in cice rst file."""
            rst_data = netCDF4.Dataset(filename, "r")
            file_date = datetime(date.year,1,1) + timedelta(seconds=float(rst_data.time))
            print(filename, date, file_date)
            return True if date == file_date else False

        # open CICE restart pointer file and read filename
        rst_dir = os.path.join(self._params.CICERUNDIR, "restart")
        rst_pointer_file = os.path.join(rst_dir, "ice.restart_file")

        with open(rst_pointer_file, "r") as rst_fp:
            rst_file = rst_fp.read().strip()

        # if model start date found in "default" CICE restart file
        if date_in_cice_rst(self._params.START_DATE, rst_file):
            print("Model start time matches CICE rst time in {}".format(rst_file))
            return

        # if not, search through all other restart files in the restart directory
        else:
            for element in os.listdir(rst_dir):
                if element.endswith(".nc"):
                    nc_abspath = os.path.join(rst_dir, element)

                    if date_in_cice_rst(self._params.START_DATE, nc_abspath):
                        with open(rst_pointer_file, "w") as rst_fp:
                            rst_fp.write(nc_abspath)

                        istep = netCDF4.Dataset(nc_abspath, "r").istep1
                        cice_start_step = (self._params.START_DATE-datetime(self._params.START_DATE.year,01,01,00)).total_seconds()/self._params.CICEDELTAT

                        for kv in self._params.CICEKEYWORDLIST:
                            if kv[0] == "CICENPT":
                                kv[1] = str(int((self._params.FCLEN/self._params.CICEDELTAT)-(istep - cice_start_step)))

                        print("Found matching start time in {}, wrote to {}".format(
                              nc_abspath, rst_pointer_file))
                        return

        raise IOError("No CICE restart file matching start date {}!".format(self._params.START_DATE))

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
            print("Params.NRREC = {}".format(self._params.NRREC))
            if (cice_rst_day == roms_ini[self._params.NRREC-1].day) : # -1 for fixing index
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
