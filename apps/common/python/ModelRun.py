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

    def run_roms(self,runoption,debugoption,architecture):
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
        result = self._run(runoption,debugoption,architecture)
        if result == 0:
            print "\nROMS run finished"
        else:
            raise RuntimeError("Error with ROMS run!")

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

    def _execute_roms_mpi(self,ncpus,infile,debugoption,architecture):
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
        Returns:
            result (int) : 0 if all good, some other value if error with run or user choices
        """
        if debugoption==Constants.DEBUG:
            executable="oceanG"
        else:
            executable="oceanM"

        if architecture==Constants.NEBULA or architecture==Constants.STRATUS:
            print 'running on NSC HPC:'
            if debugoption==Constants.PROFILE:
                print "Profiling not working yet on "+architecture
                result = 1
            else:
                print "mpprun -np "+str(ncpus)+" "+executable+" "+infile
                result = os.system("mpprun -np "+str(ncpus)+" "+executable+" "+infile)

        elif architecture==Constants.MET_PPI_OPATH:
            print 'running on MET PPI:'
            if debugoption==Constants.PROFILE:
                print "Profiling not working yet on "+architecture
                result = 1
            else:
                result = os.system("/modules/centos7/OPENMPI/3.1.4-intel2018/bin/mpirun --mca mtl psm2 " + executable + " " + infile)
                #result = os.system("/modules/centos7/OPENMPI/3.1.3-intel2018/bin/mpiexec --mca mtl psm2 " + executable + " " + infile)
	elif architecture==Constants.FRAM:
            print 'running on FRAM:'
            if debugoption==Constants.PROFILE:
                print "Profiling not working yet on "+architecture
                result = 1
            else:
		result = os.system("mpirun " + os.path.join(self._params.RUNPATH, executable) + " " + infile)
        else:
            print "Unrecognized architecture!"
            result = 1

        return result

    def _execute_roms_openmp(self,ncpus,infile,debugoption):
        """
        Execute the ROMS model itself using OpenMP.

        Returns:
            result (int) : 0 if all good, some other value if error with run
        """
        executable="oceanO" if debugoption==Constants.NODEBUG else "oceanG"
        os.environ['OMP_NUM_THREADS'] = str(ncpus)
        result = os.system("./"+executable+" < "+infile)

    def _execute_roms_serial(self,infile,debugoption):
        """
        Execute the ROMS model itself in serial mode.

        Returns:
            result (int) : 0 if all good, 1 if some error with run
        """
        executable="oceanS" if debugoption==Constants.NODEBUG else "oceanG"
        result = os.system("./"+executable+" < "+infile)

    def _run(self,runoption,debugoption,architecture):
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
        Returns:
            result (int) : 0 if all good, some other value if error with run or user choices
        """
        # Run the ROMS model:
        if architecture==Constants.MET64:
            if runoption==Constants.MPI:
                result = self._execute_roms_mpi((int(self._params.XCPU)*int(self._params.YCPU))+
                                       int(self._params.CICECPU),
                                       self._params.ROMSINFILE,debugoption,architecture)
            elif runoption==Constants.OPENMP:
                if self._params.CICECPU != 0:
                    print "MetROMS is currently not handling CICE coupling in OpenMP"
                    result = 1
                else:
                    result = self._execute_roms_openmp(int(self._params.XCPU)*int(self._params.XCPU),
                                              self._params.ROMSINFILE,debugoption)
            elif runoption==Constants.SERIAL:
                if self._params.CICECPU != 0:
                    print "MetROMS is currently not handling CICE coupling in serial"
                    result = 1
                else:
                    result = self._execute_roms_serial(self._params.ROMSINFILE,debugoption)
            elif runoption==Constants.DRY:
                print "Dry-run ok"
                result = 0
            else:
                print "No valid runoption!"
                result = 1

        elif architecture==Constants.NEBULA or architecture==Constants.STRATUS or \
             architecture==Constants.MET_PPI_OPATH or architecture==Constants.FRAM:
            if runoption==Constants.MPI:
                result = self._execute_roms_mpi((int(self._params.XCPU)*int(self._params.YCPU))+
                                       int(self._params.CICECPU),
                                       self._params.ROMSINFILE,debugoption,architecture)
            elif runoption==Constants.DRY:
                result = 0
            else:
                print "No valid runoption!"
                result = 1
        else:
            print "Unsupported architecture..."
            result = 1

        return result

    def _cycle_rst_ini(self, backup=True):
        #Cycle ocean_rst.nc to ocean_ini.nc
        _ini = self._params.RUNPATH+"/ocean_ini.nc"
        _rst = self._params.RUNPATH+"/ocean_rst.nc"
        try:
            nc_ini = netCDF4.Dataset(_ini)
        except:
            print "error finding ini-file"
        try:
            nc_rst = netCDF4.Dataset(_rst)
            if len(nc_rst.variables["ocean_time"]) == 0:
                nc_rst.close()
                raise ValueError("No time entries on rst file!")
        except Exception as e:
            print e
            print "error finding/with rst-file, will use old ini-file..."
            os.system('cp -av '+_ini+' '+_rst)
            nc_rst = netCDF4.Dataset(_rst)
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

    def _verify_cice_rst_file(self, rst_dir=None):
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
        if rst_dir is None:
            rst_dir = os.path.join(self._params.CICERUNDIR, "restart")  # default to this if nothing specified
        elif type(rst_dir) is not str:
            raise TypeError("Invalid type for rst_dir, must be str!")
            
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
