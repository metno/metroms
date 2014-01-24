from subprocess import call
from abc import abstractmethod
import os
from Params import Params
from Constants import *
from GlobalParams import *
########################################################################
# Define:
########################################################################
class ModelRun(object):
    _params=None
    _clmfileoption=None
    _atmfileoption=None

    def __init__(self,params,clmoption=NC,atmoption=NC):
        self._params=params
        self._clmfileoption=clmoption
        self._atmfileoption=atmoption

    def run_roms(self,runoption=SERIAL,debugoption=NODEBUG,architecture=MET64):
        """
        About this...
        """
        print "Running ROMS in directory: "+self._params.RUNPATH+"\n\n"
        os.chdir(self._params.RUNPATH)
        # Prepare roms input-file, replace keywords:
        self._replace_keywords_roms_in()
        self._run(runoption,debugoption,architecture)
        # Output to std.out that model has finished:
        print "\nROMS run finished"

    def preprocess(self):
        """
        Contains collection of preprocessors common for all models.
        """
        os.chdir(self._params.RUNPATH)
        self._make_OBC()
        self._tpxo2romstide(None,None,None)
        self._make_atm_force()

    def postprocess(self):
        """
        """
        os.chdir(self._params.RUNPATH)
        #pass

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

    def _execute_roms_mpi(self,ncpus,infile,debugoption=NODEBUG):
        """
        Execute the ROMS model itself using MPI.
        """
        executable="oceanM" if debugoption==NODEBUG else "oceanG"
        os.system("mpirun -np "+str(ncpus)+" "+executable+" "+infile)
        #call(["mpirun", "-np", str(ncpus), "oceanM", infile])

    def _execute_roms_openmp(self,ncpus,infile,debugoption=NODEBUG):
        """
        Execute the ROMS model itself using OpenMP.
        """
        executable="oceanO" if debugoption==NODEBUG else "oceanG"
        os.environ['OMP_NUM_THREADS'] = str(ncpus)
        os.system("./"+executable+" < "+infile)

    def _execute_roms_serial(self,infile,debugoption=NODEBUG):
        """
        Execute the ROMS model itself in serial mode.
        """
        executable="oceanS" if debugoption==NODEBUG else "oceanG"
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
        self._verticalinterp(self.get_clmfile(),CLMFILE)
        self._bry_from_clm(CLMFILE,None)
        self._ini_from_clm(CLMFILE,None)

    def get_clmfile(self): 
        if self._clmfileoption==FELT:
            self._fimex_felt2nc(self._params.FELT_CLMFILE,IN_CLMFILE,FELT2NC_CONFIG)
        elif self._clmfileoption==NC:
            self._fimex_horinterp()
        else:
            print "Illegal clmfileoption."
            exit(1)
        return IN_CLMFILE

    def _run(self,runoption=SERIAL,debugoption=NODEBUG,architecture=MET64):
        """
        """
        # Run the ROMS model:
        if architecture==MET64:
            if runoption==MPI:
                self._execute_roms_mpi(int(self._params.xcpu[1])*int(self._params.ycpu[1]),self._params.ROMSINFILE,debugoption)
            elif runoption==OPENMP:
                self._execute_roms_openmp(int(self._params.xcpu[1])*int(self._params.ycpu[1]),self._params.ROMSINFILE,debugoption)
            elif runoption==SERIAL:
                self._execute_roms_serial(self._params.ROMSINFILE,debugoption)
            elif runoption==DRY:
                pass
            else:
                print "No valid runoption!"
                exit(1)
        elif architecture in (MET32,VILJE,BYVIND):
            print "Unsupported architecture..."
            exit(1)
        else:
            print "Unsupported architecture..."
            exit(1)
