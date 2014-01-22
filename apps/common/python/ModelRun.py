from subprocess import call
import os
from params import *
########################################################################
# Define:
########################################################################
class ModelRun(object):
    def run_roms(self,runoption=SERIAL,debugoption=NODEBUG,architecture=MET64):
        """
        About this...
        """
        print "Running ROMS in directory: "+rundir[1]+"\n\n"
        os.chdir(rundir[1])
        self.preprocess()
        self.run(runoption,debugoption,architecture)
        self.postprocess()
        # Output to std.out that model has finished:
        print "\nROMS run finished"

    def preprocess(self):
        """
        Contains collection of preprocessors common for all models.
        """
        #self._fimex_felt2nc(infile,outfile,configfile) #Comment out? Need hor.interp...
        self._verticalinterp("ocean_clm_in.nc","ocean_clm.nc")
        self._bry_from_clm(None,None)
        self._ini_from_clm(None,None)
        self._tpxo2romstide(None,None,None)
        self._make_atm_force()
        # Prepare roms input-file, replace keywords:
        self._replace_keywords_roms_in(keywordpath+"/"+keywordfile, romsinfile, keywordlist)

    def run(self,runoption=SERIAL,debugoption=NODEBUG,architecture=MET64):
        """
        """
        # Run the ROMS model:
        if architecture==MET64:
            if runoption==MPI:
                self._execute_roms_mpi(int(xcpu[1])*int(ycpu[1]),romsinfile,debugoption)
            elif runoption==OPENMP:
                self._execute_roms_openmp(int(xcpu[1])*int(ycpu[1]),romsinfile,debugoption)
            elif runoption==SERIAL:
                self._execute_roms_serial(romsinfile,debugoption)
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
            
    def postprocess(self):
        """
        """
        pass

########################################################################
# PRIVATE METHODS:
########################################################################

    def _replace_keywords_roms_in(self,roms_keyword_infile,roms_infile,keywordlist):
        """
        This function will replace the keywords in the given 
        keyword.in-file.
        """
        file = open(roms_keyword_infile)
        newlines = file.read()
        for n in range(len(keywordlist[:,0])):
            newlines = newlines.replace(keywordlist[n,0],keywordlist[n,1])
        with open(roms_infile, 'w') as f:
            for line in newlines:
                f.write(line)    

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

