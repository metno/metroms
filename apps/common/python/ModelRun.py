from subprocess import call
import os
from params import *
########################################################################
# Define:
########################################################################
class ModelRun(object):
    def replace_keywords_roms_in(self,roms_keyword_infile,roms_infile,keywordlist):
        """
        This module will replace the keywords in the given 
        keyword.in-file.
        """
        file = open(roms_keyword_infile)
        newlines = file.read()
        for n in range(len(keywordlist[:,0])):
            newlines = newlines.replace(keywordlist[n,0],keywordlist[n,1])
        with open(roms_infile, 'w') as f:
            for line in newlines:
                f.write(line)    

    def execute_roms_mpi(self,ncpus,infile,debugoption=NODEBUG):
        """
        Execute the ROMS model itself using MPI.
        """
        executable="oceanM" if debugoption==NODEBUG else "oceanG"
        os.system("mpirun -np "+str(ncpus)+" "+executable+" "+infile)
        #call(["mpirun", "-np", str(ncpus), "oceanM", infile])

    def execute_roms_openmp(self,ncpus,infile,debugoption=NODEBUG):
        """
        Execute the ROMS model itself using OpenMP.
        """
        executable="oceanO" if debugoption==NODEBUG else "oceanG"
        os.environ['OMP_NUM_THREADS'] = str(ncpus)
        os.system("./"+executable+" < "+infile)

    def execute_roms_serial(self,infile,debugoption=NODEBUG):
        """
        Execute the ROMS model itself in serial mode.
        """
        executable="oceanS" if debugoption==NODEBUG else "oceanG"
        os.system("./"+executable+" < "+infile)

    def preprocess(self):
        """
        Contains collection of preprocessors.
        """
        #pass
        print "goodbye"

    def run(self,runoption=SERIAL,debugoption=NODEBUG,architecture=MET64):
        """
        """
        # Prepare roms input-file, replace keywords:
        self.replace_keywords_roms_in(keywordpath+"/"+keywordfile, romsinfile, keywordlist)
        # Run the ROMS model:
        if runoption==MPI:
            self.execute_roms_mpi(int(xcpu[1])*int(ycpu[1]),romsinfile,debugoption)
        elif runoption==OPENMP:
            self.execute_roms_openmp(int(xcpu[1])*int(ycpu[1]),romsinfile,debugoption)
        elif runoption==SERIAL:
            self.execute_roms_serial(romsinfile,debugoption)
        elif runoption==DRY:
            pass
        else:
            print "No valid runoption!"
            exit(1)

    def postprocess(self):
        """
        """
        pass

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
