from subprocess import call
import os
########################################################################
# Define modules:
########################################################################
def replace_keywords_roms_in(roms_keyword_infile,roms_infile,keywordlist):
    """
    This module does ...
    """
    file = open(roms_keyword_infile)
    newlines = file.read()
    for n in range(len(keywordlist[:,0])):
        newlines = newlines.replace(keywordlist[n,0],keywordlist[n,1])
    with open(roms_infile, 'w') as f:
        for line in newlines:
            f.write(line)    

def execute_roms_mpi(ncpus,infile):
    """
    And this module ...
    """
    call(["mpirun", "-np", str(ncpus), "oceanM", infile])

def execute_roms_openmp(ncpus,infile):
    """
    And this module ...
    """
    #call(["mpirun", "-np", str(ncpus), "oceanM", infile])

def run_roms_mpi():
    """
    About this...
    """
    print "Running ROMS in directory: "+rundir[1]+"\n\n"
    os.chdir(rundir[1])
    # Prepare roms input-file, replace keywords:
    replace_keywords_roms_in(keywordpath+"/"+keywordfile, romsinfile, keywordlist)
    # Run the ROMS model:
    execute_roms_mpi(int(xcpu[1])*int(ycpu[1]),romsinfile)
    # Output to std.out that model has finished:
    print "\nROMS run finished"

