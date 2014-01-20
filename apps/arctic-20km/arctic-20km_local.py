#!/usr/bin/python
import numpy as np
import os
from params import *
from roms_utils import *
########################################################################
print "Running ROMS in directory: "+rundir[1]+"\n\n"
os.chdir(rundir[1])
# Prepare roms input-file, replace keywords:
replace_keywords_roms_in(keywordpath+"/"+keywordfile, romsinfile, keywordlist)
# Run the ROMS model:
execute_roms_mpi(int(xcpu[1])*int(ycpu[1]),romsinfile)
# Output to std.out that model has finished:
print "\nROMS run finished"
########################################################################

