#!/usr/bin/python
from subprocess import call

rundir='/disk1/tmproms/run/arctic-20km'
print "Running ROMS in directory: "+rundir+"\n\n"

call(["mpirun", "-np", "8", "oceanM", "roms.in"])
