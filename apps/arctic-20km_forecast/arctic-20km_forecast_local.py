#!/usr/bin/python
import numpy as np
import os
from ModelRun import *
from params import *
########################################################################
class Arctic20Forecast(ModelRun):
    # def run_roms(self,runoption=SERIAL,debugoption=NODEBUG):
    #     print "hello"
    #     super(Arctic20,self).run_roms(runoption,debugoption)
    def preprocess(self):
        self._fimex_felt2nc(FOAMfeltfile,"ocean_clm_in.nc",felt2nc_config)
        super(Arctic20Forecast,self).preprocess()  #To expand method in superclass
        #print "hello"

FOAMfeltfile="None"
FOAMncfile=None
felt2nc_config=None

Arctic20Forecast().run_roms(DRY,NODEBUG,MET64)
