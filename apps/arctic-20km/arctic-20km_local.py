#!/usr/bin/python
import numpy as np
import os
from ModelRun import *
from params import *
########################################################################
class Arctic20(ModelRun):
    # def run_roms(self,runoption=SERIAL,debugoption=NODEBUG):
    #     print "hello"
    #     super(Arctic20,self).run_roms(runoption,debugoption)
    def preprocess(self):
        super(Arctic20,self).preprocess()  #To expand method in superclass
        print "hello"

Arctic20().run_roms(DRY,NODEBUG,MET64)
