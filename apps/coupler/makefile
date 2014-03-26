#
# Command-line variables
#   EXEC=<name>    ~ name given to executable, default is a.out
#   VPATH=<vpath>  ~ VPATH            , default is . (cwd only)
#   SRCS=<files>   ~ list of src files, default is all .c .F .F90 files in VPATH
#   VPFILE=<file>  ~ file with list of dirs, used to create VPATH
#   SRCFILE=<file> ~ file with list of src files, used to create SRCS
#
#   <macro defns>  ~ any macro definitions found in this file or the included 
#                    MACFILE will be over-riden by cmd-line macro definitions
#   MODEL=<model>  ~ a standard macro definition, often found in the included 
#                    MACFILE, used to trigger special compilation flags
#
# Usage examples:
#   % make MACFILE=Macros.AIX VPFILE=Filepath MODEL=ccm3 EXEC=atm
#   % make MACFILE=Macros.AIX VPFILE=Filepath SRCFILE=Srclist EXEC=pop
#   % make MACFILE=Macros.C90 VPATH="dir1 dir2" SRCS="file1.c file2.F90"
#   % make MACFILE=Macros.SUN SRCS="test.F"
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
# parse cmd-line and establish values for EXEC, VPATH, SRCS, OBJS, etc
#-------------------------------------------------------------------------------

EXEC     := a.out
MODEL    := NONE 
VPFILE   := NONE
VPATH    := .
SRCFILE  := NONE
SRCS     := NONE

ifneq ($(VPATH),.)
  # this variable was specified on cmd line or in an env var
else
  ifneq ($(VPFILE),NONE)
    # explicit list of VPATH dirs is provided
    VPATH := $(wildcard . $(shell cat $(VPFILE) ) )
  endif
endif

ifneq ($(SRCS),NONE)
  # this variable was specified on cmd line or in an env var
else
  ifneq ($(SRCFILE),NONE)
    # explicit list of src files is provided
    SRCS := $(shell cat $(SRCFILE) )
  else
    # list of src files is all .F90 .F .c files in VPATH
    SRCS := $(wildcard $(addsuffix /*.F90 , $(VPATH)) \
		       $(addsuffix /*.[cF], $(VPATH)) )
  endif
endif

OBJS  := $(addsuffix .o, $(sort $(basename $(notdir $(SRCS)))))
RM    := rm
FC    := gfortran
#INCLDIR := -I ${BUILD_DIR}/esmf/obj/objO/Linux.gfortran.64.mpiuni.default:${BUILD_DIR}/esmf/mod/modO/Linux.gfortran.64.mpiuni.default
INCLDIR := -I ${BUILD_DIR}/build \
           -I ${BUILD_DIR}/esmf/mod/modO/Linux.gfortran.64.mpiuni.default \
           -I ${BUILD_DIR}/cice/rundir4/compile

.SUFFIXES:
.SUFFIXES: .F90 .o

all: $(EXEC)

#-------------------------------------------------------------------------------
# echo file names, paths, compile flags, etc. used during build
#-------------------------------------------------------------------------------

db_files:
	@echo " "
	@echo "* EXEC    := $(EXEC)"
	@echo "* SRCFILE := $(SRCFILE)"
	@echo "* INCS    := $(INCS)"
	@echo "* INCLDIR := $(INCLDIR)"
	@echo "* SRCS    := $(SRCS)"
	@echo "* OBJS    := $(OBJS)"
	@echo "* CPP     := $(CPP)"
	@echo "* LD      := $(LD)"
db_flags:
	@echo " "
	@echo "* cpp     := $(CPP) $(CPPFLAGS) $(CPPDEFS) $(INCS) $(INCLDIR)"
	@echo "* cc      := cc -c $(CFLAGS) $(INCS) $(INCLDIR)"
	@echo "* .F.o    := $(FC) -c $(FFLAGS) $(FIXEDFLAGS) $(INCS) $(INCLDIR)"
	@echo "* .F90.o  := $(FC) -c $(FFLAGS) $(FREEFLAGS) $(INCS) $(INCLDIR)"

$(EXEC): $(OBJS)
	$(LD) -o $(EXEC) $(LDFLAGS) $(OBJS) $(ULIBS) $(SLIBS)

.c.o:
	cc $(CFLAGS) $(CPPDEFS) $(INCS) $(INCLDIR) $<

.F.o:
	$(CPP) $(CPPFLAGS) $(CPPDEFS) $(INCS) $(INCLDIR) $< > $*.f 
	$(FC) -c $(FFLAGS) $(FIXEDFLAGS) $(INCS) $(INCLDIR) $*.f  

.F90.o:
#	$(CPP) $(CPPFLAGS) $(CPPDEFS) $(INCS) $(INCLDIR) $< > $*.f90
	$(FC) -c $(FFLAGS) $(FREEFLAGS) $(INCS) $(INCLDIR) $*.F90  

clean:
	$(RM) -f *.d *.mod *.o

