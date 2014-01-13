#                                 Makefile for producing ROMS forcing
FC = xlf95
FFLAGS = -O
LIBPATH = /usr/local/netcdf/netcdf-3.6.2/lib
#
INCLUDE = /usr/local/netcdf/netcdf-3.6.2/include
#
LIBRARY = netcdf
#
TARGETS = all clean
EXECS   = MakeRivers
#
#
all: $(EXECS)

MakeRivers: MakeRivers.o
	$(FC) $(FFLAGS) -o $@ MakeRivers.f90 -I$(INCLUDE) -L$(LIBPATH) -l$(LIBRARY)

MakeRivers.o: MakeRivers.f90

clean:
	@for name in $(EXECS) ; do\
	(rm -f $$name $$name.o); \
	done

