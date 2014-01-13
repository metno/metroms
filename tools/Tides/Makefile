# Compiler

FC = xlf95
FC77 = xlf
FFLAGS = -O

# ---------------

# Source for utilities
USRCF90 = readsup.f90 readln.f90 shiftphase.f90 check_err.f90 fill.f90 new_astro.f90 new_vuf.f90
USRCF = gday.f

# Program sources
PSRC = tidenc2roms.f90

PROGS = tidenc2roms

SRC = $(USRCF90) $(PSRC)

UOBJF90 = $(USRCF90:.f90=.o)
UOBJF = $(USRCF:.f=.o)
UOBJ = $(UOBJF90) $(UOBJF)

OBJF90 = $(SRC:.f90=.o)
OBJF = $(USRCF:.f=.o)
OBJ = $(OBJF90) $(OBJF)

# NetCDF
NC_INC = -I/usr/local/netcdf/netcdf-3.6.2/include
NC_LIB = -L/usr/local/netcdf/netcdf-3.6.2/lib -lnetcdf


all: $(PROGS)

.SUFFIXES:;
.SUFFIXES: .f90 .f .o

.f90.o:
	$(FC) -c $(FFLAGS) $(NC_INC) $<

.f.o:
	$(FC77) -c $(FFLAGS) $(NC_INC) $<

$(PROGS): $(OBJ)
	$(FC) $(FFLAGS) -o $(PROGS) $(OBJ) $(NC_LIB)

clean:
	-rm -f *.o
	-rm -f *.mod
	-rm -f *~
	-rm -f $(PROGS)

love:
	@echo "not war"
