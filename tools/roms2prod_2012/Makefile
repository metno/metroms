# --- Compiler ------------------------------

### Linux pgf90
FC = ifort
FFLAGS = -O3 #-parallel -par-report3 #-mcmodel=large
LDFLAGS =



# --- Files --------------------------------

BIN  = roms2prod
SRC1 = roms2prod.f90 check_err.f90 projection.f90 global_atts.f90 setup.f90 addvar.f90 defvar.f90 dimensions.f90 netcdf_atts.f90
# NetCDF 
INC_NC = 
LIB_NC = -lnetcdf -lnetcdff


SRC = $(SRC1) 
OBJ = $(SRC:.f90=.o)

# --- Compilation --------------------------

all: $(BIN)

.SUFFIXES:

.SUFFIXES: .f90 .o

.f90.o:
	$(FC) -c $(FFLAGS) $<

# ncutil.f90 needs NetCDF include
roms2prod.o: roms2prod.f90
	$(FC) $(FFLAGS) -c $(INC_NC) $<
projection.o: projection.f90
	$(FC) $(FFLAGS) -c $(INC_NC) $<
netcdf_atts.o: netcdf_atts.f90
	$(FC) $(FFLAGS) -c $(INC_NC) $<
global_atts.o: global_atts.f90
	$(FC) $(FFLAGS) -c $(INC_NC) $<
setup.o: setup.f90 
	$(FC) $(FFLAGS) -c $(INC_NC) $<	
dimensions.o: dimensions.f90
	$(FC) $(FFLAGS) -c $(INC_NC) $<
check_err.o: check_err.f90
	$(FC) $(FFLAGS) -c $(INC_NC) $<
addvar.o: addvar.f90
	$(FC) $(FFLAGS) -c $(INC_NC) $<
defvar.o: defvar.f90
	$(FC) $(FFLAGS) -c $(INC_NC) $<
$(BIN): $(OBJ)
	$(FC) -o $(BIN) $(FFLAGS) $(LDFLAGS) $(OBJ) $(LIB_NC)


# ---- Misc. ----------

clean:
	rm -f $(OBJ) *.mod

love:
	@echo "not war"

# --- Module dependencies ---

roms2prod.o: dimensions.o netcdf_atts.o global_atts.o check_err.o setup.o addvar.o defvar.o projection.o 
netcdf_atts.o: projection.o 
