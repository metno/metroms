#!/usr/bin/sh
cd roms2roms/src
make -f Makefile_Ve
mv roms2roms ../bin/
cd ../..
#
cd dew2spec/src/
ifort -O3 -o dew2spec dew2spec.f90 -I/sw/sdev/Modules/netcdf/netcdf-4.1.3/include/ -L/sw/sdev/Modules/netcdf/netcdf-4.1.3/lib/ -lnetcdff -lnetcdf
mv dew2spec ../bin/
cd ../..
#
cd Clima/src/
make -f Makefile_Ve 
mv bry_from_clim ../bin/
cd ../..
#
cd Tides/src/
make -f Makefile_Ve 
mv tidenc2roms ../bin/
cd ../..
#
cd Tpxo/src/
make -f Makefile_Ve 
mv tpxo2grid ../bin/
cd ../..
#
cd atm2roms/src
ifort -O3 -o atm2roms atm2roms.f90 -L/prod/forecast/opt/lib -lmi
mv atm2roms ../bin/
cd ../..
#
cd romsinst2seq/src
make -f Makefile_Ve
mv romsinst2seq ../bin/
cd ../..
#
cd romsavg2seq/src
make -f Makefile_Ve
mv romsavg2seq ../bin/
cd ../..
#
