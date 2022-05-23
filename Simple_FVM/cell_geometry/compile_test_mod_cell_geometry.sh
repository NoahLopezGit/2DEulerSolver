#!/bin/bash
ParentDirectory=$(cd ../../ && pwd)
ConfigSrc="$ParentDirectory"/Simple_FVM/config.f95
ReadGridSrc="$ParentDirectory"/read_grid/mod_read_grid.f95
CellDataStrucSrc="$ParentDirectory"/Simple_FVM/cell_data_struc/mod_cell_data_struc.f95
MathSrc="$ParentDirectory"/Simple_FVM/mod_math.f95

gfortran -c $MathSrc
gfortran -c $ConfigSrc
gfortran -c $ReadGridSrc
gfortran -c $CellDataStrucSrc
gfortran -c mod_cell_geometry.f95
gfortran -c test_mod_cell_geometry.f95
gfortran test_mod_cell_geometry.o mod_cell_geometry.o mod_read_grid.o config.o mod_math.o -o ./test_mod_cell_geometry
rm *.o
rm *.mod
