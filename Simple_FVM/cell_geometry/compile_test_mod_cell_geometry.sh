#!/bin/bash
ConfigSrc=/home/frosty/projects/2DEulerSolver/Simple_FVM/config.f95
ReadGridSrc=/home/frosty/projects/2DEulerSolver/grid_generation/read_grid/mod_read_grid.f95
CellDataStrucSrc=/home/frosty/projects/2DEulerSolver/Simple_FVM/cell_data_struc/mod_cell_data_struc.f95

gfortran -c $ConfigSrc
gfortran -c $ReadGridSrc
gfortran -c $CellDataStrucSrc
gfortran -c mod_cell_geometry.f95
gfortran -c test_mod_cell_geometry.f95
gfortran test_mod_cell_geometry.o mod_cell_geometry.o mod_read_grid.o config.o -o ./test_mod_cell_geometry
rm *.o
rm *.mod
