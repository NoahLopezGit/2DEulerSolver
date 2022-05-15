#!/bin/bash
ReadGridSrc=/home/frosty/projects/2DEulerSolver/grid_generation/read_grid/mod_read_grid.f95

gfortran -c $ReadGridSrc
gfortran -c mod_cell_geometry.f95
gfortran -c test_mod_cell_geometry.f95
gfortran test_mod_cell_geometry.o mod_cell_geometry.o mod_read_grid.o -o ./test_mod_cell_geometry
rm *.o
rm *.mod
