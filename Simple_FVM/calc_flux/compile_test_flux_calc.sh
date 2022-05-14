#!/bin/bash
ReadGridSrc=/home/frosty/projects/2DEulerSolver/grid_generation/read_grid/module_read_grid.f95

gfortran -c $ReadGridSrc
gfortran -c module_flux_calculation.f95
gfortran -c test_flux_calc.f95
gfortran test_flux_calc.o module_flux_calculation.o module_read_grid.o -o ./test_flux_calc
rm *.o
rm *.mod
