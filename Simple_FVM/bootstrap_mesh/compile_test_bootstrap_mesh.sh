#!/bin/bash
ParentDirectory=$(cd ../../ && pwd)

ConfigSrc="$ParentDirectory"/Simple_FVM/config.f95
ModReadGridSrc="$ParentDirectory"/grid_generation/read_grid/mod_read_grid.f95
ModCellDataStrucSrc="$ParentDirectory"/Simple_FVM/cell_data_struc/mod_cell_data_struc.f95
ModCellGeometrySrc="$ParentDirectory"/Simple_FVM/cell_geometry/mod_cell_geometry.f95
ModMathSrc="$ParentDirectory"/Simple_FVM/mod_math.f95

gfortran -c $ModMathSrc
gfortran -c $ConfigSrc
gfortran -c $ModReadGridSrc
gfortran -c $ModCellDataStrucSrc
gfortran -c $ModCellGeometrySrc
gfortran -c ./bootstrap_mesh.f95
gfortran -c ./test_bootstrap_mesh.f95
gfortran test_bootstrap_mesh.o bootstrap_mesh.o mod_cell_data_struc.o mod_read_grid.o mod_cell_geometry.o config.o mod_math.o -o test_bootstrap_mesh
rm *.o
rm *.mod
